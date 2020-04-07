module letkf
!$$$  module documentation block
!
! module: letkf                        Update model state variables and
!                                      bias coefficients with the LETKF.
!
! prgmmr: ota              org: np23                   date: 2011-06-01
!         updates, optimizations by whitaker
!
! abstract: Updates the model state using the LETKF (Hunt et al 2007,
!  Physica D, 112-126). Uses 'gain form' of LETKF algorithm described
!  Bishop et al 2017 (https://doi.org/10.1175/MWR-D-17-0102.1).
!
!  Covariance localization is used in the state update to limit the impact 
!  of observations to a specified distance from the observation in the
!  horizontal and vertical.  These distances can be set separately in the
!  NH, tropics and SH, and in the horizontal, vertical and time dimensions,
!  using the namelist parameters  corrlengthnh, corrlengthtr, corrlengthsh,
!  lnsigcutoffnh, lnsigcutofftr, lnsigcutoffsh (lnsigcutoffsatnh,
!  lnsigcutoffsattr, lnsigcutoffsatsh for satellite obs, similar for ps obs)
!  obtimelnh, obtimeltr, obtimelsh. The length scales should be given in km for the
!  horizontal, hours for time, and 'scale heights' (units of -log(p/pref)) in the
!  vertical. Note however, that time localization *not used in LETKF*. 
!  The function used for localization (function taper)
!  is imported from module covlocal. Localization requires that
!  every observation have an associated horizontal, vertical and temporal location.
!  For satellite radiance observations the vertical location is given by
!  the maximum in the weighting function associated with that sensor/channel/
!  background state (this computation, along with the rest of the forward
!  operator calcuation, is performed by a separate program using the GSI
!  forward operator code).  
!
!  The parameter nobsl_max controls
!  the maximum number of obs that will be assimilated in each local patch.
!  (the nobsl_max closest are chosen)
!  nobsl_max=-1 (default) means all obs used.
!
!  Vertical covariance localization can be turned off with letkf_novlocal.
!  (this is done automatically when model space vertical localization 
!   with modulated ensembles is enabled via neigv>0)
!  If neigv > 0 the eigenvectors of the localization
!  matrix are read from a file called 'vlocal_eig.dat' (created by an external
!  python utility).
!
!  Updating the state in observation space is not supported in the LETKF - 
!  use lupd_obspace_serial=.true. to perform the observation space update
!  using the serial EnSRF.
!
! Public Subroutines:
!  letkf_update: performs the LETKF update
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               enkf_obsmod, radinfo, radbias, gridinfo
!
! program history log:
!   2011-06-01  ota: Created from Whitaker's serial EnSRF core module.
!   2015-07-25  whitaker: Optimization for case when no vertical localization
!               is used. Fixed missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use kdtree
!               for range search instead of original box routine. Modify
!               ob space update to use weights computed at nearest grid point.
!   2016-02-01  whitaker: Use MPI-3 shared memory pointers to reduce memory
!               footprint by only allocating observation prior ensemble
!               array on one MPI task per node. Also ensure posterior
!               perturbation mean is zero.
!   2016-05-02  shlyaeva: Modification for reading state vector from table.
!   2016-07-05  whitaker: remove buggy code for observation space update.
!               Rely on serial EnSRF to perform observation space update
!               using logical lupd_obspace_serial.
!   2016-11-29  shlyaeva: Modification for using control vector (control and
!               state used to be the same) and the "chunks" come from loadbal
!   2018-05-31  whitaker:  add modulated ensemble model-space vertical
!               localization (when neigv>0).
!               Add options for DEnKF and gain form of LETKF.

!
! attributes:
!   language: f95
!
!$$$

use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max,mpi_realkind,&
                mpi_min,numproc_shm,mpi_comm_shmem,mpi_info_null,nproc_shm,&
                mpi_comm_shmemroot,mpi_mode_nocheck,mpi_lock_exclusive,&
                mpi_address_kind
use, intrinsic :: iso_c_binding
use omp_lib, only: omp_get_num_threads,omp_get_thread_num
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single
use loadbal, only: numptsperproc, npts_max, &
                   indxproc, lnp_chunk, &
                   grdloc_chunk, kdtree_obs2, &
                   ensmean_chunk, anal_chunk, anal_chunk_prior
use controlvec, only: ncdim, index_pres
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobstot, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obsprd_prior,&
                  numobspersat, biaspreds, corrlengthsq,&
                  obtype, anal_ob, anal_ob_modens, obloclat, obloclon, stattype
use constants, only: pi, one, zero, rad2deg, deg2rad, rearth
use params, only: nlevs, sprd_tol, datapath, nanals, &
                  corrlengthnh,corrlengthtr,corrlengthsh,&
                  getkf,getkf_inflation,denkf,nbackgrounds,nobsl_max,&
                  neigv,vlocal_evecs,letkf_rtps
use gridinfo, only: nlevs_pres,lonsgrd,latsgrd,logp,npts,gridloc
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_n_nearest, kdtree2_r_nearest
implicit none

private
public :: letkf_update

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nf,nanal,nens,&
                i,nlev,nrej,npt,nn,ierr
integer(i_kind) nobsl, ngrd1, nobsl2, nthreads, nb, &
                nobslocal_mean,nobslocal_min,nobslocal_max, &
                nobslocal_meanall,nobslocal_minall,nobslocal_maxall
real(r_single)  robslocal_mean,robslocal_min,robslocal_max,re, &
                robslocal_meanall,robslocal_minall,robslocal_maxall,&
                coslatslocal_meanall, coslatslocal_mean
integer(i_kind),allocatable,dimension(:) :: oindex
real(r_single) :: deglat, dist, corrsq, oberrfact, trpa, trpa_raw, coslat
real(r_double) :: t1,t2,t3,t4,t5,tbegin,tend,tmin,tmax,tmean
real(r_kind) r_nanals,r_nanalsm1
real(r_kind) corrlength
logical kdobs
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hxens
real(r_single),allocatable,dimension(:,:) :: obens
real(r_single),allocatable,dimension(:,:,:) :: ens_tmp
real(r_single),allocatable,dimension(:,:) :: wts_ensperts,pa
real(r_single),allocatable,dimension(:) :: wts_ensmean
real(r_kind),allocatable,dimension(:) :: rdiag,rloc,robs_local,coslats_local
real(r_single),allocatable,dimension(:) :: dep
! kdtree stuff
type(kdtree2_result),dimension(:),allocatable :: sresults
integer(i_kind), dimension(:), allocatable :: indxassim, indxob
real(r_kind) eps

eps = epsilon(0.0_r_single) ! real(4) machine precision
re = rearth/1.e3_r_single

!$omp parallel
nthreads = omp_get_num_threads()
!$omp end parallel

if (nproc == 0) print *,'using',nthreads,' openmp threads'

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

kdobs=associated(kdtree_obs2)
if (.not. kdobs .and. nproc .eq. 0) then
  print *,'using brute-force search instead of kdtree in LETKF'
endif

if (neigv > 0) then
   nens = nanals*neigv ! modulated ensemble size
else
   nens = nanals
endif

nrej=0

tbegin = mpi_wtime()

t2 = zero
t3 = zero
t4 = zero
t5 = zero
tbegin = mpi_wtime()
nobslocal_max = -999
nobslocal_min = nobstot
nobslocal_mean = 0
allocate(robs_local(npts_max))
robs_local = 0
if (nobsl_max > 0) then
  allocate(coslats_local(npts_max))
  coslats_local = 0
endif

! Update ensemble on model grid.
! Loop for each horizontal grid points on this task.
!$omp parallel do schedule(dynamic) private(npt,nob,nobsl, &
!$omp                  nobsl2,oberrfact,ngrd1,corrlength,ens_tmp, &
!$omp                  nf,obens,indxassim,indxob, &
!$omp                  nn,hxens,wts_ensmean,rdiag,dep,rloc,i, &
!$omp                  oindex,deglat,dist,corrsq,nb,sresults, &
!$omp                  wts_ensperts,pa,trpa,trpa_raw) 
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()
   if (.not. allocated(ens_tmp)) allocate(ens_tmp(nens,ncdim,nbackgrounds))
   ! find obs close to this grid point (using kdtree)
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
   coslat = cos(latsgrd(ngrd1))
   corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
   corrsq = corrlength**2
   allocate(sresults(nobstot))
   do nb=1,nbackgrounds
      do i=1,ncdim ! state space ensemble spread for column being updated
         nlev = index_pres(i) ! vertical index for i'th control variable
         if (nlev .eq. nlevs+1) nlev=1 ! 2d fields, assume surface
         if (neigv > 0 ) then
            call expand_ens(neigv,nanals, &
                            anal_chunk(1:nanals,npt,i,nb), &
                            ens_tmp(:,i,nb),vlocal_evecs(:,nlev))
         else
            ens_tmp(:,i,nb) = anal_chunk(:,npt,i,nb)
         endif
      enddo
   enddo
   ! kd-tree fixed range search
   !if (allocated(sresults)) deallocate(sresults)
   if (nobsl_max > 0) then ! only use nobsl_max nearest obs (sorted by distance).
       if (kdobs) then
          call kdtree2_n_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),nn=nobsl_max,&
               results=sresults)
          nobsl = nobsl_max
       else
          ! brute force search
          call find_localobs(grdloc_chunk(:,npt),obloc,corrsq,nobstot,nobsl_max,sresults,nobsl)
       endif
       !if (nproc == 0 .and. npt == 1) then
       !   do nob=1,nobsl
       !       print *,nob,sresults(nob)%idx,sqrt(sresults(nob)%dis/corrlengthsq(sresults(nob)%idx)),obtype(sresults(nob)%idx)
       !    enddo
       !endif
   else ! find all obs within localization radius (sorted by distance).
       if (kdobs) then
         call kdtree2_r_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),r2=corrsq,&
              nfound=nobsl,nalloc=nobstot,results=sresults)
       else
         ! brute force search
         call find_localobs(grdloc_chunk(:,npt),obloc,corrsq,nobstot,-1,sresults,nobsl)
       endif
   endif

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observations in local area
   if(nobsl == 0) then
      if (allocated(sresults)) deallocate(sresults)
      if (allocated(ens_tmp)) deallocate(ens_tmp)
      cycle grdloop
   endif
   if (nobsl_max > 0) then
      robs_local(npt) = sqrt(sresults(nobsl)%dis)
      coslats_local(npt) = coslat
   else
      robs_local(npt) = nobsl
   endif

   ! Pick up variables passed to LETKF core process
   allocate(rloc(nobsl))
   allocate(oindex(nobsl))
   nobsl2=1
   do nob=1,nobsl
      nf = sresults(nob)%idx
      ! skip 'screened' obs.
      if (oberrvar(nf) > 1.e10_r_single) cycle
      dist = sqrt(sresults(nob)%dis/corrlengthsq(nf))
      if (dist >= one) cycle
      rloc(nobsl2)=taper(dist)
      oindex(nobsl2)=nf
      if(rloc(nobsl2) > eps) nobsl2=nobsl2+1
   end do
   nobsl2=nobsl2-1
   ! if there are any local obs, calculate increment.
   if(nobsl2 > 0) then
      allocate(hxens(nens,nobsl2))
      allocate(obens(nanals,nobsl2))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
         if (neigv > 0) then
         hxens(1:nens,nob)=anal_ob_modens(1:nens,nf) 
         else
         hxens(1:nens,nob)=anal_ob(1:nens,nf) 
         endif
         obens(1:nanals,nob) = &
         anal_ob(1:nanals,nf) 
         rdiag(nob)=one/oberrvar(nf)
         dep(nob)=ob(nf)-ensmean_ob(nf)
      end do
      deallocate(oindex)
      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()
      ! use gain form of LETKF (to make modulated ensemble vertical localization
      ! possible)
      allocate(wts_ensperts(nens,nanals),wts_ensmean(nens))
      ! compute analysis weights for mean and ensemble perturbations given 
      ! ensemble in observation space, ob departures and ob errors.
      ! note: if modelspace_vloc=F, hxens and obens are identical (but hxens is
      ! is used as workspace and is modified on output), and analysis
      ! weights for ensemble perturbations represent posterior ens perturbations, not
      ! analysis increments for ensemble perturbations.
      !if (nproc == numproc-1 .and. omp_get_thread_num() == 0) then
      !call letkf_core(nobsl2,hxens,obens,dep,&
      !                wts_ensmean,wts_ensperts,pa,&
      !                rdiag,rloc(1:nobsl2),nens,nens/nanals,getkf_inflation,&
      !                denkf,getkf,letkf_rtps,.true.)
      !else
      call letkf_core(nobsl2,hxens,obens,dep,&
                      wts_ensmean,wts_ensperts,pa,&
                      rdiag,rloc(1:nobsl2),nens,nens/nanals,getkf_inflation,&
                      denkf,getkf,letkf_rtps)
      !endif

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles (all time levels)
      ! analysis increments represented as a linear combination
      ! of (modulated) prior ensemble perturbations.
      do nb=1,nbackgrounds
      do i=1,ncdim
         ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + &
         sum(wts_ensmean*ens_tmp(:,i,nb))
         if (getkf) then ! gain formulation
            do nanal=1,nanals 
               anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) + &
               sum(wts_ensperts(:,nanal)*ens_tmp(:,i,nb))
            enddo
            if (.not. denkf .and. getkf_inflation) then
               ! inflate posterior perturbations so analysis variance 
               ! in original low-rank ensemble is the same as modulated ensemble
               ! (eqn 30 in https://doi.org/10.1175/MWR-D-17-0102.1)
               trpa = 0.0_r_single
               do nanal=1,nens
                  trpa = trpa + &
                  sum(pa(:,nanal)*ens_tmp(:,i,nb))*ens_tmp(nanal,i,nb)
               enddo
               trpa = max(eps,trpa)
               trpa_raw = max(eps,r_nanalsm1*sum(anal_chunk(:,npt,i,nb)**2))
               anal_chunk(:,npt,i,nb) = sqrt(trpa/trpa_raw)*anal_chunk(:,npt,i,nb)
               !if (nproc == 0 .and. omp_get_thread_num() == 0 .and. i .eq. ncdim) print *,'i,trpa,trpa_raw,inflation = ',i,trpa,trpa_raw,sqrt(trpa/trpa_raw)
            endif
         else ! original LETKF formulation
            do nanal=1,nanals 
               anal_chunk(nanal,npt,i,nb) = &
               sum(wts_ensperts(:,nanal)*ens_tmp(:,i,nb))
            enddo
         endif
         !if (nproc .eq. 0 .and. npt .eq. 1) then
         !  print *,'sprd',nb,i,r_nanalsm1*sum(anal_chunk(:,npt,i,nb)**2),&
         !         r_nanalsm1*sum(anal_chunk_prior(:,npt,i,nb)**2)
         !endif
      enddo
      enddo
      deallocate(wts_ensperts,wts_ensmean,dep,obens,rloc,rdiag,hxens)
      if (allocated(pa)) deallocate(pa)
   else ! nobsl2 > 0
      deallocate(rloc,oindex)
   endif
   t5 = t5 + mpi_wtime() - t1
   t1 = mpi_wtime()
   if (allocated(sresults)) deallocate(sresults)
   if (allocated(ens_tmp)) deallocate(ens_tmp)
end do grdloop
!$omp end parallel do

! make sure posterior perturbations still have zero mean.
! (roundoff errors can accumulate)
!$omp parallel do schedule(dynamic) private(npt,nb,i)
do npt=1,npts_max
   do nb=1,nbackgrounds
      do i=1,ncdim
         anal_chunk(1:nanals,npt,i,nb) = anal_chunk(1:nanals,npt,i,nb)-&
         sum(anal_chunk(1:nanals,npt,i,nb),1)*r_nanals
      end do
   end do
enddo
!$omp end parallel do

tend = mpi_wtime()
call mpi_reduce(tend-tbegin,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(tend-tbegin,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(tend-tbegin,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,'min/max/mean time to do letkf update ',tmin,tmax,tmean
t2 = t2/nthreads; t3 = t3/nthreads; t4 = t4/nthreads; t5 = t5/nthreads
if (nproc == 0) print *,'time to process analysis on gridpoint = ',t2,t3,t4,t5,' secs on task',nproc
call mpi_reduce(t2,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t2,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t2,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t2 = ',tmin,tmax,tmean
call mpi_reduce(t3,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t3,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t3,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t3 = ',tmin,tmax,tmean
call mpi_reduce(t4,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t4,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t4,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t4 = ',tmin,tmax,tmean
call mpi_reduce(t5,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t5,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t5,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t5 = ',tmin,tmax,tmean
if (nobsl_max > 0) then
   ! compute and print min/max/mean search radius to find nobsl_max
   robslocal_mean = sum(robs_local*coslats_local)/numptsperproc(nproc+1)
   coslatslocal_mean = sum(coslats_local)/numptsperproc(nproc+1)
   robslocal_min = minval(robs_local(1:numptsperproc(nproc+1)))
   robslocal_max = maxval(robs_local(1:numptsperproc(nproc+1)))
   call mpi_reduce(robslocal_max,robslocal_maxall,1,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
   call mpi_reduce(robslocal_min,robslocal_minall,1,mpi_real4,mpi_min,0,mpi_comm_world,ierr)
   call mpi_reduce(robslocal_mean,robslocal_meanall,1,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
   call mpi_reduce(coslatslocal_mean,coslatslocal_meanall,1,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
   if (nproc == 0) print *,'min/max/mean distance searched for local obs',re*robslocal_minall,re*robslocal_maxall,re*robslocal_meanall/coslatslocal_meanall
   deallocate(coslats_local)
else
   ! compute and print min/max/mean number of obs found within search radius
   nobslocal_mean = nint(sum(robs_local)/numptsperproc(nproc+1))
   nobslocal_min = minval(robs_local(1:numptsperproc(nproc+1)))
   nobslocal_max = maxval(robs_local(1:numptsperproc(nproc+1)))
   call mpi_reduce(nobslocal_max,nobslocal_maxall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
   call mpi_reduce(nobslocal_min,nobslocal_minall,1,mpi_integer,mpi_min,0,mpi_comm_world,ierr)
   call mpi_reduce(nobslocal_mean,nobslocal_meanall,1,mpi_integer,mpi_sum,0,mpi_comm_world,ierr)
   if (nproc == 0) print *,'min/max/mean number of obs in local volume',nobslocal_minall,nobslocal_maxall,nint(nobslocal_meanall/float(numproc))
endif
deallocate(robs_local)
if (nrej > 0 .and. nproc == 0) print *, nrej,' obs rejected by varqc'
  
if (allocated(ens_tmp)) deallocate(ens_tmp)

return

end subroutine letkf_update

subroutine letkf_core(nobsl,hxens,hxens_orig,dep,&
                      wts_ensmean,wts_ensperts,paens,&
                      rdiaginv,rloc,nanals,neigv,getkf_inflation,denkf,getkf,&
                      rtps,debug)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    letkf_core
!
!   prgmmr: whitaker
!
! abstract:  LETKF core subroutine. Returns analysis weights
! for ensemble mean update and ensemble perturbation update (increments
! represented as a linear combination of prior ensemble perturbations).
! Uses 'gain form' LETKF, which works when ensemble used to estimate
! covariances not the same as ensemble being updated.  If neigv=1
! (no modulated ensemble model-space localization), then the 
! traditional form of the LETKF analysis weights for ensemble
! perturbations are returned (which represents posterior
! ensemble perturbations, not analysis increments, as a linear
! combination of prior ensemble perturbations).
!
! program history log:
!   2011-06-03  ota: created from miyoshi's LETKF core subroutine
!   2014-06-20  whitaker: Use LAPACK routine dsyev for eigenanalysis.
!   2016-02-01  whitaker: Use LAPACK dsyevr for eigenanalysis (faster
!               than dsyev in most cases). 
!   2018-07-01  whitaker: implement gain form of LETKF from Bishop et al 2017
!   (https://doi.org/10.1175/MWR-D-17-0102.1), allow for use of modulated 
!   ensemble vert localization (ensemble used to estimate posterior covariance
!   in ensemble space different than ensemble being updated). Add denkf,
!   getkf,getkf_inflation options.
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hxens    - on input: first-guess modulated ensemble in observation space (Yb)
!                on output: overwritten with Yb * R**-1.
!     hxens_orig  - first-guess original ensembles in observation space.
!                   not used if neigv=1. 
!     dep      - nobsl observation departures (y-Hxmean)
!     rdiaginv - inverse of diagonal element of observation error covariance
!     rloc     - localization function for each ob (based on distance to
!                analysis point)
!     nanals   - number of ensemble members (1st dimension of hxens)
!     neigv    - for modulated ensemble model-space localization, number
!                of eigenvectors of vertical localization (1 if not using
!                model space localization).  1st dimension of hxens_orig is 
!                nanals/neigv.
!     getkf_inflation - if true (and getkf=T,denkf=F), 
!                return posterior covariance matrix in
!                needed to compute getkf inflation (eqn 30 in Bishop et al
!                2017).
!     denkf - if true, use DEnKF approximation (implies getkf=T)
!             See Sakov and Oke 2008 https://doi.org/10.1111/j.1600-0870.2007.00299.x
!     getkf - if true, use gain formulation
!     rtps  - relaxation coefficient for relaxation-to-prior spread inflation.
!
!   output argument list:
!
!     wts_ensmean - Factor used to compute ens mean analysis increment
!       by pre-multiplying with
!       model space ensemble perts. In notation from Bishop et al 2017,
!       wts_ensmean = C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - Hxmean)
!       where HZ^T = Yb*R**-1/2 (YbRinvsqrt),
!       C are eigenvectors of (HZ)^T HZ and Gamma are eigenvalues
!       Has dimension (nanals) - increment is weighted average of ens
!       perts, wts_ensmean are weights. 
!
!     wts_ensperts  - if getkf=T same as above, but for computing increments to 
!       ensemble perturbations. From Bishop et al 2017 eqn 29
!       wts_ensperts = -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 Hxprime
!       Has dimension (nanals,nanals/neigv), analysis weights for each
!       member. Hxprime (hxens_orig) is the original, unmodulated
!       ensemble in observation space, HZ is the modulated ensemble in
!       ob space times R**-1/2. If denkf=T, wts_ensperts is approximated
!       as wts_ensperts = -0.5*C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 Hxprime
!       If getkf=F and denkf=F, then the original LETKF formulation is used with
!       wts_ensperts =
!       C (Gamma + I)**-1/2 C^T (square root of analysis error cov in ensemble space)
!       and these weights are applied to transform the background ensemble into an
!       analysis ensemble.  Note that modulated ensemble vertical localization
!       requires the gain form (getkf=T and/or denkf=T) since this form of the weights
!       requires that the background ensemble used to compute covariances is
!       the same ensemble being updated.
!    
!     paens - only allocated and returned
!       if getkf_inflation=T (and denkf=F).  In this case
!       paens is allocated dimension (nanals,nanals) and contains posterior 
!       covariance matrix in (modulated) ensemble space.
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind), intent(in) :: nobsl,nanals,neigv
logical, intent(in), optional :: debug
real(r_single),intent(in) :: rtps
real(r_kind),dimension(nobsl),intent(in ) :: rdiaginv,rloc
real(r_kind),dimension(nanals,nobsl),intent(inout)  :: hxens
real(r_single),dimension(nanals/neigv,nobsl),intent(in)  :: hxens_orig
real(r_single),dimension(nobsl),intent(in)  :: dep
real(r_single),dimension(nanals),intent(out)  :: wts_ensmean
real(r_single),dimension(nanals,nanals/neigv),intent(out)  :: wts_ensperts
real(r_single),dimension(:,:),allocatable, intent(inout) :: paens
! local variables.
real(r_kind),allocatable,dimension(:,:) :: work3,evecs
real(r_single),allocatable,dimension(:,:) :: swork2,pa,swork3,shxens
real(r_single),allocatable,dimension(:) :: swork1
real(r_kind),allocatable,dimension(:) :: &
             rrloc,evals,gammapI,gamma_inv,gammapI_inv
real(r_kind) eps
real(r_single) analsprd,analsprd2
integer(i_kind) :: nanal,ierr,lwork,liwork
!for LAPACK dsyevr
integer(i_kind) isuppz(2*nanals)
real(r_kind) vl,vu,normfact
integer(i_kind), allocatable, dimension(:) :: iwork
real(r_kind), dimension(:), allocatable :: work1
logical, intent(in) :: getkf_inflation,denkf,getkf

if (neigv < 1) then
  print *,'neigv must be >=1 in letkf_core'
  call stop2(992)
endif

allocate(work3(nanals,nanals),evecs(nanals,nanals))
allocate(rrloc(nobsl),gammapI(nanals),evals(nanals),gamma_inv(nanals))
allocate(gammapI_inv(nanals))
! for dsyevr
allocate(iwork(10*nanals),work1(70*nanals))
! for dsyevd
!allocate(iwork(3+5*nanals),work1(1+6*nanals+2*nanals*nanals))

! HZ^T = hxens sqrt(Rinv)
rrloc = rdiaginv * rloc
eps = epsilon(0.0_r_single)
where (rrloc < eps) rrloc = eps
rrloc = sqrt(rrloc)
normfact = sqrt(real((nanals/neigv)-1,r_kind))
! normalize so dot product is covariance
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * &
   rrloc(1:nobsl)/normfact
end do

! compute eigenvectors/eigenvalues of HZ^T HZ (left SV)
! (in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl)
lwork = size(work1); liwork = size(iwork)
if(r_kind == kind(1.d0)) then ! double precision 
   !work3 = matmul(hxens,transpose(hxens))
   call dgemm('n','t',nanals,nanals,nobsl,1.d0,hxens,nanals, &
               hxens,nanals,0.d0,work3,nanals)
   ! evecs contains eigenvectors of HZ^T HZ, or left singular vectors of HZ
   ! evals contains eigenvalues (singular values squared)
   call dsyevr('V','A','L',nanals,work3,nanals,vl,vu,1,nanals,-1.d0,nanals,evals,evecs, &
               nanals,isuppz,work1,lwork,iwork,liwork,ierr)
! use LAPACK dsyevd instead of dsyevr
   !evecs = work3
   !call dsyevd('V','L',nanals,evecs,nanals,evals,work1,lwork,iwork,liwork,ierr)
else ! single precision
   call sgemm('n','t',nanals,nanals,nobsl,1.e0,hxens,nanals, &
               hxens,nanals,0.e0,work3,nanals)
   call ssyevr('V','A','L',nanals,work3,nanals,vl,vu,1,nanals,-1.e0,nanals,evals,evecs, &
               nanals,isuppz,work1,lwork,iwork,liwork,ierr)
! use LAPACK dsyevd instead of dsyevr
   !evecs = work3
   !call ssyevd('V','L',nanals,evecs,nanals,evals,work1,lwork,iwork,liwork,ierr)
end if
if (ierr .ne. 0) print *,'warning: dsyev* failed, ierr=',ierr
deallocate(work1,iwork,work3) ! no longer needed
gamma_inv = 0.0_r_kind
do nanal=1,nanals
   if (evals(nanal) > eps) then
       gamma_inv(nanal) = 1./evals(nanal)
   else
       evals(nanal) = 0.0_r_kind
   endif
enddo
! gammapI used in calculation of posterior cov in ensemble space

gammapI = evals+1.0
gammapI_inv = 1./gammaPI
! relax eigenspectrum back to prior if rtps > 0
! TODO: find out why this doesn't work as well as posterior RTPS
if (rtps > eps) then
   if (present(debug)) then
      analsprd = sum(gammapI_inv)/float(nanals)
   endif
   gammapI = gammapI/(rtps*evals+1.0)
   gammapI_inv = 1./gammaPI
   if (present(debug)) then
      analsprd2 = sum(gammapI_inv)/float(nanals)
      print *,'sprd:',analsprd,(1.-rtps)*analsprd+rtps,analsprd2
   endif
endif

! create HZ^T R**-1/2 
allocate(shxens(nanals,nobsl))
do nanal=1,nanals
   shxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
deallocate(rrloc)

! compute factor to multiply with model space ensemble perturbations
! to compute analysis increment (for mean update), save in single precision.
! This is the factor C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - HXmean)
! in Bishop paper (eqs 10-12).

allocate(swork3(nanals,nanals),swork2(nanals,nanals),pa(nanals,nanals))
do nanal=1,nanals
   swork3(nanal,:) = evecs(nanal,:)/(evals+1.)
   swork2(nanal,:) = evecs(nanal,:)
enddo
deallocate(evals)

! pa = C (Gamma + I)**-1 C^T (analysis error cov in ensemble space)
!pa = matmul(swork3,transpose(swork2))
call sgemm('n','t',nanals,nanals,nanals,1.e0,swork3,nanals,swork2,&
            nanals,0.e0,pa,nanals)
! work1 = (HZ)^ T R**-1/2 (y - HXmean)
! (nanals, nobsl) x (nobsl,) = (nanals,)
! in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl
allocate(swork1(nanals))
do nanal=1,nanals
   swork1(nanal) = sum(shxens(nanal,:)*dep(:))
end do
! wts_ensmean = C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - HXmean)
! (nanals, nanals) x (nanals,) = (nanals,)
do nanal=1,nanals
   wts_ensmean(nanal) = sum(pa(nanal,:)*swork1(:))/normfact
end do

! recompute Pa if rtps used
if (rtps > eps) then
   do nanal=1,nanals
      swork3(nanal,:) = evecs(nanal,:)/gammapI
   enddo
   ! pa = C (Gamma + I)**-1 C^T (analysis error cov in ensemble space)
   !pa = matmul(swork3,transpose(swork2))
   call sgemm('n','t',nanals,nanals,nanals,1.e0,swork3,nanals,swork2,&
               nanals,0.e0,pa,nanals)
endif

if (.not. denkf .and. getkf_inflation) then
   allocate(paens(nanals,nanals))
   paens = pa/normfact**2
endif
deallocate(swork1)

! compute factor to multiply with model space ensemble perturbations
! to compute analysis increment (for perturbation update), save in single precision.
! This is -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 HXprime
! in Bishop paper (eqn 29).
! For DEnKF factor is -0.5*C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 HXprime
! = -0.5 Pa (HZ)^ T R**-1/2 HXprime (Pa already computed)

if (getkf) then ! use Gain formulation for LETKF weights

if (denkf) then
   ! use Pa = C (Gamma + I)**-1 C^T (already computed)
   ! wts_ensperts = -0.5 Pa (HZ)^ T R**-1/2 HXprime
   pa = 0.5*pa
else
   gammapI = sqrt(1.0/gammapI)
   do nanal=1,nanals
      swork3(nanal,:) = &
      evecs(nanal,:)*(1.-gammapI(:))*gamma_inv(:)
   enddo
   ! swork2 still contains eigenvectors, over-write pa
   ! pa = C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T
   !pa = matmul(swork3,transpose(swork2))
   call sgemm('n','t',nanals,nanals,nanals,1.e0,swork3,nanals,swork2,&
               nanals,0.e0,pa,nanals)
endif
deallocate(swork2,swork3)

! work2 = (HZ)^ T R**-1/2 HXprime
! (nanals, nobsl) x (nobsl, nanals/neigv) = (nanals, nanals/neigv)
! in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl
! HXprime in paper is nobsl, nanals/neigv here it is nanals/neigv, nobsl
allocate(swork2(nanals,nanals/neigv))
!swork2 = matmul(shxens,transpose(hxens_orig))
call sgemm('n','t',nanals,nanals/neigv,nobsl,1.e0,&
            shxens,nanals,hxens_orig,nanals/neigv,0.e0,swork2,nanals)
! wts_ensperts = -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 HXprime
! (nanals, nanals) x (nanals, nanals/eigv) = (nanals, nanals/neigv)
! if denkf, wts_ensperts = -0.5 C (Gamma + I)**-1 C^T (HZ)^T R**-1/2 HXprime
!wts_ensperts = -matmul(pa, swork2)/normfact
call sgemm('n','n',nanals,nanals/neigv,nanals,-1.e0,&
            pa,nanals,swork2,nanals,0.e0,wts_ensperts,nanals)
wts_ensperts = wts_ensperts/normfact

! clean up
deallocate(shxens,swork2,pa)

else  ! use original LETKF formulation (won't work if neigv != 1)

if (neigv > 1) then
  print *,'neigv must be 1 in letkf_core if getkf=F'
  call stop2(993)
endif
! compute sqrt(Pa) - analysis weights
! (apply to prior ensemble to determine posterior ensemble,
!  not analysis increments as in Gain formulation)
! hxens_orig not used
! saves two matrix multiplications (nanals, nobsl) x (nobsl, nanals) and
! (nanals, nanals) x (nanals, nanals)
deallocate(shxens,pa)
gammapI = sqrt(1.0/gammapI)
do nanal=1,nanals
   swork3(nanal,:) = evecs(nanal,:)*gammapI
enddo
! swork2 already contains evecs
! wts_ensperts = 
! C (Gamma + I)**-1/2 C^T (square root of analysis error cov in ensemble space)
!wts_ensperts = matmul(swork3,transpose(swork2))
call sgemm('n','t',nanals,nanals,nanals,1.0,swork3,nanals,swork2,&
            nanals,0.e0,wts_ensperts,nanals)
deallocate(swork3,swork2)

endif

deallocate(evecs,gammapI,gamma_inv,gammapI_inv)

return
end subroutine letkf_core

subroutine find_localobs(grdloc,obloc,rsqmax,nobstot,nobsl_max,sresults,nobsl)
   ! brute force nearest neighbor search
   ! if nobsl_max == -1, a r_nearest search is performed, finding
   ! all neighbors with squared distance rsq.
   ! if nobsl_max > 0, a n_nearest search is performed, finding
   ! the nobsl_max nearest neighbors (rsq is ignored).
   ! inputs:
   ! grdloc = x,y,z (spherical cartesian coordinate) location
   ! for the search.  Chordal (not great circle) distance is used.
   ! obloc(3,nobstot) = x,y,z locations of nobstot obs to be
   ! searched.
   ! rsqmax = r=x**2+y**2+z**2 search radius (ignored if nobsl_max > 0)
   ! nobsl_max = number of neighbors to find (if -1 find all).
   ! nobstot = total number of obs to search.
   ! outputs:
   ! nobsl = number of neighbors found.
   ! sresults = search result structure (same as used by kdtree). Results
   ! are sorted by distance (closest neighbors first).
   implicit none
   integer, intent(in) :: nobsl_max, nobstot
   real(r_single), intent(in) :: rsqmax
   real(r_single), intent(in) :: grdloc(3)
   real(r_single), intent(in) :: obloc(3,nobstot)
   type(kdtree2_result),intent(inout) :: sresults(nobstot)
   integer, intent(out) :: nobsl
   ! local variables.
   real(r_single) rsq(nobstot)
   integer(i_kind) indxob(nobstot)
   integer nob

   ! compute squared distances.
   do nob = 1, nobstot
      rsq(nob) = sum( (grdloc(:)-obloc(:,nob))**2, 1)
   enddo
   ! create index of sorted distances.
   indxob = argsort(rsq)
   ! return all neigbhors closer than rsqmax
   if (nobsl_max == -1) then 
      nobsl = 0
      do nob=1,nobstot
         if (rsq(indxob(nob)) > rsqmax) then
            nobsl=nob
            exit
         end if
      enddo
   ! return nobls_max nearest neighbors
   else
      if (nobsl_max > nobstot) then
         print *,'nobsl_max must be <= nobstot in find_localobs'
         call stop2(992)
      else if (nobsl_max < 1) then
         print *,'nobsl_max must be -1 or >= 1 in find_localobs'
         call stop2(992)
      endif
      nobsl = nobsl_max
   endif
   ! fill search results up to nobsl in order of increasing distance
   do nob=1,nobsl
      sresults(nob)%idx = indxob(nob)
      sresults(nob)%dis = rsq(indxob(nob))
   enddo

end subroutine find_localobs

function argsort(r) result(d)
! from https://github.com/Astrokiwi/simple_fortran_argsort
    implicit none
    real(r_single), intent(in), dimension(:) :: r
    integer, dimension(size(r)) :: d
    integer, dimension(size(r)) :: il
    integer :: stepsize
    integer :: i,j,left,n,k,ksize
    n = size(r)
    do i=1,n
        d(i)=i
    end do
    if ( n==1 ) return
    stepsize = 1
    do while (stepsize<n)
        do left=1,n-stepsize,stepsize*2
            i = left
            j = left+stepsize
            ksize = min(stepsize*2,n-left+1)
            k=1
            do while ( i<left+stepsize .and. j<left+ksize )
                if ( r(d(i))<r(d(j)) ) then
                    il(k)=d(i)
                    i=i+1
                    k=k+1
                else
                    il(k)=d(j)
                    j=j+1
                    k=k+1
                endif
            enddo
            if ( i<left+stepsize ) then
                ! fill up remaining from left
                il(k:ksize) = d(i:left+stepsize-1)
            else
                ! fill up remaining from right
                il(k:ksize) = d(j:left+ksize-1)
            endif
            d(left:left+ksize-1) = il(1:ksize)
        end do
        stepsize=stepsize*2
    end do
end function argsort

end module letkf
