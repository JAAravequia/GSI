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
!  Physica D, 112-126).
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
!  forward operator code).  Although all the observation variable ensemble
!  members sometimes cannot fit in memory, they are necessary before LETKF core
!  process. So they are saved in all processors.
!
!  Adaptive observation thinning implemented in the serial EnSRF is not 
!  implemented here in the current version.

!  Updating the state in observation space is not supported in the LETKF - 
!  use lupd_obspace_serial=.true. to perform the observation space update
!  using the serial EnSRF.
!
! Public Subroutines:
!  letkf_update: performs the LETKF update (calls update_biascorr to perform
!   the bias coefficient update).  
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
!               localization (when neigv>0) and ob selection using DFS 
!               (when dfs_sort=T). Add options for DEnKF and gain form of LETKF.

!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use random_normal, only : rnorm, set_random_seed
use, intrinsic :: iso_c_binding
use omp_lib, only: omp_get_num_threads
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single
use loadbal, only: numptsperproc, npts_max, &
                   indxproc, lnp_chunk, &
                   grdloc_chunk, kdtree_obs2, &
                   ensmean_chunk, anal_chunk
use controlvec, only: ncdim, index_pres
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobstot, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obfit_post, obsprd_prior, obsprd_post,&
                  numobspersat, deltapredx, biaspreds, corrlengthsq,&
                  biasprednorm, probgrosserr, prpgerr, obtype, obpress,&
                  lnsigl, anal_ob, anal_ob_modens, obloclat, obloclon, stattype
use constants, only: pi, one, zero, rad2deg, deg2rad
use params, only: sprd_tol, datapath, nanals, iseed_perturbed_obs,&
                  iassim_order,sortinc,deterministic,nlevs,&
                  zhuberleft,zhuberright,varqc,lupd_satbiasc,huber,letkf_novlocal,&
                  lupd_obspace_serial,corrlengthnh,corrlengthtr,corrlengthsh,&
                  nbackgrounds,nobsl_max,neigv,vlocal_evecs,denkf,gletkf,dfs_sort
use radinfo, only: npred,nusis,nuchan,jpch_rad,predx
use radbias, only: apply_biascorr, update_biascorr
use gridinfo, only: nlevs_pres,lonsgrd,latsgrd,logp,npts,gridloc
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_n_nearest, kdtree2_r_nearest
use sorting, only: quicksort, isort

implicit none

private
public :: letkf_update

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nf,nanal,nens,&
                i,nlev,nrej,npt,nn,nnmax,ierr,nsvals
integer(i_kind) nobsl, ngrd1, nobsl2, nthreads, nb, &
                nobslocal_min,nobslocal_max, &
                nobslocal_minall,nobslocal_maxall
integer(i_kind),allocatable,dimension(:) :: oindex
real(r_single) :: deglat, dist, corrsq, oberrfact, gain, r
real(r_double) :: t1,t2,t3,t4,t5,tbegin,tend,tmin,tmax,tmean
real(r_kind) r_nanals,r_nanalsm1
real(r_kind) normdepart, pnge, width
real(r_kind),dimension(nobstot):: oberrvaruse
real(r_kind) vdist
real(r_kind) corrlength
real(r_kind) sqrtoberr
logical vlocal, kdobs, compute_weights
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hxens
real(r_single),allocatable,dimension(:,:,:) :: ens_tmp
real(r_single),allocatable,dimension(:,:) :: obperts,obens,hxenss
real(r_single),allocatable,dimension(:) :: kfgain, dfs, reducedgain
real(r_kind),allocatable,dimension(:) :: rdiag,dep,rloc,statesprd_prior
real(r_kind),allocatable,dimension(:,:) :: trans,u,vt
real(r_kind),dimension(nanals) :: work,work2
! kdtree stuff
type(kdtree2_result),dimension(:),allocatable :: sresults
integer(i_kind), dimension(:), allocatable :: indxassim, indxob
#ifdef MPI3
! pointers used for MPI-3 shared memory manipulations.
real(r_single), pointer, dimension(:,:) :: anal_ob_fp ! Fortran pointer
type(c_ptr)                             :: anal_ob_cp ! C pointer
real(r_single), pointer, dimension(:,:) :: anal_ob_modens_fp ! Fortran pointer
type(c_ptr)                             :: anal_ob_modens_cp ! C pointer
real(r_single), pointer, dimension(:,:) :: obperts_fp ! Fortran pointer
type(c_ptr)                             :: obperts_cp ! C pointer
integer disp_unit, shm_win, shm_win2, shm_win3
integer(MPI_ADDRESS_KIND) :: win_size, nsize, nsize2, win_size2
integer(MPI_ADDRESS_KIND) :: segment_size
#endif
real(r_single), allocatable, dimension(:) :: buffer
real(r_kind) eps

eps = epsilon(0.0_r_single) ! real(4) machine precision

!$omp parallel
nthreads = omp_get_num_threads()
!$omp end parallel

compute_weights = .not. denkf .and. deterministic
if (nproc == 0) print *,'using',nthreads,' openmp threads'
if (nproc == 0 .and. gletkf) print *,'using gain form of LETKF'
if (nproc == 0 .and. denkf) print *,'using DEnKF form of LETKF'
if (nproc == 0 .and. .not. deterministic) print *,'using perturbed obs form of LETKF'
if (nproc == 0 .and. .not. compute_weights) print *,'LETKF transformation matrix will not be computed'


! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

kdobs=associated(kdtree_obs2)

! create random numbers for perturbed obs on root task.
if (.not. deterministic .and. nproc .eq. 0) then
   call set_random_seed(iseed_perturbed_obs,nproc)
   allocate(obperts(nanals, nobstot))
   do nob=1,nobstot
      sqrtoberr=sqrt(oberrvar(nob))
      do nanal=1,nanals
         obperts(nanal,nob) = sqrtoberr*rnorm()
      enddo
      ! make mean/variance are exact.
      obperts(1:nanals,nob) = obperts(1:nanals,nob) - &
                              sum(obperts(:,nob))*r_nanals
      obperts(1:nanals,nob) = obperts(1:nanals,nob)*sqrtoberr/(sqrt(sum(obperts(:,nob)**2)*r_nanalsm1))
   enddo
endif

t1 = mpi_wtime()
#ifdef MPI3
! setup shared memory segment on each node that points to
! observation prior ensemble.
! shared window size will be zero except on root task of
! shared memory group on each node.
disp_unit = num_bytes_for_r_single ! anal_ob is r_single
nsize = nobstot*nanals
nsize2 = nobstot*nanals*neigv
if (neigv > 0) then
   nens = nanals*neigv
else
   nens = nanals
endif
if (nproc_shm == 0) then
   win_size = nsize*disp_unit
   win_size2 = nsize2*disp_unit
else
   win_size = 0
   win_size2 = 0
endif
call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                             mpi_comm_shmem, anal_ob_cp, shm_win, ierr)
if (neigv > 0) then
   call MPI_Win_allocate_shared(win_size2, disp_unit, MPI_INFO_NULL,&
                                mpi_comm_shmem, anal_ob_modens_cp, shm_win3, ierr)
endif
if (.not. deterministic) then
   call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                                mpi_comm_shmem, obperts_cp, shm_win2, ierr)
endif
if (nproc_shm == 0) then
   ! create shared memory segment on each shared mem comm
   call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win,ierr)
   call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
   ! bcast entire obs prior ensemble from root task 
   ! to a single task on each node, assign to shared memory window.
   ! send one ensemble member at a time.
   allocate(buffer(nobstot))
   do nanal=1,nanals
      if (nproc == 0) buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
      if (nproc_shm == 0) then
         call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
         anal_ob_fp(nanal,1:nobstot) = buffer(1:nobstot)
      end if 
   end do
   if (neigv > 0) then
      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win3,ierr)
      call c_f_pointer(anal_ob_modens_cp, anal_ob_modens_fp, [nens, nobstot])
      do nanal=1,nens
         if (nproc == 0) buffer(1:nobstot) = anal_ob_modens(nanal,1:nobstot)
         if (nproc_shm == 0) then
            call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
            anal_ob_modens_fp(nanal,1:nobstot) = buffer(1:nobstot)
         end if 
      end do
   endif
   if (.not. deterministic) then
      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win2,ierr)
      call c_f_pointer(obperts_cp, obperts_fp, [nanals, nobstot])
      do nanal=1,nanals
         if (nproc == 0) buffer(1:nobstot) = obperts(nanal,1:nobstot)
         if (nproc_shm == 0) then
            call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
            obperts_fp(nanal,1:nobstot) = buffer(1:nobstot)
         end if 
      end do
   endif
   deallocate(buffer)
   call MPI_Win_unlock(0, shm_win, ierr)
   if (neigv > 0) call MPI_Win_unlock(0, shm_win3, ierr)
   nullify(anal_ob_fp)
   if (neigv > 0) nullify(anal_ob_modens_fp)
   ! don't need anal_ob anymore
   if (allocated(anal_ob)) deallocate(anal_ob)
   if (allocated(anal_ob_modens)) deallocate(anal_ob_modens)
   if (.not. deterministic) then
      ! don't need obperts anymore
      call MPI_Win_unlock(0, shm_win2, ierr)
      nullify(obperts_fp)
      if (allocated(obperts)) deallocate(obperts)
   endif
endif
! barrier here to make sure no tasks try to access shared
! memory segment before it is created.
call mpi_barrier(mpi_comm_world, ierr)
! associate fortran pointer with c pointer to shared memory 
! segment (containing observation prior ensemble) on each task.
call MPI_Win_shared_query(shm_win, 0, segment_size, disp_unit, anal_ob_cp, ierr)
call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
if (neigv > 0) then
   call MPI_Win_shared_query(shm_win3, 0, segment_size, disp_unit, anal_ob_modens_cp, ierr)
   call c_f_pointer(anal_ob_modens_cp, anal_ob_modens_fp, [nens, nobstot])
endif
if (.not. deterministic) then
   call MPI_Win_shared_query(shm_win2, 0, segment_size, disp_unit, obperts_cp, ierr)
   call c_f_pointer(obperts_cp, obperts_fp, [nanals, nobstot])
endif
#else
! if MPI3 not available, need anal_ob on every MPI task
! broadcast observation prior ensemble from root one ensemble member at a time.
allocate(buffer(nobstot))
! allocate anal_ob on non-root tasks
if (nproc .ne. 0) allocate(anal_ob(nanals,nobstot))
if (neigv > 0 .and. nproc .ne. 0) allocate(anal_ob_modens(nens,nobstot))
! bcast anal_ob from root one member at a time.
do nanal=1,nanals
   buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
   call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
   if (nproc .ne. 0) anal_ob(nanal,1:nobstot) = buffer(1:nobstot)
end do
if (neigv > 0) then
   do nanal=1,nens
      buffer(1:nobstot) = anal_ob_modens(nanal,1:nobstot)
      call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
      if (nproc .ne. 0) anal_ob_modens(nanal,1:nobstot) = buffer(1:nobstot)
   end do
endif
if (.not. deterministic) then
   if (nproc .ne. 0) allocate(obperts(nanals,nobstot))
   do nanal=1,nanals
      buffer(1:nobstot) = obperts(nanal,1:nobstot)
      call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
      if (nproc .ne. 0) obperts(nanal,1:nobstot) = buffer(1:nobstot)
   end do
endif
deallocate(buffer)
#endif
t2 = mpi_wtime()
if (nproc .eq. 0) print *,'time to broadcast ob prior ensemble = ',t2-t1

if (nproc .eq. 0 .and. .not. deterministic) then
   print *,'perturbed obs LETKF'
endif
if (minval(lnsigl) > 1.e3 .or. letkf_novlocal) then
   vlocal = .false.
   if (nproc == 0) print *,'no vertical localization in LETKF'
   ! if no vertical localization, weights
   ! need only be computed once for each column.
   nnmax = 1
else
   vlocal = .true.
   ! if vertical localization on, analysis weights
   ! need to be computed for every vertical level.
   nnmax = nlevs_pres
endif

! apply bias correction with latest estimate of bias coeffs
! (if bias correction update in ob space turned on).
if (nobs_sat > 0 .and. lupd_satbiasc .and. lupd_obspace_serial) call apply_biascorr()

nrej=0
! reset ob error to account for gross errors 
if (varqc .and. lupd_obspace_serial) then
    if (huber) then ! "huber norm" QC
      do nob=1,nobstot
        ! observation space update performed in serial filter 
        ! using lupd_obspace_serial
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        ! depends of 2 parameters: zhuberright, zhuberleft.
        if (normdepart < -zhuberleft) then
           pnge = zhuberleft/abs(normdepart)
        else if (normdepart > zhuberright) then
           pnge = zhuberright/abs(normdepart)
        else
           pnge = one
        end if
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    else ! "flat-tail" QC.
      do nob=1,nobstot
        ! original form, gross error cutoff a multiple of ob error st dev.
        ! here gross err cutoff proportional to ensemble spread plus ob error
        ! Dharssi, Lorenc and Inglesby eqn (1) a = grosserrw*sqrt(S+R) 
        width = sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))
        pnge = prpgerr(nob)*sqrt(2.*pi*oberrvar(nob))/((one-prpgerr(nob))*(2.*width))
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        pnge = one - (pnge/(pnge+exp(-normdepart**2/2._r_single)))
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    endif
else
     oberrvaruse(1:nobstot) = oberrvar(1:nobstot)
end if

tbegin = mpi_wtime()

t2 = zero
t3 = zero
t4 = zero
t5 = zero
tbegin = mpi_wtime()
nobslocal_max = -999
nobslocal_min = nobstot

if (nobsl_max > 0 .and. dfs_sort) then
    allocate(statesprd_prior(ncdim))
endif

! Update ensemble on model grid.
! Loop for each horizontal grid points on this task.
!$omp parallel do schedule(dynamic) private(npt,nob,nobsl, &
!$omp                  gain,nobsl2,oberrfact,ngrd1,corrlength,ens_tmp, &
!$omp                  nf,vdist,kfgain,obens,indxassim,indxob, &
!$omp                  nn,hxens,hxenss,dfs,rdiag,dep,rloc,i,work,work2,trans, &
!$omp                  oindex,deglat,dist,corrsq,nb,sresults, &
!$omp                  nsvals,u,vt,reducedgain) &
!$omp  reduction(+:t1,t2,t3,t4,t5) &
!$omp  reduction(max:nobslocal_max) &
!$omp  reduction(min:nobslocal_min) 
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()
   if (.not. allocated(ens_tmp)) allocate(ens_tmp(nens,ncdim,nbackgrounds))
   if (.not. allocated(trans)) allocate(trans(nens,nens))
   ! find obs close to this grid point (using kdtree)
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
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
!   if (allocated(sresults)) deallocate(sresults)
   if (nobsl_max > 0) then ! only use nobsl_max nearest obs (sorted by distance).
       if (dfs_sort) then ! sort by DFS instead of distance.
          do i=1,ncdim ! state space ensemble spread for column being updated
             statesprd_prior(i) =  &
             sqrt(sum(ens_tmp(:,i,(nbackgrounds/2)+1)**2)*r_nanalsm1)
          enddo
          allocate(dfs(nobstot))
          allocate(rloc(nobstot))
          allocate(indxob(nobstot))
          ! calculate integrated DFS for each ob in local volume
          nobsl = 0
          do nob=1,nobstot
             rloc(nob) = sum((obloc(:,nob)-grdloc_chunk(:,npt))**2,1)
             dist = sqrt(rloc(nob)/corrlengthsq(nob))
             if (dist < 1.0 - eps .and. &
                 oberrvaruse(nob) < 1.e10_r_single) then
                nobsl = nobsl + 1
                dfs(nobsl) = 0.
                indxob(nobsl) = nob
                oberrfact = taper(dist)
                do i=1,ncdim
#ifdef MPI3
                    gain = sum(ens_tmp(1:nanals,i,(nbackgrounds/2)+1)*anal_ob_fp(1:nanals,nob))*r_nanalsm1
#else
                    gain = sum(ens_tmp(1:nanals,i,(nbackgrounds/2)+1)*anal_ob(1:nanals,nob))*r_nanalsm1
#endif
! DFS is estimated increment normalized by spread, summed over all variables in column, for middle of window
                    gain = gain/(obsprd_prior(nob) + oberrvaruse(nob)/oberrfact)
                    gain = gain/statesprd_prior(i)
                    dfs(nobsl) = dfs(nobsl)+abs(gain*(ob(nob)-ensmean_ob(nob)))
                enddo
             endif
          enddo
          ! sort on DFS
          allocate(indxassim(nobsl))
          call quicksort(nobsl,dfs(1:nobsl),indxassim)
          nf = 0 ! results ordered by DFS, largest to smallest
          nobsl2 = min(nobsl_max,nobsl)
          do nob=nobsl,nobsl-nobsl2+1,-1
             nf = nf + 1
             sresults(nf)%dis = rloc(indxob(indxassim(nob)))
             sresults(nf)%idx = indxob(indxassim(nob))
             !if (nproc == 0 .and. npt == 1) &
             !print *,nf,sresults(nf)%idx,dfs(indxassim(nob)),sqrt(sresults(nf)%dis/corrlengthsq(sresults(nf)%idx)),obtype(sresults(nf)%idx)
          enddo
          deallocate(rloc,dfs,indxassim,indxob)
          nobsl = nobsl2
       else
          if (kdobs) then
             call kdtree2_n_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),nn=nobsl_max,&
                  results=sresults)
             nobsl = nobsl_max
          else
             nobsl = 0
             do nob = 1, nobstot
                r = sum( (grdloc_chunk(:,npt)-obloc(:,nob))**2, 1)
                if (r < corrsq) then
                   nobsl = nobsl + 1
                   sresults(nobsl)%idx = nob
                   sresults(nobsl)%dis = r
                endif
             enddo
             nobsl_max = nobsl
          endif
       endif
   else ! find all obs within localization radius (sorted by distance).
       if (kdobs) then
         call kdtree2_r_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),r2=corrsq,&
              nfound=nobsl,nalloc=nobstot,results=sresults)
       else
         nobsl = 0
         do nob = 1, nobstot
            r = sum( (grdloc_chunk(:,npt)-obloc(:,nob))**2, 1)
            if (r < corrsq) then
              nobsl = nobsl + 1
              sresults(nobsl)%idx = nob
              sresults(nobsl)%dis = r
            endif
         enddo
       endif
   endif

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observations in local area
   if(nobsl == 0) then
      if (allocated(sresults)) deallocate(sresults)
      if (allocated(trans)) deallocate(trans)
      if (allocated(ens_tmp)) deallocate(ens_tmp)
      cycle grdloop
   endif

   ! Loop through vertical levels (nnmax=1 if no vertical localization)
   verloop: do nn=1,nnmax

      ! Pick up variables passed to LETKF core process
      allocate(rloc(nobsl))
      allocate(oindex(nobsl))
      nobsl2=1
      do nob=1,nobsl
         nf = sresults(nob)%idx
         ! skip 'screened' obs.
         if (oberrvaruse(nf) > 1.e10_r_single) cycle
         if (vlocal) then
            vdist=(lnp_chunk(npt,nn)-oblnp(nf))/lnsigl(nf)
            if(abs(vdist) >= one) cycle
         else
            vdist = zero
         endif
         dist = sqrt(sresults(nob)%dis/corrlengthsq(sresults(nob)%idx)+vdist*vdist)
         if (dist >= one) cycle
         rloc(nobsl2)=taper(dist)
         oindex(nobsl2)=nf
         if(rloc(nobsl2) > eps) nobsl2=nobsl2+1
      end do
      nobsl2=nobsl2-1
      if (nobsl2 > nobslocal_max) nobslocal_max=nobsl2
      if (nobsl2 < nobslocal_min) nobslocal_min=nobsl2
      if(nobsl2 == 0) then
         deallocate(rloc,oindex)
         cycle verloop
      end if
      allocate(hxens(nens,nobsl2))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
         if (neigv > 0) then
#ifdef MPI3
         hxens(1:nens,nob)=anal_ob_modens_fp(1:nens,nf) 
#else
         hxens(1:nens,nob)=anal_ob_modens(1:nens,nf) 
#endif
         else
#ifdef MPI3
         hxens(1:nens,nob)=anal_ob_fp(1:nens,nf) 
#else
         hxens(1:nens,nob)=anal_ob(1:nens,nf) 
#endif
         endif
         rdiag(nob)=one/oberrvaruse(nf)
         dep(nob)=ob(nf)-ensmean_ob(nf)
      end do

      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()

      if (.not. deterministic .or. denkf .or. gletkf) then
         allocate(kfgain(nobsl2),obens(nobsl2,nanals))
         if (.not. deterministic) then
            ! add ob perts to observation priors
            do nob=1,nobsl2
               nf = oindex(nob)
               obens(nob,1:nanals) = &
#ifdef MPI3
               obperts_fp(1:nanals,nf) + anal_ob_fp(1:nanals,nf) 
#else
               obperts(1:nanals,nf) + anal_ob(1:nanals,nf) 
#endif
            enddo
         else
            do nob=1,nobsl2
               nf = oindex(nob)
               obens(nob,1:nanals) = &
#ifdef MPI3
               anal_ob_fp(1:nanals,nf) 
#else
               anal_ob(1:nanals,nf) 
#endif
            enddo
         endif
      endif
      deallocate(oindex)
  
      ! Compute transformation matrix of LETKF
      if (gletkf) then
         ! use gain form
         nsvals = min(nens,nobsl2)
         allocate(u(nens,nens),vt(nsvals,nobsl2),reducedgain(nobsl2))
         call gletkf_core(nobsl2,hxens,rdiag,rloc(1:nobsl2),nens,nens/nanals,nsvals,u,vt)
         deallocate(rloc,rdiag)

         ! save single precision copy of hxens, deallocate
         allocate(hxenss(nens,nobsl2)); hxenss = hxens
         deallocate(hxens)

         t4 = t4 + mpi_wtime() - t1
         t1 = mpi_wtime()

         ! Update analysis ensembles (all time levels)
         do nb=1,nbackgrounds
         do i=1,ncdim
            ! if not vlocal, update all state variables in column.
            if(vlocal .and. index_pres(i) /= nn) cycle
            call sgemv('t',nens,nobsl2,1.e0,hxenss,nens,&
                       ens_tmp(:,i,nb),1,0.e0,kfgain,1)
            ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + sum(kfgain*dep)
            call gletkf_gain(nens,nobsl2,nsvals,u,vt,ens_tmp(:,i,nb),reducedgain)
            do nanal=1,nanals
               anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) - &
               sum(reducedgain*obens(:,nanal))
            enddo
         enddo
         enddo
         deallocate(u,vt,reducedgain,hxenss)

      else
         call letkf_core(nobsl2,hxens,rdiag,dep,rloc(1:nobsl2),trans,nens,nens/nanals,compute_weights)
         deallocate(rloc,rdiag)

         ! save single precision copy of hxens for perturbed obs LETKF/DeNKF, deallocate
         if (.not. deterministic .or. (deterministic .and. denkf)) then
            allocate(hxenss(nens,nobsl2)); hxenss = hxens
         else
            deallocate(dep)
         endif
         deallocate(hxens)

         t4 = t4 + mpi_wtime() - t1
         t1 = mpi_wtime()

         ! Update analysis ensembles (all time levels)
         do nb=1,nbackgrounds
         do i=1,ncdim
            ! if not vlocal, update all state variables in column.
            if(vlocal .and. index_pres(i) /= nn) cycle
            if (deterministic) then
               if (denkf) then
                  !do nob=1,nobsl2
                  !   kfgain(nob) = sum(hxenss(:,nob)*ens_tmp(:,i,nb))
                  !enddo
                  call sgemv('t',nens,nobsl2,1.e0,hxenss,nens,&
                             ens_tmp(:,i,nb),1,0.e0,kfgain,1)
                  ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + sum(kfgain*dep)
                  do nanal=1,nanals
                     anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) - &
                     sum(0.5*kfgain*obens(:,nanal))
                  enddo
               else
                  ! if compute_weights=F there is a problem!
                  if (.not. compute_weights) then
                    if (nproc .eq. 0) then
                       print *,'compute_weights=F and LETKF trans matrix needed!'
                    endif
                    call stop2(911)
                  endif
                  work(1:nanals) = anal_chunk(1:nanals,npt,i,nb)
                  work2(1:nanals) = ensmean_chunk(npt,i,nb)
                  if(r_kind == kind(1.d0)) then
                     call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0, &
                          & work2,1)
                  else
                     call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0, &
                          & work2,1)
                  end if
                  ensmean_chunk(npt,i,nb) = sum(work2(1:nanals)) * r_nanals
                  anal_chunk(1:nanals,npt,i,nb) = work2(1:nanals)-ensmean_chunk(npt,i,nb)
               endif
            else ! perturbed obs using LETKF gain.
               !do nob=1,nobsl2
               !   kfgain(nob) = sum(hxenss(:,nob)*ens_tmp(:,i,nb))
               !enddo
               call sgemv('t',nens,nobsl2,1.e0,hxenss,nens,&
                          ens_tmp(:,i,nb),1,0.e0,kfgain,1)
               ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + sum(kfgain*dep)
               do nanal=1,nanals
                  anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) - &
                  sum(kfgain*obens(:,nanal))
               enddo
            endif
         enddo
         enddo
      endif
      ! deallocate arrays needed for perturbed obs LETKF and DENKF
      if (allocated(hxenss)) deallocate(hxenss)
      if (allocated(kfgain)) deallocate(kfgain)
      if (allocated(dep)) deallocate(dep)
      if (allocated(obens)) deallocate(obens)

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()

   end do verloop

   if (allocated(sresults)) deallocate(sresults)
   if (allocated(trans)) deallocate(trans)
   if (allocated(ens_tmp)) deallocate(ens_tmp)
end do grdloop
!$omp end parallel do

if (allocated(statesprd_prior)) deallocate(statesprd_prior)

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
call mpi_reduce(nobslocal_max,nobslocal_maxall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(nobslocal_min,nobslocal_minall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) print *,'min/max number of obs in local volume',nobslocal_minall,nobslocal_maxall
if (nrej > 0 .and. nproc == 0) print *, nrej,' obs rejected by varqc'
  
! free shared memory segement, fortran pointer to that memory.
#ifdef MPI3
nullify(anal_ob_fp)
call MPI_Win_free(shm_win, ierr)
if (.not. deterministic) then
   nullify(obperts_fp)
   call MPI_Win_free(shm_win2, ierr)
endif
if (neigv > 0) then
   nullify(anal_ob_modens_fp)
   call MPI_Win_free(shm_win3, ierr)
endif
#endif
! deallocate anal_ob on non-root tasks.
if (nproc .ne. 0 .and. allocated(anal_ob)) deallocate(anal_ob)
if (nproc .ne. 0 .and. allocated(anal_ob_modens)) deallocate(anal_ob_modens)
if (allocated(obperts)) deallocate(obperts)
if (allocated(ens_tmp)) deallocate(ens_tmp)

return

end subroutine letkf_update

subroutine letkf_core(nobsl,hxens,rdiaginv,dep,rloc,trans,nanals,neigv,compute_weights)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    letkf_core
!
!   prgmmr: ota
!
! abstract:  LETKF core subroutine computing transform matrix. BLAS subroutines
!            are used for the computational efficiency.
!
! program history log:
!   2011-06-03  ota: created from miyoshi's LETKF core subroutine
!   2014-06-20  whitaker: optimization for case when no vertical localization
!               is used. Fixed missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use LAPACK
!               routine dsyev for eigenanalysis.
!   2016-02-01  whitaker: Use LAPACK dsyevr for eigenanalysis (faster
!               than dsyev in most cases). 
!   2018-05-31  whitaker:  Modify hxens on output so it can
!               be used to explicitly compute kalman gain. Mods
!               to use model-space vertical localization with
!               modulated ensemble.
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hxens    - on input: first-guess ensembles on observation space
!                on output: overwritten with Painv*YbRinv^T, where
!                Painv is inverse of analysis error cov in ensemble space 
!                and YbRinv is hxens * R **-1 (including ob error localization).
!                this is used to compute kalman gain by pre-multiplying with
!                model space ensemble perts.
!     rdiaginv - inverse of diagonal element of observation error covariance
!     dep      - observation departure from first guess mean
!     rloc     - localization function to each observations
!     nanals   - number of ensemble members
!     neigv    - for modulated ensemble model-space localization, number
!                of eigenvectors of vertical localization (1 not using
!                model space localization)
!     compute_weights - logical flag, if .false. transform matrix
!                not computed (not needed for DEnKF or perturbed obs LETKF).
!
!   output argument list:
!     trans    - transform matrix for this point. Not created if
!                compute_weights=F
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
implicit none
integer(i_kind)                      ,intent(in ) :: nobsl,nanals,neigv
real(r_kind),dimension(nanals,nobsl ),intent(inout) :: hxens
logical, intent(in) :: compute_weights
real(r_kind),dimension(nobsl        ),intent(in ) :: rdiaginv
real(r_kind),dimension(nobsl        ),intent(in ) :: dep
real(r_kind),dimension(nobsl        ),intent(in ) :: rloc
real(r_kind),dimension(nanals,nanals),intent(out) :: trans
real(r_kind), allocatable, dimension(:,:) :: work1,work2,eivec,pa
real(r_kind), allocatable, dimension(:) :: rrloc,eival,work3
real(r_kind) :: rho
integer(i_kind) :: i,j,nob,nanal,ierr,lwork
!for dsyevr
integer(i_kind) iwork(10*nanals),isuppz(2*nanals)
real(r_kind) vl,vu,work(70*nanals)
!for dsyevd
!integer(i_kind) iwork(5*nanals+3)
!real(r_kind) work(2*nanals*nanals+6*nanals+1)
allocate(work3(nanals),work2(nanals,nobsl))
allocate(eivec(nanals,nanals),pa(nanals,nanals))
allocate(work1(nanals,nanals),eival(nanals),rrloc(nobsl))
! hxens sqrt(Rinv)
rrloc(1:nobsl) = rdiaginv(1:nobsl) * rloc(1:nobsl)
rho = epsilon(0.0_r_single)
where (rrloc < rho) rrloc = rho
rrloc = sqrt(rrloc)
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! hxens^T Rinv hxens
!do j=1,nanals
!   do i=1,nanals
!      work1(i,j) = hxens(i,1) * hxens(j,1)
!      do nob=2,nobsl
!         work1(i,j) = work1(i,j) + hxens(i,nob) * hxens(j,nob)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nobsl,1.d0,hxens,nanals, &
        hxens,nanals,0.d0,work1,nanals)
else
   call sgemm('n','t',nanals,nanals,nobsl,1.e0,hxens,nanals, &
        hxens,nanals,0.e0,work1,nanals)
end if
! hdxb^T Rinv hdxb + (m-1) I
do nanal=1,nanals
   work1(nanal,nanal) = work1(nanal,nanal) + real(nanals/neigv-1,r_kind)
end do
! eigenvalues and eigenvectors of [ hdxb^T Rinv hdxb + (m-1) I ]
! use LAPACK dsyev
!eivec(:,:) = work1(:,:); lwork = -1
!call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
!lwork = min(nanals*nanals, int(work1(1,1)))
!call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
! use LAPACK dsyevd
!call dsyevd('V','L',nanals,eivec,nanals,eival,work,size(work),iwork,size(iwork),ierr)
! use LAPACK dsyevr 
call dsyevr('V','A','L',nanals,work1,nanals,vl,vu,1,nanals,-1.d0,nanals,eival,eivec, &
            nanals,isuppz,work,size(work),iwork,size(iwork),ierr)
if (ierr .ne. 0) print *,'warning: dsyev* failed, ierr=',ierr
! Pa = [ hdxb^T Rinv hdxb + (m-1) I ]inv
do j=1,nanals
   do i=1,nanals
      work1(i,j) = eivec(i,j) / eival(j)
   end do
end do
!do j=1,nanals
!   do i=1,nanals
!      pa(i,j) = work1(i,1) * eivec(j,1)
!      do k=2,nanals
!         pa(i,j) = pa(i,j) + work1(i,k) * eivec(j,k)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
        nanals,0.d0,pa,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
        nanals,0.e0,pa,nanals)
end if
! convert hxens * Rinv^T from hxens * sqrt(Rinv)^T
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! Pa hdxb_rinv^T
!do nob=1,nobsl
!   do nanal=1,nanals
!      work2(nanal,nob) = pa(nanal,1) * hxens(1,nob)
!      do k=2,nanals
!         work2(nanal,nob) = work2(nanal,nob) + pa(nanal,k) * hxens(k,nob)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','n',nanals,nobsl,nanals,1.d0,pa,nanals,hxens,&
        nanals,0.d0,work2,nanals)
else
   call sgemm('n','n',nanals,nobsl,nanals,1.e0,pa,nanals,hxens,&
        nanals,0.e0,work2,nanals)
end if
! over-write hxens with Pa hdxb_rinv
! (pre-multiply with ensemble perts to compute Kalman gain - 
!  eqns 20-23 in Hunt et al 2007 paper)
hxens = work2

! for DEnKF or perturbed obs the rest is not needed
if (compute_weights) then
  ! work3 = Pa hdxb_rinv^T dep
  do nanal=1,nanals
     work3(nanal) = work2(nanal,1) * dep(1)
     do nob=2,nobsl
        work3(nanal) = work3(nanal) + work2(nanal,nob) * dep(nob)
     end do
  end do
  ! T = sqrt[(m-1)Pa]
  do j=1,nanals
     rho = sqrt( real(nanals/neigv-1,r_kind) / eival(j) )
     do i=1,nanals
        work1(i,j) = eivec(i,j) * rho
     end do
  end do
  if(r_kind == kind(1.d0)) then
     call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
          & nanals,0.d0,trans,nanals)
  else
     call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
          & nanals,0.e0,trans,nanals)
  end if
  !do j=1,nanals
  !   do i=1,nanals
  !      trans(i,j) = work1(i,1) * eivec(j,1)
  !      do k=2,nanals
  !         trans(i,j) = trans(i,j) + work1(i,k) * eivec(j,k)
  !      end do
  !   end do
  !end do
  ! T + Pa hdxb_rinv^T dep
  do j=1,nanals
     do i=1,nanals
        trans(i,j) = trans(i,j) + work3(i)
     end do
  end do
endif ! compute_weights = T

deallocate(work2,eivec,pa,work1,rrloc,eival,work3)

return
end subroutine letkf_core

subroutine gletkf_core(nobsl,hxens,rdiaginv,rloc,nanals,neigv,nsvals,u,vt)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gletkf_core
!
!   prgmmr: whitaker
!
! abstract:  gain form of LETKF core subroutine
!
! program history log:
!   2018-07-01  whitaker: add gain form of LETKF
!   (https://doi.org/10.1175/MWR-D-17-0102.1)
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hxens    - on input: first-guess ensembles on observation space
!                on output: overwritten with Painv*YbRinv^T, where
!                Painv is inverse of analysis error cov in ensemble space 
!                and YbRinv is hxens * R **-1 (including ob error localization).
!                this is used to compute kalman gain by pre-multiplying with
!                model space ensemble perts.
!     rdiaginv - inverse of diagonal element of observation error covariance
!     rloc     - localization function to each observations
!     nanals   - number of ensemble members
!     neigv    - for modulated ensemble model-space localization, number
!                of eigenvectors of vertical localization (1 if not using
!                model space localization)
!     nsvals   - number of singular values of YbRsqrtinv=hxens * R **-1/2
!                (and R includes ob error localization)
!                (= nanals if nanals<=nobsl, = nobsl if nobsl<nanals)
!
!   output argument list:
!     u        - left singular vectors of YbRsqrtinv
!                (dimension (nanals,nanals)) scaled by
!                (1.-sqrt((nanals/neigv)-1)/eigvals)
!                where eigvals = svals**2+(nanals/neigv)-1.
!                svals are singular values and neigv is the number
!                of eigenvectors of vertical localization (1 if not using
!                model space vertical localization)
!     vt       - right singular vectors of YbRsqrtinv
!                (dimension (nsvals,nobsl)) scaled by
!                inverse singular values times R ** -1/2 (including ob error localzation).
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block

! equivalent python code:
!   sqrtoberrvar_inv = 1./np.sqrt(oberrvar) # with ob error localization
!   YbRsqrtinv = hxprime * sqrtoberrvar_inv
!   u, svals, v = svd(YbRsqrtinv,full_matrices=False,lapack_driver='gesvd')
!   eigvals = svals**2+(nanals/neigv)-1
! for ETKF weights: enswts =  (u * (np.sqrt((nanals-1)/eigvals))).dot(u.T)
!                   xprime = np.dot(enswts.T, xprime)
!   painv =  (u * (1./eigvals)).dot(u.T)
!   kfgain = np.dot(xprime.T,np.dot(painv,YbRsqrtinv*sqrtoberrvar_inv))
!   xmean = xmean + np.dot(kfgain, obs-hxmean)
! u,vt returned as
!     u*(1.-np.sqrt((nanals/neigv)-1)/eigvals)) 
!     (v.T/sval)*sqrtoberrvar_inv 

implicit none
integer(i_kind)                      ,intent(in ) :: nobsl,nsvals,nanals,neigv
real(r_kind),dimension(nanals,nobsl ),intent(inout)  :: hxens
real(r_kind),dimension(nanals,nanals),intent(inout) :: u
real(r_kind),dimension(nsvals,nobsl),intent(inout) :: vt
real(r_kind),dimension(nobsl        ),intent(in ) :: rdiaginv
real(r_kind),dimension(nobsl        ),intent(in ) :: rloc
real(r_kind), allocatable, dimension(:,:) :: work2,painv,work3
real(r_kind), allocatable, dimension(:) :: work1,rrloc,svals,eigval
integer(i_kind), allocatable, dimension(:) :: iwork
real(r_kind) eps
integer(i_kind) :: nanal,ierr,lwork,nob
if (nobsl < nanals) then
  print *,'warning: nobsl < nanals',nanals,nobsl,nsvals
endif
allocate(work2(nanals,nobsl),work3(nanals,nanals))
allocate(painv(nanals,nanals))
allocate(svals(nsvals),rrloc(nobsl),eigval(nanals))
! hxens sqrt(Rinv)
rrloc = rdiaginv * rloc
eps = epsilon(0.0_r_single)
where (rrloc < eps) rrloc = eps
rrloc = sqrt(rrloc)
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! SVD of YbRsqrtinv (with ob error localization)
! svals are singular values, u are left singular vecs (nanals,nanals), vt
! are right singular vecs (nanals,nobsl) if nanals<nobsl or (nobsl,nobsl) if
! nobsl<nanals
work2 = hxens
! divide and conquer algorithmm (gesdd) should be faster, use it if possible
! (as long as there as many obs than ens members).
if(r_kind == kind(1.d0)) then
   if (nobsl < nanals) then
      allocate(work1(1))
      call dgesvd('a','s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,-1,ierr)
      lwork = work1(1); deallocate(work1); allocate(work1(lwork))
      call dgesvd('a','s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,lwork,ierr)
      deallocate(work1)
   else
      allocate(work1(1))
      allocate(iwork(8*nsvals))
      call dgesdd('s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,-1,iwork,ierr)
      lwork = work1(1); deallocate(work1); allocate(work1(lwork))
      call dgesdd('s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,lwork,iwork,ierr)
      deallocate(iwork,work1)
   endif
else
   if (nobsl < nanals) then
      allocate(work1(1))
      call sgesvd('a','s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,-1,ierr)
      lwork = work1(1); deallocate(work1); allocate(work1(lwork))
      call sgesvd('a','s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,lwork,ierr)
      deallocate(work1)
   else
      allocate(work1(1))
      allocate(iwork(8*nsvals))
      call sgesdd('s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,-1,iwork,ierr)
      lwork = work1(1); deallocate(work1); allocate(work1(lwork))
      call sgesdd('s',nanals,nobsl,work2,nanals,svals,u,nanals,vt,nsvals,work1,lwork,iwork,ierr)
      deallocate(iwork,work1)
   endif
endif
if (ierr .ne. 0) print *,'warning: lapack svd failed, ierr=',ierr
where (svals < eps) svals = eps
eigval = eps+(nanals/neigv)-1
eigval(1:nsvals) = svals(1:nsvals)**2+(nanals/neigv)-1
do nanal=1,nanals
   work3(nanal,:) = u(nanal,:)/eigval
enddo
! scaling needed for reduced gain
do nob=1,nobsl
   vt(:,nob) = vt(:,nob)*rrloc(nob)/svals
enddo
! painv
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work3,nanals,u,&
        nanals,0.d0,painv,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work3,nanals,u,&
        nanals,0.e0,painv,nanals)
end if
! scaling needed for reduced gain
do nanal=1,nanals
   u(nanal,:) = u(nanal,:)*(1.-sqrt((nanals/neigv-1)/eigval))
enddo
! convert hxens * Rinv^T from hxens * sqrt(Rinv)^T
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! Painv hdxb_rinv^T
if(r_kind == kind(1.d0)) then
   call dgemm('n','n',nanals,nobsl,nanals,1.d0,painv,nanals,hxens,&
        nanals,0.d0,work2,nanals)
else
   call sgemm('n','n',nanals,nobsl,nanals,1.e0,painv,nanals,hxens,&
        nanals,0.e0,work2,nanals)
end if
! over-write hxens with Painv hdxb_rinv^T
! pre-multiply with ensemble perts to compute Kalman gain - 
! (eqns 20-23 in Hunt et al 2007 paper)
hxens = work2 
! return hxens to compute kfgain, u,v to compute reducedgain
! clean up
deallocate(work2,work3,painv,svals,eigval,rrloc)

return
end subroutine gletkf_core

subroutine gletkf_gain(nanals,nobsl,nsvals,u,vt,xens,reducedgain)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gletkf_gain
!
!   prgmmr: whitaker
!
! abstract:  compute modified (reduced) gain for ensemble perturbation update
!            using results from gletkf_core
!
! program history log:
!   2018-07-01  whitaker: add gain form of LETKF
!   (https://doi.org/10.1175/MWR-D-17-0102.1)
!
!   input argument list:
!     nanals   - number of ensemble members
!     nobsl    - number of observations in the local patch
!     nsvals   - number of singular values of YbRsqrtinv=hxens * R **-1/2
!     u        - left singular vectors of YbRsqrtinv
!                (dimension (nanals,nanals)) scaled by
!                (1.-sqrt((nanals/neigv)-1)/eigval)
!                where eigval = svals**2+(nanals/neigv)-1.
!                svals are singular values and neigv is the number
!                of eigenvectors of vertical localization (1 if not using
!                model space vertical localization)
!     vt       - right singular vectors of YbRsqrtinv
!                (dimension (nsvals,nobsl)) scaled by
!                R ** -1/2 (including ob error localzation)
!                and inverse singular values (svals).
!     xens - model space ensemble perturbations (dimension nanals)
!
!   output argument list:
!     reducedgain - modified gain for ensemble perturbations (dimension nobsl)
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
! equivalent python code:
!   eigvals = svals**2+(nanals/neigv)-1
!   where neigv is number of eigenvectors of vertical localization matrix
!   (=1 for no modulated ensemble model space vertical localization)
!   reducedgain = np.dot(xprime.T,u)
!     ( u contains u*(1.-np.sqrt((nanals/neigv-1)/eigvals)) )
!   reducedgain = np.dot(reducedgain,v)
!     ( vt contains (v.T/svals)*sqrtoberrvar_inv )
implicit none
integer(i_kind),intent(in) :: nanals,nobsl,nsvals
real(r_kind),dimension(nanals,nanals),intent(in) :: u
real(r_kind),dimension(nsvals,nobsl ),intent(in) :: vt
real(r_single),dimension(nanals     ),intent(in) :: xens
real(r_single),dimension(nobsl      ),intent(out) :: reducedgain
! local vars
real(r_single),dimension(nanals) :: work1
real(r_single),dimension(nanals,nanals) :: worku
real(r_single),dimension(nsvals,nobsl) :: workvt
!integer(i_kind) i,j

! single precision copies of u,vt
worku = u; workvt = vt

!do j=1,nanals
!   work1(j) = sum(worku(:,j)*xens(:))
!enddo
!do i=1,nobsl
!   reducedgain(i) = sum(workvt(1:nsvals,i)*work1(1:nsvals))
!enddo

! using BLAS
call sgemv('t',nanals,nanals,1.e0,worku,nanals,&
           xens,1,0.e0,work1,1)
call sgemv('t',nsvals,nobsl,1.e0,workvt,nsvals,&
           work1(1:nsvals),1,0.e0,reducedgain,1)

end subroutine gletkf_gain

end module letkf
