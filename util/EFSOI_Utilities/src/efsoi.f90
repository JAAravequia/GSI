module efsoi
!$$$  module documentation block
!
! module: efsoi                          Update observation impact estimates
!                                        with the EnSRF framework.
!
! prgmmr: ota              org: np23                   date: 2011-12-01
!
! abstract: 
!  Computes the observation impact estimates using the EnKF analysis products.
!  Formulation is based on Kalnay et al (2012, Tellus A, submitted).
!  Parallel processing is following the way of the serial EnSRF.
!
! Public Subroutines:
!  efsoi_update: performs the Forecast Sensitivity to Observations update within
!                the EnSRF framework. The EnKF other than the serial EnSRF 
!                (e.g. the LETKF) can be also used here (the method is 
!                independent on the type of EnKF).
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               kdtree2_module, obsmod, gridinfo, obs_sensitivity
!
! program history log:
!   2011-12-01  Created from the LETKF core module.
!   2012-04-04  Changed to EnSRF framework for saving memory
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use covlocal, only:  taper
use kinds, only: r_double, i_kind, r_kind
use kdtree2_module, only: kdtree2_r_nearest, kdtree2_result
use loadbal, only: numptsperproc, indxproc, lnp_chunk, kdtree_grid, &
                   iprocob, indxob_chunk, anal_obchunk_prior, numobsperproc, &
                   indxproc_obs, nobs_max
use statevec, only: anal_chunk, fcerror_chunk, e_f, e_g ! fceror_chunk_efsr
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, ensmean_obnobc, obloc, obloclon, obloclat, oblnp, &
                       obtime, nobstot, corrlengthsq, lnsigl, obtimel, anal_ob, &
                       obfit_post, obsprior_inner
use constants, only: constants_initialized, pi, zero, one
use params, only: nanals, corrlengthnh, corrlengthtr, corrlengthsh, &
                  tar_minlon, tar_maxlon, tar_minlat, tar_maxlat, &
                  tar_minlev, tar_maxlev, nbackgrounds
use gridinfo, only: nlevs_pres, lonsgrd, latsgrd, id_u, id_v, id_t, id_q, id_ps
use enkf_obs_sensitivity, only: obsense_kin, obsense_dry, obsense_moist, covar_sh, &
                                obsense_kin_efsr, obsense_dry_efsr, obsense_moist_efsr, &
                                obsense_kin_nbc, obsense_dry_nbc, obsense_moist_nbc, &
                                covar_mass, covar_kin, covar_dry, covar_moist, &
                                ferror_prior, ferror_post, ferror_prior_rhigh, &
                                ferror_post_rhigh, ferror_prior_rlow, ferror_post_rlow, &
                                adloc_chunk

implicit none

private
public :: efsoi_update

contains

subroutine efsoi_update()
implicit none
real(r_kind),allocatable,dimension(:) :: djdy_kin, djdy_dry, djdy_moist
!!!!!!!!!!!EFSR
!real(r_kind),allocatable,dimension(:) :: djdy_kin_efsr, djdy_dry_efsr, djdy_moist_efsr
!!!!!!!!!!!!!EFSR
!real(r_kind),allocatable,dimension(:) :: total_assimilation_impact
real(r_kind),allocatable,dimension(:) :: uvwork, tpwork, qwork
!real(r_kind),allocatable,dimension(:) :: uvwork_efsr, tpwork_efsr, qwork_efsr
!real(r_kind),allocatable,dimension(:) :: covar_kin_work, covar_mass_work
real(r_kind),allocatable,dimension(:) :: covar_dry_work!, covar_moist_work
!real(r_kind),allocatable,dimension(:) :: covar_sh_work
real(r_kind),allocatable,dimension(:) :: taper_disgrd
real(r_kind),dimension(nobstot) :: obdep, oberinv, obdep_efsr, obdep_nbc
real(r_single),dimension(nobstot) :: invobtimel, invlnsigl
real(r_single),dimension(nobstot) :: invcorlen
real(r_single),dimension(nobstot) :: covar_moist!, covar_dry
real(r_single),dimension(nobstot) :: covar_kin, covar_mass
real(r_single),dimension(nobstot) :: covar_sh
real(r_kind),allocatable,dimension(:,:) :: buffertmp
type(kdtree2_result),allocatable,dimension(:) :: sresults1
real(r_kind) :: anal_obtmp(nanals)
!!!!!!!!!!!!!!!!EFSR
!real(r_kind) :: anal_obtmp_efsr(nanals)
!!!!!!!!!!EFSR
real(r_kind) :: r,taper1,taper3,taperv
real(r_kind) :: mean_kin_work, mean_moist_work
real(r_kind) :: mean_mass_work, mean_dry_work
real(r_kind) :: mean_sh_work, mean_ob
real(r_kind) :: ef_sum, eg_sum, efg_sum
real(r_kind) :: eg_max, eg_min, ef_max, ef_min
real(r_single) :: lnsig
real(r_double) :: t1, t2, t3, t4, tbegin, tend
! efsoi verification variables
real(r_single) :: kinetic_reduction, mass_reduction, shum_reduction
real(r_single) :: true_kinetic_reduction, true_mass_reduction, true_shum_reduction
real(r_single) :: true_dry_reduction, true_moist_reduction
! ----------------------------
integer(i_kind) :: np, nob, nob1, nob2, nobm, npob, nf2, i, ii, npt, nanal, nn
integer(i_kind) :: nnpt, nsame, nskip, ngrd1
integer(i_kind) :: ierr
logical :: kdgrid

  ! Calculate true forecast error
  ! reduction quantities (efsoi verification)
  ! (e_t|0 - e_t|-6)*(e_t|0 + e_t|-6)
!  kinetic_reduction = zero
!  mass_reduction = zero
!  shum_reduction = zero
!  true_kinetic_reduction = zero
!  true_mass_reduction = zero
!  true_shum_reduction = zero
!  true_dry_reduction = zero
!  true_moist_reduction = zero
!  do k = 1, tar_maxlev
!    do m = 1, n_grid_pts
!      kinetic_reduction = fcdiff_chunk(m,id_u(k))*fcerror_chunk(m,id_u(k)) + fcdiff_chunk(m,id_v(k))*fcerror_chunk(m,id_v(k))
!      mass_reduction = fcdiff_chunk(m,id_t(k))*fcerror_chunk(m,id_t(k))
!      shum_reduction = fcdiff_chunk(m,id_q(k))*fcerror_chunk(m,id_q(k))
!      true_kinetic_reduction = true_kinetic_reduction + kinetic_reduction
!      true_mass_reduction = true_mass_reduction + mass_reduction
!      true_shum_reduction = true_shum_reduction + shum_reduction
!    end do
!  end do
!  true_dry_reduction = true_kinetic_reduction + true_mass_reduction
!  true_moist_reduction = true_dry_reduction + true_shum_reduction


if (.not. constants_initialized) then
    print *,'constants not initialized (with init_constants, init_constants_derived)'
    call stop2(28)
end if

allocate(sresults1(nlevs_pres*numptsperproc(nproc+1)))
allocate(taper_disgrd(nlevs_pres*numptsperproc(nproc+1)))

! Compute the inverse of cut-off length and 
! the observation departure from first guess
!$omp parallel do private(nob)
do nob=1,nobstot
   invcorlen(nob)=one/corrlengthsq(nob)
   invlnsigl(nob)=one/lnsigl(nob)
   invobtimel(nob)=one/obtimel(nob)
   oberinv(nob)=one/oberrvar(nob)
   !obdep(nob)=ob(nob)-ensmean_ob(nob)
   obdep(nob) = obsprior_inner(nob)
   obdep_nbc(nob)=ob(nob)-ensmean_obnobc(nob)
   obdep_efsr(nob)=obfit_post(nob)
end do

kdgrid=associated(kdtree_grid)
allocate(djdy_kin(nobstot))
allocate(djdy_dry(nobstot))
allocate(djdy_moist(nobstot))
!allocate(djdy_kin_efsr(nobstot))
!allocate(djdy_dry_efsr(nobstot))
!allocate(djdy_moist_efsr(nobstot))
!allocate(total_assimilation_impact(nobstot))
djdy_kin(1:nobstot) = zero
djdy_dry(1:nobstot) = zero
covar_dry(1:nobstot) = zero
djdy_moist(1:nobstot) = zero

!efsr
!djdy_kin_efsr(1:nobstot) = zero
!djdy_dry_efsr(1:nobstot) = zero
!djdy_moist_efsr(1:nobstot) = zero
!efsr

!total_assimilation_impact(1:nobstot) = zero
efg_sum = zero
ef_sum = zero
eg_sum = zero
eg_min = zero
eg_max = zero
ef_min = zero
ef_max = zero
allocate(uvwork(nanals))
allocate(tpwork(nanals))
allocate(qwork(nanals))
!allocate(uvwork_efsr(nanals))
!allocate(tpwork_efsr(nanals))
!allocate(qwork_efsr(nanals))
!allocate(covar_kin_work(nanals))
!allocate(covar_mass_work(nanals))
!allocate(covar_sh_work(nanals))
allocate(covar_dry_work(nanals))
!allocate(covar_moist_work(nanals))
nsame = 0
nskip = 0
nobm = 1
t2 = zero
t3 = zero
t4 = zero
tbegin = mpi_wtime()
! Loop for each observations
obsloop: do nob=1,nobstot

   t1 = mpi_wtime()

   if(oberrvar(nob) > 1.e10_r_kind .or. abs(obtime(nob)) >= obtimel(nob))then
      nskip = nskip + 1
      cycle obsloop
   end if

   ! what processor is this ob on?
   npob = iprocob(nob)

   ! get anal_ob from that processor and send to other processors.
   if (nproc == npob) then
      nob1 = indxob_chunk(nob)
      anal_obtmp(1:nanals) = anal_obchunk_prior(:,nob1)
   end if
   !PRINT *, npob, 'mpi_bcast'
   call mpi_bcast(anal_obtmp,nanals,mpi_realkind,npob,mpi_comm_world,ierr)
   anal_obtmp(1:nanals) = anal_obtmp(1:nanals) * oberinv(nob)


!!!!!!!!!!!!!!!!!!
!!!!EFSR
!!!!!!!!!!!!!!!!!!
  ! anal_obtmp_efsr(1:nanals) = anal_obtmp(1:nanals) * oberinv(nob) * oberinv(nob)
!!!!!!!!!EFSR


   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Only need to recalculate nearest points when lat/lon is different
   if(nob == 1 .or. &
         abs(obloclat(nob)-obloclat(nobm)) .gt. tiny(obloclat(nob)) .or. &
         abs(obloclon(nob)-obloclon(nobm)) .gt. tiny(obloclon(nob)) .or. &
         abs(corrlengthsq(nob)-corrlengthsq(nobm)) .gt. tiny(corrlengthsq(nob))) then
      nobm=nob
      ! determine localization length scales based on latitude of ob.
      nf2=0
      ! search analysis grid points for those within corrlength of 
      ! ob being assimilated (using a kd-tree for speed).
      if (kdgrid) then
         call kdtree2_r_nearest(tp=kdtree_grid,qv=obloc(:,nob), &
              r2=corrlengthsq(nob), &
              nfound=nf2,nalloc=nlevs_pres*numptsperproc(nproc+1), &
              results=sresults1)
      else
         ! use brute force search if number of grid points on this proc <= 3
         do nn=1,nlevs_pres
            do npt=1,numptsperproc(nproc+1)
               nnpt = nlevs_pres * (npt-1) + nn
               r = sum( (obloc(:,nob)-adloc_chunk(:,nnpt))**2, 1 )
               if (r < corrlengthsq(nob)) then
                  nf2 = nf2 + 1
                  sresults1(nf2)%idx = nnpt
                  sresults1(nf2)%dis = r
               end if
            end do
         end do
      end if
!$omp parallel do private(nob1)
      do nob1=1,nf2
         taper_disgrd(nob1) = taper(sqrt(sresults1(nob1)%dis*invcorlen(nob)))
      end do
   else
      nsame=nsame+1
   end if

   t3 = t3 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! forecast error sensitivity to the observations
   if (nf2 > 0) then
      taper3=taper(obtime(nob)*invobtimel(nob))
      uvwork(1:nanals) = zero
      tpwork(1:nanals) = zero
      qwork(1:nanals) = zero
    !  uvwork_efsr(1:nanals) = zero
    !  tpwork_efsr(1:nanals) = zero
    !  qwork_efsr(1:nanals) = zero
      covar_dry_work(1:nanals) = zero
      eg_min = zero
      eg_max = zero
      ef_min = zero
      ef_max = zero
   
     ! qwork(1:nanals) = zero
      ! rho * X_f^T (e_t|0 + e_t|-6) / 2
      ! contributions from (U,V), (T,Ps) and Q are computed separately
      do ii=1,nf2
         taper1=taper_disgrd(ii)*taper3
         i = (sresults1(ii)%idx-1)/nlevs_pres+1
         ngrd1=indxproc(nproc+1,i)
         if(tar_minlon <= tar_maxlon .and. &
              & (lonsgrd(ngrd1) < tar_minlon .or. lonsgrd(ngrd1) > tar_maxlon .or. &
              & latsgrd(ngrd1) < tar_minlat .or. latsgrd(ngrd1) > tar_maxlat)) &
              & cycle
         if(tar_minlon > tar_maxlon .and. &
              & ((lonsgrd(ngrd1) < tar_minlon .and. lonsgrd(ngrd1) > tar_maxlon) .or. &
              & latsgrd(ngrd1) < tar_minlat .or. latsgrd(ngrd1) > tar_maxlat)) &
              & cycle
         nn = sresults1(ii)%idx - (i-1)*nlevs_pres
         if((tar_minlev /= 1 .or. nn /= nlevs_pres) &
              & .and. (nn > tar_maxlev .or. nn < tar_minlev)) cycle
         lnsig = abs(lnp_chunk(i,nn)-oblnp(nob))
         if(lnsig < lnsigl(nob))then
            taperv=taper1*taper(lnsig*invlnsigl(nob))
         else
            cycle
         end if
         if(nn == nlevs_pres) then
            do nanal=1,nanals
               tpwork(nanal) = tpwork(nanal) + taperv &
                    & * anal_chunk(nanal,i,id_ps,nbackgrounds) * fcerror_chunk(i,id_ps)
              ! tpwork_efsr(nanal) = tpwork_efsr(nanal) + taperv &
              !      & * anal_chunk(nanal,i,id_ps,nbackgrounds) * fcerror_chunk_efsr(i,id_ps)
            end do
            cycle
         end if
         
      !   total_assimilation_impact(nob) = total_assimilation_impact(nob) + fcdiff_chunk(i,id_u(nn)) 

         ef_sum = taperv*(e_f(i,id_u(nn)) + e_f(i,id_v(nn)) + e_f(i,id_t(nn))) !+ e_f(i,id_q(nn)))
         eg_sum = taperv*(e_g(i,id_u(nn)) + e_g(i,id_v(nn)) + e_g(i,id_t(nn))) !+ e_g(i,id_q(nn)))

         ferror_post(nob) = ferror_post(nob) + ef_sum
         ferror_prior(nob) = ferror_prior(nob) + eg_sum

         efg_sum = ef_sum + eg_sum

    !     IF ( fcerror_chunk(i,id_u(nn)) /= (0.5_r_kind*(e_f(i,id_u(nn)) + e_g(i,id_u(nn))))/(real(nanals-1,r_kind)) ) then 
           !PRINT *, fcerror_chunk(i,id_u(nn)), '  ', (0.5_r_kind*(e_f(i,id_u(nn)) + e_g(i,id_u(nn))))/(real(nanals-1,r_kind))
    !       PRINT *, e_f(i,id_u(nn)), e_g(i,id_u(nn)), fcerror_chunk(i,id_u(nn))
      !     CYCLE
    !     endif

         ! Minimum forecast error assignments
    !     if (eg_sum < eg_min) then
    !       eg_min = eg_sum
    !     end if
    !     if (ef_sum < ef_min) then
    !       ef_min = ef_sum
    !     end if

         ! Maximum forecast error assignments
    !     if (eg_sum > eg_max) then
    !       eg_max = eg_sum
    !     end if
    !     if (ef_sum > ef_max) then
    !       ef_max = ef_sum
    !     end if

         if (efg_sum < eg_min) then
           eg_min = efg_sum
         end if
         if (ef_sum < ef_min) then
           ef_min = ef_sum
         end if

         ! Maximum forecast error assignments
         if (efg_sum > eg_max) then
           eg_max = efg_sum
         end if
         if (ef_sum > ef_max) then
           ef_max = ef_sum
         end if

         do nanal=1,nanals
            uvwork(nanal) = uvwork(nanal) + taperv &
                 * (anal_chunk(nanal,i,id_u(nn),nbackgrounds) * fcerror_chunk(i,id_u(nn)) &
                 & + anal_chunk(nanal,i,id_v(nn),nbackgrounds) * fcerror_chunk(i,id_v(nn)))
      !      uvwork_efsr(nanal) = uvwork_efsr(nanal) + taperv &
      !           * (anal_chunk(nanal,i,id_u(nn),nbackgrounds) * fcerror_chunk_efsr(i,id_u(nn)) &
      !           & + anal_chunk(nanal,i,id_v(nn),nbackgrounds) * fcerror_chunk_efsr(i,id_v(nn)))
      !      covar_kin_work(nanal) = covar_kin_work(nanal) + &
      !                              taperv*(anal_chunk(nanal,i,id_u(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_u(nn))) + &
      !                                      anal_chunk(nanal,i,id_v(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_v(nn)))) 


      !     covar_kin_work(nanal) = covar_kin_work(nanal) + &
      !                              taperv*(anal_chunk(nanal,i,id_u(nn),nbackgrounds) + &
      !                                      anal_chunk(nanal,i,id_v(nn),nbackgrounds))


            tpwork(nanal) = tpwork(nanal) + taperv &
                 & * anal_chunk(nanal,i,id_t(nn),nbackgrounds) * fcerror_chunk(i,id_t(nn))
       !     tpwork_efsr(nanal) = tpwork_efsr(nanal) + taperv &
       !          & * anal_chunk(nanal,i,id_t(nn),nbackgrounds) * fcerror_chunk_efsr(i,id_t(nn))
       !     covar_mass_work(nanal) = covar_mass_work(nanal) + &
       !                              taperv*anal_chunk(nanal,i,id_t(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_t(nn)))


       !     covar_mass_work(nanal) = covar_mass_work(nanal) + &
       !                              taperv*anal_chunk(nanal,i,id_t(nn),nbackgrounds)

!           covar_dry_work(nanal) = covar_dry_work(nanal) + &
!                                   taperv*(anal_chunk(nanal,i,id_t(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_t(nn))) + &
!                                           anal_chunk(nanal,i,id_u(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_u(nn))) + &
!                                           anal_chunk(nanal,i,id_v(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_v(nn))))

            covar_dry_work(nanal) = covar_dry_work(nanal) + &
                                    taperv*(anal_chunk(nanal,i,id_t(nn),nbackgrounds) + &
                                            anal_chunk(nanal,i,id_u(nn),nbackgrounds) + &
                                            anal_chunk(nanal,i,id_v(nn),nbackgrounds))
           
            if(id_q(nn) > 0) THEN 
              qwork(nanal) = qwork(nanal) + taperv * anal_chunk(nanal,i,id_q(nn),nbackgrounds) * fcerror_chunk(i,id_q(nn))
        !      qwork_efsr(nanal) = qwork_efsr(nanal) + taperv * anal_chunk(nanal,i,id_q(nn),nbackgrounds) * fcerror_chunk_efsr(i,id_q(nn))
            end if

          !    covar_sh_work(nanal) = covar_sh_work(nanal) + &
          !                           taperv*anal_chunk(nanal,i,id_q(nn),nbackgrounds)

       !       covar_moist_work(nanal) = covar_moist_work(nanal) + &
       !                                 taperv*(anal_chunk(nanal,i,id_q(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_q(nn))) + &
       !                                         anal_chunk(nanal,i,id_u(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_u(nn))) + &
       !                                         anal_chunk(nanal,i,id_v(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_v(nn))) + &
       !                                         anal_chunk(nanal,i,id_t(nn),nbackgrounds)*ABS(fcerror_chunk(i,id_t(nn))))

          !   covar_moist_work(nanal) = covar_moist_work(nanal) + &
          !                              taperv*(anal_chunk(nanal,i,id_q(nn),nbackgrounds) + &
          !                                      anal_chunk(nanal,i,id_u(nn),nbackgrounds) + &
          !                                      anal_chunk(nanal,i,id_v(nn),nbackgrounds) + &
          !                                      anal_chunk(nanal,i,id_t(nn),nbackgrounds))
           ! end if
         end do
      end do
      
      ! Assign final high and low 
      ! forecast errors
      ferror_prior_rlow(nob) = eg_min
      ferror_prior_rhigh(nob) = eg_max
      ferror_post_rlow(nob) = ef_min
      ferror_post_rhigh(nob) = ef_max

      ! Calculate means for covariance calculation
    !  mean_sh_work = SUM(covar_sh_work)/nanals
    !  mean_mass_work = SUM(covar_mass_work)/nanals
    !  mean_kin_work = SUM(covar_kin_work)/nanals
    !  mean_dry_work = SUM(covar_dry_work)/nanals
    !  mean_moist_work = SUM(covar_moist_work)/nanals
     ! mean_ob = SUM(anal_obtmp)/nanals

      ! R^-1 HX_a [rho * X_f^T (e_t|0 + e_t|-6) / 2]
      do nanal=1,nanals
         djdy_kin(nob) = djdy_kin(nob) + anal_obtmp(nanal) * uvwork(nanal)
         djdy_dry(nob) = djdy_dry(nob) + anal_obtmp(nanal) * tpwork(nanal)
         djdy_moist(nob) = djdy_moist(nob) + anal_obtmp(nanal) * qwork(nanal)

!!!!EFSR
        ! djdy_kin_efsr(nob) = djdy_kin_efsr(nob) + anal_obtmp(nanal) * uvwork_efsr(nanal)
        ! djdy_dry_efsr(nob) = djdy_dry_efsr(nob) + anal_obtmp(nanal) * tpwork_efsr(nanal)
        ! djdy_moist_efsr(nob) = djdy_moist_efsr(nob) + anal_obtmp(nanal) * qwork_efsr(nanal)
!!!!EFSR

    !     covar_kin(nob) = covar_kin(nob) + (covar_kin_work(nanal)-mean_kin_work)*(anal_obtmp(nanal)-mean_ob)
    !     covar_sh(nob) = covar_sh(nob) + (covar_sh_work(nanal)-mean_sh_work)*(anal_obtmp(nanal)-mean_ob)
    !     covar_mass(nob) = covar_mass(nob) + (covar_mass_work(nanal)-mean_mass_work)*(anal_obtmp(nanal)-mean_ob)
         covar_dry(nob) = covar_dry(nob) + covar_dry_work(nanal)*anal_obtmp(nanal)
!         IF ( covar_dry(nob) > 10.0_r_single ) THEN
        !   PRINT *, taperv, covar_dry_work(nanal), 'Likely problem instance'
!         ENDIF 
    !     covar_moist(nob) = covar_moist(nob) + (covar_moist_work(nanal)-mean_moist_work)*(anal_obtmp(nanal)-mean_ob)
      end do
    !  covar_kin(nob) = covar_kin(nob)/(nanals - 1)
    !  covar_sh(nob) = covar_sh(nob)/(nanals - 1)
    !  covar_mass(nob) = covar_mass(nob)/(nanals - 1)
      covar_dry(nob) = covar_dry(nob)/(nanals - 1)
   !   IF ( covar_dry(nob) > 1.0_fp ) THEN 
   !      PRINT *, anal_obtmp
    !  covar_moist(nob) = covar_moist(nob)/(nanals - 1)
   end if
   t4 = t4 + mpi_wtime() - t1
   t1 = mpi_wtime()
  ! PRINT *, covar_dry(nob), 'covar_dry in efsoi code'
  ! PRINT *, eg_max, eg_min, ef_max, ef_min 
   ! PRINT *, ferror_post_rlow(nob), ferror_post_rhigh(nob), ferror_prior_rlow(nob), ferror_prior_rhigh(nob)
end do obsloop

tend = mpi_wtime()
if (nproc == 0 .or. nproc == numproc-1) print *,'time to process FSO on gridpoint = ',tend-tbegin,t2,t3,t4,' secs'

if (nproc == 0 .and. nskip > 0) print *,nskip,' out of',nobstot,'obs skipped'
if (nproc == 0 .and. nsame > 0) print *,nsame,' out of', nobstot-nskip,' same lat/lon'

! Observation sensitivity post process
!$omp parallel do private(nob)
do nob=1,nobstot
   djdy_dry(nob) = djdy_dry(nob) + djdy_kin(nob)
   djdy_moist(nob) = djdy_moist(nob) + djdy_dry(nob)
   obsense_kin(nob) = djdy_kin(nob) * obdep(nob)
   obsense_dry(nob) = djdy_dry(nob) * obdep(nob)
   obsense_moist(nob) = djdy_moist(nob) * obdep(nob)
   obsense_kin_nbc(nob) = djdy_kin(nob) * obdep_nbc(nob)
   obsense_dry_nbc(nob) = djdy_dry(nob) * obdep_nbc(nob)
   obsense_moist_nbc(nob) = djdy_moist(nob) * obdep_nbc(nob)
   
!!!!EFSR
!   djdy_dry_efsr(nob) = djdy_dry_efsr(nob) + djdy_kin_efsr(nob)
!   djdy_moist_efsr(nob) = djdy_moist_efsr(nob) + djdy_dry_efsr(nob)
   obsense_kin_efsr(nob) = djdy_kin(nob) * obdep_efsr(nob)
   obsense_dry_efsr(nob) = djdy_dry(nob) * obdep_efsr(nob)
   obsense_moist_efsr(nob) = djdy_moist(nob) * obdep_efsr(nob)


end do

! Gathering analysis perturbations projected on the observation space
if(nproc /= 0) then
  call mpi_send(anal_obchunk_prior,numobsperproc(nproc+1)*nanals,mpi_real4,0, &
       1,mpi_comm_world,ierr)
else
   allocate(anal_ob(1:nanals,nobstot))
   allocate(buffertmp(nanals,nobs_max))
   do np=1,numproc-1
      call mpi_recv(buffertmp,numobsperproc(np+1)*nanals,mpi_real4,np, &
           1,mpi_comm_world,mpi_status,ierr)
      do nob1=1,numobsperproc(np+1)
         nob2 = indxproc_obs(np+1,nob1)
         anal_ob(:,nob2) = buffertmp(:,nob1)
      end do
   end do
   do nob1=1,numobsperproc(1)
      nob2 = indxproc_obs(1,nob1)
      anal_ob(:,nob2) = anal_obchunk_prior(:,nob1)
   end do
   deallocate(buffertmp)
end if

t1 = mpi_wtime() - tend
if (nproc == 0) print *,'time to observation sensitivity post process = ',t1,' secs'

deallocate(anal_obchunk_prior)
deallocate(sresults1)
deallocate(djdy_kin,djdy_dry,djdy_moist,taper_disgrd,uvwork,tpwork,qwork)!,uvwork_efsr,tpwork_efsr,qwork_efsr)
!deallocate(djdy_kin_efsr,djdy_dry_efsr,djdy_moist_efsr)
deallocate(covar_dry_work)
!deallocate(covar_sh_work,covar_mass_work,covar_kin_work,covar_dry_work,covar_moist_work)
return
end subroutine efsoi_update
end module efsoi
