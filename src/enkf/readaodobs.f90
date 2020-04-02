MODULE readaodobs
!$$$  module documentation block
!
! module: readaodobs                    read aod data from files.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read aod data from files written out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_aodobs: determine the number of observations to read.
!  get_aodobs_data: read the data and calculate H(x) for ensemble members.
!  write_aodobs_data: output diag file with spread
!   
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!   2016-11-29 shlyaeva - updated read routine to calculate linearized H(x)
!                         added write_aodobs_data to output ensemble spread
!
! attributes:
!   language: f95
!
!$$$

  USE kinds, ONLY: r_single,i_kind,r_kind,r_double
  USE params, ONLY: nsats_aod,sattypes_aod,npefiles,netcdf_diag
  USE constants, ONLY: deg2rad, zero
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: get_num_aodobs, get_aodobs_data, write_aodobs_data

CONTAINS

! get number of ozone observations
  SUBROUTINE get_num_aodobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    IMPLICIT NONE
    CHARACTER(len=500), INTENT(in)  :: obspath
    CHARACTER(len=10),  INTENT(in)  :: datestring
    CHARACTER(len=10),  INTENT(in)  :: id
    INTEGER(i_kind),    INTENT(out) :: num_obs_tot, num_obs_totdiag
    
    IF (netcdf_diag) THEN
       CALL get_num_aodobs_nc(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    ELSE
       CALL stop2(701)
    ENDIF

  END SUBROUTINE get_num_aodobs
 
! get number of aod observations
  
  SUBROUTINE get_num_aodobs_nc(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    USE nc_diag_read_mod, ONLY: nc_diag_read_get_var
    USE nc_diag_read_mod, ONLY: nc_diag_read_get_dim
    USE nc_diag_read_mod, ONLY: nc_diag_read_init, nc_diag_read_close
    IMPLICIT NONE
    
    CHARACTER(len=500), INTENT(in)  :: obspath
    CHARACTER(len=10),  intent(in)  :: datestring
    INTEGER(i_kind),    INTENT(out) :: num_obs_tot, num_obs_totdiag
    CHARACTER(len=8),   INTENT(in)  :: id
    
    character(len=500) obsfile
    character(len=4) pe_name
    real(r_kind) :: errorlimit,errorlimit2
    integer(i_kind) iunit
    integer(i_kind) :: i, nsat, ipe, nobs_curr
    integer(i_kind):: nread,nkeep
    logical :: fexist

    real(r_single),  allocatable, dimension (:) :: Pressure
    real(r_single), allocatable, dimension (:) :: Analysis_Use_Flag
    real(r_single),  allocatable, dimension (:) :: Errinv
    real(r_single),  allocatable, dimension (:) :: Observation

    num_obs_tot = 0
    num_obs_totdiag = 0
!   make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

    do nsat=1,nsats_aod
        nread = 0
        nkeep = 0

        peloop: DO ipe=0,npefiles

           WRITE(pe_name,'(i4.4)') ipe
           IF (npefiles .EQ. 0) THEN
              obsfile = TRIM(ADJUSTL(obspath))//"/"//&
                   &datestring//"/"//TRIM(ADJUSTL(id))//&
                   &"/"//TRIM(sattypes_aod(nsat))//"_hofx.nc4"
              
              INQUIRE(file=obsfile,exist=fexist)
              IF (.NOT. fexist) CALL stop2(704)

           ELSE ! read raw, unconcatenated pe* files.
              obsfile = TRIM(ADJUSTL(obspath))//TRIM(sattypes_aod(nsat))//&
                   &"_hof_"//datestring//"_"//TRIM(ADJUSTL(id))//&
                   &"_"//pe_name//".nc4"
              INQUIRE(file=obsfile,exist=fexist)
              IF (.NOT. fexist) CALL stop2(705)
           ENDIF
           
           INQUIRE(file=obsfile,exist=fexist)
           IF (.NOT. fexist) CYCLE peloop

           call nc_diag_read_init(obsfile, iunit)

           nobs_curr = nc_diag_read_get_dim(iunit,'nlocs')

           if (nobs_curr <= 0) then
              call nc_diag_read_close(obsfile)
              cycle peloop
           endif

           allocate(Pressure(nobs_curr), Analysis_Use_Flag(nobs_curr),     &
                   Errinv(nobs_curr), Observation(nobs_curr))

!           call nc_diag_read_get_var(iunit, 'Reference_Pressure', Pressure)
!@mzp not present - set to 100000 Pa so that everybody is within localization
!localizatiom replaced with first model level pressure im enkf - does not matter 
          Pressure=100000_r_kind
           call nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@PreQc', Analysis_Use_Flag)
           call nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@ObsError', Errinv)
           Errinv=1_r_kind/Errinv
           call nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@ObsValue', Observation)

           call nc_diag_read_close(obsfile)

           num_obs_totdiag = num_obs_totdiag + nobs_curr
           nread = nread + nobs_curr
           do i = 1, nobs_curr
             if (Analysis_Use_Flag(i) < 0 .or. Pressure(i) <= 0.001 .or. &
                 Pressure(i) > 120000_r_kind) cycle
             if (Errinv(i) <= errorlimit .or.  &
                 Errinv(i) >= errorlimit2 .or.  &
                 abs(Observation(i)) > 1.e9_r_kind) cycle
              nkeep = nkeep + 1
              num_obs_tot = num_obs_tot + 1

           end do
           if (ipe .eq. npefiles) then
              write(6,100) nsat,trim(sattypes_aod(nsat)),nread,nkeep,num_obs_tot
100           format(2x,i3,2x,a20,2x,'nread= ',i9,2x,'nkeep=',i9,2x,'num_obs_tot= ',i9)
           endif
           deallocate(Pressure, Analysis_Use_Flag, Errinv, Observation)
        enddo peloop ! ipe
    enddo ! satellite
    print *,num_obs_tot,' aod obs'
    print *,num_obs_totdiag, ' total aod obs in diag file'
  END SUBROUTINE get_num_aodobs_nc


! read aod observation data
  SUBROUTINE get_aodobs_data(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, hx_modens, x_obs, x_err, &
       x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal, nmem)
    USE params, ONLY: neigv
    IMPLICIT NONE
    CHARACTER*500, INTENT(in) :: obspath
    CHARACTER(len=*), INTENT(in)  :: datestring
    
    INTEGER(i_kind), INTENT(in) :: nobs_max, nobs_maxdiag
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: hx_mean, hx_mean_nobc, hx
  ! hx_modens holds modulated ensemble in ob space (zero size and not referenced if neigv=0)
    REAL(r_single), DIMENSION(neigv,nobs_max), INTENT(out) :: hx_modens
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_obs
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_err, x_errorig
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_lon, x_lat
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_press, x_time
    INTEGER(i_kind), DIMENSION(nobs_max), INTENT(out)     :: x_code
    CHARACTER(len=20), DIMENSION(nobs_max), INTENT(out)   ::  x_type
    INTEGER(i_kind), DIMENSION(nobs_maxdiag), INTENT(out) :: x_used
    
    CHARACTER(len=8), INTENT(in) :: id
    INTEGER(i_kind), INTENT(in)  :: nanal,nmem
    
    IF (netcdf_diag) THEN
       CALL get_aodobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, hx_modens, x_obs, x_err, &
            x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal, nmem)
    ELSE
       call stop2(706)
    ENDIF
    
  END SUBROUTINE get_aodobs_data

  SUBROUTINE get_aodobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, hx_modens, x_obs, x_err, &
       x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal, nmem)
    USE nc_diag_read_mod, ONLY: nc_diag_read_get_var
    USE nc_diag_read_mod, ONLY: nc_diag_read_get_dim, nc_diag_read_get_global_attr
    USE nc_diag_read_mod, ONLY: nc_diag_read_init, nc_diag_read_close
    
    USE sparsearr,ONLY:sparr, sparr2, readarray, delete, ASSIGNMENT(=)
    USE params,ONLY: nanals, lobsdiag_forenkf, neigv, vlocal_evecs
    USE statevec, ONLY: state_d
    USE mpisetup, ONLY: mpi_wtime, nproc
    USE observer_enkf, ONLY: calc_linhx, calc_linhx_modens, setup_linhx
    IMPLICIT NONE
    
    CHARACTER*500, INTENT(in) :: obspath
    CHARACTER(len=*), INTENT(in)  :: datestring
    
    INTEGER(i_kind), INTENT(in) :: nobs_max, nobs_maxdiag
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: hx_mean, hx_mean_nobc, hx
! hx_modens holds modulated ensemble in ob space (zero size and not referenced if neigv=0)
    REAL(r_single), DIMENSION(neigv,nobs_max), INTENT(out) :: hx_modens
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_obs
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_err, x_errorig
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_lon, x_lat
    REAL(r_single), DIMENSION(nobs_max), INTENT(out)      :: x_press, x_time
    INTEGER(i_kind), DIMENSION(nobs_max), INTENT(out)     :: x_code
    CHARACTER(len=20), DIMENSION(nobs_max), INTENT(out)   :: x_type
    INTEGER(i_kind), DIMENSION(nobs_maxdiag), INTENT(out) :: x_used
    
    CHARACTER(len=8), INTENT(in) :: id
    INTEGER(i_kind), INTENT(in)  :: nanal, nmem
    
    CHARACTER*500    :: obsfile, obsfile2
    CHARACTER(len=8) :: id2
    CHARACTER(len=4) :: pe_name
    
    INTEGER(i_kind) :: nobs_curr, nob, nobdiag, i, nsat, ipe, nsdim
    INTEGER(i_kind) :: iunit, iunit2
    
    REAL(r_double) t1,t2,tsum
    TYPE(sparr)   :: dhx_dx
    
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: Latitude, Longitude, Pressure, Time
    REAL(r_single), ALLOCATABLE, DIMENSION (:) :: Analysis_Use_Flag
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: Errinv
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: Observation
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: Obs_Minus_Forecast_adjusted, Obs_Minus_Forecast_adjusted2
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: Obs_Minus_Forecast_unadjusted
    REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: Observation_Operator_Jacobian
    REAL(r_single),  ALLOCATABLE, DIMENSION (:) :: bias

    LOGICAL fexist
    LOGICAL twofiles, fexist2
    REAL(r_kind) :: errorlimit,errorlimit2
    
    INTEGER(i_kind) :: ix, iy, it, ixp, iyp, itp
    REAL(r_kind) :: delx, dely, delxp, delyp, delt, deltp
    REAL(r_single) :: rlat,rlon,rtim,rlat_prev,rlon_prev,rtim_prev,eps
    
! make consistent with screenobs
    errorlimit=1._r_kind/SQRT(1.e9_r_kind)
    errorlimit2=1._r_kind/SQRT(1.e-6_r_kind)
    eps = 1.e-3
    
    twofiles = (.NOT. lobsdiag_forenkf) .AND. (nanal <= nanals)
    id2 = 'ensmean'
    IF (nanal <= nanals) THEN
       WRITE(id2,'(a3,(i3.3))') 'mem',nanal
    ENDIF
    
    tsum = 0
    nob = 0
    rlat_prev = -1.e30; rlon_prev=-1.e30; rtim_prev = -1.e30
    nobdiag = 0
    x_used = 0
    
    hx = zero
    
    DO nsat=1,nsats_aod

       peloop: DO ipe=0,npefiles
          WRITE(pe_name,'(i4.4)') ipe
          
          IF (npefiles .EQ. 0) THEN
             obsfile = TRIM(ADJUSTL(obspath))//"/"//&
                  &datestring//"/"//TRIM(ADJUSTL(id))//&
                  &"/"//TRIM(sattypes_aod(nsat))//"_hofx.nc4"
             INQUIRE(file=obsfile,exist=fexist)
             IF (.NOT. fexist) CALL stop2(704)
          ELSE ! read raw, unconcatenated pe* files.
             obsfile = TRIM(ADJUSTL(obspath))//TRIM(sattypes_aod(nsat))&
                  &//"_hofx_"//datestring//"_"//TRIM(ADJUSTL(id))//&
                  &"_"//pe_name//".nc4"
             INQUIRE(file=obsfile,exist=fexist)
             IF (.NOT. fexist) CALL stop2(705)
          ENDIF
          
          INQUIRE(file=obsfile,exist=fexist)
          IF (.NOT. fexist) CYCLE peloop
          
          CALL nc_diag_read_init(obsfile, iunit)
          
          nobs_curr = nc_diag_read_get_dim(iunit,'nlocs')

          IF (nobs_curr <= 0) THEN
             CALL nc_diag_read_close(obsfile)
             CYCLE peloop
          ENDIF
          
          ALLOCATE(Latitude(nobs_curr), Longitude(nobs_curr), Time(nobs_curr), Pressure(nobs_curr), &
               Analysis_Use_Flag(nobs_curr), Errinv(nobs_curr), Observation(nobs_curr),         &
               Obs_Minus_Forecast_adjusted(nobs_curr), Obs_Minus_Forecast_unadjusted(nobs_curr))
          ALLOCATE(bias(nobs_curr))

          CALL nc_diag_read_get_var(iunit, 'latitude@MetaData', Latitude)
          CALL nc_diag_read_get_var(iunit, 'longitude@MetaData', Longitude)
          CALL nc_diag_read_get_var(iunit, 'time@MetaData', Time)
!          CALL nc_diag_read_get_var(iunit, 'Reference_Pressure', Pressure)
!@mzp not present - set to 100000 Pa so that everybody is within localization
          Pressure=100000_r_kind
          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@PreQc', Analysis_Use_Flag)

          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@ObsError', Errinv)
          Errinv=1_r_kind/Errinv
          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@ObsValue', Observation)
!@mzp until fix ombg found use hofx0
          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@Hofx', Obs_Minus_Forecast_adjusted)
          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@Hofx', Obs_Minus_Forecast_unadjusted)
          CALL nc_diag_read_get_var(iunit, 'aerosol_optical_depth_4@KnownObsBias',bias)
          
          IF (lobsdiag_forenkf) THEN

             CALL stop2(707)
             CALL nc_diag_read_get_global_attr(iunit, "Number_of_state_vars", nsdim)
             ALLOCATE(Observation_Operator_Jacobian(nsdim, nobs_curr))
             CALL nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian', Observation_Operator_Jacobian)

          ENDIF
          
          CALL nc_diag_read_close(obsfile)
          
          IF (twofiles) THEN
             IF (npefiles .EQ. 0) THEN
                obsfile2 = TRIM(ADJUSTL(obspath))//"/"//&
                     &datestring//"/"//TRIM(ADJUSTL(id2))//&
                     &"/"//TRIM(sattypes_aod(nsat))//"_hofx.nc4"
                INQUIRE(file=obsfile2,exist=fexist2)
                IF (.NOT. fexist2) CALL stop2(714)
             ELSE ! read raw, unconcatenated pe* files.
                obsfile2 = TRIM(ADJUSTL(obspath))//TRIM(sattypes_aod(nsat))&
                     &//"_hofx_"//datestring//"_"//TRIM(ADJUSTL(id2))//&
                     &"_"//pe_name//".nc4"
                INQUIRE(file=obsfile2,exist=fexist2)
                IF (.NOT. fexist2) CALL stop2(715)
             ENDIF
             
             CALL nc_diag_read_init(obsfile2, iunit2)
             
             ALLOCATE(Obs_Minus_Forecast_adjusted2(nobs_curr))
             CALL nc_diag_read_get_var(iunit2, 'aerosol_optical_depth_4@Hofx', Obs_Minus_Forecast_adjusted2)
             
             CALL nc_diag_read_close(obsfile2)
             
          END IF

!no check for twofiles - obs must be the in the same order

          DO i = 1, nobs_curr
             nobdiag = nobdiag + 1
             IF (Analysis_Use_Flag(i) < 0 .OR. Pressure(i) <= 0.001 .OR. &
                  Pressure(i) > 120000_r_kind) CYCLE
             
             IF (Errinv(i) <= errorlimit .OR. Errinv(i) >= errorlimit2 .OR.  &
                  ABS(Observation(i)) > 5_r_kind) CYCLE
             nob = nob + 1
             x_used(nobdiag) = 1
             x_code(nob) = 800  ! made up code one channel only 
             x_lat(nob) = Latitude(i)
             x_lon(nob) = Longitude(i)
             x_press(nob) = Pressure(i)
             x_time(nob) = Time(i)
             x_err(nob) = (1./Errinv(i))**2
             x_errorig(nob) = x_err(nob)
             x_obs(nob) =  MAX(Observation(i)-bias(i),zero)
!@mzp until fix ombg found use hofx0
!             hx_mean(nob) = Observation(i) - Obs_Minus_Forecast_adjusted(i)
             hx_mean(nob) = Obs_Minus_Forecast_adjusted(i)
!@mzp until fix ombg found use hofx0
!             hx_mean_nobc(nob) = Observation(i) - Obs_Minus_Forecast_unadjusted(i)
             hx_mean_nobc(nob) = Obs_Minus_Forecast_unadjusted(i)
             x_type(nob) = 'aod                 '
             IF (nanal <= nanals) THEN
! read full Hx from diag file
                IF (.NOT. lobsdiag_forenkf) THEN
!                   hx(nob) = Observation(i) - Obs_Minus_Forecast_adjusted2(i)
                   hx(nob) = Obs_Minus_Forecast_adjusted2(i)
! run linearized Hx
                ELSE
                   !for aod should never be here for now @mzp
                   WRITE(6,*)'for aod should never be here for now @mzp'
                   CALL stop2(708)

                   dhx_dx = Observation_Operator_Jacobian(1:nsdim,i)
                   t1 = mpi_wtime()
                   rlat = x_lat(nob)*deg2rad
                   rlon = x_lon(nob)*deg2rad
                   rtim = x_time(nob)
                   IF (nob > 1) THEN
                      rlat_prev = x_lat(nob-1)*deg2rad
                      rlon_prev = x_lon(nob-1)*deg2rad
                      rtim_prev = x_time(nob-1)
                   ENDIF
                   IF (ABS(rlat-rlat_prev) > eps .OR. &
                        ABS(rlon-rlon_prev) > eps .OR. &
                        ABS(rtim-rtim_prev) > eps) THEN
                      CALL setup_linhx(rlat,rlon,rtim,              &
                           ix, delx, ixp, delxp, iy, dely,  &
                           iyp, delyp, it, delt, itp, deltp)
                   ENDIF
                   CALL calc_linhx(hx_mean_nobc(nob), state_d(:,:,:,nmem),       &
                        dhx_dx, hx(nob),                  &
                        ix, delx, ixp, delxp, iy, dely,   &
                        iyp, delyp, it, delt, itp, deltp)
! compute modulated ensemble in obs space
                   IF (neigv > 0) THEN
                      CALL calc_linhx_modens(hx_mean_nobc(nob), state_d(:,:,:,nmem), &
                           dhx_dx, hx_modens(:,nob),          &
                           ix, delx, ixp, delxp, iy, dely,    &
                           iyp, delyp, it, delt, itp, deltp, vlocal_evecs)
                   ENDIF
                   t2 = mpi_wtime()
                   tsum = tsum + t2-t1
                   
                   CALL delete(dhx_dx)
                ENDIF
             ENDIF

          END DO ! nobs_curr
          
          DEALLOCATE(Latitude, Longitude, Time, Pressure, Analysis_Use_Flag, Errinv, &
               Observation, Obs_Minus_Forecast_adjusted,                       &
               Obs_Minus_Forecast_unadjusted)
          DEALLOCATE(bias)
          IF (twofiles) THEN
             DEALLOCATE(Obs_Minus_Forecast_adjusted2)
          ENDIF
          IF (lobsdiag_forenkf) THEN
             DEALLOCATE(Observation_Operator_Jacobian)
          ENDIF
       ENDDO peloop ! ipe
    ENDDO ! satellite
    IF (nanal == nanals .AND. lobsdiag_forenkf) PRINT *, 'time in calc_linhx for aod obs on proc',nproc,' =',tsum
    
    IF (nob /= nobs_max) THEN
       PRINT *,'number of obs not what expected in get_aodobs_data',nob,nobs_max
       CALL stop2(709)
    END IF
    IF (nobdiag /= nobs_maxdiag) THEN
       PRINT *,'number of total diag obs not what expected in get_aodobs_data',nobdiag,nobs_maxdiag
       CALL stop2(710)
    END IF
    
  END SUBROUTINE get_aodobs_data_nc
  
  SUBROUTINE write_aodobs_data(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)

    CHARACTER*500, INTENT(in) :: obspath
    CHARACTER*500 obsfile,obsfile2
    CHARACTER(len=10), INTENT(in) :: datestring
    CHARACTER(len=8), INTENT(in) :: id,id2,gesid2
    CHARACTER(len=4) pe_name

    CHARACTER(20) :: isis     ! sensor/instrument/satellite id
    CHARACTER(10) :: obstype  !  type of aod obs
    CHARACTER(10) :: dplat    ! sat sensor

    INTEGER(i_kind) iunit,jiter,i,ireal,idate,nob,nobdiag,ios,nobs_max,nobs_maxdiag,nsat
    INTEGER(i_kind) iunit2
    REAL(r_single), DIMENSION(nobs_max) :: x_fit, x_sprd
    INTEGER(i_kind), DIMENSION(nobs_maxdiag) :: x_used

    REAL(r_single),ALLOCATABLE,DIMENSION(:)::diagbuf
    REAL(r_single),ALLOCATABLE,DIMENSION(:,:)::diagbufchan
    INTEGER(i_kind), ALLOCATABLE, DIMENSION(:) :: iouse,ich
    LOGICAL fexist, init_pass

    INTEGER(i_kind) :: nchanl,ipchan,nsig,idiag,nuchan
    REAL(r_single) ::  freq4,pol4,wave4,varch4

    INTEGER(i_kind) ipe

    RETURN

    iunit = 7
    iunit2 = 17
    nob = 0
    nobdiag = 0

    DO nsat=1,nsats_aod
       init_pass = .TRUE.
       IF (datestring .EQ. '0000000000') THEN
          obsfile2 = TRIM(ADJUSTL(obspath))//"diag_"//TRIM(sattypes_aod(nsat))//"_"//TRIM(ADJUSTL(gesid2))//"."//TRIM(ADJUSTL(id2))
       ELSE 
          obsfile2 = TRIM(ADJUSTL(obspath))//"diag_"//TRIM(sattypes_aod(nsat))//"_"//TRIM(ADJUSTL(gesid2))//"."//datestring//'_'//TRIM(ADJUSTL(id2))
       ENDIF
       peloop: DO ipe=0,npefiles
          WRITE(pe_name,'(i4.4)') ipe
          IF (npefiles .EQ. 0) THEN
! diag file (concatenated pe* files)
             obsfile = TRIM(ADJUSTL(obspath))//"diag_"//TRIM(sattypes_aod(nsat))//"_ges."//datestring//'_'//TRIM(ADJUSTL(id))
             INQUIRE(file=obsfile,exist=fexist)
             IF (.NOT. fexist .OR. datestring .EQ. '0000000000') THEN
                obsfile = TRIM(ADJUSTL(obspath))//"diag_"//TRIM(sattypes_aod(nsat))//"_ges."//TRIM(ADJUSTL(id))
             ENDIF
          ELSE ! raw, unconcatenated pe* files.
             obsfile = TRIM(ADJUSTL(obspath))//'gsitmp_'//TRIM(ADJUSTL(id))//'/pe'//pe_name//'.'//TRIM(sattypes_aod(nsat))//'_01'
          ENDIF
          INQUIRE(file=obsfile,exist=fexist)
          IF (.NOT. fexist) CYCLE peloop
          OPEN(iunit,form="unformatted",file=obsfile,iostat=ios)
          REWIND(iunit)
          IF (init_pass) THEN
             OPEN(iunit2,form="unformatted",file=obsfile2,iostat=ios)
             READ(iunit,err=20,END=30) isis,dplat,obstype,jiter,nchanl,idate,ireal,ipchan,nsig
             WRITE(iunit2,err=20)  isis,dplat,obstype,jiter,nchanl,idate,ireal,ipchan,nsig
             idiag=ipchan
             ALLOCATE(iouse(nchanl),ich(nchanl))

             DO i=1,nchanl
                READ(iunit,err=20,END=30) freq4,pol4,wave4,varch4,&
                     &iouse(i),nuchan,ich(i)
                WRITE(iunit2,err=20)freq4,pol4,wave4,varch4,&
                     &iouse(i),nuchan,ich(i)
             ENDDO

             init_pass = .FALSE.
          ENDIF

10        CONTINUE

          ALLOCATE(diagbuf(ireal),diagbufchan(idiag,nchanl))

          READ(iunit,err=20,END=30) diagbuf,diagbufchan
          diagbufchan(2,:) = 1.e10

          DO i=1,nchanl
             nobdiag = nobdiag + 1
             IF (x_used(nobdiag) == 1) THEN
                nob = nob + 1
                diagbufchan(2,i) = x_fit(nob)
                diagbufchan(7,i) = x_sprd(nob)
             ENDIF
          ENDDO

          WRITE(iunit2) diagbuf,diagbufchan
          DEALLOCATE(diagbuf,diagbufchan)
          go to 10
20        CONTINUE
          PRINT *,'error reading diag_aod file'
30        CONTINUE
          CLOSE(iunit)
       ENDDO peloop ! ipe
       CLOSE(iunit2)
    ENDDO ! satellite
    
    IF (nob /= nobs_max) THEN
       PRINT *,'number of obs not what expected in write_aodobs_data',nob,nobs_max
       CALL stop2(711)
    END IF
    IF (nobdiag /= nobs_maxdiag) THEN
       PRINT *,'number of total diag obs not what expected in write_aodobs_data',nobdiag,nobs_maxdiag
       CALL stop2(712)
    END IF
    
    IF(ALLOCATED(iouse))DEALLOCATE(iouse,ich)
    
  END SUBROUTINE write_aodobs_data

END MODULE readaodobs
