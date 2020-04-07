module readsatobs
!$$$  module documentation block
!
! module: readsatobs                   read data from satellite radiance
!                                      diag* files.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read data from satellite radiance diag* files written out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_satobs: determine the number of observations to read.
!  get_satobs_data: read the data and calculate H(x) for ensemble members.
!  write_satobs_data: output diag file with spread
!   
! Public Variables: 
!
! Modules Used: read_diag
!
! program history log:
!   2009-02-23  Initial version.
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!   2016-11-29 shlyaeva - updated read routine to calculate linearized H(x)
!                         added write_ozvobs_data to output ensemble spread
!   2017-12-13  shlyaeva - added netcdf diag read/write capability
!
! attributes:
!   language: f95
!
!$$$

use kinds, only: r_kind,i_kind,r_single,r_double
use read_diag, only: diag_data_fix_list,diag_header_fix_list,diag_header_chan_list, &
    diag_data_chan_list,diag_data_extra_list,read_radiag_data,read_radiag_header, &
    diag_data_name_list, open_radiag, close_radiag
use params, only: nsats_rad, dsis, sattypes_rad, use_correlated_oberrs

implicit none

private
public :: get_satobs_data, get_num_satobs, write_satobs_data

contains

! get number of radiance observations from netcdf file
subroutine get_num_satobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
  use radinfo, only: iuse_rad,nusis,jpch_rad
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close
  implicit none

    character(len=500), intent(in)  :: obspath
    character(len=10),  intent(in)  :: id, datestring
    integer(i_kind),    intent(out) :: num_obs_tot, num_obs_totdiag

    character(len=500) obsfile
    character(len=20) ::  sat_type
    integer(i_kind) iunit, nsat, nobs, nchans, nkeep, i, jpchstart
    logical fexist
    real(r_kind) :: errorlimit,errorlimit2

    integer(i_kind), dimension(:), allocatable :: Satinfo_Chan, Use_Flag, chind
    real(r_single), dimension(:), allocatable :: Pressure, QC_Flag, Inv_Error, Observation


!  make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
    iunit = 7

    num_obs_tot = 0
    num_obs_totdiag = 0

    do nsat=1,nsats_rad
        jpchstart=0
        do i=1,jpch_rad
          write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select

          if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
            jpchstart=i
            exit
          end if
        end do
        if(jpchstart == 0) cycle

        ! read diag file (concatenated pe* files)
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
        inquire(file=obsfile,exist=fexist)
        if (.not. fexist .or. datestring .eq. '0000000000') &
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"

        inquire(file=obsfile,exist=fexist)
        if (.not.fexist) cycle 

        nkeep = 0

        call nc_diag_read_init(obsfile, iunit)

        nobs = nc_diag_read_get_dim(iunit,'nobs')

        if (nobs <= 0) then
           call nc_diag_read_close(obsfile)
           cycle 
        endif

        nchans = nc_diag_read_get_dim(iunit,'nchans')
        allocate(Satinfo_Chan(nchans), Use_Flag(nchans), Pressure(nobs), QC_Flag(nobs),     &
                 Inv_Error(nobs), Observation(nobs), chind(nobs))

        call nc_diag_read_get_var(iunit, 'satinfo_chan', Satinfo_Chan)
        call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
        call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
        call nc_diag_read_get_var(iunit, 'Press_Max_Weight_Function', Pressure)
        call nc_diag_read_get_var(iunit, 'QC_Flag', QC_Flag)
        call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error', Inv_Error)
        call nc_diag_read_get_var(iunit, 'Observation', Observation)

        call nc_diag_read_close(obsfile)


        do i = 1, nobs
           num_obs_totdiag = num_obs_totdiag + 1
           if(Use_Flag(chind(i)) < 1 ) cycle 
           if(QC_Flag(i) < 0. .or. Inv_Error(i) < errorlimit &
              .or. Inv_Error(i) > errorlimit2 &
              .or. Satinfo_Chan(chind(i)) == 0) cycle
           if(abs(Observation(i)) > 1.e9_r_kind) cycle 
           nkeep = nkeep + 1
        enddo
        num_obs_tot = num_obs_tot + nkeep

        write(6,100) nsat,trim(sattypes_rad(nsat)),num_obs_tot
100     format(2x,i3,2x,a20,2x,'num_obs_tot= ',i9)

        deallocate(Satinfo_Chan, Use_Flag, Pressure, QC_Flag, Inv_Error, Observation, chind)
    enddo ! satellite

end subroutine get_num_satobs

! read radiance data from netcdf file
subroutine get_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx, hx_modens, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_indx, x_used, id, nanal, nmem)
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim, nc_diag_read_get_global_attr
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close

  use radinfo, only: iuse_rad,nusis,jpch_rad
  use params, only: nanals, neigv, vlocal_evecs
  use statevec, only: state_d
  use constants, only: deg2rad, zero
  use mpisetup, only: nproc, mpi_wtime
  use observer_enkf, only: calc_linhx, calc_linhx_modens, setup_linhx
  use sparsearr, only: sparr, assignment(=), delete, sparr2, new, &
                       init_raggedarr, raggedarr

  implicit none

  character*500, intent(in)     :: obspath
  character(len=10), intent(in) ::  datestring

  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out) :: hx_mean, hx
  ! hx_modens holds modulated ensemble in ob space (zero size and not referenced if neigv=0)
  real(r_single), dimension(neigv,nobs_max), intent(out) :: hx_modens
  real(r_single), dimension(nobs_max), intent(out) :: x_obs
  real(r_single), dimension(nobs_max), intent(out) :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out) :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out) :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out) :: x_channum, x_indx
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used


  character(len=10), intent(in) :: id
  integer(i_kind), intent(in)   :: nanal, nmem

  character*500 obsfile

  character(len=20) ::  sat_type

  integer(i_kind) iunit, nobs, nobdiag, i, nsat, jpchstart, nchans
  integer(i_kind) nob, nnz, nind
  logical fexist
  real(r_kind) :: errorlimit,errorlimit2
  real(r_double) t1,t2,tsum,tsum2
  real(r_single) :: rlat,rlon,rtim,rlat_prev,rlon_prev,rtim_prev,eps

  type(sparr2)    :: dhx_dx_read
  type(sparr)     :: dhx_dx
  type(raggedarr) :: hxpert

  integer(i_kind), dimension(:), allocatable :: Satinfo_Chan, Use_Flag, chind, chaninfoidx
  real(r_kind), dimension(:), allocatable :: error_variance
  real(r_single), dimension(:), allocatable :: Pressure, QC_Flag, Inv_Error, Inv_Error_scaled, &
                                               Observation, Observation_scaled
  real(r_single), dimension(:), allocatable :: Latitude, Longitude, Time
  real(r_single), dimension(:), allocatable :: Obs_Minus_Forecast_adjusted
  real(r_single), dimension(:), allocatable :: Obs_Minus_Forecast_adjusted_scaled
  real(r_single), dimension(:), allocatable :: Obs_Minus_Forecast_unadjusted
  integer(i_kind), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_stind
  integer(i_kind), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_endind
  real(r_single), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_val
  integer(i_kind) :: ix, iy, it, ixp, iyp, itp, nprof
  real(r_kind) :: delx, dely, delxp, delyp, delt, deltp

! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
  eps = 1.e-3

  tsum = 0; tsum2 = 0

  hx = zero
  nob = 0
  rlat_prev = huge(rlat); rlon_prev=huge(rlon); rtim_prev = huge(rtim)
  nobdiag = 0
  x_used = 0
  nprof = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
       ! The following is to sort out some historical naming conventions
       select case (sat_type(1:4))
          case ('airs')
            sat_type='airs_aqua'
          case ('iasi')
            if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
            if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
            if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
       end select

      if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle

     ! read diag file (concatenated pe* files)
     obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
     inquire(file=obsfile,exist=fexist)
     if (.not. fexist .or. datestring .eq. '0000000000') &
     obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"

     inquire(file=obsfile,exist=fexist)
     if(.not.fexist) cycle 

     t1 = mpi_wtime()
     call nc_diag_read_init(obsfile, iunit)

     nobs = nc_diag_read_get_dim(iunit,'nobs')

     if (nobs <= 0) then
        call nc_diag_read_close(obsfile)
        cycle 
     endif

     nchans = nc_diag_read_get_dim(iunit,'nchans')
     allocate(Satinfo_Chan(nchans), Use_Flag(nchans), error_variance(nchans))
     allocate(Pressure(nobs), QC_Flag(nobs), Inv_Error(nobs), Latitude(nobs), &
              Longitude(nobs), Time(nobs), Observation(nobs), chind(nobs),    &
              Obs_Minus_Forecast_unadjusted(nobs), Obs_Minus_Forecast_adjusted(nobs))
     call nc_diag_read_get_var(iunit, 'satinfo_chan', Satinfo_Chan)
     call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
     call nc_diag_read_get_var(iunit, 'error_variance', error_variance)
     call nc_diag_read_get_var(iunit, 'chaninfoidx', chaninfoidx)

     call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
     call nc_diag_read_get_var(iunit, 'Press_Max_Weight_Function', Pressure)
     call nc_diag_read_get_var(iunit, 'QC_Flag', QC_Flag)
     call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error', Inv_Error)
     call nc_diag_read_get_var(iunit, 'Latitude', Latitude)
     call nc_diag_read_get_var(iunit, 'Longitude', Longitude)
     call nc_diag_read_get_var(iunit, 'Obs_Time', Time)
     call nc_diag_read_get_var(iunit, 'Observation', Observation)
     call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_unadjusted', Obs_Minus_Forecast_unadjusted)
     call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_adjusted', Obs_Minus_Forecast_adjusted)
     if (use_correlated_oberrs) then
        call nc_diag_read_get_var(iunit, 'Observation_scaled', Observation_scaled)
        call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_adjusted_scaled', &
                                  Obs_Minus_Forecast_adjusted_scaled)
        call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error_scaled', &
                                  Inv_Error_scaled)
     endif

     call nc_diag_read_get_global_attr(iunit, "jac_nnz", nnz)
     call nc_diag_read_get_global_attr(iunit, "jac_nind", nind)
     allocate(Observation_Operator_Jacobian_stind(nind, nobs))
     allocate(Observation_Operator_Jacobian_endind(nind, nobs))
     allocate(Observation_Operator_Jacobian_val(nnz, nobs))
     call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_stind', Observation_Operator_Jacobian_stind)
     call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_endind', Observation_Operator_Jacobian_endind)
     call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_val', Observation_Operator_Jacobian_val)

     call nc_diag_read_close(obsfile)

     t2 = mpi_wtime()
     tsum2 = tsum2 + t2-t1

     do i = 1, nobs
        nobdiag = nobdiag + 1
        if (Use_Flag(chind(i)) < 1) cycle
        if (QC_Flag(i) < 0. .or. Inv_Error(i) < errorlimit &
                  .or. Inv_Error(i) > errorlimit2 &
                  .or. Satinfo_Chan(chind(i)) == 0) cycle
        if (abs(Observation(i)) > 1.e9_r_kind) cycle 

        nob = nob + 1

        x_used(nobdiag) = 1
        x_type(nob)= sat_type

        x_channum(nob) = chaninfoidx(chind(i))
        x_indx(nob) = Satinfo_Chan(chind(i))

        x_lon(nob) = Longitude(i)
        x_lat(nob) = Latitude(i)
        x_time(nob) = Time(i)
        ! bias corrected Hx
        if (use_correlated_oberrs) then
           x_obs(nob) = Observation_scaled(i)
           hx_mean(nob) = x_obs(nob) - Obs_Minus_Forecast_adjusted_scaled(i)
        else
           x_obs(nob) = Observation(i)
           hx_mean(nob) = x_obs(nob) - Obs_Minus_Forecast_adjusted(i)
        endif
        ! un-bias corrected Hx
        !hx_mean_nobc(nob) = x_obs(nob) - Obs_Minus_Forecast_unadjusted(i)

        if (nanal <= nanals) then
           ! read linearized Hx
           call new(dhx_dx_read, nnz, nind)
           dhx_dx_read%st_ind = Observation_Operator_Jacobian_stind(:,i)
           dhx_dx_read%end_ind = Observation_Operator_Jacobian_endind(:,i)
           dhx_dx_read%val = Observation_Operator_Jacobian_val(:,i)
           dhx_dx = dhx_dx_read
           call init_raggedarr(hxpert, dhx_dx%nnz)
           t1 = mpi_wtime()
           rlat = x_lat(nob)*deg2rad
           rlon = x_lon(nob)*deg2rad
           rtim = x_time(nob)
           if (nob > 1) then
              rlat_prev = x_lat(nob-1)*deg2rad
              rlon_prev = x_lon(nob-1)*deg2rad
              rtim_prev = x_time(nob-1)
           endif
           if (abs(rlat-rlat_prev) > eps .or. &
              abs(rlon-rlon_prev) > eps .or. &
              abs(rtim-rtim_prev) > eps) then
              call setup_linhx(rlat,rlon,rtim,              &
                            ix, delx, ixp, delxp, iy, dely,  &
                            iyp, delyp, it, delt, itp, deltp)
           else
              nprof = nprof + 1
           endif
           call calc_linhx(hx_mean(nob), state_d(:,:,:,nmem),       &
                           dhx_dx, hxpert,  hx(nob),     &
                           ix, delx, ixp, delxp, iy, dely,   &
                           iyp, delyp, it, delt, itp, deltp)
           ! compute modulated ensemble in obs space
           if (neigv>0) call calc_linhx_modens(hx_mean(nob),dhx_dx,hxpert,hx_modens(:,nob),vlocal_evecs)
           t2 = mpi_wtime()
           tsum = tsum + t2-t1
           call delete(dhx_dx)
           call delete(dhx_dx_read)
        endif

        x_errorig(nob) = error_variance(chind(i))**2
        if (use_correlated_oberrs) then
           x_err(nob) = (1._r_kind/Inv_Error_scaled(i))**2
        else
           x_err(nob) = (1._r_kind/Inv_Error(i))**2
        endif
        x_press(nob) = Pressure(i)

     enddo

     deallocate(Satinfo_Chan, Use_Flag, error_variance, chaninfoidx)
     deallocate(Pressure, QC_Flag, Inv_Error, Latitude, Longitude, Time, &
                Observation, chind, Obs_Minus_Forecast_unadjusted,       &
                Obs_Minus_Forecast_adjusted)
     if (use_correlated_oberrs) then
         deallocate(Obs_Minus_Forecast_adjusted_scaled,Inv_Error_scaled,&
                    Observation_scaled)
     endif
     deallocate(Observation_Operator_Jacobian_stind, Observation_Operator_Jacobian_endind, &
                Observation_Operator_Jacobian_val)

 enddo ! satellite
 if (nanal == nanals) then
     print *,'radiance ob profiles, total obs',nprof,nob
     print *,'time in calc_linhx for sat obs on proc',nproc,' = ',tsum
     print *,'time in read_raddiag_data for sat obs on proc',nproc,' = ',tsum2
 endif

  if (nob /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',nob,nobs_max
      call stop2(92)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',nobdiag,nobs_maxdiag
      call stop2(92)
  end if

 end subroutine get_satobs_data

! writing spread diagnostics to netcdf file
subroutine write_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, &
                                 x_fit, x_sprd, x_used, id, gesid)
  use netcdf, only: nf90_inq_dimid, nf90_open, nf90_close, NF90_NETCDF4, &
                    nf90_inquire_dimension, NF90_WRITE, nf90_create, nf90_def_dim
  use ncdw_climsg, only: nclayer_check

  use radinfo, only: iuse_rad,nusis,jpch_rad
  use constants, only: r_missing
  implicit none

  character*500,     intent(in) :: obspath
  character(len=10), intent(in) :: datestring
  integer(i_kind),   intent(in) :: nobs_max, nobs_maxdiag
  real(r_single),  dimension(nobs_max),     intent(in) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag), intent(in) :: x_used
  character(len=10), intent(in) :: id, gesid

  character*500 obsfile, obsfile2
  character(len=20) ::  sat_type

  integer(i_kind) :: iunit, nobsid
  integer(i_kind) :: nob, nobdiag, nobs, i, nsat, jpchstart
  integer(i_kind), dimension(:), allocatable :: enkf_use_flag
  real(r_single),  dimension(:), allocatable :: enkf_fit, enkf_sprd
  logical :: fexist

  nob  = 0
  nobdiag = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select
       if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     ! diag file (concatenated pe* files)
     obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
     obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//"_spread.nc4"
     inquire(file=obsfile,exist=fexist)
     if (.not. fexist .or. datestring .eq. '0000000000') then
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"
        obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//"_spread.nc4"
     endif

     inquire(file=obsfile,exist=fexist)
     if (.not. fexist) cycle 


     call nclayer_check(nf90_open(obsfile, NF90_WRITE, iunit))
     call nclayer_check(nf90_inq_dimid(iunit, "nobs", nobsid))
     call nclayer_check(nf90_inquire_dimension(iunit, nobsid, len = nobs))
     call nclayer_check(nf90_close(iunit))

     if (nobs <= 0) cycle 

     allocate(enkf_use_flag(nobs), enkf_fit(nobs), enkf_sprd(nobs))

     do i = 1, nobs
        nobdiag = nobdiag + 1
 
        ! skip if not used in EnKF
        if (x_used(nobdiag) == 1) then
           ! update if it is used in EnKF
           nob = nob + 1
           enkf_use_flag(i) = 1
           enkf_fit(i)  = x_fit(nob)
           enkf_sprd(i) = x_sprd(nob)
        else
           enkf_use_flag(i) = -1
           enkf_fit(i) = r_missing
           enkf_sprd(i) = r_missing
        endif
     enddo

     inquire(file=obsfile2,exist=fexist)
     if (.not. fexist) then
        call nclayer_check(nf90_create(trim(obsfile2), NF90_NETCDF4, &
                           iunit))
        call nclayer_check(nf90_def_dim(iunit, "nobs", nobs, nobsid))
     else
        call nclayer_check(nf90_open(obsfile2, NF90_WRITE, iunit))
        call nclayer_check(nf90_inq_dimid(iunit, "nobs", nobsid))
     endif

     call write_ncvar_int(iunit, nobsid, "EnKF_use_flag", enkf_use_flag)
     call write_ncvar_single(iunit, nobsid, "EnKF_fit_"//trim(gesid), enkf_fit)
     call write_ncvar_single(iunit, nobsid, "EnKF_spread_"//trim(gesid), enkf_sprd)

     call nclayer_check(nf90_close(iunit))

     deallocate(enkf_use_flag, enkf_fit, enkf_sprd)

  enddo

  if (nob .ne. nobs_max) then
      print *,'number of obs not what expected in write_satobs_data',nob,nobs_max
      call stop2(94)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total obs in diag not what expected in write_satobs_data',nobdiag, nobs_maxdiag
      call stop2(94)
  endif

  contains
  subroutine write_ncvar_single(iunit, dimid, varname, field)
    use netcdf, only: nf90_def_var, nf90_put_var, nf90_inq_varid,  &
                      nf90_def_var_deflate,NF90_FLOAT, NF90_ENOTVAR
    use ncdw_climsg, only: nclayer_check
    use ncdw_types, only: NLAYER_COMPRESSION
    implicit none
    integer(i_kind), intent(in)  :: iunit, dimid
    character(*), intent(in)     :: varname
    real(r_single), dimension(:), allocatable :: field

    integer :: ierr, varid

    ierr = nf90_inq_varid(iunit, varname, varid)
    if (ierr == NF90_ENOTVAR) then
       call nclayer_check(nf90_def_var(iunit, varname, NF90_FLOAT, dimid, varid))
       call nclayer_check(nf90_def_var_deflate(iunit, varid, 1, 1, int(NLAYER_COMPRESSION)))
    endif
    call nclayer_check(nf90_put_var(iunit, varid, field))
  end subroutine write_ncvar_single

  subroutine write_ncvar_int(iunit, dimid, varname, field)
    use netcdf, only: nf90_def_var, nf90_put_var, nf90_inq_varid,  &
                      nf90_def_var_deflate,NF90_INT, NF90_ENOTVAR
    use ncdw_climsg, only: nclayer_check
    use ncdw_types, only: NLAYER_COMPRESSION
    implicit none
    integer(i_kind), intent(in)  :: iunit, dimid
    character(*), intent(in)     :: varname
    integer(i_kind), dimension(:), allocatable :: field

    integer :: ierr, varid

    ierr = nf90_inq_varid(iunit, varname, varid)
    if (ierr == NF90_ENOTVAR) then
       call nclayer_check(nf90_def_var(iunit, varname, NF90_INT, dimid, varid))
       call nclayer_check(nf90_def_var_deflate(iunit, varid, 1, 1, int(NLAYER_COMPRESSION)))
    endif
    call nclayer_check(nf90_put_var(iunit, varid, field))
  end subroutine write_ncvar_int


end subroutine write_satobs_data

end module readsatobs
