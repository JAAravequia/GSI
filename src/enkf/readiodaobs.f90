module readiodaobs
!$$$  module documentation block
!
! module: readiodaobs                  read data from IODA files
!
! prgmmr: shlyaeva         org: esrl/psd & jcsda             date: 2019-10-07
!
! abstract: read data from IODA files (output by JEDI UFO)
!
! Public Subroutines:
!  initialize _ioda: initialize ioda (reads all the files)
!  finalize_ioda: finalizes ioda (closes all files)
!
! Public Variables: None
!
! program history log:
!   2019-10-07  Initial version
!
! attributes:
!   language: f95
!
!$$$

use, intrinsic :: iso_c_binding
use datetime_mod
implicit none

public :: initialize_ioda, finalize_ioda, construct_obsspaces_ioda,  &
          destruct_obsspaces_ioda, get_numobs_ioda, get_obs_data_ioda

private
type(c_ptr), allocatable, dimension(:) :: obsspaces
character(100), allocatable :: obstypes(:)
type(datetime) :: wincenter

contains

! initialize ioda
subroutine initialize_ioda()
  use fckit_module
  use liboops_mod
  implicit none

  call liboops_initialise()
  call fckit_main%init()
end subroutine initialize_ioda

! construct all ioda ObsSpaces (reads all the data into ObsSpace
subroutine construct_obsspaces_ioda()
  use fckit_configuration_module
  use fckit_pathname_module, only : fckit_pathname
  use datetime_mod
  use duration_mod
  use obsspace_mod
  use params, only: jedi_yaml
  implicit none

  type(fckit_configuration) :: config
  type(fckit_configuration), allocatable :: obsconfigs(:)
  type(fckit_configuration) :: obsconfig

  character(kind=c_char,len=:), allocatable :: winbgnstr
  character(kind=c_char,len=:), allocatable :: winendstr
  character(kind=c_char,len=:), allocatable :: obstype
  type(datetime) :: winbgn, winend
  type(duration) :: winlen, winlenhalf

  integer :: iobss

  !> initialize winbgn, winend, get config
  config = fckit_YAMLConfiguration(fckit_pathname(jedi_yaml))
  call config%get_or_die("window_begin", winbgnstr)
  call config%get_or_die("window_end", winendstr)
  call datetime_create(winbgnstr, winbgn)
  call datetime_create(winendstr, winend)
  !> find center of the window (to save in module)
  call datetime_diff(winend, winbgn, winlen)
  winlenhalf = duration_seconds(winlen) / 2
  wincenter = winbgn
  call datetime_update(wincenter, winlenhalf)
  !> allocate all ObsSpaces
  call config%get_or_die("Observations.ObsTypes", obsconfigs)
  if (allocated(obsspaces))    deallocate(obsspaces)
  if (allocated(obstypes))     deallocate(obstypes)
  allocate(obsspaces(size(obsconfigs)))
  allocate(obstypes(size(obsconfigs)))
  do iobss = 1, size(obsconfigs)
    call obsconfigs(iobss)%get_or_die("ObsSpace", obsconfig)
    !> construct obsspace
    obsspaces(iobss) = obsspace_construct(obsconfig, winbgn, winend)
    !> save obs type
    call obsconfig%get_or_die("EnKF obstype", obstype)
    obstypes(iobss) = obstype
  enddo
  deallocate(winbgnstr)
  deallocate(winendstr)
  deallocate(obstype)

end subroutine construct_obsspaces_ioda

! destruct all ioda ObsSpaces
subroutine destruct_obsspaces_ioda()
  use obsspace_mod
  implicit none

  integer :: iobss

  !> destruct all obsspaces
  do iobss = 1, size(obsspaces)
    call obsspace_destruct(obsspaces(iobss))
  enddo
  deallocate(obsspaces)
  deallocate(obstypes)

end subroutine destruct_obsspaces_ioda

! finalize ioda
subroutine finalize_ioda()
  use fckit_module
  use liboops_mod
  implicit none

  call fckit_main%final()
  call liboops_finalise()

end subroutine finalize_ioda

! get number of observations from JEDI IODA files (type from yaml)
subroutine get_numobs_ioda(obstype, num_obs_tot, num_obs_totdiag)
  use obsspace_mod
  use oops_variables_mod
  use kinds, only: i_kind
  implicit none

  character(len=*), intent(in)  :: obstype
  integer(i_kind),  intent(out) :: num_obs_tot, num_obs_totdiag

  integer :: iobss, ivar, nlocs, nvars
  type(oops_variables) :: vars
  integer(i_kind), dimension(:), allocatable :: values

  num_obs_tot = 0
  num_obs_totdiag = 0
  do iobss = 1, size(obsspaces)
    !> only count nobs if this ObsSpace is of the same type (conventional, ozone
    !  or radiance
    if (trim(obstypes(iobss)) == trim(obstype)) then
      nlocs = obsspace_get_nlocs(obsspaces(iobss))
      vars = obsspace_obsvariables(obsspaces(iobss))
      nvars = vars%nvars()
      allocate(values(nlocs))
      do ivar = 1, nvars
        !> get the use flag to count nunber if used observations
        if (obstype == "conventional" .or. obstype == "ozone") then
          !> for ozone and conventional, GsiUseFlag is saved (1 if used, otherwise
          !  if not)
          call obsspace_get_db(obsspaces(iobss), "GsiUseFlag", &
                               vars%variable(ivar), values)
          num_obs_tot = num_obs_tot + count(values == 1)
        elseif (obstype == "radiance") then
          !> for radiances, GSI QC is saved (0 if passed QC)
          call obsspace_get_db(obsspaces(iobss), "PreQC", &
                               vars%variable(ivar), values)
          num_obs_tot = num_obs_tot + count(values == 0)
        endif
      enddo
      deallocate(values)
      num_obs_totdiag = num_obs_totdiag + nlocs*nvars
    endif
  enddo

end subroutine get_numobs_ioda

!> fill in an array with metadata (repeat for each variable)
subroutine fill_array_metadata(obsspace, varname, x_arr)
use obsspace_mod
use oops_variables_mod
use kinds
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: varname
real(r_single), dimension(*)  :: x_arr
real(c_double), dimension(:), allocatable :: values

integer :: nlocs, nvars, ivar
type(oops_variables) :: vars

nlocs = obsspace_get_nlocs(obsspace)
vars = obsspace_obsvariables(obsspace)
nvars = vars%nvars()
allocate(values(nlocs))
call obsspace_get_db(obsspace, "MetaData", varname, values)
do ivar = 1, nvars
  x_arr(1 + (ivar-1)*nlocs : ivar*nlocs) = values(1:nlocs)
enddo
deallocate(values)

end subroutine fill_array_metadata

!> fill in an array with obs-data (different for each variable)
subroutine fill_array_obsdata(obsspace, groupname, x_arr)
use obsspace_mod
use oops_variables_mod
use kinds
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: groupname
real(r_single), dimension(*)  :: x_arr
real(c_double), dimension(:), allocatable :: values

integer :: nlocs, nvars, ivar
type(oops_variables) :: vars

nlocs = obsspace_get_nlocs(obsspace)
vars = obsspace_obsvariables(obsspace)
nvars = vars%nvars()
allocate(values(nlocs))
do ivar = 1, nvars
  call obsspace_get_db(obsspace, groupname, vars%variable(ivar), values)
  x_arr(1 + (ivar-1)*nlocs : ivar*nlocs) = values(1:nlocs)
enddo
deallocate(values)

end subroutine fill_array_obsdata

!> fill in an array with obs-data (different for each variable), integer
subroutine fill_array_obsdata_int(obsspace, groupname, x_arr)
use obsspace_mod
use oops_variables_mod
use kinds
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: groupname
integer(i_kind), dimension(*) :: x_arr
integer(c_int), dimension(:), allocatable :: values

integer :: nlocs, nvars, ivar
type(oops_variables) :: vars

nlocs = obsspace_get_nlocs(obsspace)
vars = obsspace_obsvariables(obsspace)
nvars = vars%nvars()
allocate(values(nlocs))
do ivar = 1, nvars
  call obsspace_get_db(obsspace, groupname, vars%variable(ivar), values)
  x_arr(1 + (ivar-1)*nlocs : ivar*nlocs) = values(1:nlocs)
enddo
deallocate(values)

end subroutine fill_array_obsdata_int

! read data from JEDI IODA files
subroutine get_obs_data_ioda(obstype, nobs_max, nobs_maxdiag,         &
                             hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
                             x_lon, x_lat, x_press, x_time, x_code,   &
                             x_errorig, x_type, x_used, x_indx)
  use obsspace_mod
  use oops_variables_mod
  use kinds
  use mpisetup
  use datetime_mod
  use duration_mod
  implicit none

  character(len=*), intent(in)  :: obstype
  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag
  real(r_single), dimension(nobs_max), intent(out)    :: hx_mean
  real(r_single), dimension(nobs_max), intent(out)    :: hx_mean_nobc
  real(r_single), dimension(nobs_max), intent(out)    :: hx
  real(r_single), dimension(nobs_max), intent(out)    :: x_obs
  real(r_single), dimension(nobs_max), intent(out)    :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out)    :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out)    :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out)   :: x_code
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used
  integer(i_kind), dimension(nobs_max), intent(out), optional   :: x_indx  !< only used for radiances

  integer :: iobss, iloc, ivar
  integer :: nlocs, nvars
  integer :: i1, i2
  integer :: i1_all, i2_all
  integer, dimension(1) :: var_index
  type(oops_variables) :: vars
  real(r_single), dimension(:), allocatable    :: values
  integer(i_kind), dimension(:), allocatable   :: intvalues
  character(len=20), dimension(:), allocatable :: chvalues
  logical, dimension(:), allocatable           :: used_obs
  type(datetime), dimension(:), allocatable    :: abs_time
  type(duration) :: dtime
  character(100) :: currvar
  integer :: channel, bar_index
  character(len=100) :: obsname

  character(len=60), dimension(7), parameter :: varnames_conv = &
    (/'air_temperature', 'virtual_temperature', 'specific_humidity', &
      'eastward_wind', 'northward_wind', 'surface_pressure',         &
      'bending_angle'/)
  character(len=3), dimension(7), parameter :: obtypes_enkf =   &
    (/'  t', '  t', '  q', '  u', '  v', ' ps', 'gps'/)

  i1 = 1
  i1_all = 1
  do iobss = 1, size(obsspaces)
    if (trim(obstypes(iobss)) == trim(obstype)) then
      nlocs = obsspace_get_nlocs(obsspaces(iobss))
      vars = obsspace_obsvariables(obsspaces(iobss))
      nvars = vars%nvars()
      allocate(values(nlocs*nvars), used_obs(nlocs*nvars), intvalues(nlocs*nvars))
      i2_all = i1_all + nvars*nlocs

      !> read flags (whether to use the obs)
      if (obstype == "conventional" .or. obstype == "ozone") then
        !> for ozone and conventional, GsiUseFlag is saved (1 if used, otherwise
        !  if not)
        call fill_array_obsdata_int(obsspaces(iobss), "GsiUseFlag", intvalues)
        x_used(i1_all:i2_all) = 0
        where(intvalues == 1) x_used(i1_all:i2_all) = 1
      elseif (obstype == "radiance") then
        !> for radiances, GSI QC is saved (0 if passed QC)
        call fill_array_obsdata_int(obsspaces(iobss), "PreQC", intvalues)
        x_used(i1_all:i2_all) = 0
        where(intvalues == 0) x_used(i1_all:i2_all) = 1
      endif

      used_obs = (x_used(i1_all:i2_all) == 1)
      i2 = i1 + count(used_obs)

      !> read the rest of the fields, only save values for used obs
      call fill_array_metadata(obsspaces(iobss), "longitude", values)
      x_lon(i1:i2) = pack(values, used_obs)
      call fill_array_metadata(obsspaces(iobss), "latitude",  values)
      x_lat(i1:i2) = pack(values, used_obs)
      !> read pressure
      if (obstype == "conventional" .or. obstype == "ozone") then
        call fill_array_metadata(obsspaces(iobss), "air_pressure", values)
        x_press(i1:i2) = pack(values, used_obs)
      elseif (obstype == "radiance") then
        x_press(i1:i2) = 99999.0
      endif
      allocate(abs_time(nlocs))
      call obsspace_get_db(obsspaces(iobss), "MetaData", "datetime", abs_time)
      do iloc = 1, nlocs
        call datetime_diff(abs_time(iloc), wincenter, dtime)
        do ivar = 1, nvars
          values(nlocs*(ivar-1) + iloc) = duration_seconds(dtime) / 3600.0
        enddo
      enddo
      x_time(i1:i2) = pack(values, used_obs)
      deallocate(abs_time)
      call fill_array_obsdata(obsspaces(iobss), "ObsValue", values)
      x_obs(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "GsiHofXBc", values)
      hx_mean(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "GsiHofX", values)
      hx_mean_nobc(i1:i2) = pack(values, used_obs)
      ! TODO: has to read different values for the members below!
      call fill_array_obsdata(obsspaces(iobss), "GsiHofX", values)
      hx(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "GsiFinalObsError", values)
      x_err(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "ObsError", values)
      x_errorig(i1:i2) = pack(values, used_obs)
      if (obstype == "conventional") then
        call fill_array_obsdata_int(obsspaces(iobss), "ObsType", intvalues)
        x_code(i1:i2) = pack(intvalues, used_obs)
      elseif (obstype == "ozone") then
        x_code(i1:i2) = 700
      elseif (obstype == "radiance") then
        do iloc = 1, nlocs
          do ivar = 1, nvars
            intvalues(nlocs*(ivar-1) + iloc) = ivar
          enddo
        enddo
        x_code(i1:i2) = pack(intvalues, used_obs)
      endif
      if (obstype == "conventional") then
        allocate(chvalues(nvars*nlocs))
        do ivar = 1, nvars
          var_index = findloc(varnames_conv, trim(vars%variable(ivar)))
          do iloc = 1, nlocs
            chvalues(nlocs*(ivar-1) + iloc) = obtypes_enkf(var_index(1))
          enddo
        enddo
        x_type(i1:i2) = pack(chvalues, used_obs)
        deallocate(chvalues)
      elseif (obstype == "ozone") then
        x_type(i1:i2) = ' oz'
      elseif (obstype == "radiance") then
        !> TODO: fill in obstype as satellite name
        call obsspace_obsname(obsspaces(iobss), obsname)
        x_type(i1:i2) = obsname
      endif
      if (present(x_indx)) then
        do ivar = 1, nvars
          currvar = vars%variable(ivar)
          bar_index = index(currvar,"_",back=.true.)        !final "_" before channel
          read(currvar(bar_index+1:len_trim(currvar)), *) channel
          do iloc = 1, nlocs
            intvalues(nlocs*(ivar-1) + iloc) = channel
          enddo
        enddo
        x_indx(i1:i2) = pack(intvalues, used_obs)
      endif
      i1 = i1 + count(used_obs)
      i1_all = i1_all + nvars*nlocs
      deallocate(values, intvalues, used_obs)
    endif
  enddo

end subroutine get_obs_data_ioda

end module readiodaobs
