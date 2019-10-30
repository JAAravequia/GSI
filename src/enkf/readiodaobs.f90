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
!  initialize_ioda: initialize ioda (fckit and liboops)
!  finalize_ioda: finalizes ioda (fckit and liboops)
!  construct_obsspaces_ioda: reads all ioda files and constructs ObsSpaces
!  destruct_obsspaces_ioda: destructs all ioda ObsSpaces
!  get_numobs_ioda: counts number of observations for a specific type
!                   (conventional, ozone, or radiance)
!  get_obs_data_ioda: reads observation data for a specific type
!                   (conventional, ozone, or radiance)!
! Public Variables: None
!
! program history log:
!   2019-10-07  Initial version
!
! attributes:
!   language: f95
!
!$$$

use iso_c_binding, only: c_ptr
use datetime_mod, only: datetime
implicit none

public :: initialize_ioda, finalize_ioda, construct_obsspaces_ioda,  &
          destruct_obsspaces_ioda, get_numobs_ioda, get_obs_data_ioda

private

type(c_ptr), allocatable, dimension(:) :: obsspaces  !< pointers to all ObsSpaces
character(100), allocatable :: obstypes(:)  !< types of all ObsSpaces
                                            ! (conventional, ozone or radiance)
type(datetime) :: wincenter  !< center of the assimilation window (for computing x_time)

contains

! initialize ioda
subroutine initialize_ioda()
  use fckit_module, only: fckit_main
  use liboops_mod,  only: liboops_initialise
  implicit none

  call liboops_initialise()
  call fckit_main%init()
end subroutine initialize_ioda

! construct all ioda ObsSpaces (reads all the data into ObsSpace
subroutine construct_obsspaces_ioda()
  use fckit_configuration_module, only: fckit_configuration, &
                                        fckit_YAMLConfiguration
  use fckit_pathname_module, only : fckit_pathname
  use datetime_mod, only: datetime, datetime_create, datetime_diff, &
                          datetime_update
  use duration_mod, only: duration, duration_seconds, assignment(=)
  use obsspace_mod, only: obsspace_construct
  use params, only: jedi_yaml
  use kinds, only: i_kind
  use iso_c_binding, only: c_char
  implicit none

  type(fckit_configuration) :: config
  type(fckit_configuration), allocatable :: obsconfigs(:)
  type(fckit_configuration) :: obsconfig

  character(kind=c_char,len=:), allocatable :: winbgnstr
  character(kind=c_char,len=:), allocatable :: winendstr
  character(kind=c_char,len=:), allocatable :: obstype
  type(datetime) :: winbgn, winend
  type(duration) :: winlen, winlenhalf
  integer(i_kind) :: iobss

  !> initialize winbgn, winend, get config
  config = fckit_YAMLConfiguration(fckit_pathname(jedi_yaml))
  call config%get_or_die("window_begin", winbgnstr)
  call config%get_or_die("window_end", winendstr)
  call datetime_create(winbgnstr, winbgn)
  call datetime_create(winendstr, winend)

  !> find center of the window (to save in module)
  call datetime_diff(winend, winbgn, winlen)
  winlenhalf = duration_seconds(winlen) / 2
  call datetime_create(winbgnstr, wincenter)
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
  use obsspace_mod, only: obsspace_destruct
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
  use fckit_module, only: fckit_main
  use liboops_mod,  only: liboops_finalise
  implicit none

  call fckit_main%final()
  call liboops_finalise()

end subroutine finalize_ioda

! get number of observations from JEDI IODA files (type from yaml)
subroutine get_numobs_ioda(obstype, num_obs_tot, num_obs_totdiag)
  use obsspace_mod, only: obsspace_get_nlocs, obsspace_obsvariables, &
                          obsspace_get_db
  use oops_variables_mod, only: oops_variables
  use kinds, only: i_kind
  implicit none

  character(len=*), intent(in)  :: obstype
  integer(i_kind),  intent(out) :: num_obs_tot, num_obs_totdiag

  integer(i_kind) :: iobss, ivar, nlocs, nvars
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
        call obsspace_get_db(obsspaces(iobss), "EffectiveQC", &
                             vars%variable(ivar), values)
        num_obs_tot = num_obs_tot + count(values == 0)
      enddo
      deallocate(values)
      num_obs_totdiag = num_obs_totdiag + nlocs*nvars
    endif
  enddo

end subroutine get_numobs_ioda

!> fill in an array with metadata (repeat for each variable)
subroutine fill_array_metadata(obsspace, varname, x_arr)
use obsspace_mod, only: obsspace_get_nlocs, obsspace_obsvariables, &
                        obsspace_get_db
use oops_variables_mod, only: oops_variables
use iso_c_binding, only: c_double
use kinds, only: r_single, i_kind
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: varname
real(r_single), dimension(*)  :: x_arr
real(c_double), dimension(:), allocatable :: values

integer(i_kind) :: nlocs, nvars, ivar
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
use obsspace_mod, only: obsspace_get_nlocs, obsspace_obsvariables, &
                        obsspace_get_db
use oops_variables_mod, only: oops_variables
use iso_c_binding, only: c_double
use kinds, only: r_single, i_kind
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: groupname
real(r_single), dimension(*)  :: x_arr
real(c_double), dimension(:), allocatable :: values

integer(i_kind) :: nlocs, nvars, ivar
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
use obsspace_mod, only: obsspace_get_nlocs, obsspace_obsvariables, &
                        obsspace_get_db
use oops_variables_mod, only: oops_variables
use iso_c_binding, only: c_int
use kinds, only: i_kind
implicit none
type(c_ptr) :: obsspace
character(len=*), intent(in)  :: groupname
integer(i_kind), dimension(*) :: x_arr
integer(c_int), dimension(:), allocatable :: values

integer(i_kind) :: nlocs, nvars, ivar
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
                             x_errorig, x_type, x_used, nanal, x_indx)
  use obsspace_mod, only: obsspace_get_nlocs, obsspace_obsvariables, &
                          obsspace_get_db, obsspace_obsname
  use oops_variables_mod, only: oops_variables
  use kinds, only: r_single, i_kind
  use datetime_mod, only: datetime, datetime_diff
  use duration_mod, only: duration, duration_seconds
  use params, only: nanals
  use mpisetup
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
  integer(i_kind), intent(in) :: nanal
  integer(i_kind), dimension(nobs_max), intent(out), optional   :: x_indx  !< only used for radiances

  integer(i_kind) :: iobss, iloc, ivar
  integer(i_kind) :: nlocs, nvars
  integer(i_kind) :: i1, i2         !< start and end indices for the "used" obs in current ObsSpace
  integer(i_kind) :: i1_all, i2_all !< start and end indices for all obs in current ObsSpace
  integer(i_kind), dimension(1) :: var_index
  type(oops_variables) :: vars
  real(r_single), dimension(:), allocatable    :: values
  integer(i_kind), dimension(:), allocatable   :: intvalues
  character(len=20), dimension(:), allocatable :: chvalues
  logical, dimension(:), allocatable           :: used_obs
  type(datetime), dimension(:), allocatable    :: abs_time
  type(duration) :: dtime
  character(len=100) :: obsname
  character(len=100) :: varname
  character(len=5)  :: member
  integer, allocatable :: channels(:)
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
      i2_all = i1_all + nvars*nlocs - 1

      !> read flags (whether to use the obs)
      call fill_array_obsdata_int(obsspaces(iobss), "EffectiveQC", intvalues)
      x_used(i1_all:i2_all) = 0
      where(intvalues == 0) x_used(i1_all:i2_all) = 1
      used_obs = (x_used(i1_all:i2_all) == 1)
      i2 = i1 + count(used_obs) - 1

      !> read the rest of the fields, only save values for used obs
      call fill_array_metadata(obsspaces(iobss), "longitude", values)
      x_lon(i1:i2) = pack(values, used_obs)
      call fill_array_metadata(obsspaces(iobss), "latitude",  values)
      x_lat(i1:i2) = pack(values, used_obs)
      if (obstype == "conventional" .or. obstype == "ozone") then
        call fill_array_metadata(obsspaces(iobss), "air_pressure", values)
        x_press(i1:i2) = pack(values, used_obs)
      else
        call fill_array_obsdata(obsspaces(iobss), 'Press_Max_Weight_Function', values)
        x_press(i1:i2) = pack(values, used_obs)
      endif
      !> fill in time (ObsSpaces hold datetime in Datetime objects; need to
      !compute delta between datetime(obs) and datetime(middle of assim window)
      allocate(abs_time(nlocs))
      call obsspace_get_db(obsspaces(iobss), "MetaData", "datetime", abs_time)
      do iloc = 1, nlocs
        call datetime_diff(abs_time(iloc), wincenter, dtime)
        do ivar = 1, nvars
          !> x_time is in hours; converting seconds to hours
          values(nlocs*(ivar-1) + iloc) = duration_seconds(dtime) / 3600.0
        enddo
      enddo
      x_time(i1:i2) = pack(values, used_obs)
      deallocate(abs_time)
      call fill_array_obsdata(obsspaces(iobss), "ObsValue", values)
      x_obs(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "HofX", values)
      hx_mean(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "ObsBias", values)
      hx_mean_nobc(i1:i2) = hx_mean(i1:i2) - pack(values, used_obs)
      !> read ensemble member H(x) if needed
      if (nanal <= nanals) then
        write(member,'(I4)') nanal
        varname="HofX_" //adjustl(member)
        call fill_array_obsdata(obsspaces(iobss), trim(varname), values)
        hx(i1:i2) = pack(values, used_obs)
      else
        hx(i1:i2) = 0.0_r_single
      endif
      call fill_array_obsdata(obsspaces(iobss), "EffectiveError", values)
      x_err(i1:i2) = pack(values, used_obs)
      call fill_array_obsdata(obsspaces(iobss), "ObsError", values)
      x_errorig(i1:i2) = pack(values, used_obs)
      !> need variances, not stdev
      do iloc = i1, i2
        x_err(iloc) = x_err(iloc)**2
        x_errorig(iloc) = x_errorig(iloc)**2
      enddo
      !> x_code for conventional is the code from file (120 for radiosondes,
      !  etc)
      if (obstype == "conventional") then
        call fill_array_obsdata_int(obsspaces(iobss), "ObsType", intvalues)
        x_code(i1:i2) = pack(intvalues, used_obs)
      !> for ozone use bogus 700 (as in read_ozobs_data_nc)
      elseif (obstype == "ozone") then
        x_code(i1:i2) = 700
      !> for radiances fill in channel indices
      elseif (obstype == "radiance") then
        do iloc = 1, nlocs
          do ivar = 1, nvars
            intvalues(nlocs*(ivar-1) + iloc) = ivar
          enddo
        enddo
        x_code(i1:i2) = pack(intvalues, used_obs)
      endif
      !> x_type for conventional uses short names of variables (like in
      !  read_convobs_data_bin and _nc
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
      !> for ozone is always 'oz'
      elseif (obstype == "ozone") then
        x_type(i1:i2) = ' oz'
      !> for radiances read ObsSpace.name from yaml (should have short
      !  sat/instrument name
      elseif (obstype == "radiance") then
        call obsspace_obsname(obsspaces(iobss), obsname)
        x_type(i1:i2) = obsname
      endif

      !> for radiance data also fill in x_indx, contains channel numbers (like
      !  the ones from satinfo)
      if (present(x_indx)) then
        allocate(channels(nvars))
        call obsspace_get_db(obsspaces(iobss), "VarMetaData", "satinfo_channel", channels)
        do ivar = 1, nvars
!          currvar = vars%variable(ivar)
!          bar_index = index(currvar,"_",back=.true.) ! find final "_" before channel number
!          read(currvar(bar_index+1:len_trim(currvar)), *) channel
          do iloc = 1, nlocs
            intvalues(nlocs*(ivar-1) + iloc) = channels(ivar) !channel
          enddo
        enddo
        x_indx(i1:i2) = pack(intvalues, used_obs)
        deallocate(channels)
      endif
      i1 = i1 + count(used_obs)
      i1_all = i1_all + nvars*nlocs
      deallocate(values, intvalues, used_obs)
    endif
  enddo

end subroutine get_obs_data_ioda

end module readiodaobs
