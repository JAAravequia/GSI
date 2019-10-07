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
implicit none

public :: initialize_ioda, finalize_ioda, get_numobs_ioda

private

type(c_ptr), allocatable, dimension(:) :: obsspaces

contains

! get number of conventional observations from JEDI UFO netcdf file
subroutine initialize_ioda()
  use fckit_configuration_module
  use fckit_pathname_module, only : fckit_pathname
  use fckit_module
  use datetime_mod
  use liboops_mod
  use obsspace_mod
  use params, only: jedi_yaml
  implicit none

  type(fckit_configuration) :: config
  type(fckit_configuration), allocatable :: obsconfigs(:)
  type(fckit_configuration) :: obsconfig

  character(kind=c_char,len=:), allocatable :: winbgnstr
  character(kind=c_char,len=:), allocatable :: winendstr
  type(datetime) :: winbgn, winend

  integer :: nlocs
  integer :: nvars
  integer :: iobstype

  call liboops_initialise()
  call fckit_main%init()

  !> initialize winbgn, winend, get config
  config = fckit_YAMLConfiguration(fckit_pathname(jedi_yaml))
  call config%get_or_die("window_begin", winbgnstr)
  call config%get_or_die("window_end", winendstr)
  call datetime_create(winbgnstr, winbgn)
  call datetime_create(winendstr, winend)
  !> allocate all ObsSpaces
  call config%get_or_die("Observations.ObsTypes", obsconfigs)
  if (allocated(obsspaces))    deallocate(obsspaces)
  allocate(obsspaces(size(obsconfigs)))
  do iobstype = 1, size(obsconfigs)
    call obsconfigs(iobstype)%get_or_die("ObsSpace", obsconfig)
    !> construct obsspace
    obsspaces(iobstype) = obsspace_construct(obsconfig, winbgn, winend)
    nlocs = obsspace_get_nlocs(obsspaces(iobstype))
    nvars = obsspace_get_nvars(obsspaces(iobstype))
  enddo

end subroutine initialize_ioda

! finalize ioda
subroutine finalize_ioda()
  use fckit_module
  use liboops_mod
  use obsspace_mod
  implicit none

  integer :: iobstype

  !> destruct all obsspaces
  do iobstype = 1, size(obsspaces)
    call obsspace_destruct(obsspaces(iobstype))
  enddo
  deallocate(obsspaces)

  call fckit_main%final()
  call liboops_finalise()

end subroutine finalize_ioda

! get number of observations from JEDI IODA files (type from yaml)
subroutine get_numobs_ioda(obstype, num_obs_tot, num_obs_totdiag)
  use obsspace_mod
  use kinds, only: i_kind
  implicit none

  character(len=*), intent(in)  :: obstype
  integer(i_kind),  intent(out) :: num_obs_tot, num_obs_totdiag
  integer :: iobstype, nlocs

  num_obs_tot = 0
  num_obs_totdiag = 0
  do iobstype = 1, size(obsspaces)
    nlocs = obsspace_get_nlocs(obsspaces(iobstype))
    num_obs_tot = num_obs_tot + nlocs
    num_obs_totdiag = num_obs_totdiag + nlocs
  enddo

end subroutine get_numobs_ioda


end module readiodaobs
