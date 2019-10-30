module readiodaobs
!$$$  module documentation block
!
! module: readiodaobs                stubs for read data from IODA files
!
! prgmmr: shlyaeva         org: esrl/psd & jcsda             date: 2019-10-07
!
! abstract: stubs for reading data from IODA files (if one wanted to build
!           EnKF without JEDI libraries
!
! Public Subroutines:
!  initialize_ioda:
!  finalize_ioda:
!  construct_obsspaces_ioda:
!  destruct_obsspaces_ioda:
!  get_numobs_ioda:
!  get_obs_data_ioda:
! Public Variables: None
!
! program history log:
!   2019-10-07  Initial version
!
! attributes:
!   language: f95
!
!$$$
use mpisetup
use kinds
implicit none

public :: initialize_ioda, finalize_ioda, construct_obsspaces_ioda,  &
          destruct_obsspaces_ioda, get_numobs_ioda, get_obs_data_ioda

private

contains

! initialize ioda
subroutine initialize_ioda()
  implicit none
  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)
end subroutine initialize_ioda

! construct all ioda ObsSpaces (reads all the data into ObsSpace
subroutine construct_obsspaces_ioda()
  implicit none
  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)
end subroutine construct_obsspaces_ioda

! destruct all ioda ObsSpaces
subroutine destruct_obsspaces_ioda()
  implicit none
  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)
end subroutine destruct_obsspaces_ioda

! finalize ioda
subroutine finalize_ioda()
  implicit none
  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)
end subroutine finalize_ioda

! get number of observations from JEDI IODA files (type from yaml)
subroutine get_numobs_ioda(obstype, num_obs_tot, num_obs_totdiag)
  implicit none
  character(len=*), intent(in)  :: obstype
  integer(i_kind),  intent(out) :: num_obs_tot, num_obs_totdiag

  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)

end subroutine get_numobs_ioda


! read data from JEDI IODA files
subroutine get_obs_data_ioda(obstype, nobs_max, nobs_maxdiag,         &
                             hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
                             x_lon, x_lat, x_press, x_time, x_code,   &
                             x_errorig, x_type, x_used, nanal, x_indx)
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

  if (nproc .eq. 0) print *, 'EnKF built without JEDI libraries; exiting.'
  call stop2(19)


end subroutine get_obs_data_ioda

end module readiodaobs
