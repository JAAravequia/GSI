module data
  implicit none

  private
  public :: data_t
  public :: data_ptr

  integer, parameter,public :: max_rdiag_reals = 25

  !---------------------------------------------------------
  !  index numbers into the rdiagbuf structure for gps data
  !  (see setbend.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type    = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype = 2     ! obs subtype
  integer, parameter, public :: idx_obs_lat     = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon     = 4     ! obs longitude
  integer, parameter, public :: idx_bend        = 5     ! incremental bending angle (x100 %)
  integer, parameter, public :: idx_pres        = 6     ! obs pressure (hPa)
  integer, parameter, public :: idx_hgt         = 7     ! impact height (meters)
  integer, parameter, public :: idx_time        = 8     ! obs time (hrs relative to analysis time)
  integer, parameter, public :: idx_zsges       = 9     ! model terrain (m) 
  integer, parameter, public :: idx_iqc         = 10    ! prepbufr qc or event mark (0=good)
  integer, parameter, public :: idx_iuse        = 11    ! data usage flag
  integer, parameter, public :: idx_imuse       = 12    ! muse data usage flag
  integer, parameter, public :: idx_rwgt        = 13    ! non-linear qc relative weight
  integer, parameter, public :: idx_err_input   = 14    ! original invers obs error (rad**-1)
  integer, parameter, public :: idx_errinv      = 15    ! original + represent  invers gps obs error (rad**-1)
  integer, parameter, public :: idx_errinv_fnl  = 16    ! final invers obs error (rad**-1)
  integer, parameter, public :: idx_igps        = 17    ! bending angle observation (rad) 
  integer, parameter, public :: idx_trefges     = 18    ! temp at obs location (K)
  integer, parameter, public :: idx_hob         = 19    ! model vert grid (interface)
  integer, parameter, public :: idx_one         = 20    ! uses gps_ref (one = use of bending angle)
  integer, parameter, public :: idx_qrefges     = 21    ! humidity at obs location (kg/kg)
!
!----  Danger Will Robinson!!! 
!
!   this is in the gsi/src directory but it is not yet the operational version
!   so the rdiagbuf nreal value for gpsro is 21 not 22
!
  integer, parameter, public :: idx_spread      = 22    ! spread (filled in by EnKF)


  ! Data is stored in data_t
  type :: data_t
     character(8)       :: stn_id
     real,dimension(25) :: rdiag
  end type data_t

  ! A container for storing data_t pointers
  type :: data_ptr
     type(data_t), pointer :: p
  end type data_ptr

end module data
