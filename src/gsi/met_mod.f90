module met_mod

!$$$   module documentation block
!                .      .    .                                       .
! module: some calculations between meteorological variables
! prgmmr: Junjun Hu

  use     kinds, only: r_kind

  implicit none

! set default as private
  private
! set subroutines and functions to public
  public :: qv_to_relh
  public :: rh_and_temp_to_dewpoint
  public :: sat_vapor_press_bolton
  public :: temp_and_dewpoint_to_rh
  public :: qv_to_dewpoint

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   sat_vapor_press_bolton - function that uses Bolton's approximation to
!                            compute saturation vapor pressure given
!                            temperature.
!
!   reference:  Bolton 1980, MWR, 1046-1053
!
!    sat_vapor_press_bolton - saturation vapor pressure (Pa)
!    tmpk                   - temperature (K)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sat_vapor_press_bolton(tmpk, sat_vapor_press)

    implicit none 
    real(r_kind), intent(inout) :: sat_vapor_press
    real(r_kind), intent(in) :: tmpk
    real(r_kind)             :: tmpc               ! temperature (Celsius)
    real(r_kind), parameter  :: es0C=611.0_r_kind  ! vapor pressure at 0 C (Pa) 
    real(r_kind), parameter  :: Tfrez = 273.15_r_kind ! water freezing point (K)

    tmpc = tmpk - Tfrez
    if ( tmpc <= -200.0_r_kind ) then
       print*,'sat_vapor_press_bolton:  tmpc too low ',tmpc
       stop
    end if
    sat_vapor_press = es0C * exp( 17.67_r_kind * tmpc / (tmpc + 243.5_r_kind) )

    return

end subroutine sat_vapor_press_bolton

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   qv_to_relh- function that computes the relh given a 
!               temperature, mixing ratio qv and the pressure.
!
!    tmpk - potential temperature (K)
!    pres - pressure (mb)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine qv_to_relh(qv, tmpk, pres, rh)

    implicit none
    real(r_kind), intent(in) :: qv, tmpk, pres  ! g/kg, K, mb
    real(r_kind)             :: tempc
    real(r_kind), parameter  :: es0C=611.0_r_kind  ! vapor pressure at 0 C (Pa) 
    real(r_kind), parameter  :: Tfrez = 273.15_r_kind ! water freezing point (K)
    real(r_kind) :: es ! saturation vapor pressure Pa
    real(r_kind) :: ws ! saturation mixing ratio  kg/kg
    real(r_kind), intent(inout) :: rh ! (0.00 - 1.00)

    call sat_vapor_press_bolton(tmpk, es)
    ws = ( 0.622_r_kind * es * 0.01_r_kind ) / pres  ! kg/kg
    rh = qv / (ws * 1000.0_r_kind)

    return
end subroutine qv_to_relh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   qv_to_dewpoint - function that computes the td given a 
!               temperature, mixing ratio qv, and the pressure.
!
!    tmpk - potential temperature (K)
!    pres - pressure (mb)
!    qv   - mixing ratio (kg/kg)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine qv_to_dewpoint(qv,pres,tdk)
    implicit none
    real(r_kind), intent(inout) :: tdk            ! K
    real(r_kind), intent(in) :: pres              ! mb
    real(r_kind), intent(in) :: qv                ! kg/kg
    real(r_kind), PARAMETER :: e_min = 0.001_r_kind ! threshold for minimum vapor pressure (mb),
                                                    !   to avoid problems near zero in Bolton's equation
    real(r_kind)             :: e                  ! vapor pressure (mb)

    if(qv < 0.0_r_kind .or. qv >= 1.0_r_kind) then
        print *,'qv_to_dewpoint: bad qv ',qv
        tdk = -1.0e+10_r_kind
        return
    end if

    e = qv * pres / (0.622_r_kind + qv) ! mb
    e = max(e, e_min) ! mb
    !------------------------------------------------------------------------------
    !  Use Bolton's approximation to compute dewpoint.
    !------------------------------------------------------------------------------

    tdk = 273.15_r_kind + (243.5_r_kind / ((17.67_r_kind / log(e/6.112_r_kind)) - 1.0_r_kind) )
    return

end subroutine qv_to_dewpoint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   rh_and_temp_to_dewpoint - function that computes the dewpoint
!                             given relative humidity and temperature
!
!    rh_and_temp_to_dewpoint - dewpoint (Kelvin)
!    rh                      - relative humidity (0.00 - 1.00)
!    tmpk                    - temperature (Kelvin)
!
!     created Dec. 2008 David Dowell, NCAR/MMM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine rh_and_temp_to_dewpoint(rh, tmpk, tdk)

    implicit none
    real(r_kind), intent(inout) :: tdk            ! K
    real(r_kind), intent(in) :: rh                ! 0.0-1.0
    real(r_kind), intent(in) :: tmpk

    real(r_kind)             :: e                  ! vapor pressure (Pa)
    real(r_kind)             :: es                 ! saturation vapor pressure (Pa)
    real(r_kind)             :: dptc               ! dptc (Celsius)

    real(r_kind), parameter  :: es0C=611.0_r_kind  ! vapor pressure at 0 C (Pa) 
    real(r_kind), parameter  :: Tfrez = 273.15_r_kind ! water freezing point (K)

    if ( ( rh <= 0.00_r_kind ) .or. ( rh > 1.00_r_kind ) ) then
       print*,'rh_and_temp_to_dewpoint:  bad rh ',rh
       stop
    end if
    if ( rh <= 0.01_r_kind ) then
       print*,'rh_and_temp_to_dewpoint: low rh ',rh
    end if

    call sat_vapor_press_bolton(tmpk, es)
    e = rh * es

    dptc = 243.5_r_kind / (17.67_r_kind / log(e/es0C) - 1.0_r_kind)

    tdk = dptc + Tfrez

    return
end subroutine  rh_and_temp_to_dewpoint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   temp_and_dewpoint_to_rh - function that computes the relative humidity
!                             given temperature and dewpoint
!
!    temp_and_dewpoint_to_rh - relative humidity (0.00 - 1.00)
!    tmpk                    - temperature (Kelvin)
!    dptk                    - dewpoint (Kelvin)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine temp_and_dewpoint_to_rh(tmpk, dptk, rh)

    implicit none 

    real(r_kind), intent(inout):: rh
    real(r_kind), intent(in) :: tmpk
    real(r_kind), intent(in) :: dptk

    real(r_kind)             :: e                  ! vapor pressure (Pa)
    real(r_kind)             :: es                 ! saturation vapor pressure (Pa)

    call sat_vapor_press_bolton(dptk,e)
    call  sat_vapor_press_bolton(tmpk,es)

    rh = e / es

    if (rh > 1.00_r_kind) then
        print*,'rh = ', rh, ', resetting to 1.00'
        rh = 1.00_r_kind
    end if

    return
end subroutine temp_and_dewpoint_to_rh

end module met_mod
