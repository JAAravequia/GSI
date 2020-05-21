!
! AntCorr_Application
!
! Module containing routines to apply/remove antenna corrections to/from 
! supported microwave sensor observations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Aug-2008
!                       paul.vandelst@noaa.gov
!
!   2011-04-25   A.Collard   Modified to be consistent with CRTM 2.1
! 

module AntCorr_Application

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  use kinds            , only: i_kind, r_kind
  use Message_Handler  , only: Display_Message, failure
  use ACCoeff_Define   , only: ACCoeff_type
  ! Disable all implicit typing
  implicit none


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  private
  ! Inherited procedures from definition module
  ! -------------------------------------------
  ! The AntCorr structure definition
  public :: ACCoeff_type
  public :: Remove_AntCorr
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Invalid result
  real(r_kind), parameter :: invalid = -1.0_r_kind
    
  ! Cosmic background temperature. Taken from
  ! Mather,J.C. et. al., 1999, "Calibrator Design for the COBE
  !    Far-Infrared Absolute Spectrophotometer (FIRAS)"
  !    Astrophysical Journal, vol 512, pp 511-520
  real(r_kind), parameter :: tspace = 2.7253_r_kind
  

contains


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Remove_AntCorr
!
! PURPOSE:
!       Subroutine to remove an antenna correction to microwave instrument
!       brightness temperatures, Tb, to produce antenna temperatures, Ta.
!
! CALLING SEQUENCE:
!       CALL Remove_AntCorr( AC  , &  ! Input
!                            iFOV, &  ! Input
!                            T     )  ! In/Output
!
! INPUT ARGUMENTS:
!       AC:             Structure containing the antenna correction coefficients
!                       for the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       TYPE(ACCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       iFOV:           The FOV index for a scanline of the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       T:              On input, this argument contains the brightness
!                       temperatures for the sensor channels.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       T:              On output, this argument contains the antenna
!                       temperatures for the sensor channels.
!                       If an error occurs, the return values are all -1.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The temperature array argument, T, is modified.
!
! PROCEDURE:
!       For every FOV and channel, the brightness temperature, Tb, is converted
!       to antenna temperature, Ta, using,
!
!         Ta = Ae.Tb + Ap.Tb + As.Ts
!
!       where Ae == antenna efficiency for the Earth view
!             Ap == antenna efficiency for satellite platform view
!             As == antenna efficiency for cold space view
!             Ts == cosmic background temperature.
!
!       Note that the observed earth view brightness temperature is used as a
!       proxy for the platform temperature for the (Ap.Tb) component since
!       there is no measurement of the platform temperature in-flight.
!
!--------------------------------------------------------------------------------

  subroutine Remove_AntCorr( ac  , &  ! Input
                             iFOV, &  ! Input
                             t     )  ! In/Output
    implicit none

    ! Arguments
    type(ACCoeff_type), intent(in)     :: ac
    integer(i_kind)   , intent(in)     :: iFOV
    real(r_kind)      , intent(in out) :: t(:)
    ! Local parameters
    character(*), parameter :: routine_name = 'Remove_AntCorr'
    ! Local variables
    integer(i_kind) :: l
    ! Check input
    if ( iFOV < 1 .OR. iFOV > ac%n_FOVS ) then
      call Display_Message( routine_name, 'Input iFOV inconsistent with AC data', failure )
      t = invalid
      return
    end if
    if ( size(t) /= ac%n_Channels ) then
      call Display_Message( routine_name, 'Size of T() inconsistent with AC data', failure )
      t = invalid
      return
    end if
    ! Compute the antenna temperature
    do l = 1, ac%n_Channels
      t(l) = ac%A_earth(iFOV,l)*t(l) + ac%A_platform(iFOV,l)*t(l) + ac%A_space(iFOV,l)*tspace
    end do
  end subroutine Remove_AntCorr
  
end module AntCorr_Application
