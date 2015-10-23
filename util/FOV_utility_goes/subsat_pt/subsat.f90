 program subsat

!----------------------------------------------------------------------
! Estimate the sub-satellite longitude of a geostationary satellite
! from the latitude, longitude and satellite zenith angle measured
! at a single FOV.
!
! Based on the lecture notes of Chris Hall of Va Tech.  See 
! Chapter 2 of: http://www.dept.aoe.vt.edu/~cdhall/courses/aoe4140/
!----------------------------------------------------------------------

 implicit none

 real(kind=8), parameter :: earth = 6371.22_8
 real(kind=8), parameter :: height = 35786.0_8
 real(kind=8), parameter :: geosynch = earth + height

 real(kind=8) :: pi, deg2rad, rad2deg
 real(kind=8) :: rho  ! angular earth radius
 real(kind=8) :: sine_rho  ! sine of rho
 real(kind=8) :: nu, sine_nu
 real(kind=8) :: epsilon, sat_zenith
 real(kind=8) :: lambda, dellon
 real(kind=8) :: lat_fov, lon_fov

!----------------------------------------------------------------------
! The inputs: lat and lon of the FOV.  And the satellite
! zenith angle at that FOV.
!----------------------------------------------------------------------

 lat_fov = 31.62_8
 lon_fov = -136.25_8
 sat_zenith = 36.67_8

!----------------------------------------------------------------------
! Some constants.
!----------------------------------------------------------------------

 pi = acos(-1.0_8)
 deg2rad = pi / 180.0_8
 rad2deg = 180.0_8 / pi

!----------------------------------------------------------------------
! "rho" is the earth angular radius
! "epsilon" is the elevation angle
! "nu" is the nadir angle
! "lambda" is the earth central angle
!----------------------------------------------------------------------

 sine_rho = earth / geosynch
 rho = asin(sine_rho)
 
 epsilon = 90.0_8 - sat_zenith

 sine_nu = cos(epsilon*deg2rad) * sine_rho
 nu = asin(sine_nu)*rad2deg

 lambda = 90.0_8 - nu - epsilon
 
!-------------------------------------------------------------------------
! Convert the earth central angle to a delta longitude using:
!
! cos(lambda) = cos(colat_fov)*cos(colat_sat) + sin(colat_sat) *  
!                                            sin(colat_fov)*cos(dellon) 
!
! Note: the colat of the satellite is 90 degrees, which simplifies
! the equation.  
!
! Unfortunately, there is no way to tell if the FOV is to the east
! or west of the sub-satellite point.
!-------------------------------------------------------------------------

 dellon = cos(lambda*deg2rad)/sin((90.0_8-lat_fov )*deg2rad)
 if (dellon > 1.0_8) dellon=1.0_8
 
 dellon = acos(dellon)*rad2deg

 write(6,33) lat_fov, lon_fov
 33 format(1x,'The lat/lon of the FOV: ',1x,f7.2,2x,f7.2)
 write(6,34) sat_zenith
 34 format(1x,'The satellite zenith angle at the FOV: ',1x,f7.2)
 write(6,35) (lon_fov+dellon), (lon_fov-dellon)
 35 format(1x,'The sub-satellite longitude: ',1x,f8.3,' or ',f8.3)

 stop
 end program subsat
