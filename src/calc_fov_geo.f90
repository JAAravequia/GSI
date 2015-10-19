 module calc_fov_geo
!$$$   module documentation block
!                .      .    .                                       .
! module:    calc_fov_geo
!
! abstract: contains all routines necessary for fov calculations
!   for geostationary sounders and imagers.
!
! program history log:
!   2014-09-05  gayno   initial version
!
! subroutines included:
!   sub instrument_init       - initialize varibles required by
!                               this module.
!   sub inside_fov_geo        - tests if point is inside a fov.
!   sub fov_ellipse_geo       - determines the lat/lons of the
!                               fov boundary.
!   sub fov_geo_angles_sizes  - compute the cross track and along
!                               track fov angles and sizes.
!   sub cleanup               - free up memory.
!
! Variable Definitions:
!   def npoly                 - fovs are represented by a polygon
!                               with npoly vertices.
!   def earth                 - nominal earth radius in km
!   def height                - geosynchronous altitude in km
!   def geosynch              - earth plus height
!   def satellite_azimuth     - satellite_azimuth (degrees)
!   def satellite_azimuth_rot - satellite_azimuth rotated 90 degrees
!   def rmax                  - cross track semi-axes for each channel
!   def eccen                 - fov eccentricity for each channel
!   def fovangle_31           - angular dimension of imager fovs
!   def fovangle_32           - angular dimension of sounder fovs
!
! attributes:
!   language: f90
!   machine:  ncep WCOSS
!
!$$$ end documentation block

 use kinds, only : i_kind, r_kind
 use calc_fov_crosstrk, only : npoly

 implicit none
 
 private

!integer(i_kind), parameter, public :: npoly = 120 

 real(r_kind), parameter            :: earth = 6371.22_r_kind   
 real(r_kind), parameter            :: height = 35786.0_r_kind 
 real(r_kind), parameter            :: geosynch = earth + height

 real(r_kind)                       :: satellite_azimuth
 real(r_kind)                       :: satellite_azimuth_rot
 real(r_kind),allocatable           :: rmax (:)
 real(r_kind),allocatable           :: eccen (:)

! Angular dimension of the fov for imager.
 real(r_kind), dimension(5), target      :: fovangle_31 =  &
  (/1.604282E-03, 6.417127E-03, 1.283425E-02, 6.417127E-03, 6.417127E-03/)

! Angular dimension of the fov for sounder.
 real(r_kind), dimension(19), target     :: fovangle_32 =  &
  (/1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02/)

 public instrument_init
 public inside_fov_geo
 public fov_ellipse_geo
 public cleanup

 contains

 subroutine instrument_init(instr, lat, lon, sublat, sublon, expansion, valid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    instrument_init          initialize instrument fields
!
!   prgmmr: kleespies           org: nesdis              date: 2008
!
! abstract: initialize variables required by this module:
!             1) satellite azimuth (satellite_azimuth and 
!                                   satellite_azimuth_rot)
!             2) fov eccentricity for each channel (eccen)
!             3) cross track semi-axes for each channel (rmax)
!
! program history log:
!   2008        kleespies
!   2014-09-04  gayno - modified for gsi software standards
!
! input argument list:
!   instr      - Instrument number
!                 31 = GOES imager
!                 32 = GEOS sounder
!   expansion  - expansion factor.  Must be 1.0 for accurate renderine,
!                > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!   lat        - latitude of center of fov (degrees)
!   lon        - longitude of center of fov (degrees)
!   sublat     - sub-satellite latitude (degrees)
!   sublon     - sub-satellite longitude (degrees)
!
! output argument list:
!   valid     - error status; false when inputs are incorrect.
!
! attributes:
!   language: f90
!   machine:  ncep WCOSS
!
!$$$

 use constants, only              : rad2deg
 use kinds, only                  : i_kind, r_kind

 implicit none

! Declare passed variables.
 integer(i_kind) , intent(in   ) :: instr    
 logical         , intent(  out) :: valid
 real(r_kind)    , intent(in   ) :: sublat   
 real(r_kind)    , intent(in   ) :: sublon   
 real(r_kind)    , intent(in   ) :: lat      
 real(r_kind)    , intent(in   ) :: lon      
 real(r_kind)    , intent(in   ) :: expansion

! Declare local variables.
 integer(i_kind)                 :: ichan, nchan
 real(r_kind)                    :: ang_dim, rmin 
 real(r_kind)                    :: nadir_angle
 real(r_kind)                    :: arc_angle, cc, a
 real(r_kind)                    :: along_track_angle 
 real(r_kind)                    :: cross_track_angle 
 real(r_kind)                    :: along_track_fov_size 
 real(r_kind)                    :: cross_track_fov_size 
 real(r_kind)                    :: ratio ! local ratio for eccentricity calculation
 real(r_kind)                    :: coss, dellon, s, sinalpha
 real(r_kind)                    :: prod1, prod2, prod3

 valid=.true.

 if (instr < 31 .or. instr > 32) then
   write(6,*) "INSTRUMENT_INIT: INSTRUMENT NUMBER OF: ", instr, " IS OUT OF RANGE."
   valid=.false.
   return
 end if

 if (instr == 31) then
   nchan = 5
   ang_dim=maxval(fovangle_31)  ! angular dimension of fov
 elseif (instr == 32) then
   nchan = 19
   ang_dim=maxval(fovangle_32)
 endif

 if(lat == sublat .and. lon == sublon) then
   nadir_angle = 0.0
 else
   call arch_angle(sublat,sublon,lat,lon,arc_angle)
   a = sqrt( geosynch*geosynch + earth*earth - 2.0_r_kind*geosynch*earth*cos(arc_angle) )
   cc = acos( (a*a + geosynch*geosynch - earth*earth) / (2.0_r_kind*a*geosynch) )
   nadir_angle = cc*rad2deg  ! this is our scan angle
 endif

! The arcsin of prod3 is the complement of the zenith angle for a fov.
! Prod3 can be greater than one when the fov is far away from the sub-satellite
! point.  This check used to be in routine fov_geo_angles_sizes.
! Do the check once here.  The angular dimension is really channel
! specific, but for simplicity the largest value was picked.

 prod1 = geosynch/earth
 prod2 = sin((nadir_angle+0.5_r_kind*ang_dim)/rad2deg)
 prod3 = prod1 * prod2
 if (prod3 > 0.99) then
   write(6,*) "INSTRUMENT_INIT: INVALID ZENITH ANGLE"
   valid=.false.
   return
 endif

 call cleanup

 allocate(eccen(nchan))
 allocate(rmax(nchan))

 CHANNEL : do ichan = 1, nchan

   call fov_geo_angles_sizes(instr, ichan, nadir_angle, &
                             along_track_angle, cross_track_angle, &
                             along_track_fov_size, cross_track_fov_size)

   rmax(ichan) = 0.5_r_kind*cross_track_angle* expansion ! remember, these are semiaxes
   rmin = 0.5_r_kind*along_track_angle* expansion

   ratio = rmin**2/rmax(ichan)**2
   if(ratio > 1.0_r_kind ) ratio = 1.0_r_kind  !  this takes care of some precision issues

   eccen(ichan) = sqrt(1.0_r_kind - ratio)

 enddo CHANNEL

 if( (lat == sublat) .and. (lon == sublon) ) then

   satellite_azimuth = 0.0_r_kind

 else

   dellon = lon - sublon

   call arch_angle(lat, lon, sublat, sublon, s)

   sinalpha = sin((90.0_r_kind - sublat)/rad2deg)*sin(dellon/rad2deg)/sin(s)

   if(sinalpha >  1.0_r_kind) sinalpha =  1.0_r_kind
   if(sinalpha < -1.0_r_kind) sinalpha = -1.0_r_kind

   satellite_azimuth = asin(sinalpha)*rad2deg
   if (lat < 0.0_r_kind) satellite_azimuth = sign(180.0_r_kind,dellon) - satellite_azimuth

 endif

 satellite_azimuth_rot = satellite_azimuth
 if(satellite_azimuth_rot < 0.0_r_kind) satellite_azimuth_rot = 360.0 + satellite_azimuth_rot

! 2nd, Shift rotation direction
 satellite_azimuth_rot = mod((450.0_r_kind-satellite_azimuth_rot),360.0_r_kind)

 end subroutine instrument_init

 subroutine inside_fov_geo(instr, ichan, lat, lon,  &
                           testlat, testlon, expansion, inside)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inside_fov_geo     is a point inside a fov?
!
!   prgmmr: kleespies           org: nesdis              date: 2008
!
! abstract: determine if a point is inside or outside of a geostationary 
!           fov ellipse.
!
! program history log:
!   2008        kleespies
!   2014-09-05  gayno - modified for gsi software standards
!
! input argument list:
!   instr      - Instrument number
!                 31 = GOES imager
!                 32 = GEOS sounder
!   ichan      - channel number
!                  1 thru 5 for imager
!                  1 thru 19 for sounder
!   expansion  - expansion factor.  Must be 1.0 for accurate renderine,
!                > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!   lat        - latitude of center of fov (degrees)
!   lon        - longitude of center of fov (degrees)
!   testlat    - latitude of point to be tested (degrees)
!   testlon    - longitude of point to be tested (degrees)
!
! output argument list:
!   inside     - 0 if test point is outside fov;  1 if inside.
!
! remarks:
!   Earth assumed to be locally flat around the field of view.
!
!   No provisions are made for spacecraft roll-pitch-yaw errors, which are presumed
!   to be small (SC attitude is usually held to within 0.1 deg).
!
! attributes:
!   language: f90
!   machine:  ncep WCOSS
!
!$$$

 use constants, only : deg2rad, rad2deg, pi
 use kinds, only : r_kind, i_kind

 implicit none

! Declare passed variables
 integer(i_kind) , intent(in   ) :: instr     ! instrument number
 integer(i_kind) , intent(in   ) :: ichan     ! channel number
 real(r_kind)    , intent(in   ) :: lat       ! fov center latitude
 real(r_kind)    , intent(in   ) :: lon       ! fov center longitude
 real(r_kind)    , intent(in   ) :: testlat   ! latitude to be tested
 real(r_kind)    , intent(in   ) :: testlon   ! longitude to be tested
 real(r_kind)    , intent(in   ) :: expansion ! fov expansion factor, normally = 1.0
 real(r_kind)    , intent(  out) :: inside    ! return value = 1.0 if test location inside fov

! Declare local variables
 real(r_kind), parameter         :: r1 = 1.0_r_kind   ! Equatorial radius. Work in angular distance, 
                                                      ! not km (otherwise r1=6371)
 real(r_kind), parameter         :: r2 = r1  ! assume spherical earth (otherwise r2 = polar radius)

 real(r_kind)                    :: dellon ! latitude difference from fov center to test location
 real(r_kind)                    :: dellat ! longitude difference from fov center to test location
 real(r_kind)                    :: d     ! angular distance from fov center to test location
 real(r_kind)                    :: r     ! angular distance from fov center to ellipse along d
 real(r_kind)                    :: psi   ! angle from 3:00 on fov to line from 
                                          ! fov center to test location
 real(r_kind)                    :: psip  ! angle from latitude parallel through 
                                          ! fov center to line from fov center 
                                          ! to test location
 real(r_kind)                    :: distance_north ! north angular distance from 
                                                   ! fov center to test location.
                                                   ! for flat earth.
 real(r_kind)                    :: distance_east  ! east angular distance from
                                                   ! fov center to test location.
                                                   ! for flat earth.
 real(r_kind)                    :: bearing_to_test ! same as psip. for flat earth.
 real(r_kind)                    :: bearing_to_test_deg ! in degrees. for flat earth.

 dellat = (testlat - lat)*deg2rad
 dellon = (testlon - lon)*deg2rad

! distance north and east in degrees
 distance_north =  r1*dellat
 distance_east  =  r2*cos(lat*deg2rad)*dellon

! angle to the test point
 bearing_to_test = mod(atan2(distance_north,distance_east),2.0_r_kind*pi)
 bearing_to_test_deg = bearing_to_test*rad2deg ! convert to degrees

! this is the arc distance to the test point
 d = 2.0_r_kind*asin(sqrt((sind((testlat-lat)/2.0_r_kind))**2 +     & 
              cosd(testlat)*cosd(lat)*(sind((testlon-lon)/2.0_r_kind))**2))
 d = d*rad2deg  ! convert to degrees

 psip = bearing_to_test_deg

 psi = psip  - satellite_azimuth_rot
 psi = psi*deg2rad ! convert to radians

! r is the angular distance from the fov center to the edge of the ellipse in degrees
 r = rmax(ichan) * sqrt( (1.0_r_kind - eccen(ichan)**2.0_r_kind)/(1.0_r_kind - eccen(ichan)**2 *cos(psi)**2) )

 inside = 0.0_r_kind
 
 if (d<r) inside = 1.0_r_kind

 end subroutine inside_fov_geo
!
 subroutine fov_ellipse_geo (ichan, lat, lon, elats, elons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_ellipse_geo    computes fov ellipses
!
!   prgmmr: kleespies           org: nesdis              date: 2008-07-08
!
! abstract: computes FOV ellipses in latitude/longitude coordinates of a 
!   geosynchronous instrument.  note: we model the FOV as circles.  this is
!   fine for the GOES sounder, but the imager has square fovs.
!
! program history log:
!   2008-07-08  kleespies
!   2014-09-04  gayno - modified for gsi software standards
!                     
! input argument list:
!   ichan     - channel number.  valid values are:
!                 1 thru 5 for imager
!                 1 thru 19 for sounder
!   lat       - latitude of fov center (degrees)
!   lon       - longitude of fov center (degrees)
!
! output argument list:
!   elats     - ellipse latitudes  centered about lat,lon (degrees)
!   elons     - ellipse longitudes centered about lat,lon (degrees)
!
! remarks:
!
!  Set npoly for the order of the polygon that represents the ellipse.
!
!  elats and elon must be dimensioned to npoly in the calling routine.
!
!  There are several engineering checks to handle things like 
!  arcsin( x>1.0 or x<-1.0).   These things can happen because POES navigation
!  uses an oblate spheroid earth, while here we are using a spherical earth, 
!  so there is a small inconsistency in computing arc angles.
!
!  No provisions are made for spacecraft roll-pitch-yaw errors, which 
!  are presumed to be small. (SC attitude is usually held to within 0.1 deg)
!
! attributes:
!   language: f90
!   machine:  ncep WCOSS
!
!$$$

 use constants, only : rad2deg, pi
 use kinds, only : i_kind, r_kind

 implicit none

! Declare passed variables
 integer(i_kind) , intent(in   )   :: ichan
 real(r_kind)    , intent(in   )   :: lat, lon
 real(r_kind)    , intent(  out)   :: elats(npoly), elons(npoly)

! Declare local variables
 integer(i_kind)                   :: i
 real(r_kind)                      :: pos_ang ! rotation angle of the ellipse
 real(r_kind), dimension(npoly)    :: psi, psip, r, cosc, c, sinb, b
  
 do i = 1 , npoly
   psi(i) = 2.0_r_kind*pi*(i-1)/(npoly-1)  ! fov is a polygon
 enddo

 pos_ang = satellite_azimuth 

 psip = psi + pos_ang/rad2deg
 r    = rmax(ichan) * sqrt( (1.0_r_kind - eccen(ichan)**2)/(1.0_r_kind - eccen(ichan)**2 * cos(psi)**2) ) 
 cosc = cos((90.0_r_kind-lat)/rad2deg)*cos(r/rad2deg) + sin((90.0_r_kind-lat)/rad2deg)*sin(r/rad2deg)*cos(psip)
 c    = acos(cosc)*rad2deg

 elats = 90.0_r_kind - c

 sinb = sin(r/rad2deg)*sin(psip)/sin(c/rad2deg)

 do i = 1 , npoly
   if(sinb(i) >  1.0_r_kind) sinb(i) =  1.0_r_kind
   if(sinb(i) < -1.0_r_kind) sinb(i) = -1.0_r_kind
 enddo

 b     = asin(sinb)*rad2deg

 elons = lon + b

 end subroutine fov_ellipse_geo

 subroutine fov_geo_angles_sizes(instr, chan, nadir_angle, along_track_angle, &
                                 cross_track_angle, along_track_fov_size, cross_track_fov_size)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_geo_angles_sizes    compute cross track and along track fov
!                                        size and angle.
!
!   prgmmr: kleespies           org: nesdis              date: 2008
!
! abstract: computes the cross track and along track angles of a geosynchronous
!           instrument FOV as viewed from the center of the earth, and cross track 
!           and along track FOV size in km.  presumes a spherical earth.
!
! program history log:
!   2008        kleespies
!   2014-09-05  gayno - modified for gsi software standards
!
! input argument list:
!   instr                - Instrument number
!                            31 = GOES imager
!                            32 = GEOS sounder
!   chan                 - Channel number
!                            1 thru 5 for imager
!                            1 thru 19 for sounder
!   nadir_angle          - Angle from subsatellite point to 
!                          observation point (degrees)
!
! output argument list:
!   along_track_angle    - Along track angle of the fov as viewed from 
!                          center of the earth (degrees)
!   cross_track_angle    - Cross track angle of the fov as viewed from 
!                          center of the earth (degrees)
!   along_track_fov_size - Along track fov size in km
!   cross_track_fov_size - Cross track fov size in km
!
! attributes:
!   language: f90
!   machine:  ncep WCOSS
!
!$$$

 use constants, only : rad2deg, pi
 use kinds, only : i_kind, r_kind

 implicit none

! Declare passed variables.
 integer(i_kind) , intent(in   )         :: instr
 integer(i_kind) , intent(in   )         :: chan
 real(r_kind)    , intent(in   )         :: nadir_angle
 real(r_kind)    , intent(  out)         :: along_track_angle
 real(r_kind)    , intent(  out)         :: cross_track_angle
 real(r_kind)    , intent(  out)         :: along_track_fov_size
 real(r_kind)    , intent(  out)         :: cross_track_fov_size

! Declare local variables.

 real(r_kind)                            :: comp_za_center
 real(r_kind)                            :: comp_za_p
 real(r_kind)                            :: comp_za_m
 real(r_kind)                            :: distance_to_fov
 real(r_kind), dimension(:), pointer     :: fovangle
 real(r_kind)                            :: nadir_angle_m
 real(r_kind)                            :: nadir_angle_p
 real(r_kind)                            :: prod1, prod2, prod3

 nullify(fovangle)
 select case (instr)  ! out-of-range instrument is checked in routine instrument_init
   case (31)
     fovangle=>fovangle_31
   case (32)
     fovangle=>fovangle_32
 end select

!Nadir angles of center and crosstrack extremes of fov

 nadir_angle_m = nadir_angle - fovangle(chan)*0.5_r_kind
 nadir_angle_p = nadir_angle + fovangle(chan)*0.5_r_kind

 prod1=(geosynch)/earth

!Complement of zenith angle for center and crosstrack extremes of fov

 prod2=sin(nadir_angle/rad2deg)
 prod3=prod1*prod2
 comp_za_center = 180.0_r_kind-asin(prod3)*rad2deg

 prod2=sin(nadir_angle_m/rad2deg)
 prod3=prod1*prod2
 comp_za_m = 180.0_r_kind-asin(prod3)*rad2deg

 prod2=sin(nadir_angle_p/rad2deg)
 prod3=prod1*prod2
 comp_za_p = 180.0_r_kind-asin(prod3)*rad2deg

! Cross track angle of the fov as viewed from center of the earth.
 cross_track_angle = abs(nadir_angle_p + comp_za_p - nadir_angle_m - comp_za_m)

! Cross track fov size in km
 cross_track_fov_size = abs(cross_track_angle*2.0_r_kind*pi*earth/360.0_r_kind)

! Distance from satellite to the center of the fov in km.
 distance_to_fov = (geosynch)*sin( (180.0_r_kind-nadir_angle-comp_za_center)/rad2deg)/sin((comp_za_center)/rad2deg)
 if(distance_to_fov <= 0.0_r_kind) distance_to_fov = height ! for nadir fov
  
! Along track fov size in km.  
! the following is an approximation, but it is close.  It underestimates the FOV by a smidge
 along_track_fov_size = 2.0_r_kind*distance_to_fov*tan(fovangle(chan)*0.5_r_kind/rad2deg)

! Along track angle of the fov as viewed from center of the earth.
 along_track_angle = 360.0_r_kind*along_track_fov_size/(2.0_r_kind*pi*earth)

 nullify(fovangle)

 end subroutine fov_geo_angles_sizes

 subroutine arch_angle(lat1,lon1,lat2,lon2,arc_angle)
!$$$  subprogram documentation block
!
! subprogram:   arch_angle                compute arch angle
!   prgmmr: gayno            org: np23           date: 2012-04-03
!	
! abstract:  Computes the arch angle between two points on the earth.
!
! program history log:
!   2008-07-25   kleespies - initial version
!   2014-04-03   gayno - bring up to GSI coding standards

! input argument list:
!    lat1/lon1  - latitude and longitude of point 1
!    lat2/lon2  - latitude and longitude of point 2
!
! output argument list:
!    arc_angle  - arc angle in radians
!
! attributes:
!   language: f90
!   machine:  NCEP WCOSS
!
!$$$ end documentation block

 use constants, only : deg2rad
 use kinds, only : r_kind

 implicit none

! Declare passed variables.
 real(r_kind), intent(in   ) :: lat1,lon1,lat2,lon2
 real(r_kind), intent(  out) :: arc_angle

! Declare local variables.
 real(r_kind)                :: colat1,colat2,dellon,s

 colat1 = (90._r_kind - lat1)*deg2rad
 colat2 = (90._r_kind - lat2)*deg2rad
 dellon = abs(lon2 - lon1)*deg2rad

 s = cos(colat1)*cos(colat2) + sin(colat1)*sin(colat2)*cos(dellon)

 arc_angle = acos(s)

 return

 end subroutine arch_angle

 subroutine cleanup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cleanup     free up memory
!
!   prgmmr: gayno           org: emc              date: 2014-09-05
!
! abstract: free up memory by deallocating arrays.
!
! program history log:
!   2014-09-05  gayno - initial version
!
! input argument list:  n/a
!
! output argument list:  n/a
!
! attributes:
!   language: f90
!   machine:  NCEP WCOSS
!
!$$$ end documentation block

 implicit none

 if (allocated(eccen)) deallocate(eccen)
 if (allocated(rmax))  deallocate(rmax)

 end subroutine cleanup
 end module calc_fov_geo
