!
Subroutine  OPEN_geo(geofile,NCID)
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

   CHARACTER*120 ::  geofile
   integer :: NCID
   integer :: status
!
   STATUS=NF_OPEN(trim(geofile),0,NCID)
!
   IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

end Subroutine OPEN_geo
!
Subroutine  CLOSE_geo(NCID)
! 
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

   integer :: NCID
   integer :: status
! 
   STATUS=NF_CLOSE(NCID)
! 
   IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

end Subroutine CLOSE_geo


Subroutine  GET_geo_sngl_geo(NCID,mscNlon,mscNlat,mscValueLAT,mscValueLON,modl)
!
!  Author: Ming Hu, ESRL/GSD
!  
!  First written: 12/16/2007.
!
!  IN:
!     mscNlon
!     mscNlan
!     NCID
!  out:
!     mscValueLAT
!     mscValueLON
!
!  Modification history:
!  02/22/2019  cctong  - added option "modl" to indicate grid conf.
!                          of what model is used: 1 for WRF
!                                                 2 for FV3
  IMPLICIT NONE
 
  INCLUDE 'netcdf.inc'

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data

  INTEGER ::  NCID, STATUS, MSLATID,MSLONID
  INTEGER ::  modl

  INTEGER ::   NDIMS
  PARAMETER (NDIMS=3)                  ! number of dimensions
  INTEGER START(NDIMS), COUNT(NDIMS)

  REAL ::   mscValueLAT(mscNlon,mscNlat,1)
  REAL ::   mscValueLON(mscNlon,mscNlat,1)
  INTEGER :: i,j

  REAL :: fv3LATtem(mscNlon,mscNlat)
  REAL :: fv3LONtem(mscNlon,mscNlat)
  REAL :: XX(mscNlon,mscNlat),YY(mscNlon,mscNlat)

  START(1)=1
  START(2)=1
  START(3)=1
  COUNT(1)=mscNlon
  COUNT(2)=mscNlat
  COUNT(3)=1
  
  if (modl == 1) then
    STATUS = NF_INQ_VARID (NCID, 'XLAT_M', MSLATID)
  elseif (modl == 2) then
    STATUS = NF_INQ_VARID (NCID, 'grid_latt', MSLATID)
  endif
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, MSLATID, START, COUNT, mscValueLAT)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  if (modl == 1) then
    STATUS = NF_INQ_VARID (NCID, 'XLONG_M', MSLONID)
  elseif (modl == 2) then
    STATUS = NF_INQ_VARID (NCID, 'grid_lont', MSLONID)
  endif
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, MSLONID, START, COUNT, mscValueLON)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
    
  if (modl == 2) then
    fv3LONtem=mscValueLON(:,:,1)
    fv3LATtem=mscValueLAT(:,:,1)
    DO i=1,mscNlon
      XX(i,:)=fv3LONtem(mscNlon+1-i,:)-360
      YY(i,:)=fv3LATtem(mscNlon+1-i,:)
    END DO
    DO j=1,mscNlat
      mscValueLON(:,j,1)=XX(:,mscNlat+1-j)
      mscValueLAT(:,j,1)=YY(:,mscNlat+1-j)
    END DO
  endif

  OPEN(11,file='temLAT.txt',STATUS='unknown')
  WRITE(11,'(1728(f9.3,1x))') ((mscValueLAT(i,j,1),j=1,mscNlat),i=1,mscNlon)
  CLOSE(11)
  OPEN(12,file='temLON.txt',STATUS='unknown')
  WRITE(12,'(1728(f9.3,1x))') ((mscValueLON(i,j,1)+360.,j=1,mscNlat),i=1,mscNlon)
  CLOSE(12) 

end subroutine GET_geo_sngl_geo

Subroutine  GET_DIM_ATT_geo(geosngle,LONLEN,LATLEN,modl)
!
!  Author: Ming Hu, CAPS. University of Oklahma.
!  
!  First written: 12/16/2007.
!
!   New verison of geo file
!
!  IN:
!     geosngle : name of mosaic file
!  OUT
!     LONLEN
!     LATLEN
!
!  Modification history:
!  02/22/2019  cctong  - added option "modl" to indicate grid conf.
!                          of what model is used: 1 for WRF
!                                                 2 for FV3 
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    geosngle

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID
  INTEGER ::  LONLEN, LATLEN

  INTEGER :: modl

  STATUS = NF_OPEN(trim(geosngle), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  
  if (modl == 1) then
    STATUS = NF_INQ_DIMID(NCID, 'west_east', LONID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
    STATUS = NF_INQ_DIMID(NCID, 'south_north', LATID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  elseif (modl == 2) then
    STATUS = NF_INQ_DIMID(NCID, 'grid_xt', LONID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
    STATUS = NF_INQ_DIMID(NCID, 'grid_yt', LATID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  else
    WRITE(*,*)'Model option',modl,'is not supported yet.' 
    WRITE(*,*)'Run stops here!'
    STOP
  endif
   
  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

END SUBROUTINE GET_DIM_ATT_geo
