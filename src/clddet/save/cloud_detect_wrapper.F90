PROGRAM Cloud_Detect_Wrapper

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2006, EUMETSAT, All Rights Reserved.

!**** CLOUD_DETECT_WRAPPER
!        A. Collard   ECMWF   22/05/06

!     PURPOSE
!     -------
!     Main program for testing the cloud detection software.

!     MODIFICATIONS
!     -------------
!     22/05/06   A.Collard   1.0   Original version.
!     23/11/09   R.Eresmaa   1.1   Print out the elapsed CPU time.
!     03/12/13   R.Eresmaa   2.0   Modify to include the imager-assisted scheme.
!     19/01/15   R.Eresmaa   2.1   Add imager channel IDs to the call to the
!                                  imager cloud detection.
!                                  Specify K__MINLEV and K__MAXLEV separately
!                                  for each FOV.
!     10/11/15   R.Eresmaa   2.2   Add aerosol flag separately from cloud flags.
!     21/12/16   R.Eresmaa   2.3   Separate calls to cloud and aerosol detection.


IMPLICIT NONE

INTEGER :: K__NCHANS                          ! No. of sounder channels
INTEGER :: K__SENSOR                          ! Sensor
INTEGER :: K__MINLEV                          ! Level corresponding to top of
                                              ! initial cloud search
INTEGER :: K__MAXLEV                          ! Level corresponding to bottom of
                                              ! initial cloud search
INTEGER, ALLOCATABLE :: K__CHANID(:)          ! Channel IDs
REAL,    ALLOCATABLE :: P__MODELBTS(:)        ! Model clear brightness
                                              ! temperatures (BTs)
REAL,    ALLOCATABLE :: P__OBSBTS(:)          ! Observed BTs
REAL,    ALLOCATABLE :: P__CHAN_LEVEL(:)      ! Channel height assignments
INTEGER, ALLOCATABLE :: K__CLOUD_FLAG(:)      ! Final cloud flags
INTEGER              :: K__AEROSOL_FLAG       ! Final aerosol flag

INTEGER :: K__Imager_Nchans                   ! No. of imager channels
INTEGER :: K__Imager_Nclust                   ! No. of imager clusters
INTEGER, ALLOCATABLE :: K__Chanid_Imager(:)   ! Imager channel IDs
REAL, ALLOCATABLE :: P__Imager_Cl_Fraction(:) ! Cluster fractional coverages
REAL, ALLOCATABLE :: P__Imager_Cl_Mean(:,:)   ! Cluster mean BTs
REAL, ALLOCATABLE :: P__Imager_Ov_Stddev(:)   ! Overall imager BT standard
                                              ! deviations
REAL, ALLOCATABLE :: P__Imager_Fg_Bt(:)       ! Background imager BTs
INTEGER :: K__Imager_Flag                     ! Preliminary cloud flag as
                                              ! determined from imager data only

LOGICAL :: input_file_exists
LOGICAL :: L__Imager_Found
INTEGER :: NumObs, IObs
REAL :: Latitude, Longitude
REAL :: start_time, end_time
CHARACTER(LEN=50) :: CRECORD
CHARACTER(LEN=100) :: c_input_filename
CHARACTER(LEN=100) :: c_output_filename

INCLUDE 'abor1.intfb'
INCLUDE 'cloud_detect_setup.intfb'
INCLUDE 'aerosol_detect_setup.intfb'
INCLUDE 'imager_cloud_detect.intfb'
INCLUDE 'cloud_detect.intfb'
INCLUDE 'aerosol_detect.intfb'

!---------------------------------------------------------------------

CALL CPU_TIME(start_time)

CALL Cloud_Detect_Setup

CALL Aerosol_Detect_Setup

c_input_filename='cloud_detection_input.dat'
c_output_filename='cloud_detection_output.dat'

input_file_exists=.TRUE.
INQUIRE (FILE=TRIM(c_input_filename), EXIST=input_file_exists)
IF (.NOT. input_file_exists) THEN
   CALL ABOR1('Input file not found!')
ENDIF

OPEN(1,FILE=TRIM(c_input_filename),STATUS='OLD')
OPEN(2,FILE=TRIM(c_output_filename),STATUS='UNKNOWN')

READ(1,*) K__SENSOR
READ(1,*) K__NCHANS

ALLOCATE(K__CHANID(K__NCHANS))
ALLOCATE(P__MODELBTs(K__NCHANS))
ALLOCATE(P__OBSBTs(K__NCHANS))
ALLOCATE(P__CHAN_LEVEL(K__NCHANS))
ALLOCATE(K__CLOUD_FLAG(K__NCHANS))

READ(1,*) K__CHANID(:)
READ(1,*) NumObs

READ(1,'(A)') CRECORD  ! This is either longitude/latitude pair of the first
                       ! observation or, if collocated imager data are
                       ! included, number of provided imager channels.

IF (INDEX(CRECORD,'.')==0) THEN
   L__Imager_Found=.TRUE.
   READ(CRECORD,*) K__Imager_Nchans
   ALLOCATE (K__Chanid_Imager(K__Imager_Nchans))
   READ(1,*) K__Chanid_Imager(1:K__Imager_Nchans)
   READ(1,*) K__Imager_Nclust
   ALLOCATE (P__Imager_Cl_Fraction(K__Imager_Nclust))
   ALLOCATE (P__Imager_Cl_Mean(K__Imager_Nchans,K__Imager_Nclust))
   ALLOCATE (P__Imager_Ov_Stddev(K__Imager_Nchans))
   ALLOCATE (P__Imager_FG_BT(K__Imager_Nchans))
   READ(1,*) Longitude, Latitude
ELSE
   L__Imager_Found=.FALSE.
   READ(CRECORD,*) Longitude, Latitude
END IF

DO IObs = 1, NumObs

   IF (IObs>1) READ(1,*)  Longitude, Latitude
   READ(1,*)  K__MINLEV, K__MAXLEV
   READ(1,*)  P__OBSBTs(:)
   READ(1,*)  P__MODELBTs(:)
   READ(1,*)  P__CHAN_LEVEL(:)

   K__Imager_Flag=0

   IF (L__Imager_Found) THEN
      READ (1,*) P__Imager_Cl_Fraction(:), &
&                P__Imager_Cl_Mean(:,:), &
&                P__Imager_Ov_Stddev(:), &
&                P__Imager_FG_BT(:)

      CALL Imager_Cloud_Detect( &
           K__SENSOR,             & ! in
           K__IMAGER_NCHANS,      & ! in
           K__CHANID_IMAGER,      & ! in
           K__IMAGER_NCLUST,      & ! in
           K__IMAGER_FLAG,        & ! out
           P__IMAGER_CL_FRACTION, & ! in
           P__IMAGER_CL_MEAN,     & ! in
           P__IMAGER_OV_STDDEV,   & ! in
           P__IMAGER_FG_BT )        ! in

   ENDIF

   CALL Cloud_Detect(    &
        K__SENSOR,       & ! in
        K__NCHANS,       & ! in
        K__CHANID,       & ! in
        P__MODELBTs,     & ! in
        P__OBSBTs,       & ! in
        P__CHAN_LEVEL,   & ! in
        K__CLOUD_FLAG,   & ! out
        K__MINLEV,       & ! in
        K__MAXLEV,       & ! in
        K__IMAGER_FLAG )   ! in

   CALL Aerosol_Detect(  &
        K__SENSOR,       & ! in
        K__NCHANS,       & ! in
        K__CHANID,       & ! in
        P__OBSBTs,       & ! in
        K__AEROSOL_FLAG )


   WRITE(2,FMT='(2F9.3,I5,A)') Longitude, Latitude, IObs, &
        ' ! Longitude, Latitude, ObNumber'
   WRITE(2,FMT='(10I3)') K__CLOUD_FLAG(:)
   WRITE(2,FMT='(10I3)') K__AEROSOL_FLAG

END DO

IF(ALLOCATED(K__CHANID))             DEALLOCATE(K__CHANID)
IF(ALLOCATED(P__MODELBTs))           DEALLOCATE(P__MODELBTs)
IF(ALLOCATED(P__OBSBTs))             DEALLOCATE(P__OBSBTs)
IF(ALLOCATED(P__CHAN_LEVEL))         DEALLOCATE(P__CHAN_LEVEL)
IF(ALLOCATED(K__CLOUD_FLAG))         DEALLOCATE(K__CLOUD_FLAG)

IF(ALLOCATED(K__Chanid_Imager))      DEALLOCATE (K__Chanid_Imager)
IF(ALLOCATED(P__Imager_Cl_Fraction)) DEALLOCATE (P__Imager_Cl_Fraction)
IF(ALLOCATED(P__Imager_Cl_Mean))     DEALLOCATE (P__Imager_Cl_Mean)
IF(ALLOCATED(P__Imager_Ov_Stddev))   DEALLOCATE (P__Imager_Ov_Stddev)
IF(ALLOCATED(P__Imager_FG_BT))       DEALLOCATE (P__Imager_FG_BT)

CLOSE(1)
CLOSE(2)

CALL CPU_TIME(end_time)

WRITE (*,'(A,I6,A,F8.2,A)') &
      'Processed ', NumObs, ' observations in ', &
      end_time-start_time, ' seconds'

END PROGRAM Cloud_Detect_Wrapper
