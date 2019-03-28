SUBROUTINE Aerosol_Detect_Setup

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2016, EUMETSAT, All Rights Reserved.


!**** Aerosol detection setup
!     J. Letertre-Danczak  ECMWF 06/10/15

!     PURPOSE
!     -------
!     Initialise aerosol detection parameters for advanced infrared sounders.

!**   INTERFACE
!     ---------
!     Aerosol_detect_setup is called from cloud_detect_wrapper.

!     METHOD
!     ------
!     Default values are assigned to the aerosol detection setup structure.

!     MODIFICATIONS
!     -------------
!     21/12/16   R.Eresmaa   2.3   Import to the NWPSAF CADS V2.3

USE YOMAERO, ONLY : S__Aerosol_Detect_Setup

USE YOMIASI, ONLY : JP__Min_Sensor_Index,    &
&                   JP__Max_Sensor_Index,    &
&                   INST_ID_AIRS,            &
&                   INST_ID_IASI,            &
&                   INST_ID_CRIS,            &
&                   INST_ID_IRS,             &
&                   INST_ID_IASING

IMPLICIT NONE

! Local variables

CHARACTER(LEN=6)   :: CL__InstrumentName
CHARACTER(LEN=16)  :: CL__Aerosol_Detection_File

INTEGER   :: J, J__Sensor      ! Loop variables
INTEGER   :: INIU1, IOS

!-----------------------
! Namelist variables
!-----------------------

INTEGER, PARAMETER :: I__Max_Bands         =   8
INTEGER, PARAMETER :: I__Max_Aerosol_Chans = 200

INTEGER :: M__Sensor
INTEGER :: N__Num_Aerosol_Tests
INTEGER :: N__Num_Aerosol_Chans(I__Max_Bands)
INTEGER :: N__Aerosol_Chans(I__Max_Aerosol_Chans,I__Max_Bands)
REAL    :: R__Aerosol_TBD(I__Max_Aerosol_Chans,I__Max_Bands)
INTEGER :: N__Mean_Aerosol_Chans ! number of channels to compute the mean

! Namelist

NAMELIST / Aerosol_Detect_Coeffs / M__Sensor, N__Num_Aerosol_Tests, &
&        N__Num_Aerosol_Chans, N__Aerosol_Chans, N__Mean_Aerosol_Chans, &
&        R__Aerosol_TBD


INCLUDE 'abor1.intfb'


!============================================================================

!============================================================================
!   Loop through sensors setting up aerosol detection
!============================================================================

SensorLoop : DO J__Sensor = JP__Min_Sensor_Index, JP__Max_Sensor_Index

   SELECT CASE (J__Sensor)

   CASE(INST_ID_AIRS)
      !====================
      ! Set up AIRS
      !====================

      CL__InstrumentName='AIRS'
      CL__Aerosol_Detection_File = 'AIRS_AERDET.NL'

      N__Num_Aerosol_Tests = 1
      N__Num_Aerosol_Chans(:) = 0
      N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests) = (/ 4 /)
      N__Aerosol_Chans(:,:) = 0
      N__Aerosol_Chans(N__Num_Aerosol_Tests,1:N__Num_Aerosol_Chans(1)) = &
&           (/ 1260, 914, 950, 1301 /)
      N__Mean_Aerosol_Chans = 1
      R__Aerosol_TBD(:,:) = 0.0
      R__Aerosol_TBD(N__Num_Aerosol_Tests,1:2) = (/ -0.7, -2.0 /)

   CASE(INST_ID_IASI)
      !====================
      ! Set up IASI
      !====================

      CL__InstrumentName='IASI'
      CL__Aerosol_Detection_File = 'IASI_AERDET.NL'

      N__Num_Aerosol_Tests = 1
      N__Num_Aerosol_Chans(:) = 0
      N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests) = (/ 4 /)
      N__Aerosol_Chans(:,:) = 0
      N__Aerosol_Chans(N__Num_Aerosol_Tests,1:N__Num_Aerosol_Chans(1)) = &
&           (/ 1340, 2348, 1782, 2356 /)
      N__Mean_Aerosol_Chans = 11
      R__Aerosol_TBD(:,:) = 0.0
      R__Aerosol_TBD(N__Num_Aerosol_Tests,1:2) = (/ 0.2, -1.55 /)

   CASE(INST_ID_CRIS)
      !====================
      ! Set up CRIS
      !====================

      CL__InstrumentName='CRIS'
      CL__Aerosol_Detection_File = 'CRIS_AERDET.NL'

      N__Num_Aerosol_Tests = 1
      N__Num_Aerosol_Chans(:) = 0
      N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests) = (/ 4 /)
      N__Aerosol_Chans(:,:) = 0
      N__Aerosol_Chans(N__Num_Aerosol_Tests,1:N__Num_Aerosol_Chans(1)) = &
&           (/ 730, 529, 704, 732 /)
      N__Mean_Aerosol_Chans = 5
      R__Aerosol_TBD(:,:) = 0.0
      R__Aerosol_TBD(N__Num_Aerosol_Tests,1:2) = (/ 4.0, -0.5 /)

   CASE(INST_ID_IRS)
      !====================
      ! Set up IRS
      !====================

      CL__InstrumentName='IRS'
      CL__Aerosol_Detection_File = 'IRS_AERDET.NL'

      N__Num_Aerosol_Tests = 1
      N__Num_Aerosol_Chans(:) = 0
      N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests) = (/ 4 /)
      N__Aerosol_Chans(:,:) = 0
      N__Aerosol_Chans(N__Num_Aerosol_Tests,1:N__Num_Aerosol_Chans(1)) = &
&           (/ 650, 449, 624, 652 /)
      N__Mean_Aerosol_Chans = 5
      R__Aerosol_TBD(:,:) = 0.0
      R__Aerosol_TBD(N__Num_Aerosol_Tests,1:2) = (/ 4.0, -0.5 /)

   CASE(INST_ID_IASING)
      !====================
      ! Set up IASING
      !====================

      CL__InstrumentName='IASING'
      CL__Aerosol_Detection_File = 'IASING_AERDET.NL'

      N__Num_Aerosol_Tests = 1
      N__Num_Aerosol_Chans(:) = 0
      N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests) = (/ 4 /)
      N__Aerosol_Chans(:,:) = 0
      N__Aerosol_Chans(N__Num_Aerosol_Tests,1:N__Num_Aerosol_Chans(1)) = &
&           (/ 2679, 4695, 3563, 4711 /)
      N__Mean_Aerosol_Chans = 21
      R__Aerosol_TBD(:,:) = 0.0
      R__Aerosol_TBD(N__Num_Aerosol_Tests,1:2) = (/ 0.2, -1.55 /)

   CASE DEFAULT
      CYCLE
   END SELECT

   !------------------------------------------------------------------
   ! Open and read file containing aerosol detection setup for the
   ! current instrument
   !------------------------------------------------------------------
   INIU1=10
   WRITE(*,*)'READING AEROSOL DETECTION FILE FOR ',TRIM(CL__InstrumentName)
   OPEN(INIU1,STATUS='OLD',FORM='FORMATTED', &
        FILE=TRIM(CL__Aerosol_Detection_File), IOSTAT=IOS)
   IF (IOS == 0) THEN
      READ(INIU1,nml=Aerosol_Detect_Coeffs,IOSTAT=IOS)
      IF (IOS == 0) THEN
         WRITE(*,*) TRIM(CL__InstrumentName),' AEROSOL DETECTION FILE READ OK'
      ELSE
         CALL ABOR1('PROBLEM READING '//TRIM(CL__InstrumentName)//&
&                 'AEROSOL DETECTION FILE')
      ENDIF
      CLOSE(INIU1)
   ELSE
      WRITE(*,*)'NO '//TRIM(CL__InstrumentName)//&
&              ' AEROSOL DETECTION FILE : Using Default Values'
   ENDIF

   M__Sensor = J__Sensor

   !------------------------------------------------------------------
   ! Set up the S__Aerosol_Detect_Setup structure for current sensor
   !------------------------------------------------------------------

   S__Aerosol_Detect_Setup(J__SENSOR) % M__SENSOR = M__Sensor

   S__Aerosol_Detect_Setup(J__SENSOR) % N__Num_Aerosol_Tests = &
&        N__Num_Aerosol_Tests

   ALLOCATE( S__Aerosol_Detect_Setup(J__SENSOR) % &
&        N__Num_Aerosol_Chans(N__Num_Aerosol_Tests) )

   S__Aerosol_Detect_Setup(J__SENSOR) % N__Num_Aerosol_Chans(:) = &
&        N__Num_Aerosol_Chans(1:N__Num_Aerosol_Tests)

   ALLOCATE(S__Aerosol_Detect_Setup(J__SENSOR) % N__Aerosol_Chans &
&            (N__Num_Aerosol_Tests,MAXVAL(N__Num_Aerosol_Chans(:))))

   S__Aerosol_Detect_Setup(J__SENSOR) % N__Aerosol_Chans(:,:) = 0
   DO J = 1, N__Num_Aerosol_Tests
      S__Aerosol_Detect_Setup(J__SENSOR) % &
&          N__Aerosol_Chans(J,1:N__Num_Aerosol_Chans(J)) = &
&          N__Aerosol_Chans(J,1:N__Num_Aerosol_Chans(J))
   ENDDO

   S__Aerosol_Detect_Setup(J__SENSOR) % N__Mean_Aerosol_Chans = &
&          N__Mean_Aerosol_Chans

   ALLOCATE(S__Aerosol_Detect_Setup(J__SENSOR) % R__Aerosol_TBD &
&            (N__Num_Aerosol_Tests,MAXVAL(N__Num_Aerosol_Chans(:))))

   S__Aerosol_Detect_Setup(J__SENSOR) % R__Aerosol_TBD(:,:) = 0.0
   DO J = 1, N__Num_Aerosol_Tests
      S__Aerosol_Detect_Setup(J__SENSOR) % &
&          R__Aerosol_TBD(J,1:N__Num_Aerosol_Chans(J)) = &
&          R__Aerosol_TBD(J,1:N__Num_Aerosol_Chans(J))
   ENDDO

ENDDO SensorLoop

END SUBROUTINE Aerosol_Detect_Setup
