MODULE YOMIASI

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2013, EUMETSAT, All Rights Reserved.


!**** Yomiasi
!       A. Collard  ECMWF 01/02/06

!    Purpose
!    -------
!    Set up structures to be used in processing of advanced IR sounders.

!    MODIFICATIONS
!    -------------
!    01/02/06   A.Collard   1.0   Original export version.
!    17/11/09   R.Eresmaa   1.1   Include parameters of the Quick Exit /
!                                 long-wave window gradient check.
!    11/11/11   R.Eresmaa   1.2   Add processing capability for CrIS.
!    03/12/13   R.Eresmaa   2.0   Add imager-assisted cloud detection.
!    10/11/15   R.Eresmaa   2.2   Changed instrument ID naming convention.
!                                 Changed aerosol detection parameters.
!    20/12/16   R.Eresmaa   2.3   Remove aerosol detection parameters.


IMPLICIT NONE

SAVE

INTEGER, Parameter :: INST_ID_AIRS = 11
INTEGER, Parameter :: INST_ID_IASI = 16
INTEGER, Parameter :: INST_ID_CRIS = 27
INTEGER, Parameter :: INST_ID_IRS = 57
INTEGER, Parameter :: INST_ID_IASING = 59

INTEGER, Parameter :: JP__Digital_Filter = 1

INTEGER, Parameter :: JP__Min_Sensor_Index = INST_ID_AIRS
INTEGER, Parameter :: JP__Max_Sensor_Index = INST_ID_IASING

TYPE Cloud_Detect_Type
   INTEGER         :: M__Sensor                 ! Unique ID for sensor
   INTEGER         :: N__Filter_Method          ! Averaging filter for
                                                ! cloud detection
   INTEGER         :: N__Num_Bands              ! Number of cloud detection
                                                ! bands
   INTEGER,POINTER :: N__GradChkInterval(:)     ! Window used in
                                                ! gradient calculation
   INTEGER,POINTER :: N__Band_Size(:)           ! No. of chans in each band
   INTEGER,POINTER :: N__Bands(:,:)             ! List of chans in bands
   INTEGER,POINTER :: N__Window_Width(:)        ! List of filter window
                                                ! widths per band
   INTEGER,POINTER :: N__Window_Bounds(:,:)     ! Channels to use for
                                                ! quick exit spectral
                                                ! gradient check
   INTEGER,POINTER :: N__BandToUse(:)           ! Which band to use for cloud
                                                ! height (used in cross-band)

   LOGICAL      :: L__Do_Quick_Exit             ! Allow quick exit
   LOGICAL      :: L__Do_CrossBand              ! Cross band cloud detection

   REAL,POINTER :: R__BT_Threshold(:)           ! BT threshold for cloud
                                                ! contamination
   REAL,POINTER :: R__Grad_Threshold(:)         ! Gradient threshold for
                                                ! cloud contamination
   REAL,POINTER :: R__Window_Grad_Threshold(:)  ! Window gradient threshold
                                                ! for quick exit test

   ! Imager cloud detection parameters

   LOGICAL :: L__Do_Imager_Cloud_Detection      ! Switch for the imager
                                                ! detection

   INTEGER         :: N__Num_Imager_Chans       ! No. of imager channels
   INTEGER         :: N__Num_Imager_Clusters    ! No. of clusters to be
                                                ! expected
   INTEGER,POINTER :: N__Imager_Chans(:)        ! List of used imager channels

   REAL,POINTER    :: R__Stddev_Threshold(:)    ! St. Dev. threshold, one for
                                                ! each imager channel
   REAL            :: R__Coverage_Threshold     ! Threshold for fractional
                                                ! coverage of a cluster
   REAL            :: R__FG_Departure_Threshold ! Threshold for imager first
                                                ! guess departure

END TYPE Cloud_Detect_Type

TYPE(Cloud_Detect_Type) :: &
&             S__Cloud_Detect_Setup(JP__Min_Sensor_Index:JP__Max_Sensor_Index)

END MODULE YOMIASI
