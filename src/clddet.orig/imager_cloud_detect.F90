SUBROUTINE IMAGER_CLOUD_DETECT( &
&    K__Sensor,                 &
&    K__Nchans,                 &
&    K__Chanid,                 &
&    K__Nclust,                 &
&    K__Cloud_Flag,             &
&    P__Cl_Fraction,            &
&    P__Cl_Mean,                &
&    P__Ov_Stddev,              &
&    P__FG_BT )

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2015, EUMETSAT, All Rights Reserved.


!*** *IMAGER_CLOUD_DETECT*
!        R.Eresmaa   ECMWF   12/02/13


!    PURPOSE
!    -------
!    Provide additional information for the cloud detection by making use
!    of collocated imager data, such as AVHRR collocated with IASI.

!    INTERFACE.
!    ---------
!    *CALL* * IMAGER_CLOUD_DETECT( )* (FROM CLOUD_DETECT_WRAPPER)
!    WHERE K__Sensor      : Satellite sensor id
!          K__Nchans      : Number of channels received as input
!          K__Chanid      : Provided channel IDs
!          K__Nclust      : Highest possible number of clusters
!          K__Cloud_Flag  : Output cloud flag (0-7, 0=clear)
!          P__Cl_Fraction : Fractional coverage of each cluster within FOV
!          P__Cl_Mean     : Cluster-mean brightness temperature (BT) on each
!                           channel
!          P__Ov_Stddev   : Overall BT standard deviation on each channel
!          P__FG_BT       : Forward-modelled BT on each channel

!    METHOD
!    ------
!    A preliminary indicator of presence of clouds in the sounder
!    field-of-view (FOV) is derived using statistical radiance information
!    within collocated clusters of imager pixels.

!**  EXTERNALS.
!    ---------
!    None.

!**  MODIFICATIONS.
!    -------------
!    03/12/13   R.Eresmaa   2.0   Original export version.
!    19/01/15   R.Eresmaa   2.1   Make array size specifications implicit.
!                                 Verify that channels intended to be used
!                                 are received as input.


USE YOMIASI, ONLY : S__Cloud_Detect_Setup

IMPLICIT NONE

!* Global arrays
INTEGER,INTENT(IN)  :: K__Sensor            ! Sensor id
INTEGER,INTENT(IN)  :: K__Nchans            ! No. of channels
INTEGER,INTENT(IN)  :: K__Chanid(:)         ! Channel IDs
INTEGER,INTENT(IN)  :: K__Nclust            ! No. of clusters
INTEGER,INTENT(OUT) :: K__Cloud_Flag        ! Output imager cloud flag
REAL,   INTENT(IN)  :: P__Cl_Fraction(:)    ! Cluster fractions
REAL,   INTENT(IN)  :: P__Cl_Mean(:,:)      ! Cluster-mean BTs
REAL,   INTENT(IN)  :: P__Ov_Stddev(:)      ! Overall BT st. deviations
REAL,   INTENT(IN)  :: P__FG_BT(:)          ! First guess BT

!* Local variables - Setup of the imager cloud detection
INTEGER          :: I__Num_Imager_Chans       ! No. of used imager channels
INTEGER, POINTER :: I__Imager_Chans(:)        ! List of used imager channels
REAL, POINTER    :: Z__Stddev_Threshold(:)    ! Homogeneity thresholds
REAL             :: Z__Coverage_Threshold     ! Coverage threshold
REAL             :: Z__FG_Departure_Threshold ! FG departure threshold

!* Additional local variables
INTEGER :: I, J, IK, I_Temp_Flag, ICOUNT
INTEGER :: I__Chan_Index(K__Nchans)
REAL :: Z__Wsqdev, Z__Sqdev(K__Nclust), Z__Intercluster



!* 1.0 Initialize cloud flags as clear

K__Cloud_Flag=0

IF (S__Cloud_Detect_Setup(K__Sensor) % L__Do_Imager_Cloud_Detection) THEN


!* 1.1 Setup

  I__Num_Imager_Chans       = &
&     S__Cloud_Detect_Setup(K__Sensor) % N__Num_Imager_Chans
  I__Imager_Chans           => &
&     S__Cloud_Detect_Setup(K__Sensor) % N__Imager_Chans
  Z__Stddev_Threshold       => &
&     S__Cloud_Detect_Setup(K__Sensor) % R__Stddev_Threshold
  Z__Coverage_Threshold     = &
&     S__Cloud_Detect_Setup(K__Sensor) % R__Coverage_Threshold
  Z__FG_Departure_Threshold = &
&     S__Cloud_Detect_Setup(K__Sensor) % R__FG_Departure_Threshold


!* 1.2 Channel indexing
  I__Chan_Index(:) = 0
  ICOUNT=0
  DO I=1,K__Nchans
    IK=0
    DO J=1,I__Num_Imager_Chans
      IF (K__Chanid(I)==I__Imager_Chans(J)) THEN
        ICOUNT=ICOUNT+1
        IK=ICOUNT
        EXIT
      ENDIF
    ENDDO
    I__Chan_Index(I)=IK
  ENDDO


!* 2.0 Compute squared first guess departures for each cluster

  DO J=1,K__Nclust
    Z__Sqdev(J) = 0.0
    DO I=1,K__Nchans
      IF (I__Chan_Index(I)==0) CYCLE
      Z__Sqdev(J) = Z__Sqdev(J) + (P__Cl_Mean(I,J)-P__FG_BT(I))**2
    ENDDO
  ENDDO
 

!* 2.1 Homogeneity check: Do not diagnose presence of cloud if BT
!      standard deviation falls below given threshold on at least one
!      channel.

  I_Temp_Flag=1
  DO I=1,K__Nchans
    IF (I__Chan_Index(I)==0) CYCLE
    IF (P__Ov_Stddev(I)<Z__Stddev_Threshold(I__Chan_Index(I))) I_Temp_Flag=0
  ENDDO

  IF (I_Temp_Flag==1) K__Cloud_Flag=K__Cloud_Flag+4


!* 2.2 Consistency check: Do not diagnose presence of cloud if all
!      major clusters are consistent with each other, i.e., they are
!      closer to each other than to first guess.

  Consistency_Check : DO J=2,K__Nclust
    IF (P__Cl_Fraction(J)<Z__Coverage_Threshold) CYCLE Consistency_Check
    DO IK=1,J-1
      IF (P__Cl_Fraction(IK)<Z__Coverage_Threshold) CYCLE
      Z__Intercluster = 0.0
      DO I=1,K__Nchans
        IF (I__Chan_Index(I)==0) CYCLE
        Z__Intercluster = Z__Intercluster + &
&                        (P__Cl_Mean(I,J)-P__Cl_Mean(I,IK))**2
      ENDDO
      IF (Z__Intercluster>Z__Sqdev(J) .OR. Z__Intercluster>Z__Sqdev(IK)) THEN
        K__Cloud_Flag=K__Cloud_Flag+2
        Exit Consistency_Check
      ENDIF
    ENDDO
  ENDDO Consistency_Check


!* 2.3 First guess departure check: Do not diagnose presence of cloud
!      if fraction-weighted first guess departure falls below given
!      threshold.

  Z__Wsqdev = SUM(P__Cl_Fraction(:)*Z__Sqdev(:))
  IF (Z__Wsqdev>=Z__FG_Departure_Threshold) K__Cloud_Flag=K__Cloud_Flag+1

ENDIF   ! L__Do_Imager_Cloud_Detection

END SUBROUTINE IMAGER_CLOUD_DETECT
