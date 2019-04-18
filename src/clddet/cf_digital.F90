SUBROUTINE CF_DIGITAL( &
&    K__MYPE,           &   !emily
&    K__Sensor,         &
&    K__Band,           &
&    K__NumChans,       &
&    P__DBT,            &
&    K__INDEX,          &
&    K__Chan_High,      &
&    K__Chan_Low,       &
&    K__Chan_Windows,   &
&    K__Window_Width,   &
&    K__Cloud_Flag,     &
&    K__Cloud_Level,    &
&    K__Clear_Level,    &
&    K__Imager_Flag)

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2013, EUMETSAT, All Rights Reserved.


!*** *CF_DIGITAL*
!        PHIL WATTS   ECMWF   21/01/02

!    PURPOSE
!    -------
!    FLAG THE PRESENCE OR OTHERWISE OF CLOUD CONTAMINATION IN AIRS
!    CHANNELS USING A RANK-SORTED/MODEL DIFFERENCE METHOD. METHODS
!    SUPPORTED ARE DIGITAL FILTER AND CHI-SQUARED FILTER.

!**  INTERFACE
!    ---------
!**  *CALL* * CF_DIGITAL( )* (FROM CLOUD_DETECT)
!    WHERE K__SENSOR       : Satellite sensor (AIRS/IASI/CrIS)
!          K__Band         : Band number
!          K__NumChans     : Number of channels in this band
!          P__DBT          : Input DBT signal
!          K__INDEX        : Ranking index for DBT
!          K__Chan_High    : High channel considered for initial minimum search
!          K__Chan_Low     : Low channel considered for initial minimum search
!          K__Chan_Windows : Two channels defining longwave window
!          K__Window_Width : Window width for filter
!          K__Cloud_Flag   : Cloud flag by channel; 0=clear, 1=cloudy
!          K__Cloud_Level  : Index of the highest cloud-contaminated channel
!          K__Clear_Level  : Index of the lowest clear channel
!          K__Imager_Flag  : Input flag from collocated imager data

!**  EXTERNALS
!    ---------
!    MOVINGA

!    MODIFICATIONS
!    03/02/06   A.Collard   1.0   Tidy up in preparation for IASI
!    03/05/06   A.Collard   1.0.1 Band size is now passed in (allows for
!                                 missing channels).
!    04/05/06   A.Collard   1.0.2 The index of the first cloudy channel is now
!                                 returned to allow cross-band cloud detection
!    16/02/07   A.Collard   1.0.3 Change to the padding to allow the bottom
!                                 channel to be flagged as clear in a
!                                 non-quickstart situation.
!    16/01/09   A.Collard   1.1   Gradient check on quick exit
!                                 Start channel for cold start moved to highest
!                                 channel where BT threshold exceeded
!    11/11/11   R.Eresmaa   1,2   Index of the lowest clear channel added to
!                                 the output parameters.
!                                 Change of the starting channel is no longer
!                                 allowed in cases where gradient > -threshold.
!    04/12/13   R.Eresmaa   2.0   Allow quick exit only if collocated imager
!                                 data supports hypothesis of a clear FOV
!    13/01/15   R.Eresmaa   2.1   Remove the need to create temporary array in
!                                 the call to MOVINGA.


USE YOMIASI, ONLY : S__Cloud_Detect_Setup

IMPLICIT NONE

!* 0.1 Global arrays
!- subroutine arguments
INTEGER, INTENT(IN) :: K__MYPE                ! MPI ID  !emily 
INTEGER, INTENT(IN) :: K__SENSOR              ! Sensor
INTEGER, INTENT(IN) :: K__Band                ! Band number
INTEGER, INTENT(IN) :: K__NumChans            ! Number of usable channels in
                                              ! band
REAL, INTENT(IN)    :: P__DBT(:)              ! Input ranked dBT signal
INTEGER, INTENT(IN) :: K__INDEX(:)            ! Ranking index for dBT
INTEGER, INTENT(IN) :: K__Window_Width        ! Smoothing width
INTEGER, INTENT(IN) :: K__Chan_High           ! First channel clear of high
                                              ! stratospheric model errors
INTEGER, INTENT(IN) :: K__Chan_Low            ! Last channel clear of boundary
                                              ! layer humidity errors
INTEGER, INTENT(IN) :: K__Chan_Windows(2)     ! Two channels defining bounds of
                                              ! the longwave window
INTEGER, INTENT(INOUT) :: K__Cloud_Flag(:)    ! Cloud flags
INTEGER, INTENT(OUT)   :: K__Cloud_Level      ! Index of highest cloudy channel
INTEGER, INTENT(OUT)   :: K__Clear_Level      ! Index of lowest clear channel
INTEGER, INTENT(IN)    :: K__Imager_Flag      ! Input imager cloud flag


! Local variables

REAL, ALLOCATABLE :: Z__DBT_Ranked(:)         ! Ranked DBT signal
REAL, ALLOCATABLE :: Z__DBT_Smoothed(:)       ! Smoothed-ranked DBT signal
                                              ! (extended to allow for
                                              ! detection algorithm).
INTEGER :: I__Buffer              ! Number of buffer channels
INTEGER :: I__Start_Channel       ! Primary starting channel for cloud search
INTEGER :: I__Start_Channel_Surf  ! Secondary starting channel for cloud search
INTEGER :: I__Max_Channel         ! Channel corresponding to the maximum of the
                                  ! smoothed dBT curve
INTEGER :: JCH,JMIN(1),JMAX(1),I

LOGICAL :: LLCOLD, LL__WINDOW_GRAD_CHECK, LL__StartChannelChanged
LOGICAL :: LL__Search_for_Cloud_Top

! These carry the values in S__Cloud_Detect_Setup
INTEGER :: I__GradChkInterval     ! Used in calculating the gradient

REAL  :: Z__BT_Threshold          ! Solution contaminated threshold
REAL  :: Z__Grad_Threshold        ! Gradient threshold at which
                                  ! to stop filter procession
REAL  :: Z__Window_Grad_Threshold ! Gradient threshold for window
                                  ! check

INCLUDE 'movinga.intfb'

!=============================================================================


I__GradChkInterval = &
&    S__Cloud_Detect_Setup(K__SENSOR) % N__GradChkInterval(K__Band)
Z__BT_Threshold    = &
&    S__Cloud_Detect_Setup(K__SENSOR) % R__BT_Threshold(K__Band)
Z__Grad_Threshold =  &
&    S__Cloud_Detect_Setup(K__SENSOR) % R__Grad_Threshold(K__Band)
Z__Window_Grad_Threshold = &
&    S__Cloud_Detect_Setup(K__SENSOR) % R__Window_Grad_Threshold(K__Band)

K__Cloud_Flag(:)=1


!1. Smooth with boxcar (Moving Average) filter and pad averaged array

I__BUFFER = I__GradChkInterval
ALLOCATE(Z__DBT_Ranked(K__NumChans))
ALLOCATE(Z__DBT_Smoothed(-I__Buffer+1:K__NumChans+1))

Z__DBT_Ranked(:)=P__DBT(K__INDEX(:))

CALL MOVINGA(                &
&    Z__DBT_Ranked(:),       &
&    K__NumChans,            &
&    K__Window_Width,        &
&    Z__DBT_Smoothed(1:K__NumChans) )

Z__DBT_Smoothed(-I__BUFFER+1:0)  = Z__DBT_Smoothed(1)
Z__DBT_Smoothed(K__NumChans+1) = Z__DBT_Smoothed(K__NumChans)

!2.  Prepare for the cloud search

! First define a set of key channels

JMIN=MINLOC(Z__DBT_Smoothed(K__Chan_High:K__NumChans))
I__Start_Channel_Surf = K__Chan_High+JMIN(1)-1

JMIN=MINLOC(Z__DBT_Smoothed(K__Chan_High:K__Chan_Low))
I__Start_Channel = K__Chan_High+JMIN(1)-1

! Look for highest channel with DBT<-BT_Threshold and move I__Start_Channel
! there if higher than current I__Start_Channel:
JCH = I__Start_Channel
StartChanLoop : DO I=K__Chan_High,K__NumChans
   IF (Z__DBT_Smoothed(I) < -Z__BT_Threshold .OR. I == I__Start_Channel) THEN
      JCH = I
      Exit StartChanLoop
   ENDIF
ENDDO StartChanLoop
I__Start_Channel = JCH

! Do the same with I__Start_Channel_Surf
JCH = I__Start_Channel_Surf
StartChanLoop_Surf : DO I=K__Chan_High,K__NumChans
   IF (Z__DBT_Smoothed(I) < -Z__BT_Threshold .OR. I == I__Start_Channel_Surf) THEN
      JCH = I
      Exit StartChanLoop_Surf
   ENDIF
ENDDO StartChanLoop_Surf
I__Start_Channel_Surf = JCH

! Find the position of the equivalent maximum departure (for quick exit test)
JMAX=MAXLOC(Z__DBT_Smoothed(K__Chan_High:K__NumChans))
I__Max_Channel = K__Chan_High+JMAX(1)-1


! Long-wave window gradient check

LL__WINDOW_GRAD_CHECK=.TRUE.
IF (ALL(K__Chan_Windows > 0)) LL__WINDOW_GRAD_CHECK = &
&    (ABS(Z__DBT_Smoothed(K__INDEX(K__Chan_Windows(1))) - &
&    Z__DBT_Smoothed(K__INDEX(K__Chan_Windows(2)))) &
&    < Z__Window_Grad_Threshold)


! Choose scenario to be followed

LL__Search_for_Cloud_Top=.TRUE.
IF (ABS(Z__DBT_Smoothed(I__Start_Channel_Surf)) < Z__BT_Threshold .AND. &
&    ABS(Z__DBT_Smoothed(I__Start_Channel)) < Z__BT_Threshold .AND. &
&    ABS(Z__DBT_Smoothed(I__Max_Channel)) < Z__BT_Threshold .AND. &
&    ABS(Z__DBT_Smoothed(K__NumChans)) < Z__BT_Threshold .AND. &
&    LL__WINDOW_GRAD_CHECK .AND. &
&    K__Imager_Flag==0 .AND. &
&    S__Cloud_Detect_Setup(K__SENSOR) % L__Do_Quick_Exit) THEN
   !Quick exit
   K__Cloud_Flag(K__INDEX(1:K__NumChans)) = 0
   K__Cloud_Level = 0               ! Special Indicator of Clear FOV
   K__Clear_Level = 0
   LL__Search_for_Cloud_Top=.FALSE.
ELSEIF (ABS(Z__DBT_Smoothed(I__Start_Channel)) < Z__BT_Threshold .AND. &
&    Z__DBT_Smoothed(K__NumChans) > Z__BT_Threshold ) THEN
   !Warm cloud start at next-to-bottom channel (allowing one channel for
   !gradient calculations).
   LLCOLD = .FALSE.
   I__Start_Channel = K__NumChans-1
ELSEIF (Z__DBT_Smoothed(I__Start_Channel) < -Z__BT_Threshold ) THEN
   LLCOLD = .TRUE.
ELSEIF (Z__DBT_Smoothed(I__Start_Channel) > Z__BT_Threshold ) THEN
   LLCOLD = .FALSE.
ELSE
   LLCOLD = .TRUE.
ENDIF


IF (LL__Search_for_Cloud_Top) THEN  ! Either cold or warm start
                                    ! (but not quick exit)

  JCH=I__Start_Channel

! If the primary starting channel appears clear, start from the
! secondary starting channel, if it is lower. In that case also
! re-evaluate cold or warm start is more appropriate.

  IF (I__Start_Channel /= I__Start_Channel_Surf) THEN

    LL__StartChannelChanged  = .FALSE.
    IF (LLCOLD .AND. ( (Z__DBT_Smoothed(JCH-1)-Z__DBT_Smoothed(JCH+1)) < &
&       Z__Grad_Threshold .AND. &
&       Z__DBT_Smoothed(JCH-I__GradChkInterval)-Z__DBT_Smoothed(JCH+1) < &
&       Z__Grad_Threshold .AND. &
&       ABS(Z__DBT_Smoothed(JCH)) < Z__BT_Threshold)) THEN
      I__Start_Channel = I__Start_Channel_Surf
      LL__StartChannelChanged  = .TRUE.
    ENDIF

    IF (LL__StartChannelChanged) THEN

      IF (ABS(Z__DBT_Smoothed(I__Start_Channel)) < Z__BT_Threshold .AND. &
&             Z__DBT_Smoothed(K__NumChans) > Z__BT_Threshold ) THEN
        !Warm cloud start at next-to-bottom channel (allowing one channel for
        !gradient calculations).
        LLCOLD = .FALSE.
        I__Start_Channel = K__NumChans-1
      ELSEIF (Z__DBT_Smoothed(I__Start_Channel) < -Z__BT_Threshold ) THEN
        LLCOLD = .TRUE.
      ELSEIF (Z__DBT_Smoothed(I__Start_Channel) > Z__BT_Threshold ) THEN
        LLCOLD = .FALSE.
      ELSE
        LLCOLD = .TRUE.
      ENDIF
      JCH = I__Start_Channel

    ENDIF
  ENDIF


! 3 Search for the lowest non-contaminated channel

  IF (LLCOLD) THEN

! If Cold start, then Progress towards higher channels whilst -ve difference
! is decreasing

    DO WHILE (( (Z__DBT_Smoothed(JCH-1)-Z__DBT_Smoothed(JCH+1)) > &
&                Z__Grad_Threshold .OR. &
&      (Z__DBT_Smoothed(JCH-I__GradChkInterval)-Z__DBT_Smoothed(JCH+1)) > &
&                Z__Grad_Threshold .OR. &
&       ABS(Z__DBT_Smoothed(JCH)) > Z__BT_Threshold) .AND. JCH > 1 )
      JCH = JCH-1
    ENDDO
  ELSE

! If Warm start, then Progress towards higher channels whilst +ve difference
! is decreasing

    DO WHILE ( ((Z__DBT_Smoothed(JCH-1)-Z__DBT_Smoothed(JCH+1)) < &
&                -1.0*Z__Grad_Threshold .OR. &
&      (Z__DBT_Smoothed(JCH-I__GradChkInterval)-Z__DBT_Smoothed(JCH+1)) <  &
&                -1.0*Z__Grad_Threshold .OR. &
&         ABS(Z__DBT_Smoothed(JCH)) > Z__BT_Threshold) .AND. JCH > 1 )
      JCH = JCH-1
    ENDDO
  ENDIF
  K__Cloud_Level = JCH


  IF (K__Cloud_Level /= 1) K__Cloud_Flag(K__INDEX(1:K__Cloud_Level-1))=0

! Output channel numbers for the highest cloud and lowest clear levels
  IF (K__Cloud_Level>1) THEN
    K__Clear_Level=K__INDEX(K__Cloud_Level-1)
  ELSE
    K__Clear_Level=K__INDEX(K__Cloud_Level)
  ENDIF
  K__Cloud_Level=K__INDEX(K__Cloud_Level)

ENDIF    ! Search for cloud top

IF (ALLOCATED(Z__DBT_RANKED)) DEALLOCATE(Z__DBT_RANKED)
IF (ALLOCATED(Z__DBT_SMOOTHED)) DEALLOCATE(Z__DBT_SMOOTHED)

END SUBROUTINE CF_DIGITAL
