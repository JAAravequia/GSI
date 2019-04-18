SUBROUTINE CLOUD_DETECT( &
&    K__MYPE,       &   !emily
&    K__SENSOR,     &
&    K__NCHANS,     &
&    K__CHANID,     &
&    P__MODELBTS,   &
&    P__OBSBTS,     &
&    P__CHAN_LEVEL, &
&    K__CLOUD_FLAG, &
&    K__MINLEV,     &
&    K__MAXLEV,     &
&    K__IMAGER_FLAG )

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2013, EUMETSAT, All Rights Reserved.


!*** *CLOUD_DETECT*
!        Phil Watts   ECMWF   21/01/02

!    PURPOSE
!    -------
!    Flag the presence or otherwise of cloud contamination in AIRS/IASI
!    channels using a rank-sorted/model difference method. Currently
!    only a digital filter is supported.

!**  INTERFACE
!    ---------
!**  *CALL* * CLOUD_DETECT( )* (FROM CLOUD_DETECT_WRAPPER)
!    WHERE K__SENSOR       : Satellite sensor (AIRS/IASI/CrIS)
!          K__NCHANS       : Number of channels
!          K__CHANID       : Channel indices of input channels
!          P__MODELBTS     : Clear background brightness temperatures (BTs)
!          P__OBSBTS       : Potentially cloud-affected observed BTs
!          P__CHAN_LEVEL   : Channel height assignments
!          K__CLOUD_FLAG   : Cloud flag by channel; 0=clear, 1=cloudy
!          K__MINLEV       : Highest allowed starting point for the cloud search
!          K__MAXLEV       : Lowest allowed starting point in the initial cloud
!                            search
!          K__IMAGER_FLAG  : Input cloud flag based on collocated imager data

!**  EXTERNALS
!    ---------
!    HEAPSORT, CF_DIGITAL

!    MODIFICATIONS
!    -------------
!    A.Collard   1.0   01/02/06   Original export version
!    A.Collard   1.0.1 03/05/06   Allow for missing channels
!    A.Collard   1.0.2 04/05/06   Allow cross-band cloud detection
!    A.Collard   1.0.3 15/01/07   Initialise with automatic cross-band for
!                                 all channels from band 1 for IASI
!    R.Eresmaa   1.1   17/11/09   Include parameters of the Quick Exit /
!                                 long-wave window gradient check.
!                                 Pass K__Chan_Low to CF_DIGITAL to allow
!                                 detecting cirrus in case of compensating
!                                 humidity bg error in PBL.
!    R.Eresmaa   1.2   11/11/11   Modify the cross-band option to be based
!                                 on the lowest clear channel rather than
!                                 on the highest cloud-contaminated one
!    R.Eresmaa   2.0   27/11/13   Add input cloud flag based on collocated
!                                 imager data
!    R.Eresmaa   2.1   13/01/15   Make array size specifications implicit.
!    R.Eresmaa   2.2   10/11/15   Instrument ID naming convention made
!                                 consistent with RTTOV.
!                                 Changed setting of the aerosol flag.
!    R.Eresmaa   2.2.1 13/11/15   Don't allow flagging missing channels clear
!                                 through the cross-band option.
!    R.Eresmaa   2.3   20/12/16   Remove the call to aerosol detection.


USE YOMIASI, ONLY : S__Cloud_Detect_Setup, &
&                   JP__Digital_Filter,    &
&                   INST_ID_IASI,          &
&                   INST_ID_CRIS     !emily
IMPLICIT NONE

!* 0.1 Global arrays
INTEGER,INTENT(IN)  :: K__MYPE                  ! MYPE   !emily
INTEGER,INTENT(IN)  :: K__NCHANS                ! No. of channels
INTEGER,INTENT(IN)  :: K__SENSOR                ! Sensor
INTEGER,INTENT(IN)  :: K__MINLEV                ! Highest allowed starting
                                                ! point for cloud search
INTEGER,INTENT(IN)  :: K__MAXLEV                ! Lowest allowed starting point
                                                ! in the initial cloud search
INTEGER,INTENT(IN)  :: K__CHANID(:)             ! Channel IDs
REAL,   INTENT(IN)  :: P__MODELBTS(:)           ! Model clear BTs
REAL,   INTENT(IN)  :: P__OBSBTS(:)             ! Observed BTs
REAL,   INTENT(IN)  :: P__CHAN_LEVEL(:)         ! Channel height assignments
INTEGER,INTENT(OUT) :: K__CLOUD_FLAG(:)         ! Output cloud flags
INTEGER,INTENT(IN)  :: K__IMAGER_FLAG           ! Input flag from imager data

!* 0.2 local variables
INTEGER             :: IST,ICOUNT,J,I_K,JBAND,JBAND2
INTEGER             :: I   !emily 

!* 0.3 Local variables - band splitting details
INTEGER,POINTER     :: I__Bands(:,:)            ! Channel detection bands
INTEGER,POINTER     :: I__Band_Size(:)          ! Number of chans per band
INTEGER,POINTER     :: I__BandToUse(:)          ! Cross band definitions
INTEGER             :: I__Num_Bands             ! Number of bands
INTEGER             :: I__NumFoundChans         ! Number of usable chans
INTEGER             :: I__BandNumber(K__NChans) ! Channel band indicator
INTEGER             :: I__WindowBounds(2)       ! Boundary of window
INTEGER             :: I__Window_Chans(2)       ! Boundary of longwave window
INTEGER,ALLOCATABLE :: I__INDEX(:)              ! Channel ranking within a band
INTEGER,ALLOCATABLE :: IDCHAN(:)                ! Overall channel ranking
INTEGER,ALLOCATABLE :: I__Cloud_Flag(:)         ! Rank-sorted output cloud flags

LOGICAL  :: LL__Do_CrossBand

REAL, ALLOCATABLE   :: Z__DBT(:),Z__LEVEL(:)

!* 0.4 Local variables - digital filter parameters
INTEGER             :: I__CHAN_HIGH             ! Channel at K__MINLEV
INTEGER             :: I__CHAN_LOW              ! Channel at K__MAXLEV
INTEGER             :: I__FirstCloudyChannel    ! Highest cloud-affected channel
INTEGER             :: I__LastClearChannel      ! Lowest clear channel
INTEGER             :: I__METHOD                ! Cloud detection method
INTEGER,POINTER     :: I__Window_Width(:)       ! Box-car filter width

REAL                :: Z__Cloud_Level           ! Pressure level of cloud

INCLUDE 'heapsort.intfb'
INCLUDE 'cf_digital.intfb'

!======================================================================

! Get correct processing parameters for this sensor:
I__METHOD         =  S__Cloud_Detect_Setup(K__SENSOR) % N__Filter_Method
I__Num_Bands      =  S__Cloud_Detect_Setup(K__SENSOR) % N__Num_Bands
I__Band_Size      => S__Cloud_Detect_Setup(K__SENSOR) % N__Band_Size
I__Bands          => S__Cloud_Detect_Setup(K__SENSOR) % N__Bands
I__Window_Width   => S__Cloud_Detect_Setup(K__SENSOR) % N__Window_Width
I__BandToUse      => S__Cloud_Detect_Setup(K__SENSOR) % N__BandToUse
LL__Do_CrossBand  =  S__Cloud_Detect_Setup(K__SENSOR) % L__Do_CrossBand

! Initialise
K__CLOUD_FLAG(:)=1       ! intialise ALL channels to cloudy

! If using cross-band, set up an array indicating which channels correspond
! to which bands in K__CHANID
IF (LL__Do_CrossBand) THEN
   write(K__MYPE+200000,*) 'LL__Do_CrossBand = ', LL__Do_CrossBand
   I__BandNumber(:)=-1  ! Initialise
   DO JBAND = 1, I__Num_Bands
      DO I_K=1,K__NCHANS
         IF (ANY(I__BANDS(:,JBAND) == K__CHANID(I_K))) &
&                I__BandNumber(I_K)=JBand
      ENDDO
   ENDDO
!>>emily
   DO I_K = 1, K__NCHANS
   WRITE (K__MYPE+200000,*) I_K, K__CHANID(I_K), I__BandNumber(I_K)
   ENDDO 
!<<emily
ENDIF


!1 Loop over bands
BAND_LOOP: DO JBAND = 1, I__Num_Bands         ! Loop over bands

  ! Don't bother doing the cloud detection if we're just going to use
  ! the results from another band anyway:
  IF (LL__Do_CrossBand) THEN
     IF (.NOT.(ANY(I__BandToUse(:) == JBAND))) CYCLE
  ENDIF

  ALLOCATE (Z__DBT(I__Band_Size(JBAND)))
  Z__DBT(:)   = 0.0

  ALLOCATE (Z__LEVEL(I__Band_Size(JBAND)))
  Z__LEVEL(:) = REAL(K__MAXLEV)

  ALLOCATE (I__Cloud_Flag(I__Band_Size(JBAND)))
  ALLOCATE (I__INDEX(I__Band_Size(JBAND)))

  ALLOCATE (IDCHAN(I__Band_Size(JBAND)))
  IDCHAN(:)   = 1

  I__WindowBounds(:)   = &
&      S__Cloud_Detect_Setup(K__SENSOR) % N__Window_Bounds(JBand,:)

!1.1 find channels within current band --------------------------------------
  I__NumFoundChans = 0
  I__Window_Chans(:) = -1

  DO J=1,I__Band_Size(JBAND)
    DO I_K=1,K__NCHANS
      IF (K__CHANID(I_K) == I__BANDS(J,JBAND)) THEN
    !   IF (P__OBSBTS(I_K) < 0. .OR. P__MODELBTS(I_K) < 0.) CYCLE      !orig
        IF (P__OBSBTS(I_K) <= 50. .OR. P__MODELBTS(I_K) <= 50.) CYCLE  !emily
        I__NumFoundChans = I__NumFoundChans + 1
        Z__DBT(I__NumFoundChans)=P__OBSBTS(I_K)-P__MODELBTS(I_K)
        Z__LEVEL(I__NumFoundChans)=P__CHAN_LEVEL(I_K)
        I__INDEX(I__NumFoundChans)=I__NumFoundChans
        IDCHAN(I__NumFoundChans)=I_K
        IF (K__CHANID(I_K) == I__WindowBounds(1)) &
&             I__Window_Chans(1) = I__NumFoundChans
        IF (K__CHANID(I_K) == I__WindowBounds(2)) &
&             I__Window_Chans(2) = I__NumFoundChans
      ENDIF
    ENDDO
  ENDDO
  IF ( I__NumFoundChans == 0 ) THEN
     WRITE(*,*) &
&          '**CLOUD_DETECT - WARNING: CHANNELS NOT FOUND CYCLING BAND: **', &
&          JBAND
     DEALLOCATE (Z__DBT,Z__LEVEL,I__Cloud_Flag,I__INDEX,IDCHAN)
     CYCLE BAND_LOOP
  ENDIF

!----------------------------------------------------------------------------
  IST=0
  ICOUNT=I__NumFoundChans
  I__Cloud_Flag(:)=1

!2. Sort according to channel height assignments
  CALL HEAPSORT(I__NumFoundChans,Z__LEVEL,I__INDEX)

!2.1 Find I__CHAN_LOW - lowest channel considered in the initial cloud search
  J=1
  DO WHILE (J < I__NumFoundChans .AND. Z__LEVEL(I__INDEX(J)) < REAL(K__MAXLEV))
    J=J+1
  ENDDO

  IF (J == I__NumFoundChans) THEN
    I__CHAN_LOW = I__NumFoundChans-1
  ELSE
    I__CHAN_LOW = J
  ENDIF
  IF(I__CHAN_LOW <= 1)I__CHAN_LOW=1

!2.1a Find I__CHAN_HIGH - highest allowed channel for starting the cloud search
  J=1
  DO WHILE (J < I__NumFoundChans .AND. Z__LEVEL(I__INDEX(J)) < REAL(K__MINLEV))
    J=J+1
  ENDDO
  I__CHAN_HIGH=J

!>>emily
  write(K__MYPE+200000,*)'before heapsort ...'
  write(K__MYPE+200000,*)'K__NCHANS        = ', K__NCHANS
  write(K__MYPE+200000,*)'K__MINLEV        = ', K__MINLEV
  write(K__MYPE+200000,*)'K__MAXLEV        = ', K__MAXLEV
  write(K__MYPE+200000,*)'I__NumFoundChans = ', I__NumFoundChans
  write(K__MYPE+200000,*)'I__CHAN_HIGH     = ', I__CHAN_HIGH
  write(K__MYPE+200000,*)'I__CHAN_LOW      = ', I__CHAN_LOW
  do i = 1, I__NumFoundChans 
     write(K__MYPE+200000,222) i, K__CHANID(IDCHAN(i)), Z__DBT(i), Z__LEVEL(i)
  enddo
222 format(i6,2x,i6,2x,2(f15.8,2x))
!<<emily

!>>emily
  write(K__MYPE+200000,*)'after heapsort ...'
  do i = 1, I__NumFoundChans 
     write(K__MYPE+200000,111) i, K__CHANID(IDCHAN(I__INDEX(i))), Z__DBT(I__INDEX(i)), Z__LEVEL(I__INDEX(i))
  enddo
111 format(i6,2x,i6,2x,2(f15.8,2x))
!<<emily



!3. Cloud search

  IF (I__METHOD == JP__Digital_Filter) THEN
    I__Cloud_Flag(:) = K__CLOUD_FLAG(IDCHAN(:))
    CALL CF_DIGITAL( &
&        K__MYPE,                      &  !emily
&        K__SENSOR,                    &
&        JBAND,                        &
&        I__NumFoundChans,             &
&        Z__DBT(1:I__NumFoundChans),   &
&        I__INDEX(1:I__NumFoundChans), &
&        I__CHAN_HIGH,                 &
&        I__CHAN_LOW,                  &
&        I__Window_Chans,              &
&        I__Window_Width(JBAND),       &
&        I__Cloud_Flag,                &
&        I__FirstCloudyChannel,        &
&        I__LastClearChannel,          &
&        K__IMAGER_FLAG)

    K__CLOUD_FLAG(IDCHAN(1:I__NumFoundChans)) = &
&                          I__Cloud_Flag(1:I__NumFoundChans)

    ! Set cloud level for cross-band:
    IF (I__FirstCloudyChannel == 0) THEN   ! FOV is completely clear
       Z__Cloud_Level = 1.e20   ! Large value
    ELSE
       Z__Cloud_Level = P__Chan_Level(IDCHAN(I__LastClearChannel))
    ENDIF

!>>emily
    write(K__MYPE+200000,*)'I__FirstCloudyChannel = ', I__FirstCloudyChannel, K__CHANID(IDCHAN(I__FirstCloudyChannel))
    write(K__MYPE+200000,*)'I__LastClearChannel   = ', I__LastClearChannel, K__CHANID(IDCHAN(I__LastClearChannel))
    write(K__MYPE+200000,*)'Z__Cloud_Level        = ', Z__Cloud_Level
    write(K__MYPE+200000,*)'LL__Do_CrossBand      = ', LL__Do_CrossBand 
!<<emily

    ! Automatically do cross band cloud detection for all channels
    ! (whether assigned a band or not) if JBand == 1.  This can be
    ! over-ridden for the other bands.

    IF (K__SENSOR == INST_ID_IASI .AND. JBand == 1) &
&        WHERE(P__Chan_Level(:) < Z__Cloud_Level) K__CLOUD_FLAG(:) = 0

    do i=1, K__NCHANS 
       write(K__MYPE+300000,888)  I, K__CHANID(i), I__BandNumber(i), P__CHAN_LEVEL(i), K__CLOUD_FLAG(i)
    enddo

    CrossBand : IF (LL__Do_CrossBand) THEN
       ! Cross Band:
       ! Loop through bands applying cloud detection to those that take their
       ! cloud detection information from the current band JBAND.
       write(K__MYPE+500000,*)'LL__Do_CrossBand = ', LL__Do_CrossBand
       DO JBand2 = 1, I__Num_Bands
          IF (I__BandToUse(JBand2) == JBand) THEN
             WHERE(P__Chan_Level(:) < Z__Cloud_Level .AND. &
&                    I__BandNumber == JBand2 .AND. &
&                    P__OBSBTs(:)>0.0 ) K__CLOUD_FLAG(:) = 0

!>>emily
             write(K__MYPE+500000,556) 'I','CHANID','JBAND2', 'JBAND', 'BandToUse', 'BandNumber', 'P__OBSBTS', 'Z__Cloud_Level', 'P__Chan_Level', 'K__CLOUD_FLAG'  
             DO i = 1, K__NCHANS           
             write(K__MYPE+500000,555) i, K__CHANID(i), JBAND2, JBAND, I__BandToUse(JBand2), I__BandNumber(i), P__OBSBTs(i), Z__Cloud_Level, P__Chan_Level(i), K__CLOUD_FLAG(i)  
             ENDDO
555          format(i6,2x,i10,2x,i10,2x,i10,2x,i10,2x,i10,2x,3(f15.8,2x),i15)
556          format(a6,2x,a10,2xa10,2x,a10,2x,a10,2x,a10,2x,3(a15,  2x),a15)
!<<emily


          ENDIF
       ENDDO
    ENDIF CrossBand

    write(K__MYPE+200000, * ) 'K__SENSOR    = ', K__SENSOR 
    write(K__MYPE+200000, * ) 'I__BandToUse = ', I__BandToUse(:) 
    write(K__MYPE+200000, * ) 'JBAND        = ', JBAND 
    write(K__MYPE+200000,333) K__CLOUD_FLAG(:)
333 format(10i3)
    do i=1, K__NCHANS 
       write(K__MYPE+200000,888)  I, K__CHANID(i), I__BandNumber(i), P__CHAN_LEVEL(i), K__CLOUD_FLAG(i)
    enddo
    do i=1, K__NCHANS 
       write(K__MYPE+400000,888)  I, K__CHANID(i), I__BandNumber(i), P__CHAN_LEVEL(i), K__CLOUD_FLAG(i)
    enddo
    write(K__MYPE+200000,*) 
    write(K__MYPE+300000,*) 
    write(K__MYPE+400000,*) 
888 format(i6,2x,i6,2x,i6,2x,f15.8,2x,i6) 

  ELSE
    ! No other methods implemented as yet
    WRITE(6,*) 'Only CF_DIGITAL (I__METHOD=1) is currently implemented'
  ENDIF

! Deallocate arrays
  DEALLOCATE (Z__DBT,Z__LEVEL,I__Cloud_Flag,I__INDEX,IDCHAN)

ENDDO BAND_LOOP                      ! Loop over band

! Nullify pointers
NULLIFY(I__Band_Size, I__Bands, I__Window_Width, I__BandToUse)

END SUBROUTINE CLOUD_DETECT
