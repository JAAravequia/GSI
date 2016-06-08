      SUBROUTINE ANNATV(IUNIT,IM,JM,NC,IGRID,KPDS,KGDS,OPCP,MASK)
C
C   M BALDWIN 2/6/97
C     MODIFIED VERSION OF PROGRAM PCPANA
C     1.  READ IN RAINFALL DATA - DO QC
C     2.  DO A BOX AVERAGE ANALYSIS TO THE GRID DEFINED BY KGDS
C
C
!     PARAMETER (MXSIZE=150000,NPTS=20000)
!     PARAMETER (MXSIZE=1000000,NPTS=20000)
      PARAMETER (NPTS=20000)
      DIMENSION OPCP(IM*JM),MASK(IM*JM)
!     DIMENSION ICOUNT(MXSIZE),XPTS(NPTS),YPTS(NPTS)
      DIMENSION ICOUNT(IM*JM),XPTS(NPTS),YPTS(NPTS)
      DIMENSION SROT(NPTS),CROT(NPTS)
      DIMENSION OLAT(NPTS),OLON(NPTS),PCP(NPTS)
      INTEGER KPDS(NC),KGDS(NC),MNTH(12)
      CHARACTER*9  STATID, STID(NPTS)
      CHARACTER  fn2*80
      CHARACTER*3 month(12)
      CHARACTER END*5,DAYOW*5,STI*5
C
      data month/'jan','feb','mar','apr','may','jun','jul'
     &,'aug','sep','oct','nov','dec'/

      DATA MNTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
      IMJM=IM*JM
      RADDEG=3.14159265/180.0
C
      DO 100 J=1,IMJM
      ICOUNT(J) = 0
      MASK(J) = 0
      OPCP(J) = 0.0
  100 CONTINUE
C
C  THESE ARE THE GEODETIC COORDS TO DEFINE THE GRID, FROM KGDS
C
      IF (KGDS(1).EQ.0) THEN
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
        ALAT2=KGDS(7)*1.E-3
        ALON2=KGDS(8)*1.E-3
        DX=(ALON2-ALON1)/(IM-1)
        DY=(ALAT2-ALAT1)/(JM-1)
      ELSEIF (KGDS(1).EQ.1) THEN
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
        ALAT2=KGDS(7)*1.E-3
        ALON2=KGDS(8)*1.E-3
        ALATIN=KGDS(9)*1.E-3
        DX=KGDS(12)
      ELSEIF (KGDS(1).EQ.3) THEN
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
        ELONV=KGDS(7)*1.E-3
        DX=KGDS(8)
        ALATAN=KGDS(12)*1.E-3
      ELSEIF (KGDS(1).EQ.4) THEN
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
        ALAT2=KGDS(7)*1.E-3
        ALON2=KGDS(8)*1.E-3
        NLATC=KGDS(10)
      ELSEIF (KGDS(1).EQ.5) THEN
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
        ELONV=KGDS(7)*1.E-3
        DX=KGDS(8)
      ELSEIF (KGDS(1).EQ.201) THEN
        IF (IGRID.EQ.90) THEN
            CLAT0=52.
            CLON0=111.
            DLATD=14./26.
            DLOND=15./26.
        ELSEIF (IGRID.EQ.92) THEN
            CLAT0=50.
            CLON0=107.
            DLATD=8./39.
            DLOND=2./9.
        ELSEIF (IGRID.EQ.94) THEN
            CLAT0=41.
            CLON0=97.
            DLATD=5./27.
            DLOND=7./36.
        ELSEIF (IGRID.EQ.96) THEN
            CLAT0=50.
            CLON0=111.
            DLATD=5./16.
            DLOND=41./124.
        ENDIF
        WBD=-(IM-1)*DLOND
        SBD=-((JM-1)/2)*DLATD
        ALAT1=KGDS(4)*1.E-3
        ALON1=KGDS(5)*1.E-3
      ENDIF
C
C  READ PRECIP OBS
C
      K=0
      READ(IUNIT,7773) IHR,IYR,IMN,IDA
 330  READ(IUNIT,7778,END=1005,ERR=980) ALAT,ALON,PCPK,STI
        K=K+1
        STID(K)=STI
        OLAT(K)=ALAT
        OLON(K)=-ALON
        PCP(K)=PCPK*25.4 ! CHANGE UNITS TO MM
 980  CONTINUE
      GOTO 330
C7773 FORMAT(29X,I2,5X,I4,2I2)    !before an on 20111122
C7778 FORMAT(F6.2,F9.2,F7.2,1X,A8)!
 7773 FORMAT(28X,I2,5X,I4,2I2)    !since 20111123
 7778 FORMAT(F5.2,F7.2,F6.2,1X,A7)
 1005 PRINT 101,K
 101  FORMAT(1X,'FINISHED READING DATA',I8,' OBSERVATIONS')
      NOBSK=K
C
C  CALC AVERAGE AND STANDARD DEVIATION FOR RFC DATA
C   MAXIMUM ALLOWABLE OBSERVATION WILL BE AVG + 3*STDV
C   NEED TO CHANGE THIS MAYBE?
C
      SUMK=0.
      SSQSK=0.
      DO K=1,NOBSK
       FIPCP = PCP(K)
       SUMK = SUMK + FIPCP
       SSQSK =SSQSK + FIPCP ** 2
      ENDDO
       XN = FLOAT(NOBSK)
       IF (XN.GT.0.) THEN
        AVG = SUMK / XN
        ASUMSQ    = SSQSK / XN
        YX = ASUMSQ - AVG ** 2
        QCMAX = AVG + 6.* SQRT( YX )
       ENDIF
       WRITE(6,*) 'OBS DATA avg variance qcmax',avg,yx,qcmax
C
C        NOW READ THROUGH DATA AND TOSS OBS > AVG + 6 STD DEV
C        THEN ANALYZE
C
C
C    GET X,Y COORDS OF OBS LAT/LON
C
      DO I=1,NOBSK
       XPTS(i)=0.0
       YPTS(i)=0.0
       CROT(i)=0.0
       SROT(i)=0.0
      ENDDO
      NRET=0
      CALL GDSWIZ(KGDS,-1,NOBSK,-9999.,XPTS,YPTS,OLON,OLAT,NRET,
     &                  0,CROT,SROT)
C
C     SUBROUTINE GDSWIZ(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C    &                  LROT,CROT,SROT)
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                ( 0 TO COMPUTE EARTH COORDS OF ALL THE GRID POINTS)
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<=0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<=0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>=0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>=0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C                (-1 IF PROJECTION UNRECOGNIZED)
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
       DO K=1,NOBSK
        FIPCP = PCP(K)
        IF (FIPCP.GT.QCMAX) THEN
         WRITE(6,3301) STID(K),PCP(K),QCMAX
        ELSE
C
C       FIND KINDEX OF OBSERVATION
C
         KINDEX=0
         II = NINT(XPTS(K))
         JJ = NINT(YPTS(K))
          IF (II.GE.1.AND.II.LE.IM.AND.JJ.GE.1.AND.JJ.LE.JM) THEN
           KINDEX=(JJ-1)*IM+II
          ENDIF

c         IF (KGDS(1).EQ.201) THEN
c          ALN = -OLON(K)
c          APH = OLAT(K)
c          CALL ETA2IJ (APH,ALN,IM,CLON0,CLAT0,DLOND,DLATD,
c    &                  WBD,SBD,KINDEX)
c         ENDIF

C        WRITE (6,*) 'II = ',ii,' JJ = ',jj,' KINDEX = ',kindex
          IF (KINDEX.GT.0.AND.KINDEX.LE.IMJM) THEN
           OPCP(KINDEX)=OPCP(KINDEX)+FIPCP
           ICOUNT(KINDEX)=ICOUNT(KINDEX)+1
          ENDIF
         ENDIF
        ENDDO
 3301   FORMAT(' QC TOSS STATION ',A5,' AMOUNT ',F6.2,' QCMAX ',F8.2)
C
C   DIVIDE BY SUM OF WEIGHTS
C
        AMAX=0.0
        DO K=1,IMJM
         IF (ICOUNT(K).GT.0) THEN
          RCOUNT=1./FLOAT(ICOUNT(K))
          OPCP(K)=OPCP(K)*RCOUNT
          AMAX=AMAX1(OPCP(K),AMAX)
          MASK(K)=1
         ELSE
          MASK(K)=0
         ENDIF
        ENDDO
        WRITE(6,*) 'MAX ANALYSIS VALUE = ',AMAX
C
C     SET UP DATE INFO
C
        IF(MOD(IYR,4).EQ.0) THEN
             MNTH(2) = 29
        END IF
        IYR1=IYR
        IHR1=IHR
        IDA1=IDA-1
        IMN1=IMN
        IF (IHR1.LT.0) THEN
         IHR1=IHR1+24
         IDA1=IDA1-1
        ENDIF
        IF (IDA1.LE.0) THEN
         IF(IMN1.EQ.1) THEN
          IMN1 = 12
          IYR1 = IYR1 -1
         ELSE
          IMN1 = IMN1 -1
         ENDIF
         IDA1 = MNTH(IMN1) +  IDA1
        ENDIF
       ICENT=(IYR1-1)/100 + 1
C
C     SET UP PDS
C
       DO K=1,NC
        KPDS(K)=0.
       ENDDO
       KPDS(1)=7       ! ID OF CENTER
       KPDS(2)=154     ! GENERATING PROCESS I MADE UP
       KPDS(3)=IGRID   ! GRID NUMBER
       KPDS(4)=128+64  ! TABLE 1 FLAG 192 MEANS BMS IS HERE
       KPDS(5)=61      ! PARAMETER
       KPDS(6)=1       ! TYPE OF LEVEL
       KPDS(7)=0       ! VALUE OF LEVEL
       KPDS(8)=IYR1-(ICENT-1)*100    ! YEAR  (START TIME)
       KPDS(9)=IMN1    ! MONTH
       KPDS(10)=IDA1   ! DAY
       KPDS(11)=IHR1   ! HOUR
       KPDS(12)=0      ! MINUTE
       KPDS(13)=1      ! TIME UNIT
       KPDS(14)=0      ! P1
       KPDS(15)=24     ! P2
       KPDS(16)=4      ! TIME RANGE INDICATOR
       KPDS(17)=0      ! NO INCLUDED IN AVERAGE
       KPDS(18)=1      ! GRIB VERSION NO
       KPDS(19)=2      ! PARAMETER TABLE VERSION NO
       KPDS(20)=0      ! NO MISSING
       KPDS(21)=ICENT  ! CENTURY
       KPDS(22)=2      ! DECIMAL SCALE FACTOR
       KPDS(23)=4      ! SUB CENTER NUM
       KPDS(24)=0      ! ENSEMBLE STUFF
       KPDS(25)=0      ! RESERVED
C
      RETURN
      END

