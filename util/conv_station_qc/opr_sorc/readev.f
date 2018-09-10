C-----------------------------------------------------------------------
C READ AND DISPLAY A PREPBUFR FILE ONE REPORT AT A TIME WITH EVENTS
C-----------------------------------------------------------------------
      PROGRAM READEVR

      PARAMETER (MVR=10)
      PARAMETER (MLV=255)
      PARAMETER (MEV=10)
      PARAMETER (MTB=120000)

      COMMON /EVENT/ EVNTYP(0:MEV)
!     COMMON /QUIET/ IPRT

      CHARACTER*50 TBSTR
      CHARACTER*8  SUBSET,EVNTYP,YOU,STA,CAB(MVR,MTB)
      REAL(8)      TAB(MVR,MTB)
      EQUIVALENCE  (TAB(1,1),CAB(1,1))
      LOGICAL      JSID

      DATA TBSTR /'SID XOB YOB DHR T29 ITP TYP IREC ISUB   '/
      DATA BMISS /10E10/
      DATA LUBFR /8    /
      DATA LUPRT /6    /

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      EVNTYP(0) = 'UNKNOWN'

C  TABLE THE SUBSET HEADERS
C  ------------------------

      CALL UFBTAB(LUBFR,TAB,MVR,MTB,NTAB,TBSTR)

C  OPEN THE PREPBUFR FILE AND REPORT THE FILE DATE
C  -----------------------------------------------

1     CALL OPENBF(LUBFR,'IN',LUBFR)
2     CALL READMG(LUBFR,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) STOP 'NO DATA IN FILE'
      IF(SUBSET.NE.'ADPUPA') GOTO 2
      PRINT*,'DATA VALID FOR ',IDATE
      ITAB = 0

C  MAKE A LIST OF EVENT TYPES
C  --------------------------

      DO I=1,MEV
      CALL UFBQCP(LUBFR,FLOAT(I),EVNTYP(I))
      ENDDO

C  RECIEVE INTERACTIVE INSTRUTIONS FROM THIS POINT ON
C  --------------------------------------------------

10    PRINT*,'OK'
      YOU = ' '
      READ(5,'(A8)') YOU
      CALL CAPIT(YOU)
      IF(YOU.EQ.'Q') STOP
      IF(YOU.NE.'=') STA = YOU

      DO I=1,8
      IF(STA(I:I).NE.' ') NC = I
      ENDDO

C  EITHER POSITION AT A SPECIFIED SUBSET OR AT THE NEXT SEQUENTIAL ONE
C  -------------------------------------------------------------------

      IF(STA.NE.' ') THEN
         CALL UFBCNT(LUBFR,IREC,ISUB)
         DO I=ITAB+1,NTAB
         JREC = TAB(8,I)
         JSUB = TAB(9,I)
         JSID = STA(1:NC) .EQ. CAB(1,I)(1:NC)
         IF(JSID) CALL UFBPOS(LUBFR,JREC,JSUB,SUBSET,IDATE)
         IF(JSID) ITAB = I
         IF(JSID) GOTO 20
         ENDDO
         PRINT*,'STATION ',STA,' NOT FOUND'
         ITAB = 0
         GOTO 10
      ELSE
15       CALL READSB(LUBFR,IRET)
         IF(IRET.NE.0) THEN
            CALL READMG(LUBFR,SUBSET,IDATE,IRET)
            IF(IRET.EQ.0) GOTO 15
            GOTO 1
         ENDIF
      ENDIF

C  PRINT AND EDIT THE REQUESTED STATION
C  ------------------------------------

20    CALL PRTREP(LUBFR,SUBSET,IDATE,LUPRT)
      GOTO 10

C  HERE WHEN ALL MESSAGES HAVE BEEN READ
C  -------------------------------------

100   STOP
      END
C-----------------------------------------------------------------------
C  SUBROUTINE CAPIT CAPITALIZES A STRING OF CHARACTERS
C-----------------------------------------------------------------------
      SUBROUTINE CAPIT(STR)

      CHARACTER*(*) STR
      CHARACTER*26  UPS,LOS

      DATA UPS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      data los /'abcdefghijklmnopqrstuvwxyz'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      N = LEN(STR)

      DO 20 I=1,N
      DO 10 J=1,26
      IF(STR(I:I).EQ.LOS(J:J)) THEN
         STR(I:I) = UPS(J:J)
         GOTO 20
      ENDIF
10    CONTINUE
20    CONTINUE

      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE PRTREP(LUBFR,SUBSET,IDATE,LU)

      PARAMETER (MVO=3)
      PARAMETER (MVR=10)
      PARAMETER (MLV=255)
      PARAMETER (MEV=10)
      PARAMETER (MTB=60000)

      COMMON /EVENT/ EVNTYP(0:MEV)

      CHARACTER*80 FMTS(0:4)
      CHARACTER*75 OUTL
      CHARACTER*50 HDSTR,OBSTR(2,4),GESTR(2),FMT0,FMTH,FMTV
      CHARACTER*25 OUT(3)
      CHARACTER*8  SUBSET,EVNTYP,SID
      REAL(8)      HDR(MVR),QMC(0:16)
      REAL(8)      EVR(MVO,MLV,MEV,4),CAT(MLV)
      REAL(8)      OBS(MVO,MLV,MEV,3),GES(MVO,MLV)
      LOGICAL      QOB(MVO,MLV,MEV)
      EQUIVALENCE  (OUT(1),OUTL)
      EQUIVALENCE  (HDR(1),SID )

      DATA HDSTR /'SID XOB YOB DHR ELV T29 ITP TYP PRG'/
      DATA OBSTR /'POB TOB ZOB' , 'POB UOB VOB',
     .            'PQM TQM ZQM' , 'PQM WQM WQM',
     .            'PPC TPC ZPC' , 'PPC WPC WPC',
     .            'PRC TRC ZRC' , 'PRC WRC WRC'/
      DATA GESTR /'NUL TFC ZFC' , 'NUL UFC VFC'/

!     DATA QMC /'0','1','2','3','4','5','6','7',
!    .          '8','9','A','B','C','D','E','F',
!    .          '*'/

      DATA FMTS/
     .'(80("-")/A8," LEVEL(",I3,") CATEGORY(",I2,")")                 ',
     .'PROGRAM PRESSURE|QM  TEMPTURE|QM|FORECAST  GOPOTENZ|QM|FORECAST',
     .'PROGRAM PRESSURE|QM  UCOMPONT|QM|FORECAST  VCOMPONT|QM|FORECAST',
     .'(F8.1,"|",I2)'                                                  ,
     .'(F8.1,"|",I2,"|",F8.1)'                                         /

      DATA BMISS /10E10/
      DATA BLANK /'   '/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  MOVE SUBSET CONTENTS INTO THIS PROGRAM
C  --------------------------------------

      CALL UFBINT(LUBFR,HDR,MVR,1,IRET,HDSTR)
      CALL UFBCNT(LUBFR,IREC,ISUB)
      XOB = HDR(2)
      YOB = HDR(3)
      JRT = HDR(6)
      JTP = HDR(7)
      JKX = HDR(8)*.01 
      CALL UFBINT(LUBFR,CAT,1,MLV,NLEV,'CAT')
      CALL UFBINT(LUBFR,GES,MVO,MLV,NLEV,GESTR(JKX))
      DO I=1,4
      CALL UFBEVN(LUBFR,EVR(1,1,1,I),MVO,MLV,MEV,NLV,OBSTR(JKX,I))
      IF(NLEV.NE.NLV) STOP 'NLEVS NOT EQUAL'
      ENDDO

C  PRINT A REPORT HEADING
C  ----------------------

      WRITE(LU,'(80("-")                      )')
      WRITE(LU,'("MESSAGE:  ",A8,2(2X,I4)     )') SUBSET,IREC,ISUB
      WRITE(LU,'("STATION:  ",A8,1X,2(F8.2,1X))') (HDR(I),I= 1,3)
      WRITE(LU,'("DATE/TIME:",I8,F8.2         )') IDATE,HDR(4)
      WRITE(LU,'("ELV:      ",F8.2            )') HDR(5)
      WRITE(LU,'("TYPE:     ",3(F8.0,1X)      )') (HDR(I),I= 6,8)
      WRITE(LU,'("SOURCE:   ",3A8             )') HDR(9)
      WRITE(LU,'("DATA:     "                 )')


C  PRINT EACH LEVEL WITH ALL EVENTS
C  --------------------------------

      DO J=1,NLEV
      ILEV = J
      ICAT = CAT(J)
      WRITE(LU,FMTS(   0)) SID,ILEV,ICAT
      WRITE(LU,'("'//FMTS(JKX)//'")')
      DO I=1,MVO
      DO K=1,MEV     
      OUTL = ' '
      obv = evr(i,j,k,1)
      iqm = evr(i,j,k,2)
      ipc = evr(i,j,k,3)
      if(ipc.eq.bmiss) ipc=0
      fst = ges(i,j)
      IF(I.EQ.1 .AND. OBV.LT.BMISS) WRITE(OUT(I),FMTS(3)) OBV,IQM
      IF(I.GT.1 .AND. OBV.LT.BMISS) WRITE(OUT(I),FMTS(4)) OBV,IQM,FST
      IF(OUTL.NE.' ') WRITE(LU,'(A8,A13,2A22)') EVNTYP(ipc),OUT
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END
