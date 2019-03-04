PROGRAM innov_mean_conv
!#######################################################################
!
!  This program read GSI diagnositic file for a set of ensemble member,
!  calculate the mean innovation and output the mean in the same format
!  as original diagnostic files.
!
!  It is based on read_diag_conv.f90.
!
!  for conventional data, which are
!  generated from subroutine:
!      setupps.f90
!      setupt.f90
!      setupq.f90
!      setuppw.f90
!      setupuv.f90
!      setupsst.f90
!      setupgps.f90
!
!  For example in setupt.f90:
!      the arrary contents disgnosis information is rdiagbuf.
!        cdiagbuf(ii)       ! station id
!        rdiagbuf(1,ii)     ! observation type
!        rdiagbuf(2,ii)     ! observation subtype
!        rdiagbuf(3,ii)     ! observation latitude (degrees)
!        rdiagbuf(4,ii)     ! observation longitude (degrees)
!        rdiagbuf(5,ii)     ! station elevation (meters)
!        rdiagbuf(6,ii)     ! observation pressure (hPa)
!        rdiagbuf(7,ii)     ! observation height (meters)
!        rdiagbuf(8,ii)     ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)     ! input prepbufr qc or event mark
!        rdiagbuf(10,ii)    ! setup qc or event mark (currently qtflg only)
!        rdiagbuf(11,ii)    ! read_prepbufr data usage flag
!        rdiagbuf(12,ii)    ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii)    ! nonlinear qc relative weight
!        rdiagbuf(14,ii)    ! prepbufr inverse obs error (K**-1)
!        rdiagbuf(15,ii)    ! read_prepbufr inverse obs error (K**-1)
!        rdiagbuf(16,ii)    ! final inverse observation error (K**-1)
!        rdiagbuf(17,ii)    ! temperature observation (K)
!        rdiagbuf(18,ii)    ! obs-ges used in analysis (K)
!        rdiagbuf(19,ii)    ! obs-ges w/o bias correction (K) (future slot)
!
!  It is written out as:
!     write(7)'  t',nchar,nreal,ii,mype
!     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
!
!#######################################################################
!
! AUTHOR: Y. Wang (07/07/2016)
! Initial version based on read_diag_conv.f90.
!
! MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------

  USE kinds, only: r_kind,r_single,i_kind

  IMPLICIT NONE

!
!  namelist files
!
  CHARACTER(256) :: infilename        ! file from GSI running directory
  INTEGER        :: nmem,num_digit_in_filename
  CHARACTER(256) :: outfilename       ! file name saving results
  NAMELIST /iosetup/ nmem,infilename, num_digit_in_filename, outfilename

  !INTEGER        :: MAX_NUM_VAR,MAX_NUM_OBS
  !NAMELIST /progsetup/ MAX_NUM_VAR,MAX_NUM_OBS
!
! read in variables
!
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)   :: cdiagbuf
  REAL(r_single),ALLOCATABLE, DIMENSION(:,:) :: rdiagbuf
  integer(i_kind)  :: nchar,nreal,ii,mype
  integer(i_kind)  :: idate
  CHARACTER(len=3) :: var
!
! output variables
!
  INTEGER(i_kind)  :: nreal1,ii1
  INTEGER(i_kind)  :: idate1
  CHARACTER(LEN=3) :: var1
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)   :: cmoutbuf
  REAL(r_single),ALLOCATABLE, DIMENSION(:,:) :: rmoutbuf
  REAL(r_single) :: ause

!-----------------------------------------------------------------------
!
!  ensemble variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: nen
  INTEGER, ALLOCATABLE :: inunt(:)

!-----------------------------------------------------------------------
!
!  misc.
!
!-----------------------------------------------------------------------
!
  INTEGER :: istatus, ios
  INTEGER :: unum, lenstr, ount
  LOGICAL :: iexist, record_continue

  CHARACTER(LEN=256) :: nlfile, infile
  CHARACTER(LEN=40)  :: fmtstr

  INTEGER :: i,j,n
  INTEGER :: var_count, num_diag
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  istatus = 0

  WRITE(6,'(/9(/2x,a)/)')                                               &
   '###############################################################',   &
   '###############################################################',   &
   '###                                                         ###',   &
   '###             Welcome to INNOV_MEAN_CONV                  ###',   &
   '###                                                         ###',   &
   '###############################################################',   &
   '###############################################################'

  unum = COMMAND_ARGUMENT_COUNT()
  IF (unum > 0) THEN
    CALL GET_COMMAND_ARGUMENT(1, nlfile, lenstr, istatus )
    IF ( nlfile(1:1) == ' ' .OR. istatus /= 0 ) THEN
      unum = 5
    ELSE
      INQUIRE(FILE=TRIM(nlfile),EXIST=iexist)
      IF (.NOT. iexist) THEN
        WRITE(6,'(1x,3a)') 'WARNING: namelist file - ',TRIM(nlfile),    &
              ' does not exist. Falling back to standard input.'
        unum = 5
      END IF

    END IF
  ELSE
    nlfile = 'namelist.innov'
    INQUIRE(FILE=TRIM(nlfile),EXIST=iexist)
    IF (.NOT. iexist) THEN
      WRITE(6,'(1x,3a)') 'WARNING: namelist file - ',TRIM(nlfile),  &
          ' does not exist. Falling back to standard input.'
      unum = 5
    END IF
  END IF

  IF (unum /= 5) THEN
    unum = 11
    OPEN(unum,FILE=TRIM(nlfile),STATUS='OLD',FORM='FORMATTED')
    WRITE(*,'(1x,3a,/,1x,a,/)') 'Reading namelist from file - ', &
            TRIM(nlfile),' ... ','========================================'
  ELSE
    WRITE(*,'(2(1x,a,/))') 'Waiting namelist from standard input ... ', &
                           '========================================'
  END IF

!-----------------------------------------------------------------------
!
! Read namelist
!
!-----------------------------------------------------------------------

  !MAX_NUM_VAR = 10
  !MAX_NUM_OBS = 300
  !READ(unum,progsetup)

  nmem = 35
  infilename='./diag_conv_ges.mem'
  num_digit_in_filename = 3
  outfilename='./diag_conv_ges.meam'
  READ(unum,iosetup)

  IF (unum /= 5) CLOSE( unum )

!-----------------------------------------------------------------------
!
! Allocate working arrays
!
!-----------------------------------------------------------------------

  ALLOCATE(inunt(nmem), STAT = istatus)

!-----------------------------------------------------------------------
!
! Open input file & output file
!
!-----------------------------------------------------------------------
  ount = 13
  OPEN(ount, FILE=TRIM(outfilename),STATUS='NEW',FORM='UNFORMATTED',    &
       ACCESS='SEQUENTIAL',IOSTAT=ios)
  IF(ios /= 0 ) then
    WRITE(*,*) ' cannot open file ', TRIM(outfilename), ' for writing.'
    STOP
  ELSE
    WRITE(*,*) ' output file <', TRIM(outfilename),'> opened.'
  END IF

  WRITE(fmtstr,'(a,I0,a,I0,a)') '(a,I',num_digit_in_filename,'.',num_digit_in_filename,')'

  nen = 1
  DO n = 1, nmem

    inunt(nen) = 20+nen
    WRITE(infile,FMT=fmtstr) TRIM(infilename),n

    OPEN (inunt(nen),FILE=TRIM(infile),STATUS='OLD',FORM='UNFORMATTED', &
          ACCESS='SEQUENTIAL',IOSTAT=ios)
    IF(ios /= 0 ) THEN
      WRITE(*,*) ' Error: opening input file <', TRIM(infile),'>, ios = ', ios
      !STOP
      CONTINUE
    END IF

    nen = nen + 1
  END DO
  nen = nen-1

  IF (nen /= nmem) THEN
    WRITE(*,'(1x,a,i0,a,i0,a)') ' WARNING: requested for ',nmem,' ensemble members, but find only ',nen,' valid files.'
  END IF
!-----------------------------------------------------------------------
!
! Member loop over records, assume observations are in the same order
!
!-----------------------------------------------------------------------

  !
  ! Process date first
  !
  DO n = 1, nen

    READ(inunt(n), ERR=9999) idate

    IF (n == 1) THEN
      idate1 = idate
      WRITE(*,*) ' process date: ',idate1

    ELSE
      IF (idate /= idate1) THEN
        WRITE(*,*) 'ERROR: wrong idate for member: ',n,', expected: ', idate1, ', got: ',idate,'.'
        GOTO 9999
      END IF

    END IF

  END DO   ! ensemble members

  WRITE(ount) idate1

  !
  ! Now, loop over all availabe records for all members
  !
  record_continue = .TRUE.

  var_count = 0
  DO WHILE(record_continue)

    DO n = 1, nen

      READ(inunt(n), ERR=9999,END=110) var, nchar,nreal,ii,mype
      !WRITE(*,*) ' -',n,var_count,'- ',var,nchar,', nreal = ',nreal,', ii = ',ii,mype

      IF (n == 1) THEN
        var1   = var
        nreal1 = nreal
        ii1    = ii
        var_count = var_count + 1
        WRITE(*,*) ' -',var_count,'- ',var1,nchar,', nreal = ',nreal1,', ii = ',ii1,mype

        IF (var .EQ. " uv") THEN  !    ** When the data is uv, additional output is needed **/
          num_diag = 21
        ELSE
          num_diag = 19
        END IF

        IF (ii1 > 0) THEN
          ALLOCATE(cdiagbuf(ii1),rdiagbuf(nreal1,ii1))
          ALLOCATE(cmoutbuf(ii1),rmoutbuf(nreal1,ii1))
        END IF
      ELSE
        IF (var /= var1) THEN
          WRITE(*,*) 'ERROR: wrong var for member: ',n,', expected: ', var1, ', got: ',var,'.'
          GOTO 9999
        END IF

        IF (nreal /= nreal1) THEN
          WRITE(*,*) 'ERROR: wrong nreal for member: ',n,', expected: ', nreal1, ', got: ',nreal,'.'
          GOTO 9999
        END IF

        IF (ii /= ii1) THEN
          WRITE(*,*) 'ERROR: wrong ii for member: ',n,', expected: ', ii1, ', got: ',ii,'.'
          GOTO 9999
        END IF

      END IF

      IF (ii1 > 0) THEN

        READ(inunt(n),ERR=9999,end=110) cdiagbuf, rdiagbuf

        IF (n == 1) THEN
          cmoutbuf = cdiagbuf
          rmoutbuf = rdiagbuf
        ELSE
          DO i = 1, ii
            !IF (cdiagbuf(i) /= cmoutbuf(i)) THEN
            !  WRITE(*,'(1x,a,i0,a,i3.3,4a)')                            &
            !    'ERROR: wrong cdiagbuf(',i,') for member: ',n,          &
            !    ', expected: ', cdiagbuf(i), ', got: ',cmoutbuf(i),'.'
            !  GOTO 9999
            !END IF

            DO j = 1, num_diag

              SELECT CASE (j)
              !CASE (1,2,4,5,7,8,9,10,11,13,14,15,17)    ! must match
              ! CASE (1,2,4,7,8,9,10,11,13,14,15,17)    ! must match
               CASE (1,2,4,7,8,9,10,11,13,17)    ! must match
                IF (rdiagbuf(j,i) /= rmoutbuf(j,i)) THEN
           !       WRITE(*,'(1x,2(a,I0),a,I3.3,2(a,f),a)')               &
           !         'ERROR: wrong rdiagbuf(',j,',',i,') for member: ',n,&
           !         ', expected: ', rdiagbuf(j,i), ', got: ',rmoutbuf(j,i),'.'
           !       GOTO 9999
                END IF

              CASE (12)                                  ! use maximum value
                ! 12 - analysis usage flag (1=use, -1=not used)
                IF (rdiagbuf(j,i) /= rmoutbuf(j,i)) THEN
                  !ause = MAX(rmoutbuf(j,i),rdiagbuf(j,i)) ! USE OB IF AT ONE OR MORE MEM IS  **NOT AN OUTLIER
                  ause = MIN(rmoutbuf(j,i),rdiagbuf(j,i))  ! DO NOT USE OB IF ONE OR MORE MEM **IS AN OUTLIER
                  WRITE(*,'(1x,a,f,2(a,i0),a,f,a,I3.3,a,/,9x,a,f,a)')   &
                    'WARNING: reset analysis usage flag, original = ',  &
                     rmoutbuf(j,i),', rdiagbuf(',j,',',i,') = ',         &
                     rdiagbuf(j,i),' for member: ',n, '.',               &
                     ' It will be set as ',ause ,'.'
                  rmoutbuf(j,i)=ause
                END IF

              CASE (6,16,18,19,20,21)                   ! Accumulate for average
                rmoutbuf(j,i)=rmoutbuf(j,i)+rdiagbuf(j,i)
              CASE DEFAULT                              ! ignore
                ! 3,
                ! Do nothing
              END SELECT

            END DO

          END DO   ! i  end for one station
        END IF

      ELSE
        READ(inunt(n))
      ENDIF
    END DO   ! ensemble members

    WRITE(ount) var1, nchar,nreal1,ii1,mype

    IF (ii1 > 0) THEN

      DO i = 1, ii1
        DO j = 1, num_diag
          SELECT CASE (j)
          CASE (6,16,18,19,20,21)
            rmoutbuf(j,i) =rmoutbuf(j,i)/nen
          CASE DEFAULT
            ! do nothing
          END SELECT
        END DO
      END DO

      WRITE(ount) cmoutbuf, rmoutbuf

      DEALLOCATE(cdiagbuf,rdiagbuf)
      DEALLOCATE(cmoutbuf,rmoutbuf)
    ELSE
      WRITE(ount)
    END IF

  END DO
  110  CONTINUE
  record_continue = .FALSE.

!-----------------------------------------------------------------------
!
! Close opened files
!
!-----------------------------------------------------------------------

  DO n = 1, nen
    CLOSE(inunt(n))
  END DO
  DEALLOCATE(inunt)

  CLOSE(ount)

!-----------------------------------------------------------------------
!
! Terminate the program nicely
!
!-----------------------------------------------------------------------

  WRITE(6,'(/,a)') ' ==== Normal successful completion of INNOV_MEAN_CONV. ===='
  STOP

  9999    PRINT *,'error read in diag file, nen = ',nen
  WRITE(6,'(/,a)') ' **** Program INNOV_MEAN_CONV terminated with error. ****'
  STOP

END PROGRAM innov_mean_conv
