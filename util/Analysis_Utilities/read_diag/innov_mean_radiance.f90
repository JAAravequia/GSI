PROGRAM innov_mean_radiance
!
!#######################################################################
!
!  This program read GSI diagnositic file for a set of ensemble member,
!  calculate the mean innovation and output the mean in the same format
!  as original diagnostic files.
!
!  It is based on read_diag_rad.f90.

!  It is to read GSI radiance diagnositic data written from
!  subroutine setuprad.f90
!
!-----------------------------------------------------------------------
!
!  Here is code in setuprad.f90 that write out diagnostic information
!
!       write(14) isis,dplat(is),obstype,jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra
!        do i=1,nchanl
!           write(14)freq4,pol4,wave4,varch4,tlap4,iuse_rad(n),nuchan(n),ich(i)
!        end do
!
!       if (.not.lextra) then
!          write(14) diagbuf,diagbufchan
!       else
!          write(14) diagbuf,diagbufchan,diagbufex
!       endif
!
!       diagbuf(1)  = cenlat                         ! observation latitude (degrees)
!       diagbuf(2)  = cenlon                         ! observation longitude (degrees)
!       diagbuf(3)  = zsges                          ! model (guess) elevation at observation location
!
!       diagbuf(4)  = dtime                          ! observation time (hours relative to analysis time)
!
!       diagbuf(5)  = data_s(iscan_pos,n)            ! sensor scan position
!       diagbuf(6)  = zasat*rad2deg                  ! satellite zenith angle (degrees)
!       diagbuf(7)  = data_s(ilazi_ang,n)            ! satellite azimuth angle (degrees)
!       diagbuf(8)  = pangs                          ! solar zenith angle (degrees)
!       diagbuf(9)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
!       diagbuf(10) = sgagl                          ! sun glint angle (degrees) (sgagl)
!
!       diagbuf(11) = surface(1)%water_coverage         ! fractional coverage by water
!       diagbuf(12) = surface(1)%land_coverage          ! fractional coverage by land
!       diagbuf(13) = surface(1)%ice_coverage           ! fractional coverage by ice
!       diagbuf(14) = surface(1)%snow_coverage          ! fractional coverage by snow
!       diagbuf(15) = surface(1)%water_temperature      ! surface temperature over water (K)
!       diagbuf(16) = surface(1)%land_temperature       ! surface temperature over land (K)
!       diagbuf(17) = surface(1)%ice_temperature        ! surface temperature over ice (K)
!       diagbuf(18) = surface(1)%snow_temperature       ! surface temperature over snow (K)
!       diagbuf(19) = surface(1)%soil_temperature       ! soil temperature (K)
!       diagbuf(20) = surface(1)%soil_moisture_content  ! soil moisture
!       diagbuf(21) = surface(1)%land_type              ! surface land type
!       diagbuf(22) = surface(1)%vegetation_fraction    ! vegetation fraction
!       diagbuf(23) = surface(1)%snow_depth             ! snow depth
!       diagbuf(24) = surface(1)%wind_speed             ! surface wind speed (m/s)
!
!       if (.not.microwave) then
!          diagbuf(25)  = cld                        ! cloud fraction (%)
!          diagbuf(26)  = cldp                       ! cloud top pressure (hPa)
!       else
!          diagbuf(25)  = clw                        ! cloud liquid water (kg/m**2)
!          diagbuf(26)  = tpwc                       ! total column precip. water (km/m**2)
!       endif
!
!       do i=1,nchanl
!          diagbufchan(1,i)=tb_obs(i)       ! observed brightness temperature (K)
!          diagbufchan(2,i)=tbc(i)          ! observed - simulated Tb with bias corrrection (K)
!          diagbufchan(3,i)=tbcnob(i)       ! observed - simulated Tb with no bias correction (K)
!          errinv = sqrt(varinv(i))
!          diagbufchan(4,i)=errinv          ! inverse observation error
!          useflag=one
!          if (iuse_rad(ich(i))/=1) useflag=-one
!          diagbufchan(5,i)= id_qc(i)*useflag! quality control mark or event indicator
!
!          diagbufchan(6,i)=emissivity(i)   ! surface emissivity
!          diagbufchan(7,i)=tlapchn(i)      ! stability index
!          do j=1,npred+1
!             diagbufchan(7+j,i)=predterms(j,i) ! Tb bias correction terms (K)
!          end do
!       end do
!
!
!#######################################################################
!
! AUTHOR: Y. Wang (09/12/2016)
! Initial version based on innov_mean_conv.f90.
!
! MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------

!
  USE kinds, only: r_kind,r_single,i_kind
!
  IMPLICIT NONE

!
!  Namelist files
!
  CHARACTER(256) :: infilename        ! file from GSI running directory
  INTEGER        :: nmem,num_digit_in_filename
  CHARACTER(256) :: outfilename       ! file name saving results
  NAMELIST /iosetup/ nmem,infilename, num_digit_in_filename, outfilename

!
! read in variables
!
  CHARACTER(10) :: OBSTYPE
  CHARACTER(20) :: ISIS
  CHARACTER(10) :: dplat
  INTEGER(i_kind) :: jiter
  INTEGER(i_kind) :: nchanl
  INTEGER(i_kind) :: npred
  INTEGER(i_kind) :: idate
  INTEGER(i_kind) :: ireal, idiag, angord, iversion, inewpc
  INTEGER(i_kind) :: ipchan
  INTEGER(i_kind) :: iextra,jextra

  REAL(r_single) freq4,pol4,wave4,varch4,tlap4
  INTEGER(i_kind) :: iuse_rad
  INTEGER(i_kind) :: nuchan
  INTEGER(i_kind), ALLOCATABLE :: ich(:)   ! nchanl

  INTEGER(i_kind) :: ifirst

  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: diagbufchan   !  ipchan+npred+1,nchanl
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: diagbufextra     ! iextra,jextra
  REAL(r_single), ALLOCATABLE, DIMENSION (:)   :: diagbuf       ! ireal

!
! output variables
!
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: mdiagbufchan   !  ipchan+npred+1,nchanl
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: mdiagbufextra
  REAL(r_single), ALLOCATABLE, DIMENSION (:)   :: mdiagbuf       ! ireal
  real :: rlat,rlon,rprs
  real :: rdhr

!
! Dimension variables
!
  INTEGER(i_kind) :: nchanl1
  INTEGER(i_kind) :: npred1
  INTEGER(i_kind) :: idate1
  INTEGER(i_kind) :: ireal1
  INTEGER(i_kind) :: ipchan1


!-----------------------------------------------------------------------
!
!  Ensemble variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: nen
  INTEGER, ALLOCATABLE :: inunt(:)

!-----------------------------------------------------------------------
!
!  Misc. variables
!
!-----------------------------------------------------------------------

  INTEGER :: istatus, ios
  INTEGER :: unum, lenstr, ount
  LOGICAL :: iexist, record_continue

  CHARACTER(LEN=256) :: nlfile, infile
  CHARACTER(LEN=40)  :: fmtstr

  character(10) ::  cipe,cloop
  character(20) ::  this_instrument

  character ::  ch
  integer   :: ipe,i,j,k,n, iloop, iinstrument
  integer   :: ic, iflg

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  istatus = 0

  WRITE(6,'(/9(/2x,a)/)')                                               &
   '###############################################################',   &
   '###############################################################',   &
   '###                                                         ###',   &
   '###             Welcome to INNOV_MEAN_RADIANCE              ###',   &
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

  nmem = 35
  infilename='./diag_rad_ges.mem'
  num_digit_in_filename = 3
  outfilename='./diag_rad_ges.meam'
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
  ! Process header first
  !

  DO n = 1, nen

    READ(inunt(n), ERR=9999) isis,dplat,obstype,jiter,nchanl,           &
                              npred,idate,ireal,ipchan,iextra,jextra,   &
                              idiag, angord, iversion, inewpc
    IF (n == 1) THEN

      nchanl1 = nchanl
      ipchan1 = ipchan
      npred1  = npred
      ireal1  = ireal
      idate1  = idate

      WRITE(*,*) ' -',ipchan1,npred1,', ireal = ',ireal1,', nchanl = ',nchanl1

      WRITE(ount) isis,dplat,obstype,jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra,idiag, angord, iversion, inewpc

      IF (nchanl > 0) THEN
        ALLOCATE(ich(nchanl), STAT = istatus)

        !ALLOCATE(diagbufchan(ipchan+npred+1,nchanl), STAT = istatus)
        ALLOCATE(diagbufchan(idiag,nchanl), STAT = istatus)
        ALLOCATE(diagbufextra(iextra,jextra),        STAT = istatus)
        ALLOCATE(diagbuf(ireal),                     STAT = istatus)

        !ALLOCATE(mdiagbufchan(ipchan+npred+1,nchanl), STAT = istatus)
        ALLOCATE(mdiagbufchan(idiag,nchanl), STAT = istatus)
        ALLOCATE(mdiagbufextra(iextra,jextra),        STAT = istatus)
        ALLOCATE(mdiagbuf(ireal),                     STAT = istatus)
      END IF

    ELSE

      IF (nchanl /= nchanl1) THEN
        WRITE(*,*) 'ERROR: wrong nchanl for member: ',n,', expected: ', nchanl1, ', got: ',nchanl,'.'
        GOTO 9999
      END IF

      IF (ireal /= ireal1) THEN
        WRITE(*,*) 'ERROR: wrong ireal for member: ',n,', expected: ', ireal1, ', got: ',ireal,'.'
        GOTO 9999
      END IF

      IF (ipchan /= ipchan1) THEN
        WRITE(*,*) 'ERROR: wrong ipchan for member: ',n,', expected: ', ipchan1, ', got: ',ipchan,'.'
        GOTO 9999
      END IF

      IF (idate /= idate1) THEN
        WRITE(*,*) 'ERROR: wrong idate for member: ',n,', expected: ', idate1, ', got: ',idate,'.'
        GOTO 9999
      END IF

    END IF


    IF (nchanl > 0) THEN

      DO i=1,nchanl
         READ(inunt(n), ERR=9999) freq4,pol4,wave4,varch4,              &
                                          tlap4,iuse_rad,nuchan,ich(i)
         IF (n == 1) THEN
           WRITE(ount) freq4,pol4,wave4,varch4,tlap4,iuse_rad,nuchan,ich(i)
         END IF

      END DO
    END IF

  END DO

  !
  ! Now, loop over all availabe records for all members
  !
  record_continue = .TRUE.

  DO WHILE(record_continue)

    DO n = 1, nen

        READ(inunt(n),ERR=9999,end=110) diagbuf,diagbufchan,diagbufextra

        IF (n == 1) THEN
          mdiagbuf     = diagbuf
          mdiagbufchan = diagbufchan
          mdiagbufextra = diagbufextra
        ELSE
          DO i = 1, ireal
            IF (i <= 10) THEN       ! must match
               IF (diagbuf(i) /= mdiagbuf(i)) THEN
                 WRITE(*,'(1x,a,i0,a,i3.3,4a)')                            &
                   'ERROR: wrong diagbuf(',i,') for member: ',n,           &
                   ', expected: ', mdiagbuf(i), ', got: ',diagbuf(i),'.'
                 GOTO 9999
               END IF
            ELSE                    ! Accumulate for average
              mdiagbuf(i)=mdiagbuf(i)+diagbuf(i)
            END IF
          END DO

          DO i = 1, nchanl
            DO j = 1, idiag
            !DO j = 1,ipchan+npred+1
              !IF (j <= 3) THEN         ! must match
              !
              !  IF (diagbufchan(j,i) /= mdiagbufchan(j,i)) THEN
              !    WRITE(*,'(1x,2(a,I0),a,I3.3,2(a,f),a)')               &
              !      'ERROR: wrong diagbufchan(',j,',',i,') for member: ',n,&
              !      ', expected: ', mdiagbufchan(j,i), ', got: ',diagbufchan(j,i),'.'
              !    GOTO 9999
              !  END IF
              !ELSE                    ! Accumulate for average
                mdiagbufchan(j,i) = mdiagbufchan(j,i) + diagbufchan(j,i)
              !END IF
            END DO
          END DO
        
          ! EXTRA ARRAY: GOES IMAGER ONLY
          DO i = 1, jextra
            DO j = 1, iextra
                ! WRITE(*,*) j,i, diagbufextra(j,i), n
               mdiagbufextra(j,i) = mdiagbufextra(j,i) + diagbufextra(j,i)
            END DO
          END DO

        END IF

    END DO   ! ensemble members

    DO i = 11,ireal
      mdiagbuf(i)=mdiagbuf(i)/nen
    END DO

    DO i = 1, nchanl
      !DO j = 1, ipchan+npred+1
      DO j = 1, idiag
        mdiagbufchan(j,i) = mdiagbufchan(j,i)/nen
      END DO
    END DO
    ! EXTRA ARRAY
    DO i = 1, jextra
      DO j = 1, iextra
        mdiagbufextra(j,i) = mdiagbufextra(j,i)/nen
      ENDDO
    ENDDO

    WRITE(ount) mdiagbuf,mdiagbufchan,mdiagbufextra

    print*, n, mdiagbufextra(:,:)

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

  DEALLOCATE(diagbufchan,diagbufextra,diagbuf)
  DEALLOCATE(mdiagbufchan,mdiagbufextra,mdiagbuf)
  DEALLOCATE(ich)

!-----------------------------------------------------------------------
!
! Terminate the program nicely
!
!-----------------------------------------------------------------------

  WRITE(6,'(/,a)') ' ==== Normal successful completion of INNOV_MEAN_RADIANCE. ===='
  STOP

  9999    PRINT *,'error read in diag file, nen = ',nen
  WRITE(6,'(/,a)') ' **** Program INNOV_MEAN_RADIANCE terminated with error. ****'
  STOP

END PROGRAM innov_mean_radiance
