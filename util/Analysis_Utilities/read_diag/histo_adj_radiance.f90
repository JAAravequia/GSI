PROGRAM histo_adj_radiance
!
!#######################################################################
!
!  This program reads GSI diagnositic radiance file for a single member,
!  calculates a histogram matched radiance bias adjustment and outputs a new
!  diagnostic files.
!
! Perform histogram matching according to the method of
! Gonzales and Woods in Digital Image Processing, pp 94-102
!
!  It is based on read_diag_rad.f90.
!
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
  INTEGER        :: num_digit_in_filename
  CHARACTER(256) :: outfilename       ! file name saving results
  CHARACTER(256) :: cdffilename  
  NAMELIST /iosetup/ infilename, num_digit_in_filename, outfilename

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

! histo adjust variables
  REAL(r_single) ispline

  INTEGER(i_kind) :: maxobs, hh,ttt,nobs, maxh, qq, a, c,sss,lll
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_obs   !  ipchan+npred+1,nchanl
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_sim   !  ipchan+npred+1,nchanl 
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_adj
  INTEGER(i_kind), ALLOCATABLE, DIMENSION (:,:) :: tb_obs_histo
  INTEGER(i_kind), ALLOCATABLE, DIMENSION (:,:) :: tb_sim_histo
  INTEGER(i_kind), ALLOCATABLE, DIMENSION (:,:) :: tb_adj_histo 
  INTEGER(i_kind), ALLOCATABLE, DIMENSION (:,:) :: use_flag
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_obs_cpf
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_sim_cpf
  REAL(r_single), ALLOCATABLE, DIMENSION (:,:) :: tb_adj_cpf
  REAL(r_single), ALLOCATABLE, DIMENSION (:) :: temptb
  REAL(r_single), parameter :: minval = 1.0
  REAL(r_single), parameter :: maxval = 350.0 
  REAL(r_single), parameter :: binval = 0.5   
  INTEGER(i_kind), parameter :: sm_size = 350*2
  INTEGER(i_kind), parameter :: lg_size = sm_size*10
  
  REAL(r_single) :: fitprob_sm(sm_size) = (/(a, a=1,sm_size, 1)/)
  REAL(r_single) :: bins_sm(sm_size) = (/(a, a=int(minval),int(maxval)*2, 1)/)
  REAL(r_single) :: fitprob_lg(lg_size) = (/(a, a=1,lg_size, 1)/)
  REAL(r_single) :: bins_lg(lg_size) = (/(a, a=int(minval),int(maxval)*10*2, 1)/)
  REAL(r_single), dimension (lg_size,10) :: fitcpf1, fitcpf2, tb1, tb2
  REAL(r_single), dimension (sm_size) ::  b1, c1, d1, tempobscdf, tempsimcdf
  REAL(r_single), dimension (sm_size) ::  b2, c2, d2 
  INTEGER(i_kind),dimension (10) :: startbin, lastbin, num_adj, fitct, ngoodobs 
  REAL(r_single), dimension (10) :: const, coeff, corr, mean_obs, mean_sim, mean_adj, inbias, outbias
  INTEGER(i_kind), dimension (lg_size) :: diff
  
  INTEGER(i_kind), dimension (1) :: loc
  REAL(r_single) :: sumx, sumxx, sumxy, sumy, sumyy, b, m, r
 
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
!  Misc. variables
!
!-----------------------------------------------------------------------

  INTEGER :: inunt, inunt2
  INTEGER :: istatus, ios
  INTEGER :: unum, lenstr, ount, ount2
  LOGICAL :: iexist, record_continue

  CHARACTER(LEN=256) :: nlfile, infile
  CHARACTER(LEN=40)  :: fmtstr

  character(10) ::  cipe,cloop
  character(20) ::  this_instrument

  character ::  ch
  integer   :: ipe,i,j,k,n, nn, iloop, iinstrument
  integer   :: ic, iflg

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  istatus = 0

  WRITE(6,'(/9(/2x,a)/)')                                               &
   '###############################################################',   &
   '###############################################################',   &
   '###                                                         ###',   &
   '###             Welcome to HISTO_ADJ_RADIANCE               ###',   &
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
    nlfile = 'namelist.adj'
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

  infilename='./diag_rad_ges.mem'
  num_digit_in_filename = 2
  
  outfilename='./diag_rad_ges.adj'
  READ(unum,iosetup)

  IF (unum /= 5) CLOSE( unum )

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

  inunt = 20
  WRITE(infile,FMT=fmtstr) TRIM(infilename)

  OPEN (inunt,FILE=TRIM(infile),STATUS='OLD',FORM='UNFORMATTED', &
        ACCESS='SEQUENTIAL',IOSTAT=ios)
  IF(ios /= 0 ) THEN
    WRITE(*,*) ' Error: opening input file <', TRIM(infile),'>, ios = ', ios
      !STOP
    CONTINUE
  END IF



!-----------------------------------------------------------------------
!
! Open diag file
!
!-----------------------------------------------------------------------

!
!  header first
!
maxobs = 55000

READ(inunt, ERR=9999) isis,dplat,obstype,jiter,nchanl,           &
                         npred,idate,ireal,ipchan,iextra,jextra,   &
                         idiag, angord, iversion, inewpc


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
    ALLOCATE(diagbufchan(idiag,nchanl),          STAT = istatus)
    ALLOCATE(diagbufextra(iextra,jextra),        STAT = istatus)
    ALLOCATE(diagbuf(ireal),                     STAT = istatus)

    !ALLOCATE(mdiagbufchan(ipchan+npred+1,nchanl), STAT = istatus)
    ALLOCATE(mdiagbufchan(idiag,nchanl),          STAT = istatus)
    ALLOCATE(mdiagbufextra(iextra,jextra),        STAT = istatus)
    ALLOCATE(mdiagbuf(ireal),                     STAT = istatus)
    
    ALLOCATE(tb_obs(maxobs,nchanl),               STAT = istatus)   
    ALLOCATE(tb_sim(maxobs,nchanl),               STAT = istatus)  
    ALLOCATE(tb_adj(maxobs,nchanl),               STAT = istatus)      
    
    ALLOCATE(use_flag(maxobs,nchanl),             STAT = istatus) 
    ALLOCATE(temptb(maxobs),                      STAT = istatus) 
    
    DO i=1,nchanl
     READ(inunt, ERR=9999) freq4,pol4,wave4,varch4,tlap4,iuse_rad,nuchan,ich(i)
     WRITE(ount) freq4,pol4,wave4,varch4,tlap4,iuse_rad,nuchan,ich(i)
    END DO
END IF

!
! Now, loop over all availabe records 
record_continue = .TRUE.

! READ IN TB DATA FOR HISTO MATCHING
n=0
nobs = 0
ngoodobs(:) = 0
DO WHILE(record_continue)

    READ(inunt,ERR=9999,end=110) diagbuf,diagbufchan,diagbufextra
    DO i = 1, nchanl
      use_flag(n,i) = nint(diagbufchan(5,i))
      
      !CHECK IF USEFLAG IS TRUE (== 0)
      IF ( use_flag(n,i) == 0 ) THEN 
        tb_obs(n,i) = diagbufchan(1,i)
        tb_sim(n,i) = diagbufchan(3,i) + tb_obs(n,i)
        ngoodobs(i)=ngoodobs(i)+1
      ENDIF 
    END DO
    n=n+1
END DO
110  CONTINUE
record_continue = .FALSE.
CLOSE(inunt)

nobs = n   ! number of observations in file
print*, nobs, ngoodobs

ALLOCATE(tb_obs_histo(sm_size,nchanl),        STAT = istatus) 
ALLOCATE(tb_sim_histo(sm_size,nchanl),        STAT = istatus)
ALLOCATE(tb_adj_histo(sm_size,nchanl),        STAT = istatus)
ALLOCATE(tb_obs_cpf(sm_size,nchanl),          STAT = istatus) 
ALLOCATE(tb_sim_cpf(sm_size,nchanl),          STAT = istatus)
ALLOCATE(tb_adj_cpf(sm_size,nchanl),          STAT = istatus)

bins_sm(:) = bins_sm(:)*binval
bins_lg(:) = bins_lg(:)*binval
bins_lg(:) = bins_lg(:) / 10.0 + binval

DO hh=2, sm_size
  DO i = 1, nchanl
    tb_obs_histo(hh,i) = count( tb_obs(:,i) > bins_sm(hh-1) .and. tb_obs(:,i) <= bins_sm(hh))
    tb_sim_histo(hh,i) = count( tb_sim(:,i) > bins_sm(hh-1) .and. tb_sim(:,i) <= bins_sm(hh))  
  END DO
END DO


! Find a mapping from the input pixels to s.
DO hh=2, sm_size
  DO i = 1, nchanl
    tb_obs_cpf(hh,i) = tb_obs_histo(hh,i) + tb_obs_cpf(hh-1,i)
    tb_sim_cpf(hh,i) = tb_sim_histo(hh,i) + tb_sim_cpf(hh-1,i)
  END DO
END DO

DO i = 1, nchanl
 tb_obs_cpf(:,i) = tb_obs_cpf(:,i)/float(ngoodobs(i))
 tb_sim_cpf(:,i) = tb_sim_cpf(:,i)/float(ngoodobs(i))
END DO
tb_obs_cpf(1,:) = 0.0
tb_sim_cpf(1,:) = 0.0


! DO CUBLIC SPLINE FITTING TO CPFS
fitprob_sm(:) = (fitprob_sm(:))/float(sm_size)
fitprob_lg(:) = (fitprob_lg(:))/float(lg_size)


DO i = 1, nchanl
  b1(:) = 0
  c1(:) = 0
  d1(:) = 0 
  b2(:) = 0
  c2(:) = 0
  d2(:) = 0   
  
  tempobscdf(:) = tb_obs_cpf(:,i) 
  call spline(fitprob_sm(:),tempobscdf(:),b1,c1,d1,sm_size)
  tempsimcdf(:) = tb_sim_cpf(:,i)
  call spline(fitprob_sm(:),tempsimcdf(:),b2,c2,d2,sm_size)

  DO qq=1, lg_size
    !fitcpf1(qq,i)=ispline(fitprob_lg(qq),fitprob_sm(:),tempobscdf(:),b1,c1,d1,sm_size)
    !fitcpf2(qq,i)=ispline(fitprob_lg(qq),fitprob_sm(:),tempsimcdf(:),b2,c2,d2,sm_size)

    fitcpf1(qq,i)=ispline(fitprob_lg(qq),fitprob_sm(:),tempsimcdf(:),b1,c1,d1,sm_size)
    fitcpf2(qq,i)=ispline(fitprob_lg(qq),fitprob_sm(:),tempobscdf(:),b2,c2,d2,sm_size)


    IF (fitcpf1(qq,i) > 1.0) THEN
       fitcpf1(qq,i) = 1.0
    ENDIF
    IF (fitcpf2(qq,i) > 1.0) THEN
       fitcpf2(qq,i) = 1.0
    ENDIF  
    IF (fitcpf1(qq,i) < 0.0) THEN
       fitcpf1(qq,i) = 0.0
    ENDIF
    IF (fitcpf2(qq,i) < 0.0) THEN
       fitcpf2(qq,i) = 0.0
    ENDIF  

  ENDDO
END DO

tb1(:,:) = 0.0
tb2(:,:) = 0.0
fitct(:) = 0

DO qq=1, lg_size
  DO i = 1, nchanl

	 diff(:) = 0
	 WHERE  (nint(fitcpf1(:,i)*1500) == nint(fitcpf2(qq,i)*1500)) diff = 1
	 loc = maxloc(diff)
	 
	 IF ( sum(diff) > 0 .and. sum(diff) <= 2 ) THEN 
		tb1(qq,i)=bins_lg(qq)
		tb2(qq,i)=bins_lg(loc(1))
		num_adj(i) = num_adj(i)+1
		
		fitct(i) = fitct(i)+1
		
	 ENDIF
  ENDDO
ENDDO


!PERFORM REGRESSION 
DO i = 1, nchanl
  sumx=0
  sumxx=0
  sumxy=0
  sumy=0
  sumyy=0
  m=0
  b=0
  r=0
  
  sumx   = SUM(tb2(:,i))
  sumxx  = SUM(tb2(:,i)*tb2(:,i))
  sumxy  = SUM(tb2(:,i)*tb1(:,i))
  sumy   = SUM(tb1(:,i))
  sumyy  = SUM(tb1(:,i)*tb1(:,i))   
     
  IF ( fitct(i) > 10 ) THEN   
     
   m = (num_adj(i) * sumxy  -  sumx * sumy) / (num_adj(i) * sumxx - sumx**2)    ! compute slope
   b = (sumy * sumxx  -  sumx * sumxy) / (num_adj(i) * sumxx  -  sumx**2)       ! compute y-intercept
   r = (sumxy - sumx * sumy /num_adj(i)) /                                 &    ! compute correlation coefficient
        sqrt((sumxx - sumx**2/num_adj(i)) * (sumyy - sumy**2/num_adj(i)))
                     
   tb_adj(1:nobs,i) = b + tb_sim(1:nobs,i)*m
   !tb_adj(1:nobs,i) = b + tb_obs(1:nobs,i)*m
  ELSE
    tb_adj(1:nobs,i) = tb_sim(1:nobs,i)
  ENDIF
  
  coeff(i) = m
  const(i) = b
  corr(i) = r
  
  WHERE  (use_flag(1:nobs,i) .ne. 0) tb_adj(1:nobs,i) = 0.0
ENDDO

!CALCULATE ADJUSTED HISTOS
DO hh=2, sm_size
  DO i = 1, nchanl
    tb_adj_histo(hh,i) = count( tb_adj(:,i) > bins_sm(hh-1) .and. tb_adj(:,i) <= bins_sm(hh))
  END DO
  print *, hh, bins_sm(hh), tb_obs_histo(hh,4),  tb_sim_histo(hh,4), tb_adj_histo(hh,4)
END DO
DO hh=2, sm_size
  DO i = 1, nchanl
      tb_adj_cpf(hh,i) = tb_adj_histo(hh,i) + tb_adj_cpf(hh-1,i)  
  ENDDO
ENDDO
DO i = 1, nchanl
  tb_adj_cpf(:,i) = tb_adj_cpf(:,i)/float(ngoodobs(i))
END DO
tb_adj_cpf(1,:) = 0.0

!PRINT FINAL CDFS TO FILE
cdffilename = 'ADJ_CDF_WV.txt'
ount2=17
OPEN(ount2, FILE=TRIM(cdffilename),STATUS='REPLACE',IOSTAT=ios)
 DO hh=2, sm_size
   WRITE(ount2,'(I7,11F10.3)'), hh, bins_sm(hh), tb_obs_cpf(hh,2:4), tb_sim_cpf(hh,2:4), tb_adj_cpf(hh,2:4)
 ENDDO
CLOSE (ount2)


!print *, tb_adj(:,4)
print *, ' '
print *, SUM(tb_obs_histo(:,4)), SUM(tb_sim_histo(:,4)),SUM(tb_adj_histo(:,4))
!print *, tb_obs_histo(1,4),  tb_sim_histo(1,4), tb_adj_histo(1,4)



! CALCULATE MEANS
DO i = 1, nchanl
 mean_obs(i) = SUM(tb_obs(1:nobs,i))
 mean_sim(i) = SUM(tb_sim(1:nobs,i))
 mean_adj(i) = SUM(tb_adj(1:nobs,i)) 
ENDDO
mean_obs(:) = mean_obs(:)/float(ngoodobs(:))
mean_sim(:) = mean_sim(:)/float(ngoodobs(:))
mean_adj(:) = mean_adj(:)/float(ngoodobs(:))

inbias(:) = mean_obs(:) - mean_sim(:)
outbias(:) = mean_obs(:) - mean_adj(:)

! PRINT SOME THINGS
print *, ' '
print '(A11, 10I9)', 'FIT CT: ', fitct(:)
print '(A11, 10F9.2)', 'MEAN OBS: ', mean_obs(:)
print '(A11, 10F9.2)', 'MEAN SIM: ', mean_sim(:)
print '(A11, 10F9.2)', 'MEAN ADJ: ', mean_adj(:)
print *, ' '
print '(A11, 10F9.2)', 'IN BIAS:  ', inbias(:)
print '(A11, 10F9.2)', 'ADJ BIAS: ', outbias(:)
print *, ' '
print '(A11, 10F9.3)', 'CONST: ', const(:)
print '(A11, 10F9.3)', 'COEFF: ', coeff(:)
print '(A11, 10F9.3)', 'CORR : ', corr(:)
print *, ' '


! WRITE NEW FILE CONTAINING HISTO MATCHED FILE
inunt2 = 30
OPEN (inunt2,FILE=TRIM(infile),STATUS='OLD',FORM='UNFORMATTED',    &
       ACCESS='SEQUENTIAL',IOSTAT=ios)
IF(ios /= 0 ) THEN
    WRITE(*,*) ' Error: opening input file again <', TRIM(infile),'>, ios = ', ios
      !STOP
    CONTINUE
END IF

READ(inunt2, ERR=9999) isis,dplat,obstype,jiter,nchanl,           &
                         npred,idate,ireal,ipchan,iextra,jextra,   &
                         idiag, angord, iversion, inewpc
                         
IF (nchanl > 0) THEN
    DO i=1,nchanl
     READ(inunt2, ERR=9999) freq4,pol4,wave4,varch4,tlap4,iuse_rad,nuchan,ich(i)
    END DO
END IF

DO n=1, nobs

    READ(inunt2,ERR=9999) mdiagbuf,mdiagbufchan,mdiagbufextra
	!REPLACE ADJUSTED TB DATA (only if enough obs to generate good adjustment)
	!**** ONLY ADJUST IF |outbias|	<  |inbias|
	DO i = 1, nchanl
	  IF (ngoodobs(i) > 100 .and. abs(outbias(i)) < abs(inbias(i)) .and. fitct(i) > 10 .and. use_flag(n,i) == 0) THEN
	    mdiagbufchan(2,i) = tb_obs(n,i) - tb_adj(n,i)  
           !IF (i == 2 ) THEN
           ! print*, tb_obs(n,i), tb_obs(n,i) - tb_sim(n,i), tb_obs(n,i) - tb_adj(n,i)
           !ENDIF
	  ENDIF
	END DO

	WRITE(ount) mdiagbuf,mdiagbufchan,mdiagbufextra
ENDDO

DO i = 1, nchanl
 IF (ngoodobs(i) <= 100 .or. abs(outbias(i)) >= abs(inbias(i)) .or. fitct(i) <= 10  ) THEN
	print*, '*** WARNING: NO BIAS ADJUSTMENT APPLIED', i, fitct(i), abs(outbias(i)) - abs(inbias(i))
 ENDIF
ENDDO

!-----------------------------------------------------------------------
!
! Close opened files
!
!-----------------------------------------------------------------------

  CLOSE(inunt2)
  CLOSE(ount)

  DEALLOCATE(diagbufchan,diagbufextra,diagbuf)
  DEALLOCATE(mdiagbufchan,mdiagbufextra,mdiagbuf)
  DEALLOCATE(ich, temptb)
  DEALLOCATE(tb_obs,tb_sim,tb_adj, use_flag)
  DEALLOCATE(tb_obs_histo,tb_sim_histo,tb_adj_histo)
  

!-----------------------------------------------------------------------
!
! Terminate the program nicely
!
!-----------------------------------------------------------------------

  WRITE(6,'(/,a)') ' ==== Normal successful completion of HISTO_ADJ_RADIANCE. ===='
  STOP

  9999    PRINT *,'error read in diag file'
  WRITE(6,'(/,a)') ' **** Program HISTO_ADJ_RADIANCE terminated with error. ****'
  STOP

END PROGRAM histo_adj_radiance


subroutine spline (x, y, b, c, d, n)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
!======================================================================
USE kinds, only: r_single

implicit none
integer n
REAL(r_single) x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
REAL(r_single) h

gap = n-1
! check input
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
!print*, 'GAP', gap
!print*, 'X', x(1:10)
!print*, 'Y', y(1:10)

!
! step 1: preparation
!
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)

!print*,'D1', d(1), x(2), x(1)
!print*,'C2', c(2)

do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do

!
! step 2: end conditions 
!
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
!
! step 3: forward elimination 
!
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
!
! step 4: back substitution
!
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
!
! step 5: compute spline coefficients
!
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)

end subroutine spline


function ispline(u, x, y, b, c, d, n)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================

USE kinds, only: r_single

implicit none
REAL(r_single) ispline
integer n
REAL(r_single)  u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
REAL(r_single) dx

! if u is ouside the x() interval take a boundary value (left or right)


if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if

!print*, 'X1, Xn, U :', x(1), x(n), u

!*
!  binary search for for i, such that x(i) <= u <= x(i+1)
!*
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
!*
!  evaluate spline interpolation
!*
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline 
