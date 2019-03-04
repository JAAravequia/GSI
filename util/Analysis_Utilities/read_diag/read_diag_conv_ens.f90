PROGRAM read_diag_conv_ens
!
!  This program is to show how to 
!  read GSI diagnositic file for conventional data
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none
!
!  namelist files
!
  integer :: numens 
  character(180) :: infilenamebase    ! file base from GSI running directory
  character(180) :: outfilepath       ! file path saving results
  namelist/iosetup/ numens, infilenamebase, outfilepath
!
! files
!
  integer  :: iens
  character(180) :: infilename        ! file from GSI running directory
  integer :: iunitdiag
!
! observations 
!
  integer ::  numobs_t,numobs_ps,numobs_uv,numobs_q
  integer ::  nreal_t,nreal_ps,nreal_uv,nreal_q

  character(8),allocatable,dimension(:):: cstaid_t
  real(r_single),allocatable,dimension(:,:)::rdiagbuf_t
  character(8),allocatable,dimension(:):: cstaid_q
  real(r_single),allocatable,dimension(:,:)::rdiagbuf_q
  character(8),allocatable,dimension(:):: cstaid_ps
  real(r_single),allocatable,dimension(:,:)::rdiagbuf_ps
  character(8),allocatable,dimension(:):: cstaid_uv
  real(r_single),allocatable,dimension(:,:)::rdiagbuf_uv
!
!  ensemble forecast to the observations
! 
  real(r_single),allocatable,dimension(:,:):: fcst_t
  real(r_single),allocatable,dimension(:,:):: fcst_q
  real(r_single),allocatable,dimension(:,:):: fcst_ps
  real(r_single),allocatable,dimension(:,:):: fcst_u
  real(r_single),allocatable,dimension(:,:):: fcst_v
!
!  misc.
!
  integer :: ios
!
!
  iunitdiag=17
!
  open(11,file='namelist_ens.conv')
   read(11,iosetup)
  close(11)

  iens=1
  write(infilename,'(a,i3.3)') trim(infilenamebase),iens
  write(*,*) trim(infilename)
!
!
  OPEN (iunitdiag,FILE=trim(infilename),STATUS='OLD',IOSTAT=ios,ACCESS='SEQUENTIAL',  &
             FORM='UNFORMATTED')
     if(ios > 0 ) then
       write(*,*) ' file is unavailabe: ', trim(infilename)
       stop 123
     endif
!
! count how many observations
  call read_diag_conv_count(iunitdiag,numobs_t,numobs_ps,numobs_uv,numobs_q,&
                                          nreal_t,nreal_ps,nreal_uv,nreal_q)
  write(*,*) 'number of observation T  =',numobs_t,nreal_t
  write(*,*) 'number of observation ps =',numobs_ps,nreal_ps
  write(*,*) 'number of observation uv =',numobs_uv,nreal_uv
  write(*,*) 'number of observation q  =',numobs_q,nreal_q
!
!  read in observations
  allocate(cstaid_t(numobs_t),rdiagbuf_t(nreal_t,numobs_t))
  allocate(cstaid_q(numobs_q),rdiagbuf_q(nreal_q,numobs_q))
  allocate(cstaid_ps(numobs_ps),rdiagbuf_ps(nreal_ps,numobs_ps))
  allocate(cstaid_uv(numobs_uv),rdiagbuf_uv(nreal_uv,numobs_uv))
  rewind(iunitdiag)
  call read_diag_conv_obs(iunitdiag,nreal_t,numobs_t,cstaid_t,rdiagbuf_t, &
                                    nreal_q,numobs_q,cstaid_q,rdiagbuf_q, &
                                nreal_ps,numobs_ps,cstaid_ps,rdiagbuf_ps, &
                                nreal_uv,numobs_uv,cstaid_uv,rdiagbuf_uv)
  close(iunitdiag)

!
!  read in the ensemble forecast to observations
!
  allocate(fcst_t(numens,numobs_t))
  allocate(fcst_q(numens,numobs_q))
  allocate(fcst_ps(numens,numobs_ps))
  allocate(fcst_u(numens,numobs_uv))
  allocate(fcst_v(numens,numobs_uv))
  do iens=1,numens
     write(infilename,'(a,i3.3)') trim(infilenamebase),iens
     write(*,*) 'read member ',iens, ' from ',trim(infilename)
     OPEN (iunitdiag,FILE=trim(infilename),STATUS='OLD',IOSTAT=ios,ACCESS='SEQUENTIAL',  &
             FORM='UNFORMATTED')
     if(ios > 0 ) then
       write(*,*) ' file is unavailabe: ', trim(infilename)
       stop 123
     endif

     call read_diag_conv_fcst(iunitdiag, nreal_t,numobs_t,rdiagbuf_t,  &
                                         nreal_q,numobs_q,rdiagbuf_q,  &
                                      nreal_ps,numobs_ps,rdiagbuf_ps,  &
                                      nreal_uv,numobs_uv,rdiagbuf_uv,  &
       fcst_t(iens,:),fcst_q(iens,:),fcst_ps(iens,:),fcst_u(iens,:),fcst_v(iens,:))
                              
     close(iunitdiag)
  enddo

  call write_diag_conv('  t',outfilepath,nreal_t,numobs_t,cstaid_t,rdiagbuf_t,numens,fcst_t)
  call write_diag_conv('  q',outfilepath,nreal_q,numobs_q,cstaid_q,rdiagbuf_q,numens,fcst_q)
  call write_diag_conv(' ps',outfilepath,nreal_ps,numobs_ps,cstaid_ps,rdiagbuf_ps,numens,fcst_ps)
  call write_diag_conv('  u',outfilepath,nreal_uv,numobs_uv,cstaid_uv,rdiagbuf_uv,numens,fcst_u)
  call write_diag_conv('  v',outfilepath,nreal_uv,numobs_uv,cstaid_uv,rdiagbuf_uv,numens,fcst_v)

  deallocate(fcst_t,fcst_q,fcst_ps,fcst_u,fcst_v)
  deallocate(cstaid_t,rdiagbuf_t)
  deallocate(cstaid_ps,rdiagbuf_ps)
  deallocate(cstaid_q,rdiagbuf_q)
  deallocate(cstaid_uv,rdiagbuf_uv)

END PROGRAM read_diag_conv_ens
!
subroutine write_diag_conv(var,outfilepath,nreal,numobs,cstaid,rdiagbuf,numens,fcst)
!
!  
!  write GSI diagnositic file for conventional data
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  integer,intent(in)  :: nreal,numobs
  integer,intent(in)  :: numens
  character(180),intent(in) :: outfilepath

  character(len=3),intent(in) :: var

  character(8),  intent(in):: cstaid(numobs)
  real(r_single),intent(in) :: rdiagbuf(nreal,numobs)
  real(r_single),intent(in) :: fcst(numens,numobs)
!
! out put file
!
  character(180) :: outfilename,outfilename_bin     ! file name saving results
  integer  :: iunit,iunit_bin

!
! output variables
!

  real(r_single) :: rlat,rlon,robs1,rdpt1,robs2,rdpt2,ruse,rerr
  real(r_single) :: rwgt,errinv_input,errinv_adjst,errinv_final
  real(r_single) :: stnelv,rpress,obshgt,rdhr, ddiff
  real(r_single) :: diffwob
  character(8) :: stationID
  integer :: itype,iuse,iusev,ictype,iqc,iqt

  real(r_single) :: fcst1(numens)
!
!  misc.
!
  character ::  ch
  integer :: i,j,k,ios
  integer :: ic, iflg
!
!
  iunit=46
  iunit_bin=47
!
!
  if (var == " ps") then
     write(outfilename,'(a,a)') trim(outfilepath),"results_ps.txt"
     write(outfilename_bin,'(a,a)') trim(outfilepath),"results_ps.bin"
  endif
  if (var == "  t") then
     write(outfilename,'(a,a)') trim(outfilepath),"results_t.txt"
     write(outfilename_bin,'(a,a)') trim(outfilepath),"results_t.bin"
  endif
  if (var == "  u") then
     write(outfilename,'(a,a)') trim(outfilepath),"results_u.txt"
     write(outfilename_bin,'(a,a)') trim(outfilepath),"results_u.bin"
  endif
  if (var == "  v") then
     write(outfilename,'(a,a)') trim(outfilepath),"results_v.txt"
     write(outfilename_bin,'(a,a)') trim(outfilepath),"results_v.bin"
  endif
  if (var == "  q") then
     write(outfilename,'(a,a)') trim(outfilepath),"results_q.txt"
     write(outfilename_bin,'(a,a)') trim(outfilepath),"results_q.bin"
  endif
!
  open(iunit, file=trim(outfilename),IOSTAT=ios)
  if(ios > 0 ) then
       write(*,*) ' cannot open file ', trim(outfilename)
       stop 123
  else
       write(*,*) ' open file ', trim(outfilename)
  endif
!
  open(iunit_bin, file=trim(outfilename_bin),IOSTAT=ios,form='unformatted')
  if(ios > 0 ) then
       write(*,*) ' cannot open file ', trim(outfilename_bin)
       stop 123
  else
       write(*,*) ' open file ', trim(outfilename_bin)
  endif
!
  write(iunit,'(3I20)') numobs,nreal,numens
  write(iunit_bin) numobs,nreal,numens
!
     do i=1,numobs
             itype   = int(rdiagbuf(1,i))    ! observation type
             ictype  = rdiagbuf(2,i)         ! observation subtype
             rlat    = rdiagbuf(3,i)         ! observation latitude (degrees)
             rlon    = rdiagbuf(4,i)         ! observation longitude (degrees)
             stnelv  = rdiagbuf(5,i)         ! station elevation (meters)
             rpress  = rdiagbuf(6,i)         ! observation pressure (hPa)
             obshgt  = rdiagbuf(7,i)         ! observation height (meters)
             rdhr    = rdiagbuf(8,i)         ! obs time (hours relative to analysis time)
             iqc     = int(rdiagbuf(9,i))    ! input prepbufr qc or event mark
             iqt     = int(rdiagbuf(10,i))   ! setup qc or event mark (currently qtflg only)
             iusev   = int(rdiagbuf(11,i))   ! analysis usage flag ( value ) 
             iuse    = int(rdiagbuf(12,i))   ! analysis usage flag (1=use, -1=monitoring ) 
             rwgt    = rdiagbuf(13,i)        ! nonlinear qc relative weight
             errinv_input=rdiagbuf(14,i)    ! prepbufr inverse obs error(K**-1)
             errinv_adjst=rdiagbuf(15,i)    ! read_prepbufr inverse obs error(K**-1)   
             errinv_final=rdiagbuf(16,i)    ! final inverse observation error(K**-1) 
             rerr = 0
             if (errinv_final > 0) then    ! final inverse observation error (K**-1)
               rerr=1.0/errinv_final
             end if 

             robs1   = rdiagbuf(17,i)        !  observation (K)
             ddiff   = rdiagbuf(18,i)        !  obs-ges used in analysis (K)
             diffwob = rdiagbuf(19,i)        ! obs-ges w/o bias correction 

!       rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
!       rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
!       rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s)        
             if (var == "  v") then
!  ** When the data is uv, additional output is needed **/
                robs1    = rdiagbuf(20,i)   ! earth relative v wind component observation (m/s)   
                ddiff    = rdiagbuf(21,i)   ! earth relative v obs-ges used in analysis (m/s)  
                diffwob  = rdiagbuf(22,i)   ! earth relative v obs-ges w/o bias correction (m/s)     
             endif

! get station ID
             stationID = cstaid(i)
             iflg = 0
             do ic=8,1,-1
              ch = stationID(ic:ic)
              if (ch > ' ' .and. ch <= 'z') then
                iflg = 1
              else
                 stationID(ic:ic) = ' '
              end if
              if (ch == ' '  .and. iflg == 1) then
                 stationID(ic:ic) = '_'
              endif 
             enddo
!
!   When the data is q, unit convert kg/kg -> g/kg **/
             if (var == "  q") then
                robs1  = robs1  * 1000.0
                ddiff = ddiff * 1000.0
                rerr  = rerr  * 1000.0
                fcst1(:) = fcst(:,i) * 1000.0
             end if
!   When the data is pw, replase the rprs to -999.0 **/
             if (var == " pw") rpress=-999.0
!
             if(robs1 > 1.0e8) then
               robs1=-99999.9
               ddiff=-99999.9
             endif

             fcst1(:) = fcst(:,i) 
             if (var == "  q") fcst1(:) = fcst1(:) * 1000.0
             do k=1,numens
                if( fcst1(k) > 1.0e8 ) fcst1(k)=-99999.9
             enddo
!
!  write out result for one variable on one pitch
             write (iunit,'(F10.2, 10x,A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5)') &
                   robs1,var,stationID,itype,rdhr,rlat,rlon,rpress,iuse
             write (iunit,'(10F10.2)') fcst1(:)

             write (iunit_bin) &
                   robs1,var,stationID,itype,rdhr,rlat,rlon,rpress,iuse
             write (iunit_bin) fcst1(:)

    enddo   ! i  end for one station

    close(iunit)
    close(iunit_bin)
    return 

end subroutine write_diag_conv
!
!
subroutine  read_diag_conv_fcst(iunitdiag,nreal_t,numobs_t,rdiagbuf_t, &
                                          nreal_q,numobs_q,rdiagbuf_q, &
                                       nreal_ps,numobs_ps,rdiagbuf_ps, &
                                       nreal_uv,numobs_uv,rdiagbuf_uv, &
                                   fcst_t,fcst_q,fcst_ps,fcst_u,fcst_v)
!
!  This program is to show how to 
!  read GSI diagnositic file for each ensemble forecast 
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  integer,intent(in)  :: iunitdiag
  integer,intent(in)  :: nreal_t,numobs_t,nreal_q,numobs_q
  integer,intent(in)  :: nreal_ps,numobs_ps,nreal_uv,numobs_uv
  
  real(r_single),intent(in):: rdiagbuf_t(nreal_t,numobs_t)
  real(r_single),intent(in):: rdiagbuf_q(nreal_q,numobs_q)
  real(r_single),intent(in):: rdiagbuf_ps(nreal_ps,numobs_ps)
  real(r_single),intent(in):: rdiagbuf_uv(nreal_uv,numobs_uv)

!
!  ensemble forecast to the observations
! 
  real(r_single),intent(inout) :: fcst_t(numobs_t)
  real(r_single),intent(inout) :: fcst_q(numobs_q)
  real(r_single),intent(inout) :: fcst_ps(numobs_ps)
  real(r_single),intent(inout) :: fcst_u(numobs_uv)
  real(r_single),intent(inout) :: fcst_v(numobs_uv)
!
!
! read in variables
!
  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii,mype
  integer(i_kind) idate
!
!  variables
!

  character(len=3)  :: var
  character(8) :: stationID
!
!  misc.
!
  integer :: it,iq,ips,iuv,ios,i
  integer :: k,n

  real :: robs1,robs2
  real :: ddiff
  real :: rbk1,rbk2,diffwob,diffv,diffvwob

!
!
  it=0
  iq=0
  iuv=0
  ips=0
!
     read(iunitdiag, ERR=999) idate
     write(*,*) 'process date: ',idate
100  continue
     read(iunitdiag, ERR=999,end=110) var, nchar,nreal,ii,mype
!     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
          read(iunitdiag,ERR=999,end=110) cdiagbuf, rdiagbuf
          do i=1,ii
             stationID = cdiagbuf(i)
!
             robs1   = rdiagbuf(17,i)        !  observation (K)
             ddiff   = rdiagbuf(18,i)        !  obs-ges used in analysis (K)
             diffwob = rdiagbuf(19,i)        ! obs-ges w/o bias correction 
             rbk1    = robs1-diffwob         !  guess in observation location 

!  write out result for one variable on one pitch
             if (var == "  t") then
                it=it+1
                n=0
                do k=1,11
                  if(k.ne.6) then
                     if(abs(rdiagbuf_t(k,it)-rdiagbuf(k,i)) > 1.0e-10) n=n+1
                  endif
                enddo
                if(n==0) then
                   fcst_t(it)=rbk1
                else
                   write(*,*) 'observation mismatch',var,it,i
                   stop 2345
                endif
                if(it > numobs_t) then
                   write(*,*) 'too many T obs '
                   stop 1234
                endif
             endif
             if (var == "  q") then
                iq=iq+1
                n=0
                do k=1,11
                  if(k.ne.6) then
                     if(abs(rdiagbuf_q(k,iq)-rdiagbuf(k,i)) > 1.0e-10) n=n+1
                  endif
                enddo
                if(n==0) then
                   fcst_q(iq)=rbk1
                else
                   write(*,*) 'observation mismatch',var,iq,i
                   stop 2345
                endif
                if(iq > numobs_q) then
                   write(*,*) 'too many Q obs '
                   stop 1234
                endif
             endif
             if (var == " ps") then
                ips=ips+1
                n=0
                do k=1,11
                  if(k.ne.6) then
                     if(abs(rdiagbuf_ps(k,ips)-rdiagbuf(k,i)) > 1.0e-10) n=n+1
!                    write(*,*) k,rdiagbuf_ps(k,ips),rdiagbuf(k,i)
                  endif
                enddo
                if(n==0) then
                   fcst_ps(ips)=rbk1
                else
                   write(*,*) 'observation mismatch',var,ips,i
                   stop 2345
                endif
                if(ips > numobs_ps) then
                   write(*,*) 'too many Ps obs '
                   stop 1234
                endif
             endif
             if (var == " uv") then
                robs2    = rdiagbuf(20,i)   ! earth relative v wind component observation (m/s)   
                diffv    = rdiagbuf(21,i)   ! earth relative v obs-ges used in analysis (m/s)  
                diffvwob = rdiagbuf(22,i)   ! earth relative v obs-ges w/o bias correction (m/s)     
                rbk2     = robs2-diffvwob
                iuv=iuv+1
                n=0
                do k=1,11
                  if(k.ne.6) then
                    if(abs(rdiagbuf_uv(k,iuv)-rdiagbuf(k,i)) > 1.0e-10) n=n+1
!                    write(*,*) k,rdiagbuf_uv(k,iuv),rdiagbuf(k,i)
                  endif
                enddo
                if(n==0) then
                   fcst_u(iuv)=rbk1
                   fcst_v(iuv)=rbk2
                else
                   write(*,*) 'observation mismatch',var,iuv,i
                   stop 2345
                endif
                if(iuv > numobs_uv) then
                   write(*,*) 'too many UV obs '
                   stop 1234
                endif
             endif
          enddo   ! i  end for one station

          deallocate(cdiagbuf,rdiagbuf)
     else
        read(iunitdiag)
     endif
     goto 100  ! goto another variable
110  continue

    return 

999    PRINT *,'error read in diag file'
      stop 1234

end subroutine read_diag_conv_fcst
!
!
subroutine  read_diag_conv_obs(iunitdiag,nreal_t,numobs_t,cstaid_t,rdiagbuf_t, &
                                         nreal_q,numobs_q,cstaid_q,rdiagbuf_q, &
                                     nreal_ps,numobs_ps,cstaid_ps,rdiagbuf_ps, &
                                     nreal_uv,numobs_uv,cstaid_uv,rdiagbuf_uv)
!
!  This program is to show how to 
!  read GSI diagnositic file for conventional data
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  integer,intent(in)  :: iunitdiag
  integer,intent(in)  :: nreal_t,numobs_t,nreal_q,numobs_q
  integer,intent(in)  :: nreal_ps,numobs_ps,nreal_uv,numobs_uv
  
  character(8),  intent(out):: cstaid_t(numobs_t)
  real(r_single),intent(out):: rdiagbuf_t(nreal_t,numobs_t)
  character(8),  intent(out):: cstaid_q(numobs_q)
  real(r_single),intent(out):: rdiagbuf_q(nreal_q,numobs_q)
  character(8),  intent(out):: cstaid_ps(numobs_ps)
  real(r_single),intent(out):: rdiagbuf_ps(nreal_ps,numobs_ps)
  character(8),  intent(out):: cstaid_uv(numobs_uv)
  real(r_single),intent(out):: rdiagbuf_uv(nreal_uv,numobs_uv)
!
!
! read in variables
!
  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii,mype
  integer(i_kind) idate
!
!  variables
!

  character(len=3)  :: var
  character(8) :: stationID
!
!  misc.
!
  integer :: it,iq,ips,iuv,ios,i
!
!
  it=0
  iq=0
  iuv=0
  ips=0
!
     read(iunitdiag, ERR=999) idate
     write(*,*) 'process date: ',idate
100  continue
     read(iunitdiag, ERR=999,end=110) var, nchar,nreal,ii,mype
!     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
          read(iunitdiag,ERR=999,end=110) cdiagbuf, rdiagbuf
          do i=1,ii
             stationID = cdiagbuf(i)
!
!  write out result for one variable on one pitch
             if (var == "  t") then
                it=it+1
                cstaid_t(it)=cdiagbuf(i)
                rdiagbuf_t(:,it)=rdiagbuf(:,i)
                if(it > numobs_t) then
                   write(*,*) 'too many T obs '
                   stop 1234
                endif
             endif
             if (var == "  q") then
                iq=iq+1
                cstaid_q(iq)=cdiagbuf(i)
                rdiagbuf_q(:,iq)=rdiagbuf(:,i)
                if(iq > numobs_q) then
                   write(*,*) 'too many Q obs '
                   stop 1234
                endif
             endif
             if (var == " ps") then
                ips=ips+1
                cstaid_ps(ips)=cdiagbuf(i)
                rdiagbuf_ps(:,ips)=rdiagbuf(:,i)
                if(ips > numobs_ps) then
                   write(*,*) 'too many Ps obs '
                   stop 1234
                endif
             endif
             if (var == " uv") then
                iuv=iuv+1
                cstaid_uv(iuv)=cdiagbuf(i)
                rdiagbuf_uv(:,iuv)=rdiagbuf(:,i)
                if(iuv > numobs_uv) then
                   write(*,*) 'too many UV obs '
                   stop 1234
                endif
             endif
          enddo   ! i  end for one station

          deallocate(cdiagbuf,rdiagbuf)
     else
        read(iunitdiag)
     endif
     goto 100  ! goto another variable
110  continue

    return 

999    PRINT *,'error read in diag file'
      stop 1234

end subroutine read_diag_conv_obs
!
!
subroutine read_diag_conv(iunitdiag,iunitt,iunitps,iunituv,iunitq)
!
!  This program is to show how to 
!  read GSI diagnositic file for conventional data, which are
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

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  integer,intent(in)  :: iunitt,iunitps,iunituv,iunitq,iunitdiag
!
  real(r_kind) tiny_r_kind
!
! read in variables
!
  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii,mype
  integer(i_kind) idate
!
! output variables
!

  character(len=3)  :: var
  real :: rlat,rlon,rprs,robs1,rdpt1,robs2,rdpt2,ruse,rerr
  real :: rwgt,errinv_input,errinv_adjst,errinv_final
  real :: stnelv,rpress,obshgt,rdhr, ddiff
  real :: rbk1,rbk2,diffwob,diffv,diffvwob
  character(8) :: stationID
  integer :: itype,iuse,iusev,ictype,iqc,iqt
!
!  misc.
!
  character ::  ch
  integer :: i,j,k,ios
  integer :: ic, iflg
!
!
     read(iunitdiag, ERR=999) idate
     write(*,*) 'process date: ',idate
100  continue
     read(iunitdiag, ERR=999,end=110) var, nchar,nreal,ii,mype
     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
          read(iunitdiag,ERR=999,end=110) cdiagbuf, rdiagbuf
          do i=1,ii
             itype   = int(rdiagbuf(1,i))    ! observation type
             ictype  = rdiagbuf(2,i)         ! observation subtype
             rlat    = rdiagbuf(3,i)         ! observation latitude (degrees)
             rlon    = rdiagbuf(4,i)         ! observation longitude (degrees)
             stnelv  = rdiagbuf(5,i)         ! station elevation (meters)
             rpress  = rdiagbuf(6,i)         ! observation pressure (hPa)
             obshgt  = rdiagbuf(7,i)         ! observation height (meters)
             rdhr    = rdiagbuf(8,i)         ! obs time (hours relative to analysis time)
             iqc     = int(rdiagbuf(9,i))    ! input prepbufr qc or event mark
             iqt     = int(rdiagbuf(10,i))   ! setup qc or event mark (currently qtflg only)
             iusev   = int(rdiagbuf(11,i))   ! analysis usage flag ( value ) 
             iuse    = int(rdiagbuf(12,i))   ! analysis usage flag (1=use, -1=monitoring ) 
             rwgt    = rdiagbuf(13,i)        ! nonlinear qc relative weight
             errinv_input=rdiagbuf(14,ii)    ! prepbufr inverse obs error(K**-1)
             errinv_adjst=rdiagbuf(15,ii)    ! read_prepbufr inverse obs error(K**-1)   
             errinv_final=rdiagbuf(16,ii)    ! final inverse observation error(K**-1) 
             rerr = 0
             if (errinv_final > 0) then    ! final inverse observation error (K**-1)
               rerr=1.0/errinv_final
             end if 
             robs1   = rdiagbuf(17,i)        !  observation (K)
             ddiff   = rdiagbuf(18,i)        !  obs-ges used in analysis (K)
             diffwob = rdiagbuf(19,i)        ! obs-ges w/o bias correction 
             rbk1    = robs1-diffwob         !  guess in observation location 

!       rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
!       rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
!       rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s)        


! get station ID
             stationID = cdiagbuf(i)
             iflg = 0
             do ic=8,1,-1
              ch = stationID(ic:ic)
              if (ch > ' ' .and. ch <= 'z') then
                iflg = 1
              else
                 stationID(ic:ic) = ' '
              end if
              if (ch == ' '  .and. iflg == 1) then
                 stationID(ic:ic) = '_'
              endif 
             enddo
!
!   When the data is q, unit convert kg/kg -> g/kg **/
             if (var == "  q") then
                robs1 = robs1 * 1000.0
                rbk1  = rbk1  * 1000.0
                ddiff = ddiff * 1000.0
                rerr  = rerr  * 1000.0
             end if
!   When the data is pw, replase the rprs to -999.0 **/
             if (var == " pw") rpress=-999.0
!
             if(robs1 > 1.0e8) then
               robs1=-99999.9
               ddiff=-99999.9
               rbk1=-99999.9
             endif
!
!  write out result for one variable on one pitch
             if (var == "  t") then
                write (iunitt,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,3F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rpress,iuse,robs1,ddiff,rbk1
             endif
             if (var == "  q") then
                write (iunitq,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,3F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rpress,iuse,robs1,ddiff,rbk1
             endif
             if (var == " ps") then
                write (iunitps,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,3F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rpress,iuse,robs1,ddiff,rbk1
             endif
             if (var == " uv") then
!  ** When the data is uv, additional output is needed **/
                robs2    = rdiagbuf(20,i)   ! earth relative v wind component observation (m/s)   
                diffv    = rdiagbuf(21,i)   ! earth relative v obs-ges used in analysis (m/s)  
                diffvwob = rdiagbuf(22,i)   ! earth relative v obs-ges w/o bias correction (m/s)     
                rbk2     = robs2-diffvwob
                if(robs2 > 1.0e8) then
                  robs2=-99999.9
                  diffv=-99999.9
                  rbk2=-99999.9
                endif
                write (iunituv,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,6F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rpress,iuse,robs1,ddiff,rbk1,robs2,diffv,rbk2
             endif



          enddo   ! i  end for one station

          deallocate(cdiagbuf,rdiagbuf)
     else
        read(iunitdiag)
     endif
     goto 100  ! goto another variable
110  continue

    return 

999    PRINT *,'error read in diag file'
      stop 1234

end subroutine read_diag_conv
!
subroutine read_diag_conv_count(iunitdiag,numobs_t,numobs_ps,numobs_uv,numobs_q,&
                                          nreal_t,nreal_ps,nreal_uv,nreal_q)
!
!  This program is to count observation numbers in diag file
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  integer,intent(in)  :: iunitdiag
  integer,intent(out)  :: numobs_t,numobs_ps,numobs_uv,numobs_q
  integer,intent(out)  :: nreal_t,nreal_ps,nreal_uv,nreal_q
!
! read in variables
!
  integer(i_kind) nchar,nreal,ii,mype
  integer(i_kind) idate
!
! output variables
!

  character(len=3)  :: var
!
!  misc.
!
  integer :: i
!
!
     numobs_t=0
     numobs_ps=0
     numobs_uv=0
     numobs_q=0

     read(iunitdiag, ERR=999) idate
     write(*,*) 'process date: ',idate
100  continue
     read(iunitdiag, ERR=999,end=110) var, nchar,nreal,ii,mype
!     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          read(iunitdiag,ERR=999,end=110) 
             if (var == "  t") then
                numobs_t=numobs_t+ii
                nreal_t=nreal
             endif
             if (var == "  q") then
                numobs_q=numobs_q+ii
                nreal_q=nreal
             endif
             if (var == " ps") then
                numobs_ps=numobs_ps+ii
                nreal_ps=nreal
             endif
             if (var == " uv") then
                numobs_uv=numobs_uv+ii
                nreal_uv=nreal
             endif
     else
        read(iunitdiag)
     endif
     goto 100  ! goto another variable
110  continue

     return 

999  PRINT *,'error read in diag file'
     stop 1234

end subroutine read_diag_conv_count
