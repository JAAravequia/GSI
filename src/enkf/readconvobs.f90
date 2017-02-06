module readconvobs
!$$$  module documentation block
!
! module: readconvobs                  read data from diag_conv* files
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read data from diag_conv* files (containing prepbufr data) written
! out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_convobs: determine the number of observations to read.
!  get_convobs_data: read the data and calculate H(x) for ensemble members.
!  write_convobs_data: output diag file with spread
!
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!   2016-11-29  shlyaeva - updated read routine to calculate linearized H(x)
!                          added write_convobs_data to output ensemble spread
!
! attributes:
!   language: f95
!
!$$$
use kinds, only: r_kind,i_kind,r_single,r_double
use constants, only: one,zero,deg2rad
use params, only: npefiles
implicit none

private
public :: get_num_convobs, get_convobs_data, write_convobs_data

contains

subroutine get_num_convobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    character (len=500), intent(in) :: obspath
    character (len=10), intent(in) :: datestring
    character(len=500) obsfile
    character(len=10), intent(in) :: id
    character(len=4) pe_name
    character(len=3) :: obtype
    integer(i_kind) iunit, nchar, nreal, ii, mype,ios, idate, i, ipe, ioff0
    integer(i_kind),intent(out) :: num_obs_tot, num_obs_totdiag
    integer(i_kind),dimension(2):: nn,nobst, nobsps, nobsq, nobsuv, nobsgps, &
         nobstcp,nobstcx,nobstcy,nobstcz,nobssst, nobsspd, nobsdw, nobsrw, nobspw, nobssrw
    character(8),allocatable,dimension(:):: cdiagbuf
    real(r_single),allocatable,dimension(:,:)::rdiagbuf
    real(r_kind) :: errorlimit,errorlimit2,error,pres,obmax
    real(r_kind) :: errorlimit2_obs,errorlimit2_bnd
    logical :: fexist, init_pass
    iunit = 7
    ! If ob error > errorlimit or < errorlimit2, skip it.
    errorlimit = 1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2_obs = 1._r_kind/sqrt(1.e-6_r_kind)
    errorlimit2_bnd = 1.e3_r_kind*errorlimit2_obs
    num_obs_tot = 0
    num_obs_totdiag = 0
    nobst = 0
    nobsq = 0
    nobsps = 0
    nobsuv = 0
    nobssst = 0
    nobsspd = 0
    nobsdw = 0
    nobsrw = 0
    nobspw = 0
    nobsgps = 0
    nobssrw = 0
    nobstcp = 0; nobstcx = 0; nobstcy = 0; nobstcz = 0
    init_pass = .true.
    peloop: do ipe=0,npefiles
       write(pe_name,'(i4.4)') ipe
       if (npefiles .eq. 0) then
           ! read diag file (concatenated pe* files)
           obsfile = trim(adjustl(obspath))//"diag_conv_ges."//datestring//'_'//trim(adjustl(id))
           inquire(file=obsfile,exist=fexist)
           if (.not. fexist .or. datestring .eq. '0000000000') &
             obsfile = trim(adjustl(obspath))//"diag_conv_ges."//trim(adjustl(id))
       else ! read raw, unconcatenated pe* files.
           obsfile =&
             trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.conv_01'
       endif
       inquire(file=obsfile,exist=fexist)
       if (.not. fexist) cycle peloop
       open(iunit,form="unformatted",file=obsfile,iostat=ios)
       if (init_pass) then
          read(iunit) idate
          init_pass = .false.
       endif
10     continue
       read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype,ioff0
       errorlimit2=errorlimit2_obs
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       if (obtype=='gps') then
          if (rdiagbuf(20,1)==1) errorlimit2=errorlimit2_bnd
       end if

       nn=0
       do i=1,ii
         if(obtype == 'tcx' .or. obtype == 'tcy' .or. obtype == 'tcz')then
           error=rdiagbuf(6,i)
           pres=rdiagbuf(4,i)
           obmax=abs(rdiagbuf(7,i))
         else
           if(rdiagbuf(12,i) < zero)cycle
           if (obtype == '  q') then
             error=rdiagbuf(20,i)*rdiagbuf(16,i)
             pres=rdiagbuf(6,i)
             obmax=abs(rdiagbuf(17,i)/rdiagbuf(20,i))
           else
              if(obtype == ' ps' .or. obtype == 'tcp')then
                pres=rdiagbuf(17,i)
              else
                pres=rdiagbuf(6,i)
              end if
              error=rdiagbuf(16,i)
              obmax=abs(rdiagbuf(17,i))
              if(obtype == ' uv')obmax = max(obmax,abs(rdiagbuf(20,i)))
           end if
         end if
         nn(1)=nn(1)+1  ! number of read obs
         if(error > errorlimit .and. error < errorlimit2 .and. &
            abs(obmax) <= 1.e9_r_kind .and. pres >= 0.001_r_kind .and. &
            pres <= 1200._r_kind) nn(2)=nn(2)+1  ! number of keep obs
       end do
       if (obtype == '  t') then
          nobst = nobst + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == ' uv') then
          nobsuv = nobsuv + 2*nn
          num_obs_tot = num_obs_tot + 2*nn(2)
       else if (obtype == ' ps') then
           nobsps = nobsps + nn
           num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == '  q') then
          nobsq = nobsq + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'spd') then
          nobsspd = nobsspd + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'sst') then ! skip sst
          nobssst = nobssst + nn
!  Not currently used so do not add to num_obs_tot
!  num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'srw') then
          nobssrw = nobssrw + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == ' rw') then
          nobsrw = nobsrw + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'gps') then
          nobsgps = nobsgps + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == ' dw') then
          nobsdw = nobsdw + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == ' pw') then
          nobspw = nobspw + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'tcp') then
          nobstcp = nobstcp + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'tcx') then
          nobstcx = nobstcx + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'tcy') then
          nobstcy = nobstcy + nn
          num_obs_tot = num_obs_tot + nn(2)
       else if (obtype == 'tcz') then
          nobstcz = nobstcz + nn
          num_obs_tot = num_obs_tot + nn(2)
       else
           print *,'unknown obtype ',trim(obtype)
       end if
       num_obs_totdiag = num_obs_totdiag + ii
       deallocate(cdiagbuf,rdiagbuf)
       go to 10
20     continue
       print *,'error reading diag_conv file',obtype
30     continue
       if (ipe .eq. npefiles) then
          print *,num_obs_tot,' obs in diag_conv_ges file, ', num_obs_totdiag, ' total obs in diag_conv_ges file'
          write(6,*)'columns below obtype,nread, nkeep'
          write(6,100) 't',nobst(1),nobst(2)
          write(6,100) 'q',nobsq(1),nobsq(2)
          write(6,100) 'ps',nobsps(1),nobsps(2)
          write(6,100) 'uv',nobsuv(1),nobsuv(2)
          write(6,100) 'sst',nobssst(1),nobssst(2)
          write(6,100) 'gps',nobsgps(1),nobsgps(2)
          write(6,100) 'spd',nobsspd(1),nobsspd(2)
          write(6,100) 'pw',nobspw(1),nobspw(2)
          write(6,100) 'dw',nobsdw(1),nobsdw(2)
          write(6,100) 'srw',nobsrw(1),nobsrw(2)
          write(6,100) 'rw',nobssrw(1),nobssrw(2)
          write(6,100) 'tcp',nobstcp(1),nobstcp(2)
          if (nobstcx(2) .gt. 0) then
             write(6,100) 'tcx',nobstcx(1),nobstcx(2)
             write(6,100) 'tcy',nobstcy(1),nobstcy(2)
             write(6,100) 'tcz',nobstcz(1),nobstcz(2)
          endif
100       format(2x,a3,2x,i9,2x,i9)
       endif
       close(iunit)
    enddo peloop ! ipe loop
end subroutine get_num_convobs

subroutine get_convobs_data(obspath, datestring, nobs_max, nobs_maxdiag,   &
                            hx_mean, hx_mean_nobc, hx, x_obs, x_err,       &
                            x_lon, x_lat, x_press, x_time, x_code,         &
                            x_errorig, x_type, x_used, id, nanal)
  use sparsearr, only: sparr2, readarray, delete
  use params, only: nanals, lobsdiag_forenkf
  use statevec, only: state_d
  use mpisetup, only: nproc, mpi_wtime
  use observer_enkf, only: calc_linhx
  implicit none

  character*500,   intent(in) :: obspath
  character*10,    intent(in) :: datestring
  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out)    :: hx_mean
  real(r_single), dimension(nobs_max), intent(out)    :: hx_mean_nobc
  real(r_single), dimension(nobs_max), intent(out)    :: hx
  real(r_single), dimension(nobs_max), intent(out)    :: x_obs
  real(r_single), dimension(nobs_max), intent(out)    :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out)    :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out)    :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out)   :: x_code
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used

  character(len=10), intent(in) :: id
  integer, intent(in)           :: nanal

  real(r_double) t1,t2,tsum
  character(len=4) pe_name
  character*500 obsfile, obsfile2
  character(len=10) :: id2

  type(sparr2)         :: dhx_dx

  character(len=3) :: obtype, obtype2
  integer(i_kind) :: iunit, iunit2
  integer(i_kind) :: nob, nobdiag, n, i
  integer(i_kind) :: nchar, nreal, ii, mype, ioff0
  integer(i_kind) :: nchar2, nreal2, ii2, mype2, ioff02, idate2
  integer(i_kind) :: ipe, ios, idate
  integer(i_kind) :: ind
  character(8),allocatable,dimension(:)     :: cdiagbuf, cdiagbuf2
  real(r_single),allocatable,dimension(:,:) :: rdiagbuf, rdiagbuf2
  real(r_kind) :: errorlimit,errorlimit2,error,errororig
  real(r_kind) :: obmax, pres
  real(r_kind) :: errorlimit2_obs,errorlimit2_bnd
  logical fexist, init_pass
  logical twofiles, fexist2, init_pass2

! Error limit is made consistent with screenobs routine
  errorlimit = 1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2_obs = 1._r_kind/sqrt(1.e-6_r_kind)
  errorlimit2_bnd = 1.e3_r_kind*errorlimit2_obs
  iunit = 7
  iunit2 = 17

  twofiles = (.not. lobsdiag_forenkf) .and. (nanal <= nanals)
  id2 = 'ensmean'
  if (nanal <= nanals) then
     write(id2,'(a3,(i3.3))') 'mem',nanal
  endif

  tsum = 0

  nob  = 0
  nobdiag = 0
  x_used = 0

  hx = zero

  init_pass = .true.; init_pass2 = .true.

  peloop: do ipe=0,npefiles

    write(pe_name,'(i4.4)') ipe
    if (npefiles .eq. 0) then
        ! read diag file (concatenated pe* files)
        obsfile = trim(adjustl(obspath))//"diag_conv_ges."//datestring//'_'//trim(adjustl(id))
        inquire(file=obsfile,exist=fexist)
        if (.not. fexist .or. datestring .eq. '0000000000') &
        obsfile = trim(adjustl(obspath))//"diag_conv_ges."//trim(adjustl(id))
    else ! read raw, unconcatenated pe* files.
        obsfile = &
        trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.conv_01'
    endif

    inquire(file=obsfile,exist=fexist)
    if (.not. fexist) cycle peloop

    open(iunit,form="unformatted",file=obsfile,iostat=ios)
    rewind(iunit)
    if (init_pass) then
      read(iunit) idate
      init_pass = .false.
    endif

    if(twofiles) then
       if (npefiles .eq. 0) then
         ! read diag file (concatenated pe* files)
         obsfile2 = trim(adjustl(obspath))//"diag_conv_ges."//datestring//'_'//trim(adjustl(id2))
         inquire(file=obsfile2,exist=fexist2)
         if (.not. fexist2 .or. datestring .eq. '0000000000') &
         obsfile2 = trim(adjustl(obspath))//"diag_conv_ges."//trim(adjustl(id2))
       else ! read raw, unconcatenated pe* files.
         obsfile2 =&
         trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id2))//'/pe'//pe_name//'.conv_01'
      endif
  
      inquire(file=obsfile2,exist=fexist2)
      open(iunit2,form="unformatted",file=obsfile2,iostat=ios)
      rewind(iunit2)
      if (init_pass2) then
        read(iunit2) idate2
        init_pass2 = .false.
      endif
    end if

10  continue

    read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype,ioff0
    errorlimit2=errorlimit2_obs

    if(twofiles) then
       read(iunit2,err=20,end=30) obtype2,nchar2,nreal2,ii2,mype2,ioff02
       if(obtype /= obtype2 .or. nchar /= nchar2 .or. nreal /= nreal2 .or. ii /= ii2)then
          write(6,*) ' conv obs mismatch '

          write(6,*) ' obtype ',obtype,obtype2
          write(6,*) ' nchar ',nchar,nchar2
          write(6,*) ' nreal ',nreal,nreal2
          write(6,*) ' ii ',ii,ii2
          go to 10
       end if
    end if


    if (obtype == '  t' .or. obtype == ' uv' .or. obtype == ' ps' .or. &
        obtype == 'tcp' .or. obtype == '  q' .or. obtype == 'spd' .or. &
        obtype == 'sst' .or. obtype == 'srw' .or. obtype == ' rw' .or. &
        obtype == 'gps' .or. obtype == ' dw' .or. obtype == ' pw')  then

       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)

       if (twofiles) then
          allocate(cdiagbuf2(ii2), rdiagbuf2(nreal2,ii2))
          read(iunit2) cdiagbuf2(1:ii2),rdiagbuf2(:,1:ii2)
       endif

       ! special handling for error limits for GPS bend angle
       if (obtype == 'gps' .and. rdiagbuf(20,1)==1) errorlimit2=errorlimit2_bnd

       do n=1,ii
          nobdiag = nobdiag + 1
          ! for q, normalize by qsatges
          if (obtype == '  q') then
             obmax     = abs(rdiagbuf(17,n)/rdiagbuf(20,n))
             errororig = rdiagbuf(14,n)*rdiagbuf(20,n)
             error     = rdiagbuf(16,n)*rdiagbuf(20,n)
          else
             obmax     = abs(rdiagbuf(17,n))
             errororig = rdiagbuf(14,n)
             error     = rdiagbuf(16,n)
          endif
          if (obtype == ' uv') then
             obmax = max(obmax,abs(rdiagbuf(20,n)))
          endif
          if (obtype == ' ps' .or. obtype == 'tcp') then
             pres = rdiagbuf(17,n)
          else
             pres = rdiagbuf(6,n)
          endif
          if (rdiagbuf(12,n) < zero .or.                        &
              error < errorlimit .or. error > errorlimit2 .or.  &
              abs(obmax) > 1.e9_r_kind .or.                     &
              pres < 0.001_r_kind .or. pres > 1200._r_kind) cycle
          if ( twofiles .and.                                    &
              (rdiagbuf(1,n) /= rdiagbuf2(1,n) .or.              &
               abs(rdiagbuf(3,n)-rdiagbuf2(3,n)) .gt. 1.e-5 .or. &
               abs(rdiagbuf(4,n)-rdiagbuf2(4,n)) .gt. 1.e-5 .or. &
               abs(rdiagbuf(8,n)-rdiagbuf2(8,n)) .gt. 1.e-5)) then
             write (6,*) obtype, ' conv ob data inconsistency '
             write (6,*) (rdiagbuf(i,n),i=1,8)
             write (6,*) (rdiagbuf2(i,n),i=1,8)
             call stop2(-98)
          end if

          nob = nob + 1
  
          x_used(nobdiag) = 1
          x_code(nob)  = rdiagbuf(1,n)

          ! observation location and time
          x_lat(nob)   = rdiagbuf(3,n)
          x_lon(nob)   = rdiagbuf(4,n)
          x_press(nob) = pres
          x_time(nob)  = rdiagbuf(8,n)

          ! observation errors
          if (errororig > 1.e-5_r_kind) then
             x_errorig(nob) = (one/errororig)**2
          else
             x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob)   = (one/error)**2
          ! special handling of gps error
          if (obtype == 'gps' .and. x_errorig(nob) .gt. 1.e9)  x_errorig(nob)=x_err(nob)
  
          ! observation
          x_obs(nob)   = rdiagbuf(17,n)

          ! hx and hxnobc
          ! special handling of gps hx
          if (obtype == 'gps') then
             hx_mean(nob) = rdiagbuf(17,n) - (rdiagbuf(5,n)*rdiagbuf(17,n))
             hx_mean_nobc(nob) = rdiagbuf(17,n) - (rdiagbuf(5,n)*rdiagbuf(17,n))
          else
             hx_mean(nob)     = rdiagbuf(17,n)-rdiagbuf(18,n)
             hx_mean_nobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          endif
          ! ????? just repeating whatever was in the previous code; I don't know
          ! whether that's reasonable
          if (obtype == '  q' .or. obtype == 'spd' .or. obtype == ' dw' .or. &
              obtype == ' pw') then
             hx_mean_nobc(nob) = hx_mean(nob)
          endif

          ! observation type
          x_type(nob)  = obtype
          if (obtype == ' uv')   x_type(nob) = '  u'
          if (obtype == 'tcp')   x_type(nob) = ' ps'
          if (obtype == ' rw')   x_type(nob) = '  u'

          ! get Hx
          if (nanal <= nanals) then
             ! read full Hx from file
             if (.not. lobsdiag_forenkf) then
                if (obtype == 'gps') then
                   hx(nob) = rdiagbuf2(17,n) - (rdiagbuf2(5,n)*rdiagbuf2(17,n))
                else
                   hx(nob) = rdiagbuf(17,n) - rdiagbuf2(19,n)
                endif
                if (obtype == '  q' .or. obtype == 'spd' .or. obtype == ' dw' .or. &
                    obtype == ' pw') then
                   hx(nob) = rdiagbuf(17,n) - rdiagbuf2(18,n)
                endif

             ! run the linearized Hx 
             else
                ind = ioff0 + 1
                call readarray(dhx_dx, rdiagbuf(ind:nreal,n))
                ind = ind + dhx_dx%nnz + dhx_dx%nind*2 + 2

                t1 = mpi_wtime()
                call calc_linhx(hx_mean_nobc(nob), state_d,             &
                              real(x_lat(nob)*deg2rad,r_single),      &
                              real(x_lon(nob)*deg2rad,r_single),      &
                              x_time(nob),                            &
                              dhx_dx, hx(nob))
                t2 = mpi_wtime()
                tsum = tsum + t2-t1

                call delete(dhx_dx)
             endif

!             if (nanal == 2) print *, nob, x_type(nob), x_obs(nob), hx_mean_nobc(nob), hx(nob)


             ! normalize q by qsatges
             if (obtype == '  q') then
                hx(nob) = hx(nob) /rdiagbuf(20,n)
             endif

          endif

          ! normalize q by qsatges
          if (obtype == '  q') then
             x_obs(nob)   = x_obs(nob) /rdiagbuf(20,n)
             hx_mean(nob)     = hx_mean(nob) /rdiagbuf(20,n)
             hx_mean_nobc(nob) = hx_mean_nobc(nob) /rdiagbuf(20,n)
          endif

          ! for wind, also read v-component
          if (obtype == ' uv' .or. obtype == ' rw') then
             nob = nob + 1
             x_code(nob)  = rdiagbuf(1,n)

             ! observation location and time
             x_lat(nob)   = rdiagbuf(3,n)
             x_lon(nob)   = rdiagbuf(4,n)
             x_press(nob) = pres
             x_time(nob)  = rdiagbuf(8,n)
 
             ! errors
             if (errororig > 1.e-5_r_kind) then
                x_errorig(nob) = (one/errororig)**2
             else
                x_errorig(nob) = 1.e10_r_kind
             endif
             x_err(nob)   = (one/error)**2

             ! observation
             x_obs(nob)   = rdiagbuf(20,n)
  
             ! hx and hxnobc
             hx_mean(nob)     = rdiagbuf(20,n)-rdiagbuf(21,n)
             hx_mean_nobc(nob) = rdiagbuf(20,n)-rdiagbuf(22,n)
  
             ! observation type
             x_type(nob)  = '  v'

             ! run linearized hx
             if (nanal <= nanals) then
                ! read full Hx
                if (.not. lobsdiag_forenkf) then
                   hx(nob) = rdiagbuf(20,n)-rdiagbuf2(22,n)
                ! run linearized Hx
                else
                   call readarray(dhx_dx, rdiagbuf(ind:nreal,n))
  
                   t1 = mpi_wtime()
                   call calc_linhx(hx_mean_nobc(nob), state_d,                  &
                                 real(x_lat(nob)*deg2rad,r_single),      &
                                 real(x_lon(nob)*deg2rad,r_single),      &
                                 x_time(nob),                            &
                                 dhx_dx, hx(nob))
                   t2 = mpi_wtime()
                   tsum = tsum + t2-t1

                   call delete(dhx_dx)
                endif
!                if (nanal == 2) print *, nob, x_type(nob), x_obs(nob), hx_mean_nobc(nob), hx(nob)
             endif
          endif
       enddo
       deallocate(cdiagbuf,rdiagbuf)
       if (twofiles) deallocate(cdiagbuf2,rdiagbuf2)

    else if (obtype == 'tcx' .or. obtype == 'tcy' .or. obtype == 'tcz') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)

       if(twofiles)then
         allocate(cdiagbuf2(ii2),rdiagbuf2(nreal2,ii2))
         read(iunit2) cdiagbuf2(1:ii2),rdiagbuf2(:,1:ii2)
       end if

       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(6,n) < errorlimit .or.        &
             rdiagbuf(6,n) > errorlimit2)cycle
          if(abs(rdiagbuf(7,n)) > 1.e9_r_kind  .or. &
             rdiagbuf(4,n) < 0.001_r_kind .or.      &
             rdiagbuf(4,n) > 1200._r_kind) cycle
          if(twofiles .and.                                      &
             (abs(rdiagbuf(2,n)-rdiagbuf2(2,n)) .gt. 1.e-5 .or. &
              abs(rdiagbuf(3,n)-rdiagbuf2(3,n)) .gt. 1.e-5)) then
             write (6,*) obtype, ' conv ob data inconsistency '
             write (6,*) rdiagbuf(:,n)
             write (6,*) rdiagbuf2(:,n)
             call stop2(-98)
          endif

          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob)    = rdiagbuf(1,n)
          x_lat(nob)     = rdiagbuf(2,n)
          x_lon(nob)     = rdiagbuf(3,n)
          x_press(nob)   = rdiagbuf(4,n)
          x_time(nob)    = 0
          x_obs(nob)     = rdiagbuf(7,n)
          x_errorig(nob) = rdiagbuf(6,n)**2
          x_err(nob)     = rdiagbuf(6,n)**2
          x_type(nob)    = obtype
          if (obtype == 'tcy')  x_type(nob) = 'tcx'
          hx_mean(nob)       = rdiagbuf(5,n)
          hx_mean_nobc(nob)  = rdiagbuf(5,n)
          if (.not. lobsdiag_forenkf) hx(nob) = rdiagbuf2(5,n)
       enddo
       deallocate(cdiagbuf,rdiagbuf)
       if (twofiles) deallocate(cdiagbuf2,rdiagbuf2)
    else
       print *,'warning - unknown ob type ',obtype
    endif

    go to 10
20  continue
    print *,'error reading diag_conv file'
30  continue
    close(iunit)
    if (twofiles) close(iunit2)
  
  enddo peloop ! ipe loop

  if (nanal == nanals) print *,'time in calc_linhx for conv obs on proc',nproc,' =',tsum
  if (nob .ne. nobs_max) then
      print *,'number of obs not what expected in get_convobs_data',nob,nobs_max
      call stop2(94)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total obs in diag not what expected in get_convobs_data',nobdiag, nobs_maxdiag
      call stop2(94)
  endif

 end subroutine get_convobs_data

! writing spread diagnostics
subroutine write_convobs_data(obspath, datestring, nobs_max, nobs_maxdiag, &
                              x_fit, x_sprd, x_used, id, id2, gesid2)

  character*500, intent(in) :: obspath
  character*10, intent(in) :: datestring
  
  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(in)      :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag), intent(in) :: x_used

  character(len=10), intent(in) :: id, id2, gesid2


  character*500 obsfile,obsfile2
  character(len=4) pe_name

  character(len=3) :: obtype
  integer(i_kind) :: iunit, iunit2
  integer(i_kind) :: nob, nobdiag, n, ind_sprd
  integer(i_kind) :: nchar, nreal, ii, ipe, ios, idate, mype, ioff0
  character(8),allocatable,dimension(:)     :: cdiagbuf
  real(r_single),allocatable,dimension(:,:) :: rdiagbuf
  logical :: fexist, init_pass

  iunit = 7
  iunit2 = 17

  nob  = 0
  nobdiag = 0
  init_pass = .true.


  if (datestring .eq. '0000000000') then
     obsfile2 = trim(adjustl(obspath))//"diag_conv_"//trim(adjustl(gesid2))//"."//trim(adjustl(id2))
  else
     obsfile2 = trim(adjustl(obspath))//"diag_conv_"//trim(adjustl(gesid2))//"."//datestring//'_'//trim(adjustl(id2))
  endif
  peloop: do ipe=0,npefiles

    write(pe_name,'(i4.4)') ipe
    if (npefiles .eq. 0) then
       ! diag file (concatenated pe* files)
       obsfile = trim(adjustl(obspath))//"diag_conv_ges."//datestring//'_'//trim(adjustl(id))
       inquire(file=obsfile,exist=fexist)
       if (.not. fexist .or. datestring .eq. '0000000000') then
          obsfile = trim(adjustl(obspath))//"diag_conv_ges."//trim(adjustl(id))
       endif
    else ! read raw, unconcatenated pe* files.
       obsfile = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.conv_01'
    endif

    inquire(file=obsfile,exist=fexist)
    if (.not. fexist) cycle peloop

    open(iunit,form="unformatted",file=obsfile,iostat=ios)
    rewind(iunit)
    if (init_pass) then
       open(iunit2,form="unformatted",file=obsfile2,iostat=ios)
       read(iunit) idate
       write(iunit2) idate
       init_pass = .false.
    endif
10 continue
    read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype,ioff0
    allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
    read(iunit,err=20) cdiagbuf(1:ii),rdiagbuf(1:nreal,1:ii)

    ind_sprd = -1
    if (obtype == '  t' .or. obtype == ' ps' .or. obtype == 'tcp' .or. &
        obtype == ' pw') then
       ind_sprd = 20
    elseif (obtype == '  q' .or. obtype == 'spd') then
       ind_sprd = 21
    elseif (obtype == 'gps') then
       ind_sprd = 22
    elseif (obtype == ' dw') then
       ind_sprd = 27
    elseif (obtype == 'srw') then
       ind_sprd = 25
    endif

    if (obtype == '  t' .or. obtype == ' ps' .or. obtype == 'tcp' .or. &
        obtype == '  q' .or. obtype == ' dw' .or. obtype == ' pw' .or. &
        obtype == 'srw' .or. obtype == 'spd' .or. obtype == 'gps') then
       ! defaults for not used in EnKF
       rdiagbuf(12,:) = -1        ! not used in EnKF
       ! only process if this record was used in EnKF
       do n=1,ii
          nobdiag = nobdiag + 1
          ! skip if not used in EnKF
          if (x_used(nobdiag) == 1) then
             ! update if it is used in EnKF
             nob = nob + 1
             rdiagbuf(12,n) = 1
             if (obtype == 'gps') then
                rdiagbuf(5,n)  = x_fit(nob) / rdiagbuf(17,n)
             else if (obtype == '  q') then
                rdiagbuf(19,n) = (x_fit(nob) + rdiagbuf(19,n) - rdiagbuf(18,n)) * rdiagbuf(20,n)
                rdiagbuf(18,n) = x_fit(nob) * rdiagbuf(20,n)
             else
                rdiagbuf(19,n) = x_fit(nob) + rdiagbuf(19,n) - rdiagbuf(18,n)
                rdiagbuf(18,n) = x_fit(nob)
             endif
             rdiagbuf(ind_sprd,n) = x_sprd(nob)
             if (obtype == '  q') then
                rdiagbuf(ind_sprd,n) = x_sprd(nob) * rdiagbuf(20,n)*rdiagbuf(20,n)
             endif
          endif
       enddo
    ! special processing for u and v
    else if (obtype == ' uv') then
       ! defaults for not used in EnKF
       rdiagbuf(12,:) = -1
       do n=1,ii
          nobdiag = nobdiag + 1
          if (x_used(nobdiag) == 1) then
             nob = nob + 1
             rdiagbuf(12,n) = 1
             ! u should be saved first
             rdiagbuf(19,n) = x_fit(nob) + rdiagbuf(19,n) - rdiagbuf(18,n)
             rdiagbuf(18,n) = x_fit(nob)
             rdiagbuf(24,n) = x_sprd(nob)
             nob = nob + 1
             rdiagbuf(22,n) = x_fit(nob) + rdiagbuf(22,n) - rdiagbuf(21,n)
             rdiagbuf(21,n) = x_fit(nob)
             rdiagbuf(25,n) = x_sprd(nob)
          endif
       enddo
    ! tcx, tcy, tcz have guess in different field from the rest
    else if ((obtype == 'tcx') .or. (obtype == 'tcy') .or. (obtype == 'tcz')) then
       rdiagbuf(5,:) = 1.e10
       do n=1,ii
          nobdiag = nobdiag + 1
          if (x_used(nobdiag) == 1) then
             nob = nob + 1
             rdiagbuf(5,n) = x_fit(nob)
          endif
       enddo
    else
       nobdiag = nobdiag + ii
    endif
    ! write the updated rdiagbuf
    write(iunit2,err=20) obtype,nchar,nreal,ii,mype,ioff0
    write(iunit2) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
    deallocate(cdiagbuf,rdiagbuf)

    go to 10
20  continue
    print *,'error reading diag_conv file'
30  continue
    close(iunit)
  enddo peloop ! ipe loop
  close(iunit2)

  if (nob .ne. nobs_max) then
      print *,'number of obs not what expected in write_convobs_data',nob,nobs_max
      call stop2(94)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total obs in diag not what expected in write_convobs_data',nobdiag, nobs_maxdiag
      call stop2(94)
  endif

 end subroutine write_convobs_data

end module readconvobs


