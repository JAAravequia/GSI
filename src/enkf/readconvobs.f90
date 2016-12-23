module readconvobs
!$$$  module documentation block
!
! module: readconvobs                  read data from diag_conv* files
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read data from diag_conv* files (containing prepbufr data) written out
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
use kinds, only: r_kind,i_kind,r_single
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
    integer(i_kind) iunit, nchar, nreal, ii, mype,ios, idate, i, ipe
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
       read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype
       errorlimit2=errorlimit2_obs
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       !print *,obtype,nchar,nreal,ii,mype
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

subroutine get_convobs_data(obspath, datestring, nobs_max, nobs_maxdiag, h_x, h_xnobc, dhx, x_obs, x_err, &
     x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal)
use sparsearr
use params, only: nanals
use statevec, only: state_d
implicit none

  character*500,   intent(in) :: obspath
  character*10,    intent(in) :: datestring
  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out)    :: h_x
  real(r_single), dimension(nobs_max), intent(out)    :: h_xnobc
  real(r_single), dimension(nobs_max), intent(out)    :: dhx
  real(r_single), dimension(nobs_max), intent(out)    :: x_obs
  real(r_single), dimension(nobs_max), intent(out)    :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out)    :: x_lon, x_lat, x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out)   :: x_code
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  integer(i_kind), dimension(nobs_maxdiag) :: x_used

  character(len=10), intent(in) :: id
  integer, intent(in) :: nanal

  character(len=4) pe_name
  character*500 obsfile

  type(sparr2)         :: dhx_dx

  character(len=3) :: obtype
  integer(i_kind) :: iunit
  integer(i_kind) :: nob, nobdiag, n
  integer(i_kind) :: nchar, nreal, ii, mype
  integer(i_kind) :: ipe, ios, idate
  integer(i_kind) :: nnz, ind, nind
  character(8),allocatable,dimension(:)     :: cdiagbuf
  real(r_single),allocatable,dimension(:,:) :: rdiagbuf
  real(r_kind) :: errorlimit,errorlimit2,error
  real(r_kind) :: errorlimit2_obs,errorlimit2_bnd
  logical fexist, init_pass

! Error limit is made consistent with screenobs routine
  errorlimit = 1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2_obs = 1._r_kind/sqrt(1.e-6_r_kind)
  errorlimit2_bnd = 1.e3_r_kind*errorlimit2_obs
  iunit = 7

  nob  = 0
  nobdiag = 0
  x_used = 0

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
  rewind(iunit)
  if (init_pass) then
      read(iunit) idate
      init_pass = .false.
  endif
10 continue
  read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype
  errorlimit2=errorlimit2_obs
      
    if (obtype == '  t') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)

       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob)  = rdiagbuf(1,n)
          x_lat(nob)   = rdiagbuf(3,n)
          x_lon(nob)   = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob)  = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob)   = (one/rdiagbuf(16,n))**2
          x_obs(nob)   = rdiagbuf(17,n)
          h_x(nob)     = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob)  = obtype

          if (nanal <= nanals) then
            ind = 20
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob),&
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
          endif

       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = prest              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = data(iqt,i)        ! setup qc or event mark (currently qtflg only)
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
!        rdiagbuf(17,ii) = data(itob,i)       ! temperature observation (K)
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
!        rdiagbuf(19,ii) = tob-tges           ! obs-ges w/o bias correction (K) (future slot)
    else if (obtype == ' uv') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind .or. &
             rdiagbuf(6,n) < 0.001_r_kind .or. &
             rdiagbuf(6,n) > 1200._r_kind .or. &
             abs(rdiagbuf(20,n)) > 1.e9_r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob)  = rdiagbuf(1,n)
          x_lat(nob)   = rdiagbuf(3,n)
          x_lon(nob)   = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob)  = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob)   = (one/rdiagbuf(16,n))**2
          x_obs(nob)   = rdiagbuf(17,n)
          h_x(nob)     = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob)  = '  u'

          if (nanal <= nanals) then
            ! interpolate x horizontally and in time to obs location
            ind = 24

            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)
            ind = ind + nnz

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob),&
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
          endif

          nob = nob + 1
          x_code(nob)  = rdiagbuf(1,n)
          x_lat(nob)   = rdiagbuf(3,n)
          x_lon(nob)   = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob)  = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob)   = (one/rdiagbuf(16,n))**2
          x_obs(nob)   = rdiagbuf(20,n)
          h_x(nob)     = rdiagbuf(20,n)-rdiagbuf(21,n)
          h_xnobc(nob) = rdiagbuf(20,n)-rdiagbuf(22,n)
          x_type(nob)  = '  v'

          if (nanal <= nanals) then
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob), &
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
          endif
       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
!        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1
!        rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
!        rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
!        rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s) (future slot)
!        rdiagbuf(20,ii) = data(ivob,i)       ! v wind component observation (m/s)
!        rdiagbuf(21,ii) = dvdiff             ! v obs-ges used in analysis (m/s)
!        rdiagbuf(22,ii) = vob-vgesin         ! v obs-ges w/o bias correction (m/s) (future slot)
!        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor
    else if (obtype == ' ps') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(rdiagbuf(17,n) < 0.001_r_kind .or. &
             rdiagbuf(17,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(17,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          ! error modified by GSI.
          x_err(nob) = (one/rdiagbuf(16,n))**2
          ! unmodified error from read_prepbufr
          !if (rdiagbuf(15,n) > tiny(rdiagbuf(1,1))) then
          !x_err(nob) = (one/rdiagbuf(15,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          x_type(nob) = obtype 
          ! ob minus ens mean bias-corrected background 
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n) 
          ! ob minus un-bias-corrected background (individ members)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)

          if (nanal <= nanals) then
            ind = 20

            ! read dHx/dx profile
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob), &
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)

          endif

       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = data(ipres,i)*r10  ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = dhgt               ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (hPa**-1)
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (hPa**-1)
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (hPa**-1)
!        rdiagbuf(17,ii) = pob                ! surface pressure observation (hPa)
!        rdiagbuf(18,ii) = pob-pges           ! obs-ges used in analysis (coverted to hPa)
!        rdiagbuf(19,ii) = pob-pgesorig       ! obs-ges w/o adjustment to guess surface pressure (hPa)
    else if (obtype == 'tcp') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(rdiagbuf(17,n) < 0.001_r_kind .or. &
             rdiagbuf(17,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(17,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          ! error modified by GSI.
          x_err(nob) = (one/rdiagbuf(16,n))**2
          ! unmodified error from read_prepbufr
          !if (rdiagbuf(15,n) > tiny(rdiagbuf(1,1))) then
          !x_err(nob) = (one/rdiagbuf(15,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          x_type(nob) = ' ps'
          ! ob minus ens mean bias-corrected background 
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          ! ob minus un-bias-corrected background (individ members)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)

          if (nanal <= nanals) then
            ind = 20

            ! read dHx/dx profile
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob),&
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)

          endif

       enddo
       deallocate(cdiagbuf,rdiagbuf)
    else if (obtype == 'tcx') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(6,n) < errorlimit .or. &
             rdiagbuf(6,n) > errorlimit2)cycle
          if(abs(rdiagbuf(7,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(4,n) < 0.001_r_kind .or. &
               rdiagbuf(4,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(2,n)
          x_lon(nob) = rdiagbuf(3,n)
          x_press(nob) = rdiagbuf(4,n)
          x_time(nob) = 0
          x_obs(nob) = rdiagbuf(7,n)
          x_errorig(nob) = rdiagbuf(6,n)**2
          x_err(nob) = rdiagbuf(6,n)**2
          x_type(nob) = 'tcx'
          h_x(nob) = rdiagbuf(5,n)
          h_xnobc(nob) = rdiagbuf(5,n)
       enddo
       deallocate(cdiagbuf,rdiagbuf)
    else if (obtype == 'tcy') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(6,n) < errorlimit .or. &
             rdiagbuf(6,n) > errorlimit2)cycle
          if(abs(rdiagbuf(7,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(4,n) < 0.001_r_kind .or. &
               rdiagbuf(4,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(2,n)
          x_lon(nob) = rdiagbuf(3,n)
          x_press(nob) = rdiagbuf(4,n)
          x_time(nob) = 0
          x_obs(nob) = rdiagbuf(7,n)
          x_errorig(nob) = rdiagbuf(6,n)**2
          x_err(nob) = rdiagbuf(6,n)**2
          x_type(nob) = 'tcx'
          h_x(nob) = rdiagbuf(5,n)
          h_xnobc(nob) = rdiagbuf(5,n)
       enddo
       deallocate(cdiagbuf,rdiagbuf)
    else if (obtype == 'tcz') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(6,n) < errorlimit .or. &
             rdiagbuf(6,n) > errorlimit2)cycle
          if(abs(rdiagbuf(7,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(4,n) < 0.001_r_kind .or. &
               rdiagbuf(4,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(2,n)
          x_lon(nob) = rdiagbuf(3,n)
          x_press(nob) = rdiagbuf(4,n)
          x_time(nob) = 0
          x_obs(nob) = rdiagbuf(7,n)
          x_errorig(nob) = rdiagbuf(6,n)**2
          x_err(nob) = rdiagbuf(6,n)**2
          x_type(nob) = 'tcz'
          h_x(nob) = rdiagbuf(5,n)
          h_xnobc(nob) = rdiagbuf(5,n)
       enddo
       deallocate(cdiagbuf,rdiagbuf)
    else if (obtype == '  q') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          error=rdiagbuf(16,n)*rdiagbuf(20,n)
          if(rdiagbuf(12,n) < zero .or. error < errorlimit .or. &
             error > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)/rdiagbuf(20,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n)*rdiagbuf(20,n) > 1.e-5_r_kind) then
! normalize by qsatges
            x_errorig(nob) = (1._r_kind/(rdiagbuf(20,n)*rdiagbuf(14,n)))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
! normalize by qsatges
          x_err(nob) = (1._r_kind/(rdiagbuf(20,n)*rdiagbuf(16,n)))**2
          x_obs(nob) = rdiagbuf(17,n) !/rdiagbuf(20,n)
          h_x(nob) = (rdiagbuf(17,n)-rdiagbuf(18,n)) !/rdiagbuf(20,n)
          h_xnobc(nob) = (rdiagbuf(17,n)-rdiagbuf(18,n)) !/rdiagbuf(20,n)
          x_type(nob) = obtype 

          if (nanal <= nanals) then
            ind = 21

            ! read dHx/dx profile
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single),x_time(nob),&
                          dhx_dx, dhx(nob))

            x_obs(nob)   = x_obs(nob) /rdiagbuf(20,n)
            h_x(nob)     = h_x(nob) /rdiagbuf(20,n)
            h_xnobc(nob) = h_xnobc(nob) /rdiagbuf(20,n)
            dhx(nob)     = dhx(nob) /rdiagbuf(20,n)

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
          endif

       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = presq              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark 
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse observation error
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error
!        rdiagbuf(17,ii) = data(iqob,i)       ! observation
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis
!        rdiagbuf(19,ii) = qob-qges           ! obs-ges w/o bias correction (future slot)
!        rdiagbuf(20,ii) = qsges              ! guess saturation specific humidity
    else if (obtype == 'spd') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          !h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob) = obtype
       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1
!        rdiagbuf(17,ii) = spdob              ! wind speed observation (m/s)
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m/s)
!        rdiagbuf(19,ii) = spdob0-spdges      ! obs-ges w/o bias correction (m/s) (future slot)
!        rdiagbuf(20,ii) = factw              ! 10m wind reduction factor
     else if (obtype == 'sst') then ! skip sst
        allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
        read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        do n=1,ii
          nobdiag = nobdiag + 1
    !      if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
    !         rdiagbuf(16,n) > errorlimit2)cycle
    !      if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
    !           rdiagbuf(6,n) < 0.001_r_kind .or. &
    !           rdiagbuf(6,n) > 1200._r_kind) cycle
    !      nob = nob + 1
    !      x_code(nob) = rdiagbuf(1,n)
    !      x_lat(nob) = rdiagbuf(3,n)
    !      x_lon(nob) = rdiagbuf(4,n)
    !      x_press(nob) = rdiagbuf(6,n)
    !      x_time(nob) = rdiagbuf(8,n)
    !      if (rdiagbuf(14,n) > 1.e-5_r_kind) then
    !        x_errorig(nob) = (one/rdiagbuf(14,n))**2
    !      else
    !        x_errorig(nob) = 1.e10_r_kind
    !      endif
    !      x_err(nob) = (one/rdiagbuf(16,n))**2
    !      x_obs(nob) = rdiagbuf(17,n)
    !      h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
    !      x_type(nob) = obtype
       enddo
       deallocate(cdiagbuf,rdiagbuf)
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = rmiss_single       ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(idepth,i)     ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
!        rdiagbuf(17,ii) = data(isst,i)       ! SST observation (K)
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
!        rdiagbuf(19,ii) = data(isst,i)-sstges! obs-ges w/o bias correction (K) (future slot)
!        rdiagbuf(20,ii) = data(iotype,i)     ! type of measurement
    else if (obtype == 'srw') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob) = obtype
       enddo
       deallocate(cdiagbuf,rdiagbuf)
! radar wind superobs
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = rmiss_single       ! station elevation (meters)
!        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error
!        rdiagbuf(17,ii) = data(ihat1,i)      ! observation
!        rdiagbuf(18,ii) = d1diff             ! obs-ges used in analysis
!        rdiagbuf(19,ii) = data(ihat1,i)-srw1gesin ! obs-ges w/o bias correction (future slot)
!        rdiagbuf(20,ii) = data(ihat2,i)      ! observation
!        rdiagbuf(21,ii) = d2diff             ! obs_ges used in analysis
!        rdiagbuf(22,ii) = data(ihat2,i)-srw2gesin ! obs-ges w/o bias correction (future slot)
!        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor
!        rdiagbuf(24,ii)= data(irange,i)      ! superob mean range from radar (m)
    else if (obtype == ' rw') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob) = '  u'
       enddo
       do n=1,ii
          nob = nob + 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(20,n)
          h_x(nob) = rdiagbuf(20,n)-rdiagbuf(21,n)
          h_xnobc(nob) = rdiagbuf(20,n)-rdiagbuf(22,n)
          x_type(nob) = '  v'
       enddo
       deallocate(cdiagbuf,rdiagbuf)
! radar radial winds
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
!        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(12,ii) = -one
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1
!        rdiagbuf(17,ii) = data(irwob,i)      ! radial wind speed observation (m/s)
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m/s)
!        rdiagbuf(19,ii) = data(irwob,i)-rwwind  ! obs-ges w/o bias correction (m/s) (future slot)
!        rdiagbuf(20,ii)=data(iazm,i)*rad2deg ! azimuth angle
!        rdiagbuf(21,ii)=data(itilt,i)*rad2deg! tilt angle
!        rdiagbuf(22,ii) = factw              ! 10m wind reduction factor
    else if (obtype == 'gps') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       if (rdiagbuf(20,1)==1) errorlimit2=errorlimit2_bnd
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          end if
          x_err(nob) = (one/rdiagbuf(16,n))**2
          if (x_errorig(nob) .gt. 1.e9) x_errorig(nob)=x_err(nob)
          x_obs(nob) = rdiagbuf(17,n)

! Convert to innovation (as pointed out by Lidia)
          h_x(nob) = rdiagbuf(17,n) - (rdiagbuf(5,n)*rdiagbuf(17,n))
          h_xnobc(nob) = rdiagbuf(17,n) - (rdiagbuf(5,n)*rdiagbuf(17,n))

          x_type(nob) = obtype
          if (nanal <= nanals) then
            ind = 22

            ! read dHx/dx profile
            nnz = rdiagbuf(ind,n)
            dhx_dx%nnz = nnz
            ind = ind + 1
            nind = rdiagbuf(ind,n)
            dhx_dx%nind = nind
            ind = ind + 1

            allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
            dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1, n)
            ind = ind + nind
            dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,n)
            ind = ind + nind
            dhx_dx%val = rdiagbuf(ind:ind+nnz-1,n)

            call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single),real(x_lon(nob)*deg2rad,r_single), x_time(nob), &
                          dhx_dx, dhx(nob))

            deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
          endif


       enddo
       deallocate(cdiagbuf,rdiagbuf)
! refractivity (setupref.f90)
!    rdiagbuf(1,i)         = ictype(ikx)    ! observation type
!    rdiagbuf(2,i)         = zero           ! uses gps_ref (one=use of bending angle)
!    rdiagbuf(3,i)         = data(ilate,i)  ! lat in degrees
!    rdiagbuf(4,i)         = data(ilone,i)  ! lon in degrees
!    rdiagbuf(5,i)    = gps2work(3,i) ! incremental bending angle (x100 %)
!    rdiagbuf(6,i)         = pressure(i)    ! guess observation pressure (hPa)
!    rdiagbuf(7,i)         = elev           ! height in meters
!    rdiagbuf(8,i)         = dtime          ! obs time (hours relative to analysis time)
!    rdiagbuf(9,i)         = data(ipctc,i)  ! input bufr qc - index of per cent confidence    
!    rdiagbuf(9,i)         = elev-zsges     ! height above model terrain (m)      
!    rdiagbuf(11,i)        = data(iuse,i)   ! data usage flag
! bending angle (setupbend.f90)
!    rdiagbuf(1,i)         = ictype(ikx)     ! observation type
!    rdiagbuf(2,i)         = one             ! uses gps_ref (one = use of bending angle)
!    rdiagbuf(3,i)         = data(ilate,i)   ! lat in degrees
!    rdiagbuf(4,i)         = data(ilone,i)   ! lon in degrees
!    rdiagbuf(5,i)    = gps2work(3,i) ! incremental bending angle (x100 %)
!    rdiagbuf(6,i)         = dpressure(i)    ! guess observation pressure (hPa)
!    rdiagbuf(7,i)         = tpdpres-rocprof ! impact height in meters
!    rdiagbuf(8,i)         = dtptimes        ! obs time (hours relative to analysis time)
!    rdiagbuf(9,i)         = data(ipctc,i)   ! input bufr qc - index of per cent confidence
!    if(qcfail_loc(i) == one) rdiagbuf(10,i) = one
!    if(qcfail_high(i) == one) rdiagbuf(10,i) = two
!    if(qcfail_gross(i) == one) then
!        if(qcfail_high(i) == one) then
!           rdiagbuf(10,i) = four
!        else
!           rdiagbuf(10,i) = three
!        endif
!    else if(qcfail_stats_1(i) == one) then
!       if(qcfail_high(i) == one) then
!           rdiagbuf(10,i) = six
!        else
!           rdiagbuf(10,i) = five
!        endif
!    else if(qcfail_stats_2(i) == one) then
!       if(qcfail_high(i) == one) then
!           rdiagbuf(10,i) = eight
!        else
!           rdiagbuf(10,i) = seven
!        endif
!    end if
!    if(muse(i)) then            ! modified in genstats_gps due to toss_gps
!       rdiagbuf(12,i) = one     ! minimization usage flag (1=use, -1=not used)
!    else
!       rdiagbuf(12,i) = -one
!    endif
!     rdiagbuf(13,i) = zero !nonlinear qc relative weight - will be defined in genstats_gps
!     rdiagbuf(14,i) = errinv_input ! original inverse gps obs error (N**-1)
!     rdiagbuf(15,i) = errinv_adjst ! original + represent error inverse gps 
!                                   ! obs error (N**-1)
!     rdiagbuf(16,i) = errinv_final ! final inverse observation error due to 
!                                   ! superob factor (N**-1)
!                                   ! modified in genstats_gps
!      rdiagbuf (17,i)  = data(igps,i)  ! refractivity observation (units of N)
!      rdiagbuf (18,i)  = data(igps,i)-nrefges ! obs-ges used in analysis (units of N) 
!      rdiagbuf (19,i)  = data(igps,i)-nrefges ! obs-ges w/o bias correction (future slot)  
!    rdiagbuf(11,i)        = data(iuse,i)    ! data usage flag
!    rdiagbuf (17,i)  = data(igps,i)  ! bending angle observation (degrees)
!    rdiagbuf (18,i)  = data(igps,i)-dbend(i) ! obs-ges used in analysis (degrees)
!    rdiagbuf (19,i)  = data(igps,i)-dbend(i) ! obs-ges w/o bias correction (future slot)
    else if (obtype == ' dw') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          !h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob) = obtype
       enddo
       deallocate(cdiagbuf,rdiagbuf)
! doppler lidar winds
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = rmiss_single       ! station elevation (meters)
!        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(12,ii) = -one
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error
!        rdiagbuf(17,ii) = data(ilob,i)       ! observation
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis 
!        rdiagbuf(19,ii) = data(ilob,i)-dwwind! obs-ges w/o bias correction (future slot)
!        rdiagbuf(20,ii) = factw              ! 10m wind reduction factor
!        rdiagbuf(21,ii) = data(ielva,i)*rad2deg! elevation angle (degrees)
!        rdiagbuf(22,ii) = data(iazm,i)*rad2deg ! bearing or azimuth (degrees)
!        rdiagbuf(23,ii) = data(inls,i)         ! number of laser shots
!        rdiagbuf(24,ii) = data(incls,i)        ! number of cloud laser shots
!        rdiagbuf(25,ii) = data(iatd,i)         ! atmospheric depth
!        rdiagbuf(26,ii) = data(ilob,i)         ! line of sight component of wind orig.
    else if (obtype == ' pw') then
       allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
       read(iunit) cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       do n=1,ii
          nobdiag = nobdiag + 1
          if(rdiagbuf(12,n) < zero .or. rdiagbuf(16,n) < errorlimit .or. &
             rdiagbuf(16,n) > errorlimit2)cycle
          if(abs(rdiagbuf(17,n)) > 1.e9_r_kind  .or. &
               rdiagbuf(6,n) < 0.001_r_kind .or. &
               rdiagbuf(6,n) > 1200._r_kind) cycle
          nob = nob + 1
          x_used(nobdiag) = 1
          x_code(nob) = rdiagbuf(1,n)
          x_lat(nob) = rdiagbuf(3,n)
          x_lon(nob) = rdiagbuf(4,n)
          x_press(nob) = rdiagbuf(6,n)
          x_time(nob) = rdiagbuf(8,n)
          if (rdiagbuf(14,n) > 1.e-5_r_kind) then
            x_errorig(nob) = (one/rdiagbuf(14,n))**2
          else
            x_errorig(nob) = 1.e10_r_kind
          endif
          x_err(nob) = (one/rdiagbuf(16,n))**2
          x_obs(nob) = rdiagbuf(17,n)
          h_x(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(18,n)
          !h_xnobc(nob) = rdiagbuf(17,n)-rdiagbuf(19,n)
          x_type(nob) = obtype
       enddo
       deallocate(cdiagbuf,rdiagbuf)
! total column water
!        cdiagbuf(ii)    = station_id         ! station id
!        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
!        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
!        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
!        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
!        rdiagbuf(6,ii)  = data(iobsprs,i)    ! observation pressure (hPa)
!        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
!        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
!        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
!        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
!        rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(12,ii) = -one
!        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
!        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
!        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
!        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error
!        rdiagbuf(17,ii) = dpw                ! total precipitable water obs (kg/m**2)
!        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (kg/m**2)
!        rdiagbuf(19,ii) = dpw-pwges          ! obs-ges w/o bias correction (kg/m**2) (future slot)
    else
          print *,'warning - unknown ob type ',obtype
    end if
    go to 10
20  continue
    print *,'error reading diag_conv file'
30  continue
    close(iunit)
  enddo peloop ! ipe loop
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
subroutine write_convobs_data(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)

  character*500, intent(in) :: obspath
  character*500 obsfile,obsfile2
  character*10, intent(in) :: datestring
  character(len=10), intent(in) :: id, id2, gesid2
  character(len=4) pe_name

  real(r_single), dimension(nobs_max)  :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag) :: x_used

  character(len=3) :: obtype
  integer(i_kind) iunit, iunit2,nobs_max, nobs_maxdiag, nob, nobdiag, n, nchar, nreal, nreal_new, ii, ipe, ios, idate, mype
  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  logical fexist, init_pass

  iunit = 7
  iunit2 = 17

  nob  = 0
  nobdiag = 0
  init_pass = .true.

  obsfile2 = trim(adjustl(obspath))//"diag_conv_"//trim(adjustl(gesid2))//"."//datestring//'_'//trim(adjustl(id2))

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
  read(iunit,err=20,end=30) obtype,nchar,nreal,ii,mype
  nreal_new = nreal
  allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
  read(iunit,err=20) cdiagbuf(1:ii),rdiagbuf(1:nreal,1:ii)

  if ((obtype == '  t') .or. (obtype == ' ps') .or. (obtype == 'tcp') .or. &
      (obtype == '  q') .or. (obtype == 'spd') .or. (obtype == ' dw') .or. &
      (obtype == ' pw')) then

     nreal_new = 19
     if (obtype == 'tcp') then   !dealing with tcp being saved with x_type = 'ps'
       obtype = ' ps'
     endif

     ! defaults for not used in EnKF
     rdiagbuf(18,:) = -1.e10
     rdiagbuf(19,:) = 1.e10
     rdiagbuf(12,:) = -1        ! not used in EnKF
     ! only process if this record was used in EnKF
     do n=1,ii
        nobdiag = nobdiag + 1
        ! skip if not used in EnKF
        if (x_used(nobdiag) == 1) then
           ! update if it is used in EnKF
           nob = nob + 1
           rdiagbuf(12,n) = 1
           rdiagbuf(18,n) = x_fit(nob)
           rdiagbuf(19,n) = x_sprd(nob)
        endif
     enddo
  ! don't know what to do with gps
  else if (obtype == 'gps') then
     nreal_new = 19
     rdiagbuf(18,:) = -1.e10
     do n=1,ii
        nobdiag = nobdiag + 1
        ! skip if not used in EnKF
        if (x_used(nobdiag) == 1) then
           nob = nob + 1
        endif
     enddo
  ! special processing for u and v
  else if (obtype == ' uv') then
     nreal_new = 22
     ! defaults for not used in EnKF
     rdiagbuf(18,:) = -1.e10
     rdiagbuf(19,:) = 1.e10
     rdiagbuf(21,:) = -1.e10
     rdiagbuf(22,:) = 1.e10
     rdiagbuf(12,:) = -1
     do n=1,ii
        nobdiag = nobdiag + 1
        if (x_used(nobdiag) == 1) then
           nob = nob + 1
           rdiagbuf(12,n) = 1
           ! u should be saved first
           rdiagbuf(18,n) = x_fit(nob)
           rdiagbuf(19,n) = x_sprd(nob)
           nob = nob + 1
           rdiagbuf(21,n) = x_fit(nob)
           rdiagbuf(22,n) = x_sprd(nob)
        endif
     enddo
  ! tcx, tcy, tcz have guess in different field from the rest
  else if ((obtype == 'tcx') .or. (obtype == 'tcy') .or. (obtype == 'tcz')) then
     if (obtype == 'tcy') then   !dealing with tcy being saved with x_type = 'tcx'
       obtype = 'tcx'
     endif
     nreal_new = 5
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
  write(iunit2,err=20) obtype,nchar,nreal_new,ii,mype
  write(iunit2) cdiagbuf(1:ii),rdiagbuf(1:nreal_new,1:ii)
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
