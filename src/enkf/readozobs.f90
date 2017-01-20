module readozobs
!$$$  module documentation block
!
! module: readozobs                    read ozone data from diag_sbuv2* files.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read ozone data from diag_sbuv2* files written out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_ozobs: determine the number of observations to read.
!  get_ozobs_data: read the data and calculate H(x) for ensemble members.
!  write_ozvobs_data: output diag file with spread
!   
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!   2016-11-29 shlyaeva - updated read routine to calculate linearized H(x)
!                         added write_ozvobs_data to output ensemble spread
!
! attributes:
!   language: f95
!
!$$$

use kinds, only: r_single,i_kind,r_kind,r_double
use params, only: nsats_oz,sattypes_oz,npefiles
use constants, only: deg2rad, zero
implicit none

private
public :: get_num_ozobs, get_ozobs_data, write_ozobs_data

contains

subroutine get_num_ozobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    character (len=500), intent(in) :: obspath
    character (len=10), intent(in) :: datestring
    character(len=500) obsfile
    character(len=8), intent(in) :: id
    character(len=4) pe_name
    integer(i_kind) :: nlevs  ! number of levels (layer amounts + total column) per obs   
    character(20) :: isis     ! sensor/instrument/satellite id
    character(10) :: obstype  !  type of ozone obs
    character(10) :: dplat    ! sat sensor
    real(r_single), allocatable, dimension(:) :: err,grs,pob
    real(r_single),allocatable,dimension(:,:)::diagbuf
    real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
    real(r_kind) :: errorlimit,errorlimit2
    integer(i_kind),allocatable,dimension(:,:)::idiagbuf
    integer(i_kind) iunit,jiter,ii,ireal,irdim1,ioff0,iint,idate,ios,nsat,n,k,ipe
    integer(i_kind), intent(out) :: num_obs_tot, num_obs_totdiag
    integer(i_kind), allocatable, dimension(:) :: iouse
    integer(i_kind):: nread,nkeep
    logical :: fexist, init_pass
    iunit = 7
    num_obs_tot = 0
    num_obs_totdiag = 0
!   make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
    do nsat=1,nsats_oz
        nread = 0
        nkeep = 0
        init_pass = .true.
        peloop: do ipe=0,npefiles
           write(pe_name,'(i4.4)') ipe
           if (npefiles .eq. 0) then
               ! read diag file (concatenated pe* files)
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
               inquire(file=obsfile,exist=fexist)
               if (.not. fexist .or. datestring .eq. '0000000000') &
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//trim(adjustl(id))
           else ! read raw, unconcatenated pe* files.
               obsfile =&
               trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_oz(nsat))//'_01'
           endif
           inquire(file=obsfile,exist=fexist)
           if (.not. fexist) cycle peloop
           open(iunit,form="unformatted",file=obsfile,iostat=ios)
           if (init_pass) then
              read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,irdim1,ioff0
              if(allocated(pob))deallocate(pob,grs,err,iouse)
              allocate(pob(nlevs),grs(nlevs),err(nlevs),iouse(nlevs))
              read(iunit,err=20,end=30) pob,grs,err,iouse
              init_pass = .false.
           endif
10         continue
           read(iunit,err=20,end=30) ii
           allocate(idiagbuf(iint,ii))
           allocate(diagbuf(ireal,ii))
           allocate(rdiagbuf(irdim1,nlevs,ii))
           read(iunit,err=20,end=30) idiagbuf,diagbuf,rdiagbuf
           do k=1,nlevs
             nread=nread+ii
             num_obs_totdiag = num_obs_totdiag + ii
             if (iouse(k) < 0 .or. pob(k) <= 0.001 .or. &
                 pob(k) > 1200._r_kind) cycle
             do n=1,ii
               if (rdiagbuf(3,k,n) <= errorlimit .or.  &
                   rdiagbuf(3,k,n) >= errorlimit2 .or.  &
                   abs(rdiagbuf(1,k,n)) > 1.e9_r_kind) cycle
               nkeep = nkeep + 1
               num_obs_tot = num_obs_tot + 1
             end do
           end do
           deallocate(idiagbuf,diagbuf,rdiagbuf)
           go to 10
20         continue
           print *,'error reading diag_sbuv file'
30         continue
           close(iunit)
           if (ipe .eq. npefiles) then
              write(6,100) nsat,trim(sattypes_oz(nsat)),nread,nkeep,num_obs_tot
100           format(2x,i3,2x,a20,2x,'nread= ',i9,2x,'nkeep= ',i9,2x,'num_obs_tot= ',i9)
           endif
        enddo peloop ! ipe
    enddo ! satellite
    print *,num_obs_tot,' ozone obs'
    print *,num_obs_totdiag, ' total ozone obs in diag file'
    if(allocated(pob))deallocate(pob,grs,err,iouse)
end subroutine get_num_ozobs

subroutine get_ozobs_data(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal)

  use sparsearr,only:sparr2, readarray, delete
  use params,only: nanals, lobsdiag_forenkf
  use statevec, only: state_d
  use mpisetup, only: mpi_wtime, nproc

  character*500, intent(in) :: obspath
  character*10, intent(in)  :: datestring

  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag
  real(r_single), dimension(nobs_max), intent(out)      :: hx_mean, hx_mean_nobc, hx
  real(r_single), dimension(nobs_max), intent(out)      :: x_obs
  real(r_single), dimension(nobs_max), intent(out)      :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out)      :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out)      :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out)     :: x_code
  character(len=20), dimension(nobs_max), intent(out)   ::  x_type
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used

  character(len=8), intent(in) :: id
  integer(i_kind), intent(in)  :: nanal

  character*500    :: obsfile, obsfile2
  character(len=8) :: id2
  character(len=4) :: pe_name

  integer(i_kind) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  character(20) :: isis,isis2        ! sensor/instrument/satellite id
  character(10) :: obstype,obstype2  !  type of ozone obs
  character(10) :: dplat,dplat2      ! sat sensor
  integer(i_kind) nob, nobdiag, n, ios, nsat, k
  integer(i_kind) iunit,jiter,ii,ireal,iint,irdim1,idate,ioff0
  integer(i_kind) iunit2,jiter2,ii2,ireal2,iint2,irdim12,idate2,ioff02,nlevs2
  integer(i_kind) ipe,ind

  real(r_double) t1,t2,tsum
  type(sparr2)         :: dhx_dx

  real(r_single),allocatable,dimension(:,:)::diagbuf,diagbuf2
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf,rdiagbuf2
  integer(i_kind),allocatable,dimension(:,:)::idiagbuf,idiagbuf2
  real(r_single), allocatable, dimension(:) :: err,grs,pob
  real(r_single), allocatable, dimension(:) :: err2,grs2,pob2
  integer(i_kind), allocatable, dimension(:) :: iouse,iouse2
  logical fexist, init_pass
  logical twofiles, fexist2, init_pass2
  real(r_kind) :: errorlimit,errorlimit2
 
! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

  twofiles = (.not. lobsdiag_forenkf) .and. (nanal <= nanals)
  id2 = 'ensmean'
  if (nanal <= nanals) then
     write(id2,'(a3,(i3.3))') 'mem',nanal
  endif


  tsum = 0
  iunit = 7
  iunit2 = 17
  nob = 0
  nobdiag = 0
  x_used = 0

  hx = zero

  do nsat=1,nsats_oz
      init_pass = .true.
      init_pass2 = .true.
      peloop: do ipe=0,npefiles
         write(pe_name,'(i4.4)') ipe
         if (npefiles .eq. 0) then
             ! read diag file (concatenated pe* files)
             obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
             inquire(file=obsfile,exist=fexist)
             if (.not. fexist .or. datestring .eq. '0000000000') &
             obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//trim(adjustl(id))
         else ! read raw, unconcatenated pe* files.
             obsfile =&
             trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_oz(nsat))//'_01'
         endif
         inquire(file=obsfile,exist=fexist)
         if (.not. fexist) then 
            cycle peloop
         endif
         open(iunit,form="unformatted",file=obsfile,iostat=ios)
         rewind(iunit)
         if (init_pass) then
            read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,irdim1,ioff0
            if(allocated(pob))deallocate(pob,grs,err,iouse)
            allocate(pob(nlevs),grs(nlevs),err(nlevs),iouse(nlevs))
            read(iunit,err=20,end=30) pob,grs,err,iouse
            init_pass = .false.
         endif
         if(twofiles)then
           if (npefiles .eq. 0) then
               ! read diag file (concatenated pe* files)
               obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//datestring//'_'//trim(adjustl(id2))
               inquire(file=obsfile2,exist=fexist2)
               if (.not. fexist2 .or. datestring .eq. '0000000000') &
               obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//trim(adjustl(id2))
           else ! read raw, unconcatenated pe* files.
               obsfile2 =&
               trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id2))//'/pe'//pe_name//'.'//trim(sattypes_oz(nsat))//'_01'
           endif
           open(iunit2,form="unformatted",file=obsfile2,iostat=ios)
           rewind(iunit2)
           if (init_pass2) then
              read(iunit2,err=20,end=30) isis2,dplat2,obstype2,jiter2,nlevs2,idate2,iint2,ireal2,irdim12,ioff02
              if(isis /= isis2 .or. dplat /= dplat2 .or. obstype /= obstype2 .or. jiter /= jiter2 .or. &
                 nlevs /= nlevs2 .or. idate /= idate2 .or. iint /= iint2 .or. ireal /= ireal2)then
                 write(6,*) 'inconsistency in ozone files'
                 write(6,*) 'isis',isis,isis2
                 write(6,*) 'dplat',dplat,dplat2
                 write(6,*) 'obstype',obstype,obstype2
                 write(6,*) 'jiter',jiter,jiter2
                 write(6,*) 'nlevs',nlevs,nlevs2
                 write(6,*) 'idate',idate,idate2
                 write(6,*) 'iint',iint,iint2
                 write(6,*) 'ireal',ireal,ireal2
                 call stop2(66)
              end if
              if (allocated(pob2)) deallocate(pob2,err2,grs2,iouse2)
              allocate(pob2(nlevs),grs2(nlevs),err2(nlevs),iouse2(nlevs))
              read(iunit2,err=20,end=30) pob2,grs2,err2,iouse2
              do k=1,nlevs
                if(pob(k) /= pob2(k) .or. grs(k) /= grs2(k) .or. err(k) /= err2(k) .or. &
                   iouse(k) /= iouse2(k))then
                   write(6,*) ' ozone file vertical inconsistency level = ',k
                   write(6,*) 'pob',pob(k),pob2(k)
                   write(6,*) 'grs',grs(k),grs2(k)
                   write(6,*) 'err',err(k),err2(k)
                   write(6,*) 'iouse',iouse(k),iouse2(k)
                   call stop2(67)
                 end if
              end do
              init_pass2 = .false.
           endif
         end if

10       continue
         read(iunit,err=20,end=30) ii
         allocate(idiagbuf(iint,ii))
         allocate(diagbuf(ireal,ii))
         allocate(rdiagbuf(irdim1,nlevs,ii))
         read(iunit,err=20,end=30) idiagbuf,diagbuf,rdiagbuf
         if (twofiles) then
            read(iunit2,err=20,end=30) ii2
            if(ii /= ii2)then
               write(6,*) 'ii inconsistency in ozone ',ii,ii2
               call stop2(68)
            end if
            allocate(idiagbuf2(iint,ii), diagbuf2(ireal,ii),rdiagbuf2(irdim1,nlevs,ii))
            read(iunit2,err=20,end=30) idiagbuf2,diagbuf2,rdiagbuf2
         endif
         do k=1,nlevs
           if (iouse(k) < 0 .or. pob(k) <= 0.001 .or. &
               pob(k) > 1200._r_kind) then
              nobdiag = nobdiag + ii
              cycle
           endif
           do n=1,ii
             if(twofiles .and.                                &
                (diagbuf(1,n) /= diagbuf2(1,n) .or. diagbuf(2,n) /=diagbuf2(2,n)))then
                write(6,*) 'lat lon inconsistency in ozone '
                write(6,*) 'lat',diagbuf(1,n),diagbuf2(1,n)
                write(6,*) 'lon',diagbuf(2,n),diagbuf2(2,n)
             end if

             nobdiag = nobdiag + 1
             if (rdiagbuf(3,k,n) <= errorlimit .or.  &
                 rdiagbuf(3,k,n) >= errorlimit2 .or.  &
                 abs(rdiagbuf(1,k,n)) > 1.e9_r_kind) cycle
             nob = nob + 1
             x_used(nobdiag) = 1
             x_code(nob) = 700 + k ! made up code for ozone level k
             x_lat(nob) = diagbuf(1,n)
             x_lon(nob) = diagbuf(2,n)
             x_press(nob) = pob(k)
             x_time(nob) = diagbuf(3,n)
             x_err(nob) = (1./rdiagbuf(3,k,n))**2
             x_errorig(nob) = x_err(nob)
             x_obs(nob) = rdiagbuf(1,k,n)
             hx_mean(nob) = rdiagbuf(1,k,n)-rdiagbuf(2,k,n)
             hx_mean_nobc(nob) = rdiagbuf(1,k,n)-rdiagbuf(2,k,n)
             x_type(nob) = ' oz                 '
             if (nanal <= nanals) then
               ! read full Hx from diag file
               if (.not. lobsdiag_forenkf) then
                  hx(nob) = rdiagbuf(1,k,n)-rdiagbuf2(2,k,n)
               ! run linearized Hx 
               else
                  ind = ioff0 + 1
                  ! read dHx/dx profile
                  call readarray(dhx_dx, rdiagbuf(ind:irdim1,k,n))
   
                  t1 = mpi_wtime()
                  call observer(hx_mean_nobc(nob), state_d,                  &
                             real(x_lat(nob)*deg2rad,r_single),         & 
                             real(x_lon(nob)*deg2rad,r_single),         &
                             x_time(nob),                               &
                             dhx_dx, hx(nob))
                  t2 = mpi_wtime()
                  tsum = tsum + t2-t1

                  call delete(dhx_dx)
               endif
             endif

           end do ! nn
         end do ! k
         deallocate(idiagbuf,diagbuf,rdiagbuf)
         if (twofiles) deallocate(idiagbuf2,diagbuf2,rdiagbuf2)
         go to 10
20       continue
         print *,'error reading diag_sbuv file'
30       continue
         close(iunit)
         if(twofiles) close(iunit2)
      enddo peloop ! ipe
  enddo ! satellite
  if (nanal == nanals) print *,'time in observer for oz obs on proc',nproc,' = ',tsum

  if (nob /= nobs_max) then
      print *,'number of obs not what expected in get_ozobs_data',nob,nobs_max
      call stop2(93)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_ozobs_data',nobdiag,nobs_maxdiag
      call stop2(93)
  end if

  if(allocated(pob))deallocate(pob,grs,err,iouse)
  if(allocated(pob2))deallocate(pob2,grs2,err2,iouse2)

 end subroutine get_ozobs_data

subroutine write_ozobs_data(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)

  character*500, intent(in) :: obspath
  character*500 obsfile,obsfile2
  character*10, intent(in) :: datestring
  character(len=8), intent(in) :: id,id2,gesid2
  character(len=4) pe_name

  integer(i_kind) :: nlevs  ! number of levels (layer amounts + total column) per obs
  character(20) :: isis     ! sensor/instrument/satellite id
  character(10) :: obstype  !  type of ozone obs
  character(10) :: dplat    ! sat sensor
  integer(i_kind) iunit,jiter,ii,ireal,iint,irdim1,idate,nob,nobdiag,n,ios,nobs_max,nobs_maxdiag,nsat,k,ipe,ioff0
  integer(i_kind) iunit2
  real(r_single), dimension(nobs_max) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag) :: x_used

  real(r_single),allocatable,dimension(:,:)::diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
  integer(i_kind),allocatable,dimension(:,:)::idiagbuf
  real(r_single), allocatable, dimension(:) :: err,grs,pob
  integer(i_kind), allocatable, dimension(:) :: iouse
  logical fexist, init_pass
 
  iunit = 7
  iunit2 = 17
  nob = 0
  nobdiag = 0

  do nsat=1,nsats_oz
      init_pass = .true.
      obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_"//trim(adjustl(gesid2))//"."//datestring//'_'//trim(adjustl(id2))
      peloop: do ipe=0,npefiles
         write(pe_name,'(i4.4)') ipe
         if (npefiles .eq. 0) then
            ! diag file (concatenated pe* files)
            obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
            inquire(file=obsfile,exist=fexist)
            if (.not. fexist .or. datestring .eq. '0000000000') then
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_oz(nsat))//"_ges."//trim(adjustl(id))
            endif
         else ! raw, unconcatenated pe* files.
            obsfile = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_oz(nsat))//'_01'
         endif
         inquire(file=obsfile,exist=fexist)
         if (.not. fexist) cycle peloop
         open(iunit,form="unformatted",file=obsfile,iostat=ios)
         rewind(iunit)
         if (init_pass) then
            open(iunit2,form="unformatted",file=obsfile2,iostat=ios)
            read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,irdim1,ioff0
            write(iunit2,err=20)      isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,irdim1,ioff0
            if(allocated(pob))deallocate(pob,grs,err,iouse)
            allocate(pob(nlevs),grs(nlevs),err(nlevs),iouse(nlevs))
            read(iunit,err=20,end=30) pob,grs,err,iouse
            write(iunit2,err=20) pob,grs,err,iouse
            init_pass = .false.
         endif
10       continue
         read(iunit,err=20,end=30) ii
         allocate(idiagbuf(iint,ii))
         allocate(diagbuf(ireal,ii))
         allocate(rdiagbuf(irdim1,nlevs,ii))
         read(iunit,err=20,end=30) idiagbuf,diagbuf,rdiagbuf
         rdiagbuf(2,:,:) = 1.e10
         do k=1,nlevs
            do n=1,ii
               nobdiag = nobdiag + 1
               if (x_used(nobdiag) == 1) then
                  nob = nob + 1
                  rdiagbuf(2,k,n) = x_fit(nob)
                  rdiagbuf(7,k,n) = x_sprd(nob)
               endif
            enddo 
         enddo
         write(iunit2) ii
         write(iunit2) idiagbuf,diagbuf,rdiagbuf
         deallocate(idiagbuf,diagbuf,rdiagbuf)
         go to 10
20       continue
         print *,'error reading diag_oz file'
30       continue
         close(iunit)
      enddo peloop ! ipe
    close(iunit2)
  enddo ! satellite

  if (nob /= nobs_max) then
      print *,'number of obs not what expected in write_ozobs_data',nob,nobs_max
      call stop2(93)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in write_ozobs_data',nobdiag,nobs_maxdiag
      call stop2(93)
  end if

  if(allocated(pob))deallocate(pob,grs,err,iouse)

end subroutine write_ozobs_data

end module readozobs
