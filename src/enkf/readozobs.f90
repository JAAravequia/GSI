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

use kinds, only: r_single,i_kind,r_kind
use params, only: nsats_oz,sattypes_oz,npefiles
use constants, only: deg2rad  
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
    integer(i_kind) iunit,jiter,ii,ireal,iint,nreal,idate,ios,nsat,n,k,ipe
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
              read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,nreal
              if(allocated(pob))deallocate(pob,grs,err,iouse)
              allocate(pob(nlevs),grs(nlevs),err(nlevs),iouse(nlevs))
              read(iunit,err=20,end=30) pob,grs,err,iouse
              init_pass = .false.
           endif
10         continue
           read(iunit,err=20,end=30) ii
           allocate(idiagbuf(iint,ii))
           allocate(diagbuf(ireal,ii))
           allocate(rdiagbuf(nreal,nlevs,ii))
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

subroutine get_ozobs_data(obspath, datestring, nobs_max, nobs_maxdiag, h_x, h_xnobc, dhx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type, x_used, id, nanal)

use sparsearr,only:sparr2
use params,only: nanals
use statevec, only: state_d

  character*500, intent(in) :: obspath
  character*500 obsfile
  character*10, intent(in) :: datestring
  character(len=8), intent(in) :: id
  integer(i_kind),intent(in) :: nanal
  character(len=4) pe_name

  integer(i_kind) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  character(20) :: isis     ! sensor/instrument/satellite id
  character(10) :: obstype  !  type of ozone obs
  character(10) :: dplat    ! sat sensor
  integer(i_kind) iunit,jiter,ii,ireal,iint,nreal,idate,nob,nobdiag,n,ios,nobs_max,nobs_maxdiag,nsat,k
  integer(i_kind) ipe,ind,nnz,nind,ioff0

  real(r_single), dimension(nobs_max) :: h_x,h_xnobc,x_obs,x_err,x_lon,&
                               x_lat,x_press,x_time,x_errorig,dhx
  type(sparr2)         :: dhx_dx
  integer(i_kind), dimension(nobs_maxdiag) :: x_used
  integer(i_kind), dimension(nobs_max) :: x_code
  character(len=20), dimension(nobs_max) ::  x_type


  real(r_single),allocatable,dimension(:,:)::diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
  integer(i_kind),allocatable,dimension(:,:)::idiagbuf
  real(r_single), allocatable, dimension(:) :: err,grs,pob
  integer(i_kind), allocatable, dimension(:) :: iouse
  logical fexist, init_pass
  real(r_kind) :: errorlimit,errorlimit2
 
! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

  iunit = 7
  nob = 0
  nobdiag = 0
  x_used = 0

  do nsat=1,nsats_oz
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
         rewind(iunit)
         if (init_pass) then
            read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,nreal,ioff0
            if(allocated(pob))deallocate(pob,grs,err,iouse)
            allocate(pob(nlevs),grs(nlevs),err(nlevs),iouse(nlevs))
            read(iunit,err=20,end=30) pob,grs,err,iouse
            init_pass = .false.
         endif
10       continue
         read(iunit,err=20,end=30) ii
         allocate(idiagbuf(iint,ii))
         allocate(diagbuf(ireal,ii))
         allocate(rdiagbuf(nreal,nlevs,ii))
         read(iunit,err=20,end=30) idiagbuf,diagbuf,rdiagbuf
         do k=1,nlevs
           if (iouse(k) < 0 .or. pob(k) <= 0.001 .or. &
               pob(k) > 1200._r_kind) then
              nobdiag = nobdiag + ii
              cycle
           endif
           do n=1,ii
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
             h_x(nob) = rdiagbuf(1,k,n)-rdiagbuf(2,k,n)
             h_xnobc(nob) = rdiagbuf(1,k,n)-rdiagbuf(2,k,n)
             x_type(nob) = ' oz                 '
             if (nanal <= nanals) then
               ind = ioff0
               ! read dHx/dx profile
               nnz = rdiagbuf(ind,k,n)
               dhx_dx%nnz = nnz
               ind = ind + 1
               nind = rdiagbuf(ind,k,n)
               dhx_dx%nind = nind
               ind = ind + 1

               allocate(dhx_dx%val(nnz), dhx_dx%st_ind(nind), dhx_dx%end_ind(nind))
               dhx_dx%st_ind = rdiagbuf(ind:ind+nind-1,k,n)
               ind = ind + nind
               dhx_dx%end_ind = rdiagbuf(ind:ind+nind-1,k,n)
               ind = ind + nind
               dhx_dx%val = rdiagbuf(ind:ind+nnz-1,k,n)

               call observer(h_xnobc(nob), state_d,                  &
                          real(x_lat(nob)*deg2rad,r_single), real(x_lon(nob)*deg2rad,r_single), x_time(nob), &
                          dhx_dx, dhx(nob))

               deallocate(dhx_dx%val, dhx_dx%st_ind, dhx_dx%end_ind)
             endif

           end do ! nn
         end do ! k
         deallocate(idiagbuf,diagbuf,rdiagbuf)
         go to 10
20       continue
         print *,'error reading diag_sbuv file'
30       continue
         close(iunit)
      enddo peloop ! ipe
  enddo ! satellite

  if (nob /= nobs_max) then
      print *,'number of obs not what expected in get_ozobs_data',nob,nobs_max
      call stop2(93)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_ozobs_data',nobdiag,nobs_maxdiag
      call stop2(93)
  end if

  if(allocated(pob))deallocate(pob,grs,err,iouse)

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
  integer(i_kind) iunit,jiter,ii,ireal,iint,nreal,idate,nob,nobdiag,n,ios,nobs_max,nobs_maxdiag,nsat,k,ipe
  integer(i_kind) iunit2
  real(r_single), dimension(nobs_max) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag) :: x_used
  integer(i_kind), dimension(:,:,:), allocatable :: x_filelim

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
            read(iunit,err=20,end=30) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,nreal
            write(iunit2,err=20) isis,dplat,obstype,jiter,nlevs,idate,iint,ireal,nreal
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
         allocate(rdiagbuf(6,nlevs,ii))
         read(iunit,err=20,end=30) idiagbuf,diagbuf,rdiagbuf
         rdiagbuf(2,:,:) = 1.e10
         do k=1,nlevs
            do n=1,ii
               nobdiag = nobdiag + 1
               if (x_used(nobdiag) == 1) then
                  nob = nob + 1
                  rdiagbuf(2,k,n) = x_fit(nob)
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
