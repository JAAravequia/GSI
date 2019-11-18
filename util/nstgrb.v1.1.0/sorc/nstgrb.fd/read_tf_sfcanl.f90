subroutine read_tf_sfcanl(filename,tf,slmsk,xlats,xlons,nlat,nlon)
!$$$  main program documentation block
!
! subroutine:  read_tf_sfcanl
!
! prgmmr: Xu Li         org: noaa               date: 2019-03-13
!
! abstract:   read tf & mask from GFS FV3 SFC anal file (nemsio)
!
! program history log:
!
! attributes:
!   language: f95
!
!$$$

  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv

  implicit none

  character(len=6), intent(in) :: filename
  integer, intent(in) :: nlon,nlat
  real,    dimension(nlon,nlat), intent(out) :: tf
  integer, dimension(nlon,nlat), intent(out) :: slmsk
  real,    dimension(nlat), intent(out) :: xlats
  real,    dimension(nlon), intent(out) :: xlons
! local 
  real(4), dimension(nlon*nlat) :: rwork1d
  real, dimension(nlon,nlat) :: rwork2d

  logical :: nemsio
  integer :: iret,nrec
  integer :: lonb, latb, i, j
  integer, dimension(7):: idate

  type(nemsio_gfile) :: gfile

  nemsio=.false.

  call nemsio_init(iret)

  call nemsio_open(gfile,trim(filename),'READ',iret)
  if (iret == 0 ) then
     nemsio = .true.
  else
     write(6,*)'***ERROR*** ',trim(filename),' contains unrecognized format.  ABORT'
  endif

  if (.not.nemsio ) then
     write(6,*)'invalid sfc format'
     stop
  endif

  call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=lonb, dimy=latb, iret=iret)

  if ( lonb /= nlon .or. latb /= nlat ) then
     write(*,*) ' inconsistent dimensions, nlon, nlat = ',nlon, nlat,' vs lonb, latb = ',lonb, latb
  endif
        
  rwork1d=0.0
  rwork2d=0.0

  call nemsio_getfilehead(gfile, lon=rwork1d, iret=iret)
  rwork2d=reshape(rwork1d,(/size(rwork2d,1),size(rwork2d,2)/))
  xlons(:) = rwork2d(:,1)

  call nemsio_getfilehead(gfile, lat=rwork1d, iret=iret)
  rwork2d=reshape(rwork1d,(/size(rwork2d,1),size(rwork2d,2)/))
  do j = 1, nlat
     do i = 1, nlon
        xlats(j) = rwork2d(1,nlat+1-j)
     enddo
  enddo

  call nemsio_readrecv(gfile,'land','sfc',1,rwork1d,iret)
  rwork2d=reshape(rwork1d,(/size(slmsk,1),size(slmsk,2)/))
  do j = 1, nlat
     do i = 1, nlon
        slmsk(i,j) = int(rwork2d(i,nlat+1-j))
     enddo
  enddo

  call nemsio_readrecv(gfile,'tref', 'sfc',1,rwork1d,iret)
  rwork2d=reshape(rwork1d,(/size(tf,1),size(tf,2)/))
  do j = 1, nlat
     do i = 1, nlon
        tf(i,j) = rwork2d(i,nlat+1-j)
     enddo
  enddo

  call nemsio_close(gfile, iret)

end subroutine read_tf_sfcanl

subroutine get_sfcanl_dim(filename,nlat,nlon)
!
! subroutine:  sfcanl_dim_get
!
! prgmmr: Xu Li         org: noaa               date: 2019-03-13
!
! abstract:   get dimensions of nemsio sfcanl 
!
! program history log:
!
! attributes:
!   language: f95
!
!$$$

  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead

  implicit none

  character(len=6), intent(in) :: filename
  integer, intent(out) :: nlat,nlon

  integer :: iret,nrec
  integer, dimension(7):: idate

  logical :: nemsio
  type(nemsio_gfile) :: gfile

  nemsio=.false.

  write(*,*) 'filename : ',trim(filename)
  call nemsio_init(iret)

  call nemsio_open(gfile,trim(filename),'READ',iret)
  if (iret == 0 ) then
     nemsio = .true.
  else
     write(6,*)'***ERROR*** ',trim(filename),' contains unrecognized format.  ABORT'
  endif

  if (.not.nemsio ) then
     write(6,*)'invalid sfc format'
     stop
  endif

  call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=nlon, dimy=nlat, iret=iret)

  write(*,*) 'get_sfcanl_dim : nlat, nlon :',nlat, nlon 

  call nemsio_close(gfile, iret)

 end subroutine get_sfcanl_dim

