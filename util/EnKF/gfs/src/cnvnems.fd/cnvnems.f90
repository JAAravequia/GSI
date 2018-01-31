program cnvnems
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                           nemsio_readrec,nemsio_writerec, &
                           nemsio_readrecv,nemsio_writerecv

  implicit none

  real,parameter :: zero=0.0_4
  integer,parameter :: iunit=21

  logical :: lexist
  character(nemsio_charkind8) :: dtype
  character(nemsio_charkind8) :: outformat
  character(len=500) :: filenamein,filenameout
  character(len=16),allocatable,dimension(:) :: recnam
  integer :: iret,nlevs,ntrac,ntrunc,nanals,ngrd,k
  integer :: nrec,latb,lonb,npts,n,idrt
  integer,allocatable,dimension(:) :: reclev
  real(4),allocatable,dimension(:,:) :: rwork


  type(nemsio_gfile) :: gfile,gfileo,gfileos

! Get user input from command line
  call getarg(1,filenamein)
  call getarg(2,filenameout)
  call getarg(3,outformat)

  write(6,'(a)')  'Command line input'
  write(6,'(a,a)')' filenamein = ',trim(filenamein)
  write(6,'(a,a)')' filenameout = ',trim(filenameout)
  write(6,'(a,a)')' outformat  = ',trim(outformat)
  

  call nemsio_init(iret)
     
  call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
  if ( iret == 0 ) then
        call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
             dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
        write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
        allocate(reclev(nrec),recnam(nrec))
        call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
        call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
  else
        write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized format. ABORT!'
        stop
  endif
  write(6,'(a,i9)')' lonb    = ',lonb
  write(6,'(a,i9)')' latb    = ',latb
  write(6,'(a,i9)')' nrec    = ',nrec

  npts=lonb*latb
  allocate(rwork(npts,nrec))

  rwork = zero
  do n = 1,nrec
     call nemsio_readrec(gfile,n,rwork(:,n),iret=iret)
  enddo
  gfileo=gfile
  call nemsio_open(gfileo,trim(filenameout),'WRITE',modelname='GFS',iret=iret,gdatatype=outformat)
  do n = 1,nrec
     print *,n,trim(recnam(n)),reclev(n),minval(rwork(:,n)),maxval(rwork(:,n))
     call nemsio_writerec(gfileo,n,rwork(:,n),iret=iret)
  end do

! Deallocate structures and arrays
  call nemsio_close(gfile,iret)
  call nemsio_close(gfileo,iret)
  if (allocated(rwork)) deallocate(rwork)

end program cnvnems
