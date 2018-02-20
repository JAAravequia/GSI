program cnvnems
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                           nemsio_readrec,nemsio_writerec, &
                           nemsio_readrecv,nemsio_writerecv

  implicit none

  real,parameter :: zero=0.0_4
  integer,parameter :: iunit=21

  character(nemsio_charkind8) :: dtype
  character(nemsio_charkind8) :: outformat
  character(len=500) :: filenamein,filenameout,fileprefix,datapath
  character(len=16),allocatable,dimension(:) :: recnam
  integer :: iret,nlevs,ntrac,ntrunc,nanals,k
  integer :: nrec,latb,lonb,npts,n
  integer,allocatable,dimension(:) :: reclev
  real(4),allocatable,dimension(:,:) :: rwork
  real(8) :: rnanals
  character(len=3) :: charnanal
  integer,allocatable,dimension(:) :: new_group_members

  integer :: mype,mype1,npe,orig_group,new_group,new_comm


  type(nemsio_gfile) :: gfile,gfileo

! mpi definitions.
  include 'mpif.h'

! Initialize mpi, mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call mpi_comm_rank(mpi_comm_world,mype,iret)
  call mpi_comm_size(mpi_comm_world,npe,iret)

  mype1 = mype + 1

  if ( mype == 0 ) call w3tagb('CNVNEMSP',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,fileprefix)
  call getarg(3,charnanal)
  read(charnanal,'(i3)') nanals
  call getarg(4,outformat)

  rnanals = nanals
  rnanals = 1.0_8/rnanals
  filenameout = trim(adjustl(datapath)) // trim(adjustl(filenameout))

  if ( mype == 0 ) then
     write(6,'(a)')  'Command line input'
     write(6,'(a,a)')' datapath    = ',trim(datapath)
     write(6,'(a,a)')' filenameout = ',trim(filenameout)
     write(6,'(a,a)')' fileprefix  = ',trim(fileprefix)
     write(6,'(a,a)')' nanals      = ',trim(charnanal)
     write(6,'(a,a)')' outformat  = ',trim(outformat)
     write(6,'(a)')  ' '
  endif
  
  if ( npe < nanals ) then
     write(6,'(2(a,i4))')'***ERROR***  npe too small.  npe = ',npe,' < nanals = ',nanals
     call mpi_abort(mpi_comm_world,99,iret)
     stop
  end if

! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do

  call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if ( iret /= 0 ) then
     write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

! Process input files (one file per task)
  if ( mype1 <= nanals ) then

  if ( mype1 == 1 ) then
     filenamein = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_ensmean'
     filenameout = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_ensmean' //'.'//outformat
  else
     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_mem' // charnanal
     filenameout = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_mem' // charnanal //'.'//outformat
  endif

  call nemsio_init(iret)
     
  call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
  if ( iret == 0 ) then
        call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
             dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
        write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
        if (.not. allocated(reclev)) allocate(reclev(nrec))
        if (.not. allocated(recnam)) allocate(recnam(nrec))
        call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
        call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
  else
        write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized format. ABORT!'
        if (mype .ne. 0) then
           ! if ens mean missing, don't abort
           call mpi_abort(mpi_comm_world,99,iret)
           stop
        endif
  endif
  if (mype == 0 .and. iret == 0) then
  write(6,'(a,i9)')' lonb    = ',lonb
  write(6,'(a,i9)')' latb    = ',latb
  write(6,'(a,i9)')' nrec    = ',nrec
  endif

  npts=lonb*latb
  if (.not. allocated(rwork)) allocate(rwork(npts,nrec))

  if (iret == 0) then
     rwork = zero
     do n = 1,nrec
        call nemsio_readrec(gfile,n,rwork(:,n),iret=iret)
     enddo
     gfileo=gfile
     call nemsio_open(gfileo,trim(filenameout),'WRITE',modelname='GFS',iret=iret,gdatatype=outformat)
     do n = 1,nrec
        ! skip microphysics vars to save space (not needed for replay)
        if (trim(recnam(n)) .ne. 'clwmr' .and. trim(recnam(n)) .ne. 'icmr') then
        if (mype == 0) print *,n,trim(recnam(n)),reclev(n),minval(rwork(:,n)),maxval(rwork(:,n))
        call nemsio_writerec(gfileo,n,rwork(:,n),iret=iret)
        endif
     end do
     call nemsio_close(gfileo,iret)
  endif
  call nemsio_close(gfile,iret)

  if (mype == 0) then
  ! if nanal=1, do ensemble mean and then member 1
  write(charnanal,'(i3.3)') mype1
  filenamein = trim(adjustl(datapath)) // &
       trim(adjustl(fileprefix)) // '_mem' // charnanal
  filenameout = trim(adjustl(datapath)) // &
       trim(adjustl(fileprefix)) // '_mem' // charnanal //'.'//outformat
  call nemsio_init(iret)
  call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
  if ( iret == 0 ) then
        call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
             dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
        write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
        if (.not. allocated(reclev)) allocate(reclev(nrec))
        if (.not. allocated(recnam)) allocate(recnam(nrec))
        call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
        call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
  else
        write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized format. ABORT!'
        call mpi_abort(mpi_comm_world,99,iret)
        stop
  endif
  rwork = zero
  do n = 1,nrec
     call nemsio_readrec(gfile,n,rwork(:,n),iret=iret)
  enddo
  gfileo=gfile
  call nemsio_open(gfileo,trim(filenameout),'WRITE',modelname='GFS',iret=iret,gdatatype=outformat)
  do n = 1,nrec
     call nemsio_writerec(gfileo,n,rwork(:,n),iret=iret)
  end do
  call nemsio_close(gfile,iret)
  call nemsio_close(gfileo,iret)
  endif

! Deallocate structures and arrays
  if (allocated(rwork)) deallocate(rwork)
  if (allocated(reclev)) deallocate(reclev)
  if (allocated(recnam)) deallocate(recnam)

! Jump here if more mpi processors than files to process
  else
     write(6,'(a,i5)') 'No files to process for mpi task = ',mype
  endif

  call mpi_barrier(mpi_comm_world,iret)
  
  if ( mype == 0 ) call w3tage('CNVNEMSP')
  
 deallocate(new_group_members)
 
 call mpi_finalize(iret)

end program cnvnems
