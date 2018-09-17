program blendinc
!$$$  main program documentation block
!
! program:  blendinc
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Read NCEP GFS spectral sigma file from a file, 
!            remove mean specified from another file, add a 
!            new mean specified from a third file, and write
!            out result to a fourth file.
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!
!$$$

  USE SIGIO_MODULE
  implicit none

  include "mpif.h"

  TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO,SIGHEADI1,SIGHEADI2
  TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO,SIGDATAI1,SIGDATAI2
  character*500 filename_inc1,filename_inc2,filenamein,filenameout
  character*3 charnanal
  character(len=4) charnin
  integer nsigi,nsigo,iret,nproc,numproc,nanal,nanals,ierr
  real alpha, beta

! Initialize mpi
  call MPI_Init(ierr)

! nproc is process number, numproc is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

  if (nproc==0) call w3tagb('BLENDANAL',2011,0319,0055,'NP25')
  
  NSIGI=21
  NSIGO=61

! read data from this file
  call getarg(1,filenamein)

! add alpha times this increment
  call getarg(2,filename_inc1)

! plus beta times this increment
  call getarg(3,filename_inc2)

! and put in this file.
  call getarg(4,filenameout)

! how many ensemble members to process
  call getarg(5,charnin)
  read(charnin,'(i4)') nanals

! blending coefficients
  call getarg(6,charnin)
  read(charnin,'(i4)') alpha
  alpha = alpha/1000.
  call getarg(7,charnin)
  read(charnin,'(i4)') beta
  beta = beta/1000.
  
  if (nproc==0) then
     write(6,*)'BLENDANAL:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'filenamein=',trim(filenamein)
     write(6,*)'filename_inc1=',trim(filename_inc1)
     write(6,*)'filename_inc2=',trim(filename_inc2)
     write(6,*)'filenameout=',trim(filenameout)
     write(6,*)'ensemble members=',nanals
     write(6,*)'blending coefficients = ',alpha,beta
  endif

  nanal = nproc+1
  if (nanal.gt.nanals) then
     write (6,*) 'no files to process for mpi task = ',nproc
  else
     call sigio_srohdc(nsigi,trim(filename_inc1),  &
          sigheadi1,sigdatai1,iret)
     call sigio_srohdc(nsigi,trim(filename_inc2),  &
          sigheadi2,sigdatai2,iret)

     write(charnanal,'(i3.3)') nanal
     call sigio_srohdc(nsigi,trim(filenamein)//"_mem"//charnanal,  &
          sigheadi,sigdatai,iret)

     sigheado = sigheadi2 
     call sigio_aldata(sigheado,sigdatao,iret)

     sigdatao%hs = sigdatai%hs
     sigdatao%ps = sigdatai%ps + alpha*sigdatai1%ps + beta*sigdatai2%ps
     sigdatao%t = sigdatai%t + alpha*sigdatai1%t + beta*sigdatai2%t
     sigdatao%z = sigdatai%z + alpha*sigdatai1%z + beta*sigdatai2%z
     sigdatao%d = sigdatai%d + alpha*sigdatai1%d + beta*sigdatai2%d
     sigdatao%q = sigdatai%q + alpha*sigdatai1%q + beta*sigdatai2%q

     call sigio_swohdc(nsigo,trim(filenameout)//"_mem"//charnanal,sigheado,sigdatao,iret)
     write(6,*)'task nproc=',nproc,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret
     
  end if  ! end if mype

  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (nproc==0) call w3tage('BLENDANAL')

  call MPI_Finalize(ierr)
  if (nproc .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if

END program blendinc
