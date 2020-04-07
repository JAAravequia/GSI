module mpi_readobs
!$$$  module documentation block
!
! module: mpi_readobs                  read obs, ob priors and associated
!                                      metadata if called from root task, 
!                                      otherwise receive data from root task.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:
!
! Public Subroutines:
!  mpi_readobs: called by subroutine readobs in module enkf_obsmod. 
!   Read obs, ob priors and metadata from diag* files
!   created by GSI forward operator code and broadcast to all tasks.
!   
! Public Variables: None
!
! Modules Used:
!  readsatobs: to read satellite radiance diag* files.
!  readconvobs: to read diag_conv* files (obs from prepbufr file).
!  readozobs: to read diag_sbuv* ozone files.
!  mpisetup
!
! program history log:
!   2009-02-23  Initial version.
!   2016-11-29  shlyaeva: Added the option of writing out ensemble spread in
!               diag files
!
! attributes:
!   language: f95
!
!$$$
  
use kinds, only: r_kind, r_single, i_kind, r_double
use params, only: ntasks_io, nanals_per_iotask, nanal1, nanal2
use radinfo, only: npred
use readconvobs
use readsatobs
use readozobs
use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max,mpi_realkind,&
                mpi_min,numproc_shm,mpi_comm_shmem,mpi_info_null,nproc_shm,&
                mpi_comm_shmemroot,mpi_mode_nocheck,mpi_lock_exclusive,&
                mpi_address_kind
use, intrinsic :: iso_c_binding
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single

implicit none

private
public :: mpi_getobs

contains

subroutine mpi_getobs(obspath, datestring, nobs_conv, nobs_oz, nobs_sat, nobs_tot, &
                      nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag, &
                      sprd_ob, ensmean_ob, ob, &
                      oberr, oblon, oblat, obpress, &
                      obtime, oberrorig, obcode, obtype, &
                      biaspreds, diagused,  anal_ob, anal_ob_modens, anal_ob_cp, anal_ob_modens_cp, &
                      shm_win, shm_win2, indxsat, nanals, neigv)
    character*500, intent(in) :: obspath
    character*10, intent(in) :: datestring
    character(len=10) :: id
    real(r_single), allocatable, dimension(:)   :: ensmean_ob,ob,oberr,oblon,oblat
    real(r_single), allocatable, dimension(:)   :: obpress,obtime,oberrorig,sprd_ob
    integer(i_kind), allocatable, dimension(:)  :: obcode,indxsat
    integer(i_kind), allocatable, dimension(:)  :: diagused
    real(r_single), allocatable, dimension(:,:) :: biaspreds
    ! pointers used for MPI-3 shared memory manipulations.
    real(r_single), pointer, dimension(:,:)     :: anal_ob, anal_ob_modens
    type(c_ptr) anal_ob_cp, anal_ob_modens_cp
    integer shm_win, shm_win2
    real(r_single), allocatable, dimension(:)   :: mem_ob
    real(r_single), allocatable, dimension(:,:) :: mem_ob_modens
    real(r_single) :: analsim1
    real(r_double) t1,t2
    character(len=20), allocatable,  dimension(:) ::  obtype
    integer(i_kind) nob, ierr, iozproc, isatproc, neig, nens1, nens2, na, nmem,&
            np, nobs_conv, nobs_oz, nobs_sat, nobs_tot, nanal, nanalo, nens
    integer(i_kind) :: nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag
    integer(i_kind), intent(in) :: nanals, neigv

    integer disp_unit
    integer(MPI_ADDRESS_KIND) :: win_size, nsize, nsize2, win_size2
    integer(MPI_ADDRESS_KIND) :: segment_size

    iozproc=max(0,min(1,numproc-1))
    isatproc=max(0,min(2,numproc-2))
! get total number of conventional and sat obs for ensmean.
    id = 'ensmean'
    if(nproc == 0)call get_num_convobs(obspath,datestring,nobs_conv,nobs_convdiag,id)
    if(nproc == iozproc)call get_num_ozobs(obspath,datestring,nobs_oz,nobs_ozdiag,id)
    if(nproc == isatproc)call get_num_satobs(obspath,datestring,nobs_sat,nobs_satdiag,id)
    call mpi_bcast(nobs_conv,1,mpi_integer,0,mpi_comm_world,ierr)
    call mpi_bcast(nobs_convdiag,1,mpi_integer,0,mpi_comm_world,ierr)
    call mpi_bcast(nobs_oz,1,mpi_integer,iozproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_ozdiag,1,mpi_integer,iozproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_sat,1,mpi_integer,isatproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_satdiag,1,mpi_integer,isatproc,mpi_comm_world,ierr)
    if(nproc == 0)print *,'nobs_conv, nobs_oz, nobs_sat = ',nobs_conv,nobs_oz,nobs_sat
    if(nproc == 0)print *,'total diag nobs_conv, nobs_oz, nobs_sat = ', nobs_convdiag, nobs_ozdiag, nobs_satdiag
    nobs_tot = nobs_conv + nobs_oz + nobs_sat
    nobs_totdiag = nobs_convdiag + nobs_ozdiag + nobs_satdiag
    if (neigv > 0) then
       nens = nanals*neigv ! modulated ensemble size
    else
       nens = nanals
    endif
! if nobs_tot != 0 (there were some obs to read)
    if (nobs_tot > 0) then
       ! these arrays needed on all processors.
       allocate(mem_ob(nobs_tot)) 
       allocate(mem_ob_modens(neigv,nobs_tot))  ! zero size if neigv=0
       allocate(sprd_ob(nobs_tot),ob(nobs_tot),oberr(nobs_tot),oblon(nobs_tot),&
       oblat(nobs_tot),obpress(nobs_tot),obtime(nobs_tot),oberrorig(nobs_tot),obcode(nobs_tot),&
       obtype(nobs_tot),ensmean_ob(nobs_tot),&
       biaspreds(npred+1, nobs_sat),indxsat(nobs_sat), diagused(nobs_totdiag))
    else
! stop if no obs found (must be an error somewhere).
       print *,'no obs found!'
       call stop2(11)
    end if

! setup shared memory segment on each node that points to
! observation prior ensemble.
! shared window size will be zero except on root task of
! shared memory group on each node.
    disp_unit = num_bytes_for_r_single ! anal_ob is r_single
    nsize = nobs_tot*nanals
    nsize2 = nobs_tot*nanals*neigv
    if (nproc_shm == 0) then
       win_size = nsize*disp_unit
       win_size2 = nsize2*disp_unit
    else
       win_size = 0
       win_size2 = 0
    endif
    call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                                 mpi_comm_shmem, anal_ob_cp, shm_win, ierr)
    if (neigv > 0) then
       call MPI_Win_allocate_shared(win_size2, disp_unit, MPI_INFO_NULL,&
                                    mpi_comm_shmem, anal_ob_modens_cp, shm_win2, ierr)
    endif
    if (nproc_shm == 0) then
       ! create shared memory segment on each shared mem comm
       call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win,ierr)
       call MPI_Win_unlock(0, shm_win, ierr)
       if (neigv > 0) then
          call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win2,ierr)
          call MPI_Win_unlock(0, shm_win2, ierr)
       endif
    endif
    ! barrier here to make sure no tasks try to access shared
    ! memory segment before it is created.
    call mpi_barrier(mpi_comm_world, ierr)
    ! associate fortran pointer with c pointer to shared memory 
    ! segment (containing observation prior ensemble) on each task.
    call MPI_Win_shared_query(shm_win, 0, segment_size, disp_unit, anal_ob_cp, ierr)
    call c_f_pointer(anal_ob_cp, anal_ob, [nanals, nobs_tot])
    if (neigv > 0) then
       call MPI_Win_shared_query(shm_win2, 0, segment_size, disp_unit, anal_ob_modens_cp, ierr)
       call c_f_pointer(anal_ob_modens_cp, anal_ob_modens, [nens, nobs_tot])
    endif

! read ensemble mean and every ensemble member
    if (nproc <= ntasks_io-1) then
        nens1 = nanal1(nproc); nens2 = nanal2(nproc)
    else
        nens1 = nanals+1; nens2 = nanals+1
    endif

    id = 'ensmean'

    nmem = 0
    do nanal=nens1,nens2 ! loop over ens members on this task
    nmem = nmem + 1 
! read obs.
! only thing that is different on each task is mem_ob.  All other
! fields are defined from ensemble mean.
! individual members read on 1st nanals tasks, ens mean read on all tasks.
    if (nobs_conv > 0) then
! first nobs_conv are conventional obs.
      call get_convobs_data(obspath, datestring, nobs_conv, nobs_convdiag, &
        ensmean_ob(1:nobs_conv),                                           &
        mem_ob(1:nobs_conv), mem_ob_modens(1:neigv,1:nobs_conv),           &
        ob(1:nobs_conv),                                                   &
        oberr(1:nobs_conv), oblon(1:nobs_conv), oblat(1:nobs_conv),        &
        obpress(1:nobs_conv), obtime(1:nobs_conv), obcode(1:nobs_conv),    &
        oberrorig(1:nobs_conv), obtype(1:nobs_conv),                       &
        diagused(1:nobs_convdiag), id, nanal, nmem)
    end if
    if (nobs_oz > 0) then
! second nobs_oz are conventional obs.
      call get_ozobs_data(obspath, datestring, nobs_oz, nobs_ozdiag,  &
        ensmean_ob(nobs_conv+1:nobs_conv+nobs_oz),                    &
        mem_ob(nobs_conv+1:nobs_conv+nobs_oz),                        &
        mem_ob_modens(1:neigv,nobs_conv+1:nobs_conv+nobs_oz),         &
        ob(nobs_conv+1:nobs_conv+nobs_oz),               &
        oberr(nobs_conv+1:nobs_conv+nobs_oz),            &
        oblon(nobs_conv+1:nobs_conv+nobs_oz),            &
        oblat(nobs_conv+1:nobs_conv+nobs_oz),            &
        obpress(nobs_conv+1:nobs_conv+nobs_oz),          &
        obtime(nobs_conv+1:nobs_conv+nobs_oz),           &
        obcode(nobs_conv+1:nobs_conv+nobs_oz),           &
        oberrorig(nobs_conv+1:nobs_conv+nobs_oz),        &
        obtype(nobs_conv+1:nobs_conv+nobs_oz),           &
        diagused(nobs_convdiag+1:nobs_convdiag+nobs_ozdiag),&
        id,nanal,nmem)
    end if
    if (nobs_sat > 0) then
      biaspreds = 0. ! initialize bias predictor array to zero.
! last nobs_sat are satellite radiance obs.
      call get_satobs_data(obspath, datestring, nobs_sat, nobs_satdiag, &
        ensmean_ob(nobs_conv+nobs_oz+1:nobs_tot),         &
        mem_ob(nobs_conv+nobs_oz+1:nobs_tot),                &
        mem_ob_modens(1:neigv,nobs_conv+nobs_oz+1:nobs_tot),            &
        ob(nobs_conv+nobs_oz+1:nobs_tot),                 &
        oberr(nobs_conv+nobs_oz+1:nobs_tot),              &
        oblon(nobs_conv+nobs_oz+1:nobs_tot),              &
        oblat(nobs_conv+nobs_oz+1:nobs_tot),              &
        obpress(nobs_conv+nobs_oz+1:nobs_tot),            &
        obtime(nobs_conv+nobs_oz+1:nobs_tot),             &
        obcode(nobs_conv+nobs_oz+1:nobs_tot),             &
        oberrorig(nobs_conv+nobs_oz+1:nobs_tot),          &
        obtype(nobs_conv+nobs_oz+1:nobs_tot),             &
        biaspreds,indxsat,                                &
        diagused(nobs_convdiag+nobs_ozdiag+1:nobs_totdiag),&
        id,nanal,nmem)
    end if ! read obs.

! use mpi_send/mpi_recv to gather ob prior ensemble on root.
! a bit slower, but does not require large temporary array like mpi_gather.
    if (nproc <= ntasks_io-1) then
     if (nproc == 0) then
        t1 = mpi_wtime()
        anal_ob(nmem,:) = mem_ob(:)
        ! if nproc <= ntasks_io-1, then 
        ! nanal = nmem+nproc*nanals_per_iotask
        do np=2,ntasks_io
           call mpi_recv(mem_ob,nobs_tot,mpi_real4,np-1, &
                         1,mpi_comm_io,mpi_status,ierr)
           anal_ob(nmem+(np-1)*nanals_per_iotask,:) = mem_ob(:)
        enddo
        ! mem_ob_modens and anal_ob_modens not referenced unless neigv>0
        if (neigv > 0) then
           do neig=1,neigv
              nanalo = neigv*(nmem-1) + neig
              anal_ob_modens(nanalo,:) = mem_ob_modens(neig,:)
           enddo
           do np=2,ntasks_io
              call mpi_recv(mem_ob_modens,neigv*nobs_tot,mpi_real4,np-1, &
                            2,mpi_comm_io,mpi_status,ierr)
              do neig=1,neigv
                 na = nmem+(np-1)*nanals_per_iotask
                 nanalo = neigv*(na-1) + neig
                 anal_ob_modens(nanalo,:) = mem_ob_modens(neig,:)
              enddo
           enddo
        endif
        t2 = mpi_wtime()
        print *,'time to gather ob prior ensemble on root = ',t2-t1

     else ! nproc != 0
        ! send to root.
        call mpi_send(mem_ob,nobs_tot,mpi_real4,0,1,mpi_comm_io,ierr)
        if (neigv > 0) then
            call mpi_send(mem_ob_modens,neigv*nobs_tot,mpi_real4,0,2,mpi_comm_io,ierr)
        endif
     end if 
    end if ! io task

    enddo ! nanal loop (loop over ens members on each task)

! make anal_ob contain ob prior ensemble *perturbations*
    if (nproc == 0) then
       analsim1=1._r_single/float(nanals-1)
!$omp parallel do private(nob)
       do nob=1,nobs_tot
! compute sprd
          if (neigv > 0) then
             sprd_ob(nob) = sum(anal_ob_modens(:,nob)**2)*analsim1
          else
             sprd_ob(nob) = sum(anal_ob(:,nob)**2)*analsim1
          endif
       enddo
!$omp end parallel do
       print *, 'prior spread conv: ', minval(sprd_ob(1:nobs_conv)), maxval(sprd_ob(1:nobs_conv))
       print *, 'prior spread oz: ', minval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz)), &
                                     maxval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz))
       print *, 'prior spread sat: ',minval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot)), &
                                     maxval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot))
       do nob =nobs_conv+nobs_oz+1 , nobs_tot
          if (sprd_ob(nob) > 1000.) then 
             print *, nob, ' sat spread: ', sprd_ob(nob), ', ensmean_ob: ', ensmean_ob(nob), &
                           ', anal_ob: ', anal_ob(:,nob), ', mem_ob: ', mem_ob(nob)
          endif
       enddo
    endif

    if (allocated(mem_ob)) deallocate(mem_ob)
    if (allocated(mem_ob_modens)) deallocate(mem_ob_modens)

! obs prior ensemble now defined on root task, bcast to other tasks.
    if (nproc == 0) print *,'broadcast ob prior ensemble perturbatons and spread'
    if (nproc == 0) t1 = mpi_wtime()
    if (nproc_shm == 0) then
       ! bcast entire obs prior ensemble from root task 
       ! to a single task on each node, assign to shared memory window.
       ! send one ensemble member at a time.
       allocate(mem_ob(nobs_tot))
       do nanal=1,nanals
          if (nproc == 0) then
             mem_ob(1:nobs_tot) = anal_ob(nanal,1:nobs_tot)
          endif
          call mpi_bcast(mem_ob,nobs_tot,mpi_real4,0,mpi_comm_shmemroot,ierr)
          if (nproc .ne. 0) anal_ob(nanal,1:nobs_tot) = mem_ob(1:nobs_tot)
       end do
       if (neigv > 0) then
          do nanal=1,nens
             if (nproc == 0) then
               mem_ob(1:nobs_tot) = anal_ob_modens(nanal,1:nobs_tot)
             endif
             call mpi_bcast(mem_ob,nobs_tot,mpi_real4,0,mpi_comm_shmemroot,ierr)
             if (nproc .ne. 0) anal_ob_modens(nanal,1:nobs_tot) = mem_ob(1:nobs_tot)
          end do
       endif
       if (allocated(mem_ob)) deallocate(mem_ob)
    endif
    if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time to broadcast ob prior ensemble perturbations = ',t2-t1
    endif

! broadcast ob prior ensemble spread to every task.

    if (nproc == 0) t1 = mpi_wtime()
    call mpi_bcast(sprd_ob,nobs_tot,mpi_real4,0,mpi_comm_world,ierr)
    if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time to broadcast ob prior ensemble spread = ',t2-t1
    endif


 end subroutine mpi_getobs

end module mpi_readobs
