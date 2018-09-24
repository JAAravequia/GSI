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
!   2018-02-26  shlyaeva: modifications for model-space localization
!
! attributes:
!   language: f95
!
!$$$
  
use kinds, only: r_kind, r_single, i_kind
use radinfo, only: npred
use readconvobs
use readsatobs
use readozobs
use mpisetup
use params, only: ensrf_modloc
use sparsearr, only: sparr
use intweight

implicit none

private
public :: mpi_getobs, mpi_obsstats

contains

subroutine mpi_getobs(obspath, datestring, nobs_conv, nobs_oz, nobs_sat, nobs_tot, &
                      nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag, &
                      sprd_ob, ensmean_ob, ensmean_obbc, ensmean_ob_linerr, interp, dhx_dx, ob, &
                      oberr, oblon, oblat, obpress, &
                      obtime, oberrorig, obcode, obtype, &
                      biaspreds, diagused,  anal_ob, anal_ob_modens, mem_ob, indxsat, nanals, neigv)
    character*500, intent(in) :: obspath
    character*10, intent(in) :: datestring
    character(len=10) :: id,id2
    real(r_single), allocatable, dimension(:)   :: ensmean_ob,ob,oberr,oblon,oblat
    real(r_single), allocatable, dimension(:)   :: obpress,obtime,oberrorig,ensmean_obbc,sprd_ob
    real(r_single), allocatable, dimension(:)   :: ensmean_ob_linerr
    integer(i_kind), allocatable, dimension(:)  :: obcode,indxsat
    integer(i_kind), allocatable, dimension(:)  :: diagused
    real(r_single), allocatable, dimension(:,:) :: biaspreds
    real(r_single), allocatable, dimension(:,:) :: anal_ob, anal_ob_modens
    real(r_single), allocatable, dimension(:)   :: mem_ob
    type(sparr), allocatable, dimension(:)      :: dhx_dx
    type(intw),  allocatable, dimension(:)      :: interp

    real(r_single) :: analsi,analsim1
    real(r_double) t1,t2
    character(len=20), allocatable,  dimension(:) ::  obtype
    integer(i_kind) nob, ierr, iozproc, isatproc, neig, &
            nobs_conv, nobs_oz, nobs_sat, nobs_tot, nanal, nanalo
    integer(i_kind) :: nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag
    integer(i_kind), intent(in) :: nanals, neigv
    real(r_single), allocatable, dimension(:,:) :: mem_ob_modens
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
! if nobs_tot != 0 (there were some obs to read)
    if (nobs_tot > 0) then
       if (nproc == 0) then
          ! this array only needed on root.
          allocate(anal_ob(nanals,nobs_tot))
          ! note: if neigv=0 (ob space localization), this array is size zero.
          allocate(anal_ob_modens(nanals*neigv,nobs_tot))
       end if
       ! these arrays needed on all processors.
       allocate(mem_ob(nobs_tot)) 
       allocate(mem_ob_modens(neigv,nobs_tot))
       allocate(sprd_ob(nobs_tot),ob(nobs_tot),oberr(nobs_tot),oblon(nobs_tot),&
       oblat(nobs_tot),obpress(nobs_tot),obtime(nobs_tot),oberrorig(nobs_tot),obcode(nobs_tot),&
       obtype(nobs_tot),ensmean_ob(nobs_tot),ensmean_obbc(nobs_tot),&
       biaspreds(npred+1, nobs_sat),indxsat(nobs_sat), diagused(nobs_totdiag))
       allocate(dhx_dx(nobs_tot), interp(nobs_tot))
       if (ensrf_modloc) then
         allocate(ensmean_ob_linerr(nobs_tot))
      endif
    else
! stop if no obs found (must be an error somewhere).
       print *,'no obs found!'
       call stop2(11)
    end if

! read ensemble mean and every ensemble member
    nanal = nproc+1
    id = 'ensmean'
    id2 = id
    if (nanal <= nanals) then
       write(id2,'(a3,(i3.3))') 'mem',nanal
    endif
! read obs.
! only thing that is different on each task is mem_ob.  All other
! fields are defined from ensemble mean.
! individual members read on 1st nanals tasks, ens mean read on all tasks.
    if (nobs_conv > 0) then
! first nobs_conv are conventional obs.
      call get_convobs_data(obspath, datestring, nobs_conv, nobs_convdiag, &
        ensmean_obbc(1:nobs_conv), ensmean_ob(1:nobs_conv),                &
        mem_ob(1:nobs_conv), mem_ob_modens(1:neigv,1:nobs_conv),           &
        ensmean_ob_linerr(1:nobs_conv),                                    &
        interp(1:nobs_conv), dhx_dx(1:nobs_conv), ob(1:nobs_conv),         &
        oberr(1:nobs_conv), oblon(1:nobs_conv), oblat(1:nobs_conv),        &
        obpress(1:nobs_conv), obtime(1:nobs_conv), obcode(1:nobs_conv),    &
        oberrorig(1:nobs_conv), obtype(1:nobs_conv),                       &
        diagused(1:nobs_convdiag), id, nanal)
    end if
    if (nobs_oz > 0) then
! second nobs_oz are conventional obs.
      call get_ozobs_data(obspath, datestring, nobs_oz, nobs_ozdiag,  &
        ensmean_obbc(nobs_conv+1:nobs_conv+nobs_oz),     &
        ensmean_ob(nobs_conv+1:nobs_conv+nobs_oz),       &
        mem_ob(nobs_conv+1:nobs_conv+nobs_oz),              &
        mem_ob_modens(1:neigv,nobs_conv+1:nobs_conv+nobs_oz),         &
        interp(nobs_conv+1:nobs_conv+nobs_oz),               &
        dhx_dx(nobs_conv+1:nobs_conv+nobs_oz),               &
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
        id,nanal)
    end if
    if (nobs_sat > 0) then
      biaspreds = 0. ! initialize bias predictor array to zero.
! last nobs_sat are satellite radiance obs.
      call get_satobs_data(obspath, datestring, nobs_sat, nobs_satdiag, &
        ensmean_obbc(nobs_conv+nobs_oz+1:nobs_tot),       &
        ensmean_ob(nobs_conv+nobs_oz+1:nobs_tot),         &
        mem_ob(nobs_conv+nobs_oz+1:nobs_tot),                &
        mem_ob_modens(1:neigv,nobs_conv+nobs_oz+1:nobs_tot),            &
        ensmean_ob_linerr(nobs_conv+nobs_oz+1:nobs_tot),     &
        interp(nobs_conv+nobs_oz+1:nobs_tot),                &
        dhx_dx(nobs_conv+nobs_oz+1:nobs_tot),                &
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
        id,nanal)
    end if ! read obs.

    call mpi_barrier(mpi_comm_world,ierr)  ! synch tasks.

! use mpi_gather to gather ob prior ensemble on root.
! requires allocation of nobs_tot x nanals temporory array.
!    if (nproc == 0) then
!       t1 = mpi_wtime()
!       allocate(anal_obtmp(nobs_tot,nanals))
!    endif
!    if (nproc <= nanals-1) then
!       call mpi_gather(h_xnobc,nobs_tot,mpi_real4,&
!       anal_obtmp,nobs_tot,mpi_real4,0,mpi_comm_io,ierr)
!       if (nproc .eq. 0) then
!          anal_ob = transpose(anal_obtmp); deallocate(anal_obtmp)
!          t2 = mpi_wtime()
!          print *,'time to create ob prior ensemble on root = ',t2-t1
!       endif
!    endif

! use mpi_send/mpi_recv to gather ob prior ensemble on root.
! a bit slower, but does not require large temporary array like mpi_gather.
    if (nproc <= nanals-1) then
     if (nproc == 0) then
        t1 = mpi_wtime()
        anal_ob(1,:) = mem_ob(:)
        do nanal=2,nanals
           call mpi_recv(mem_ob,nobs_tot,mpi_real4,nanal-1, &
                         1,mpi_comm_io,mpi_status,ierr)
           anal_ob(nanal,:) = mem_ob(:)
        enddo
        ! mem_ob_modens and anal_ob_modens not referenced unless neigv>0
        if (neigv > 0) then
           nanal = 1
           do neig=1,neigv
              nanalo = neigv*(nanal-1) + neig
              anal_ob_modens(nanalo,:) = mem_ob_modens(neig,:)
           enddo
           do nanal=2,nanals
              call mpi_recv(mem_ob_modens,neigv*nobs_tot,mpi_real4,nanal-1, &
                            2,mpi_comm_io,mpi_status,ierr)
              do neig=1,neigv
                 nanalo = neigv*(nanal-1) + neig
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
    end if ! nanal <= nanals

! make anal_ob contain ob prior ensemble *perturbations*
    if (nproc == 0) then
        analsi=1._r_single/float(nanals)
        analsim1=1._r_single/float(nanals-1)
!$omp parallel do private(nob,nanal)
        do nob=1,nobs_tot
           ensmean_ob(nob)  = sum(anal_ob(:,nob))*analsi
! remove ensemble mean from each member.
! ensmean_ob is unbiascorrected ensemble mean (anal_ob is ens pert)
           anal_ob(:,nob) = anal_ob(:,nob)-ensmean_ob(nob)
! compute sprd
           sprd_ob(nob) = sum(anal_ob(:,nob)**2)*analsim1
! modulated ensemble.
           if (neigv > 0) then
              anal_ob_modens(:,nob) = anal_ob_modens(:,nob)-ensmean_ob(nob)
              sprd_ob(nob) = sum(anal_ob_modens(:,nob)**2)*analsim1
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

! broadcast ob prior ensemble mean and spread to every task.

    if (.not. ensrf_modloc) then
      if (allocated(mem_ob)) deallocate(mem_ob)
      if (allocated(interp)) deallocate(interp)
      if (allocated(dhx_dx)) deallocate(dhx_dx)
    endif
    if (allocated(mem_ob_modens)) deallocate(mem_ob_modens)

    if (nproc == 0) t1 = mpi_wtime()
    call mpi_bcast(ensmean_ob,nobs_tot,mpi_real4,0,mpi_comm_world,ierr)
    call mpi_bcast(sprd_ob,nobs_tot,mpi_real4,0,mpi_comm_world,ierr)
    if (ensrf_modloc) then
      call mpi_bcast(ensmean_ob_linerr,nobs_tot,mpi_real4,0,mpi_comm_world,ierr)
    endif
    if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time to broadcast ob prior ensemble mean and spread = ',t2-t1
    endif

 end subroutine mpi_getobs

subroutine mpi_obsstats(nobs_conv, nobs_oz, nobs_sat, nobs_tot, &
                         bg_ob, bc_ob, interp, dhx_dx, mean_ob, sprd_ob, nanals)

use observer_enkf, only: calc_linhx
use controlvec, only: grdin,ncdim
use params, only: nbackgrounds
use gridinfo, only: npts
implicit none
real(r_single), dimension(nobs_tot) :: bg_ob, bc_ob
real(r_single), allocatable, dimension(:,:) :: anal_ob
real(r_single), allocatable, dimension(:)   :: mem_ob, sprd_ob, mean_ob
type(sparr), dimension(nobs_tot) :: dhx_dx
type(intw),  dimension(nobs_tot) :: interp

integer(i_kind) nob, ierr, nobs_conv, nobs_oz, nobs_sat, nobs_tot, nanal
integer(i_kind), intent(in) :: nanals

if (nproc == 0) then
  ! this array only needed on root.
   allocate(anal_ob(nanals,nobs_tot), mean_ob(nobs_tot), sprd_ob(nobs_tot))
end if
allocate(mem_ob(nobs_tot))

! use mpi_send/mpi_recv to gather ob prior ensemble on root.
! a bit slower, but does not require large temporary array like mpi_gather.
if (nproc <= nanals-1) then
   do nob = 1, nobs_tot
      call calc_linhx(bg_ob(nob), grdin, interp(nob),   &
                      dhx_dx(nob), mem_ob(nob), npts, ncdim, nbackgrounds)  ! add dHx/dx * analysis increment to background Hx
   enddo

   if (nproc == 0) then
     anal_ob(1,:) = mem_ob(:)
     do nanal=2,nanals
        call mpi_recv(mem_ob,nobs_tot,mpi_real4,nanal-1, &
                      1,mpi_comm_io,mpi_status,ierr)
        anal_ob(nanal,:) = mem_ob(:)
     enddo

  else ! nproc != 0
     ! send to root.
     call mpi_send(mem_ob,nobs_tot,mpi_real4,0,1,mpi_comm_io,ierr)
  end if
end if ! nanal <= nanals

! make anal_ob contain ob prior ensemble *perturbations*
if (nproc == 0) then

!$omp parallel do private(nob)
  do nob=1,nobs_tot
! remove ensemble mean from each member.
     mean_ob(nob)  = sum(anal_ob(:,nob)) /real(nanals)
! ensmean_ob is unbiascorrected ensemble mean (anal_ob
     anal_ob(:,nob) = anal_ob(:,nob)-mean_ob(nob)
! compute sprd
     sprd_ob(nob) = sum(anal_ob(:,nob)**2)/(nanals-1)

! calc bias corrected mean
     mean_ob(nob) = mean_ob(nob) + bc_ob(nob)
  enddo
!$omp end parallel do

!  print *, 'anal: ', anal_ob(:,1)
!  print *, 'mean: ', mean_ob-bc_ob, mean_ob

!  print *, 'mean before bc: ', mean_ob - bc_ob
!  print *, 'mean after bc: ', mean_ob
  print *, 'post spread conv: ', minval(sprd_ob(1:nobs_conv)), maxval(sprd_ob(1:nobs_conv))
  print *, 'post spread oz: ',   minval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz)), &
                                 maxval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz))
  print *, 'post spread sat: ',  minval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot)),  &
                                 maxval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot))
  deallocate(anal_ob)

endif
deallocate(mem_ob)

end subroutine mpi_obsstats

end module mpi_readobs
