!---------------------------------------------------------------------!
! Author: Ting-Chi Wu @ CIRA/CSU
! Time: 5/23/2016
!
! Module purpose: CWM-Partitioned qfields (partition 2 or partition 6)
!---------------------------------------------------------------------!


module parqvarsmod

  use kinds, only: i_kind, r_kind
  use gridmod, only: nlat, nlon, lat1, lon1, lat2, lon2, nsig  

  public :: prep_parqvars_sm
  public :: smooth_parqvars_sm
  public :: allocate_parqvars_sm
  public :: destroy_parqvars_sm
  
  ! get_smoothed_vars, get_mpi_parms_for_loc, digitalfilter
  ! putwbf2dtonbf1d, and put1dto2d are internal calls
  
  !public :: get_p6_input 
  !public :: ges_fice
  !public :: ges_frain
  !public :: ges_frimef

  !public
  public :: ges_ql_sm
  public :: ges_qi_sm
  public :: ges_qr_sm
  public :: ges_qs_sm
  public :: ges_qg_sm
  public :: ges_qh_sm

  public :: lpartition2, lpartition6

  !real(r_kind),dimension(:,:,:),pointer:: ges_fice=>NULL() 
  !real(r_kind),dimension(:,:,:),pointer:: ges_frain=>NULL() 
  !real(r_kind),dimension(:,:,:),pointer:: ges_frimef=>NULL() 

  real(r_kind),dimension(:,:,:,:),pointer:: ges_ql_sm=>NULL() 
  real(r_kind),dimension(:,:,:,:),pointer:: ges_qi_sm=>NULL() 
  real(r_kind),dimension(:,:,:,:),pointer:: ges_qr_sm=>NULL() 
  real(r_kind),dimension(:,:,:,:),pointer:: ges_qs_sm=>NULL() 
  real(r_kind),dimension(:,:,:,:),pointer:: ges_qg_sm=>NULL() 
  real(r_kind),dimension(:,:,:,:),pointer:: ges_qh_sm=>NULL() 

  ! not public, but internal to all subroutines in this module
  integer(i_kind),dimension(:),pointer :: ijn_loc=>NULL()      !dimension(npe)
  integer(i_kind),dimension(:),pointer :: displs_loc=>NULL()   !dimension(npe)
  integer(i_kind),dimension(:),pointer :: ltosi_loc=>NULL()    !dimension(itotsub_loc) 
  integer(i_kind),dimension(:),pointer :: ltosj_loc=>NULL()    !dimension(itotsub_loc)
  !integer(i_kind),pointer                      :: itotsub_loc=>NULL()  !dimension(1)
  integer(i_kind)                      :: itotsub_loc  !dimension(1)

  logical :: lpartition2, lpartition6

  contains

!!-------------------------------------------------------------------------
!subroutine get_p6_input(itguess,iret)
!!-------------------------------------------------------------------------
!
!  use gsi_bundlemod, only: gsi_bundlegetpointer
!  use gsi_metguess_mod,  only: gsi_metguess_bundle
!
!  implicit none
!
!  integer(i_kind),intent(in   ) :: itguess
!  integer(i_kind),intent(  out) :: iret
!
!  call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'fice',ges_fice,iret)
!  call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'frain',ges_frain,iret)
!  call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'frimef',ges_frimef,iret)
!  if ( iret == 0 ) write(6,*) 'GET_P6_INPUT: ges_fice, ges_frain, and ges_frimef done!'
!
!  return
!end subroutine get_p6_input
!
!-------------------------------------------------------------------------
subroutine prep_parqvars_sm(itguess,pflag)
!-------------------------------------------------------------------------
  
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod,  only: gsi_metguess_bundle

  implicit none

  integer(i_kind),intent(in   ) :: itguess
  integer(i_kind),intent(in   ) :: pflag
  integer(i_kind) :: iret
  real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()

  if (pflag==2) then ! pflag if loop

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'ql',ges_ql,iret)
    ges_ql_sm(:,:,:,itguess)=ges_ql

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qi',ges_qi,iret)
    ges_qi_sm(:,:,:,itguess)=ges_qi

    write(6,*) 'PREP_PARQVARS2_SM: update ges_qfield_sm by ges_q_field!'

  elseif (pflag==6) then ! pflag if loop

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'ql',ges_ql,iret)
    ges_ql_sm(:,:,:,itguess)=ges_ql

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qi',ges_qi,iret)
    ges_qi_sm(:,:,:,itguess)=ges_qi

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qr',ges_qr,iret)
    ges_qr_sm(:,:,:,itguess)=ges_qr

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qs',ges_qs,iret)
    ges_qs_sm(:,:,:,itguess)=ges_qs

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qg',ges_qg,iret)
    ges_qg_sm(:,:,:,itguess)=ges_qg

    call gsi_bundlegetpointer(gsi_metguess_bundle(itguess),'qh',ges_qh,iret)
    ges_qh_sm(:,:,:,itguess)=ges_qh

    write(6,*) 'PREP_PAR6VARS6_SM: update ges_qfield_sm by ges_q_field!'

  else ! pflag if loop

    write(6,*) 'PREP_PARQVARS_SM: ERROR!!!! WRONG PARTITION'

  endif ! pflag if loop

  return
end subroutine prep_parqvars_sm

!-------------------------------------------------------------------------
subroutine allocate_parqvars_sm(dim1,dim2,dim3,dim4,pflag)
!-------------------------------------------------------------------------

  implicit none
  integer(i_kind),intent(in   ) :: dim1, dim2, dim3, dim4
  integer(i_kind),intent(in   ) :: pflag
  integer(i_kind) :: istatus

  if (pflag==2) then
    allocate(ges_ql_sm(dim1,dim2,dim3,dim4), &
             ges_qi_sm(dim1,dim2,dim3,dim4),stat=istatus)
  elseif (pflag==6) then
    allocate(ges_ql_sm(dim1,dim2,dim3,dim4), &
             ges_qi_sm(dim1,dim2,dim3,dim4), &
             ges_qr_sm(dim1,dim2,dim3,dim4), &
             ges_qs_sm(dim1,dim2,dim3,dim4), &
             ges_qg_sm(dim1,dim2,dim3,dim4), &
             ges_qh_sm(dim1,dim2,dim3,dim4),stat=istatus)
  endif

end subroutine allocate_parqvars_sm

!-------------------------------------------------------------------------
subroutine destroy_parqvars_sm(pflag)
!-------------------------------------------------------------------------

  implicit none
  integer(i_kind),intent(in   ) :: pflag
  integer(i_kind) :: istatus

  if (pflag==2) then
    deallocate(ges_ql_sm, &
               ges_qi_sm,stat=istatus)
  elseif (pflag==6) then
    deallocate(ges_ql_sm, &
               ges_qi_sm, &
               ges_qr_sm, &
               ges_qs_sm, &
               ges_qg_sm, &
               ges_qh_sm,stat=istatus)
  endif

end subroutine destroy_parqvars_sm

!-------------------------------------------------------------------------
subroutine get_mpi_parms_for_loc(nhalfx,nhalfy)
!-------------------------------------------------------------------------

  use mpimod, only: npe
  use gridmod, only: istart, jstart, ilat1, jlon1

  integer(i_kind),               intent(in   ) :: nhalfx, nhalfy
  integer(i_kind) :: i, j, n, ns
  integer(i_kind) :: istatus

  write(6,*) 'GET_MPI_PARMS_FOR_LOC: npe = ', npe
  write(6,*) 'GET_MPI_PARMS_FOR_LOC: ilat1 = ', ilat1
  write(6,*) 'GET_MPI_PARMS_FOR_LOC: jlon1 = ', jlon1
  allocate(ijn_loc(npe), & 
           displs_loc(npe),stat=istatus)
  if (istatus/=0) then
    write(6,*) 'GET_MPI_PARMS_FOR_LOC: allocate ijn_loc and displs_loc failed = ', istatus
  else
    write(6,*) 'GET_MPI_PARMS_FOR_LOC: allocate ijn_loc and displs_loc done'
  endif

  displs_loc(1) = 0
  do n = 1, npe

    ijn_loc(n) = (ilat1(n)+2*(1+nhalfx)) * (jlon1(n)+2*(1+nhalfy))
    if (n/=1) then
      displs_loc(n) = displs_loc(n-1)+ijn_loc(n-1)
    endif
    write(6,*) 'GET_MPI_PARMS_FOR_LOC: ijn_loc, displs_loc = ', n, ijn_loc(n), displs_loc(n)

  enddo
  write(6,*) 'GET_MPI_PARMS_FOR_LOC: ijn_loc(npe), displs_loc(npe) = ', ijn_loc(npe), displs_loc(npe)
  !allocate(itotsub_loc)
  itotsub_loc=displs_loc(npe)+ijn_loc(npe)

  write(6,*) 'GET_MPI_PARMS_FOR_LOC: itotsub_loc = ', itotsub_loc
  allocate(ltosi_loc(itotsub_loc),ltosj_loc(itotsub_loc))

  do n = 1, npe
    do j=1,jlon1(n)+2*(1+nhalfy)
      ns=displs_loc(n)+(j-1)*(ilat1(n)+2*(1+nhalfx))
      do i=1,ilat1(n)+2*(1+nhalfx)
        ns=ns+1
        !ltosi_loc(ns)=min(max(1,istart(n)+i-(1+nhalfy)),nlat)
        !ltosj_loc(ns)=min(max(1,jstart(n)+j-(1+nhalfx)),nlon)
        ltosi_loc(ns)=min(max(1,istart(n)+i-1-(1+nhalfy)),nlat)
        ltosj_loc(ns)=min(max(1,jstart(n)+j-1-(1+nhalfx)),nlon)
      end do
    end do
  enddo

  return

end subroutine get_mpi_parms_for_loc


!-------------------------------------------------------------------------
subroutine get_smoothed_vars(ges_qvar_sm,hydro,itguess,mype,wl,nitr,mx)
!-------------------------------------------------------------------------
!
! Purpose: Smooth CWM-Partitioned (2 or 6) qfield variables
!          with recursive filter (wl,nitr,mx)     
!

  use mpimod, only: npe, mpi_rtype, mpi_comm_world, ierror
  use gridmod, only: ijn, displs_g, ijn_s, displs_s, itotsub, &
                     strip, iglobal
  use general_commvars_mod, only: ltosi_s, ltosj_s, ltosi, ltosj
  use constants, only: one, half, one_tenth, r0_01, ten

  implicit none
  real(r_kind),dimension(:,:,:),intent(inout)  :: ges_qvar_sm
  character(2),                 intent(in   )  :: hydro
  integer(i_kind),              intent(in   )  :: itguess
  integer(i_kind),              intent(in   )  :: mype
  real(r_kind),                 intent(in   )  :: wl
  integer(i_kind),              intent(in   )  :: nitr,mx

  integer(i_kind)                :: i,j,ij,k,kk,istatus
  integer(i_kind)                :: nn, iunit, iunita, iunitb
  character(14)                  :: filename_sm 
  character(15)                  :: filename_loc 
  character(21)                  :: filename_before 
  character(20)                  :: filename_after
  real(r_kind)                   :: eps
  real(r_kind),dimension(nsig)   :: deltax

  !real(r_kind),dimension(:,:,:), allocatable :: ges_qvar_glb
  real(r_kind),dimension(nlat,nlon) :: temp
  !real(r_kind),dimension(:),     allocatable :: ges_qvar_glb_nbf_1d
  !real(r_kind),dimension(:,:),   allocatable :: ges_qvar_glb_nbf_2d
  !real(r_kind),dimension(:),     allocatable :: ges_qvar_glb_wbfsm_1d
  !real(r_kind),dimension(:),     allocatable :: ges_qvar_sm_wbf_1d, ges_qvar_sm_nbf_1d
  real(r_kind),dimension(nlat*nlon) :: ges_qvar_glb_nbf_1d
  real(r_kind),dimension(nlat,nlon) :: ges_qvar_glb_nbf_2d, ges_qvar_glb_nbf_2d_p
  real(r_kind),dimension(itotsub)   :: ges_qvar_glb_wbfsm_1d
  real(r_kind),dimension(lat2,lon2) :: ges_qvar_sm_wbf_1d
  real(r_kind),dimension(lat1,lon1) :: ges_qvar_sm_nbf_1d

  deltax(:) = 0.0
  ges_qvar_sm_nbf_1d=0.0
  ges_qvar_glb_wbfsm_1d=0.0
  ges_qvar_glb_nbf_2d=0.0

!  ! compute ijn_loc, displs_loc, itotsub_loc, ltosi_loc, and ltosj_loc for ges_qvar_loc 
!  write(6,*) 'GET_SMOOTHED_VARS: get ijn_loc, displs_loc, itotsub_loc, ltosi_loc, and ltosj_loc'
!  call get_mpi_parms_for_loc(nhalfx,nhalfy)
 
!  ! allocate ges_qvar_glb and ges_qvar_loc (with buffer points; 2*(1+nhalf*))
!  allocate(ges_qvar_glb_nbf_1d(nlat*nlon), &
!           ges_qvar_glb_wbfsm_1d(itotsub), &
!           ges_qvar_glb_nbf_2d(nlat,nlon), &
!           ges_qvar_sm_wbf_1d(lat2*lon2), &
!           ges_qvar_sm_nbf_1d(lat1*lon1), stat=istatus)
!  if (istatus/=0) then
!    write(6,*) 'GET_SMOOTHED_VARS: allocate ges_qvar_1ds and _loc failed, istatus = ', istatus
!  else
!    write(6,*) 'GET_SMOOTHED_VARS: allocate ges_qvar_1ds and _loc done! = ', &
!               nlat*nlon, itotsub, nlat, nlon, lat1*lon1
!  endif

  do k = 1, nsig

!!--------------------------------------------------------!
!    if (itguess==1 .and. k==35) then
!      iunitb=mype+1
!      close(iunitb)
!      write(filename_before,'("ges_qvar_sm_before_",i2.2)') iunitb
!      open(iunitb,file=filename_before,form='formatted') 
!      do j = 1, lon2
!        do i = 1, lat2
!          write(iunitb,*) ges_qvar_sm(i,j,k)
!        enddo
!      enddo
!      close(iunitb,status='keep')
!   endif
!!--------------------------------------------------------!


    call strip(ges_qvar_sm(:,:,k),ges_qvar_sm_nbf_1d)    
    write(6,*) 'GET_SMOOTHED_VARS: strip ges_qvar_sm for ', hydro

    if ( maxval(ges_qvar_sm(:,:,k)) .ne. maxval(ges_qvar_sm_nbf_1d) .and. &
         minval(ges_qvar_sm(:,:,k)) .ne. minval(ges_qvar_sm_nbf_1d) ) then
      write(6,*) 'GET_SMOOTHED_VARS: Error with strip for ', hydro,       &
                  maxval(ges_qvar_sm(:,:,k)), maxval(ges_qvar_sm_nbf_1d),  &
                  minval(ges_qvar_sm(:,:,k)), minval(ges_qvar_sm_nbf_1d)
    endif
            


    !call mpi_allgatherv(ges_qvar_sm_nbf_1d,ijn(mype+1),mpi_rtype,&
    !    ges_qvar_glb_wbfsm_1d,ijn,displs_g,mpi_rtype,&
    !    mpi_comm_world,ierror)
    !write(6,*) 'GET_SMOOTHED_VARS: mpi_allgatherv ierror = ', ierror
    call mpi_gatherv(ges_qvar_sm_nbf_1d,ijn(mype+1),mpi_rtype,&
                     ges_qvar_glb_wbfsm_1d,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)
    write(6,*) 'GET_SMOOTHED_VARS: mpi_gatherv for ',hydro, ' ierror = ', ierror
    write(6,*) 'GET_SMOOTHED_VARS: ges_qvar_glb_wbfsm_1d for ',hydro, ' = ', maxval(ges_qvar_glb_wbfsm_1d), minval(ges_qvar_glb_wbfsm_1d)

    do kk=1,iglobal
      i=ltosi(kk) ; j=ltosj(kk)
      ges_qvar_glb_nbf_2d(i,j)=ges_qvar_glb_wbfsm_1d(kk)
    end do
    write(6,*) 'GET_SMOOTHED_VARS: put ges_qvar_glb for ',hydro ,' back to iglobal = ', iglobal


!    deltax(k) = one_tenth*maxval(ges_qvar_glb_nbf_2d)
!    eps = one 
!    !eps = ten 
!    if (mype == 0) write(6,*) 'GET_SMOOTHED_VARS: perturbation = ', k, eps*deltax(k) 
!    ges_qvar_glb_nbf_2d_p = ges_qvar_glb_nbf_2d + eps*deltax(k)
!    write(6,*) 'GET_SMOOTHED_VARS: before smooth (perturbed) : ', k, &
!                maxval(ges_qvar_glb_nbf_2d_p), minval(ges_qvar_glb_nbf_2d_p)
!
!!--------------------------------------------------------!
!    if (mype == 0 .and. itguess==1 .and. k==35) then
!      close(77)
!      open(77,file='ges_qvar_glb_before',form='formatted') 
!      do j = 1, nlon
!        do i = 1, nlat
!          write(77,*) ges_qvar_glb_nbf_2d(i,j)
!        enddo
!      enddo
!      close(77,status='keep')
!    endif
!!--------------------------------------------------------!   
!!--------------------------------------------------------!
!    if (mype == 0 .and. itguess==1 .and. k==35) then
!      close(88)
!      open(88,file='ges_qvar_glb_beforep',form='formatted') 
!      do j = 1, nlon
!        do i = 1, nlat
!          write(88,*) ges_qvar_glb_nbf_2d_p(i,j)
!        enddo
!      enddo
!      close(88,status='keep')
!    endif
!!--------------------------------------------------------!   
!
!    call smoothww(nlat,nlon,ges_qvar_glb_nbf_2d_p,wl,nitr,mx)
!    write(6,*) 'GET_SMOOTHED_VARS: after smooth (perturbed) : ', k, &
!                maxval(ges_qvar_glb_nbf_2d_p), minval(ges_qvar_glb_nbf_2d_p)

    !write(6,*) 'GET_SMOOTHED_VARS: before smooth at it = : ', itguess, k, &
    !            maxval(ges_qvar_glb_nbf_2d), minval(ges_qvar_glb_nbf_2d)
    write(6,*) 'GET_SMOOTHED_VARS: before sm for ',hydro,' = ', maxval(ges_qvar_glb_nbf_2d), minval(ges_qvar_glb_nbf_2d)
    !call smoothww(nlat,nlon,ges_qvar_glb_nbf_2d,half,2,1)
    call smoothww(nlat,nlon,ges_qvar_glb_nbf_2d,wl,nitr,mx)
    !write(6,*) 'GET_SMOOTHED_VARS: after smooth at it = : ', itguess, k, &
    !            maxval(ges_qvar_glb_nbf_2d), minval(ges_qvar_glb_nbf_2d)
    write(6,*) 'GET_SMOOTHED_VARS: after sm for ',hydro,' = : ', maxval(ges_qvar_glb_nbf_2d), minval(ges_qvar_glb_nbf_2d)


    do kk=1,itotsub
       i=ltosi_s(kk)
       j=ltosj_s(kk)
       ges_qvar_glb_wbfsm_1d(kk)=ges_qvar_glb_nbf_2d(i,j)
    end do


    call mpi_scatterv(ges_qvar_glb_wbfsm_1d,ijn_s,displs_s,mpi_rtype, &
                      ges_qvar_sm_wbf_1d,ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
    write(6,*) 'GET_SMOOTHED_VARS: mpi_scatterv for ',hydro,' ierror = ', ierror

!!--------------------------------------------------------!
!    if (mype == 0 .and. itguess==1 .and. k==35) then
!      close(99)
!      open(99,file='ges_qvar_glb_after',form='formatted') 
!      do j = 1, nlon
!        do i = 1, nlat
!          write(99,*) ges_qvar_glb_nbf_2d(i,j)
!          !write(99,*) ges_qvar_glb_nbf_2d_p(i,j)
!          !write(99,*) ges_qvar_glb_nbf_2d_p(i,j)-ges_qvar_glb_nbf_2d(i,j)
!          !write(99,*)(ges_qvar_glb_nbf_2d_p(i,j)-ges_qvar_glb_nbf_2d(i,j))/(eps*deltax(k))
!        enddo
!     enddo
!      close(99,status='keep')
!    endif
!!--------------------------------------------------------!   

    call put1dto2d(ges_qvar_sm_wbf_1d,ges_qvar_sm(:,:,k),lat2,lon2)
    write(6,*) 'GET_SMOOTHED_VARS: update ges_qvar_sm for ', hydro

!!--------------------------------------------------------!
!    if (itguess==1 .and. k==35) then
!      iunita=mype+1
!      close(iunita)
!      write(filename_after,'("ges_qvar_sm_after_",i2.2)') iunita
!      open(iunita,file=filename_after,form='formatted') 
!      do j = 1, lon2
!        do i = 1, lat2
!          write(iunita,*) ges_qvar_sm(i,j,k)
!        enddo
!      enddo
!      close(iunita,status='keep')
!   endif
!!--------------------------------------------------------!

  enddo


!  ! deallocate ges_qvar_glb and ges_qvar_loc (with buffer points; 2*(1+nhalf*))
!  deallocate(ges_qvar_glb_nbf_1d, &
!             ges_qvar_glb_wbfsm_1d, &
!             ges_qvar_glb_nbf_2d, &
!             ges_qvar_sm_wbf_1d, &
!             ges_qvar_sm_nbf_1d, stat=istatus)
!  if (istatus/=0) then
!    write(6,*) 'GET_SMOOTHED_VARS: deallocate ges_qvar_1ds failed, istatus = ', istatus
!  else
!    write(6,*) 'GET_SMOOTHED_VARS: deallocate ges_qvar_1ds done!'
!  endif

  return

end subroutine get_smoothed_vars

!-------------------------------------------------------------------------
subroutine putwbf2dtonbf1d(var2d,addbfx,addbfy,var1d,imaxnbf,jmaxnbf)
!-------------------------------------------------------------------------
! call putwbf2dtonbf1d(ges_qvar_sm(:,:,k),1,1,ges_qvar_sm_nbf_1d,lat1,lon1)
! call putwbf2dtonbf1d(ges_qvar_loc(:,:,k),1+nhalfx,1+nhalfy,ges_qvar_loc_nbf_1d,lat1,lon1)

  implicit none

  integer(i_kind),intent(in   ) :: imaxnbf, jmaxnbf, addbfx, addbfy
  real(r_kind),dimension(imaxnbf+2*addbfx,jmaxnbf+2*addbfy),intent(in   ) :: var2d
  real(r_kind),dimension(imaxnbf*jmaxnbf),intent(  out) :: var1d

  integer(i_kind) :: i,j,ij,jp1

    ij=0
    do j=1,jmaxnbf
      jp1 = j+addbfy
         do i=1,imaxnbf
            ij = ij+1
            var1d(ij)=var2d(i+addbfx,jp1)
         end do
    end do

end subroutine putwbf2dtonbf1d


!-------------------------------------------------------------------------
subroutine put1dto2d(var1d,var2d,imax2d,jmax2d)
!-------------------------------------------------------------------------
! call put1dto2d(ges_qvar_sm_wbf_1d,ges_qvar_sm(:,:,k),lat2,lon2)

  implicit none

  integer(i_kind),intent(in   ) :: imax2d, jmax2d
  real(r_kind),dimension(imax2d*jmax2d),intent(in   ) :: var1d
  real(r_kind),dimension(imax2d,jmax2d),intent(  out) :: var2d

  integer(i_kind) :: i, j, ij,jp1

! origional
    do i=1,imax2d
      do j=1,jmax2d
        ij=i+(j-1)*imax2d
        var2d(i,j) = var1d(ij)
      enddo
    enddo
! origional

!    ij=0
!    do j=1,jmax2d
!         do i=1,imax2d
!            ij = ij+1
!            var2d(i,j)=var1d(ij)
!         end do
!    end do

end subroutine put1dto2d

!-------------------------------------------------------------------------
subroutine smooth_parqvars_sm(itguess,mype,rfp1,rfp2,rfp3,pflag)
!-------------------------------------------------------------------------

  use mpimod, only: mpi_comm_world, ierror
   
  implicit none
  integer(i_kind),intent(in   )  :: itguess
  integer(i_kind),intent(in   )  :: mype
  real(r_kind),   intent(in   )  :: rfp1
  integer(i_kind),intent(in   )  :: rfp2, rfp3
  integer(i_kind),intent(in   )  :: pflag
  integer(i_kind)                :: iret

  if (pflag==2) then ! pflag if loop

    write(6,*) 'SMOOTH_PARQVARS2_SM: before ges_qfields_sm'
    call get_smoothed_vars(ges_qi_sm(:,:,:,itguess),'qi',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_ql_sm(:,:,:,itguess),'ql',itguess,mype,rfp1,rfp2,rfp3)
    write(6,*) 'SMOOTH_PARQVARS2_SM: after ges_qfields_sm'

  elseif (pflag==6) then ! pflag if loop

    write(6,*) 'SMOOTH_PARQVARS6_SM: before ges_qfields_sm'
    call get_smoothed_vars(ges_ql_sm(:,:,:,itguess),'ql',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_qi_sm(:,:,:,itguess),'qi',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_qr_sm(:,:,:,itguess),'qr',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_qs_sm(:,:,:,itguess),'qs',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_qg_sm(:,:,:,itguess),'qg',itguess,mype,rfp1,rfp2,rfp3)
    call get_smoothed_vars(ges_qh_sm(:,:,:,itguess),'qh',itguess,mype,rfp1,rfp2,rfp3)
    write(6,*) 'SMOOTH_PARQVARS6_SM: after ges_qfields_sm'

  else ! pflag if loop
    write(6,*) 'SMOOTH_PARQVARS_SM: ERROR!!!! WRONG PARTITION'
  endif ! pflag if loop

!!--------------------------------------------------------!
!    call mpi_barrier(mpi_comm_world,ierror)
!    stop 
!!--------------------------------------------------------!

  return
end subroutine smooth_parqvars_sm

!-------------------------------------------------------------------------
subroutine digitalfilter(xinout,nx,ny,nz,nhalfx,nhalfy,nhalfz)
!-------------------------------------------------------------------------
! *              .      .    .
! * PROGRAM: DigitalFilter
! *
! * PRGMMR:     TING-CHI WU     ORG: CIRA/CSU   DATE: 2016-05-18
! *
! * ABSTRACT:   THIS PROGRAM FOLLOWS THE WORK BY LYNCH AND HUANG (1992) 
! *             TITLED "Initialization if the HIRLAM Model Using 
! *             a Digital Filter", ESPECIALLY THE LANCZOS WINDOW PART 
! *             1) CALCULATE COEFFICIENTS (hx, hy, hz)
! *             2) UPDATE FIELDS WITH COEFFICIENTS (xinout)
! *
!-------------------------------------------------------------------------
  use constants, only: pi

  real(r_kind),   intent(inout) :: xinout(1:nx,1:ny,1:nz)
  integer(i_kind),intent(in   ) :: nx, ny, nz
  integer(i_kind),intent(in   ) :: nhalfx, nhalfy, nhalfz

  integer(i_kind) :: i, j, k, ii, jj, kk
  integer(i_kind) :: ystart, yend, xstart, xend, zstart, zend
  real(r_kind) :: thetacx, thetacy, thetacz
  real(r_kind) :: hx, hy, hz, sumhx, sumhy, sumhz
  real(r_kind),dimension(1:nx,1:ny,1:nz) :: xtemp

  thetacx = pi/nhalfx
  thetacy = pi/nhalfy
  if (nhalfz == 0) then
    thetacz = pi
  else
    thetacz = pi/nhalfz
  endif

  do k=1,nz
    do j=1,ny
      do i=1,nx

        xstart = max(1,i-nhalfx)
        xend   = min(nx,i+nhalfx)
        ystart = max(1,j-nhalfy)
        yend   = min(ny,j+nhalfy)
        zstart = max(1,k-nhalfz)
        zend   = min(nz,k+nhalfz)

        xtemp(i,j,k) = 0.0

        sumhz = 0.0
        do kk = zstart, zend
          if ( k == kk ) then
            hz = thetacz/pi
          else
            hz = sin((k-kk)*pi/(nhalfz+1))/((k-kk)*pi/(nhalfz+1))*  &
                 sin((k-kk)*thetacz)/((k-kk)*pi)
          endif
          sumhz=sumhz+hz

        sumhy = 0.0
        do jj = ystart, yend
          if ( j == jj ) then
            hy = thetacy/pi
          else
            hy = sin((j-jj)*pi/(nhalfy+1))/((j-jj)*pi/(nhalfy+1))*  &
                 sin((j-jj)*thetacy)/((j-jj)*pi)
          endif
          sumhy=sumhy+hy

        sumhx = 0.0
        do ii = xstart, xend
          if ( i == ii ) then
            hx = thetacx/pi
          else
            hx = sin((i-ii)*pi/(nhalfx+1))/((i-ii)*pi/(nhalfx+1))*  &
                 sin((i-ii)*thetacx)/((i-ii)*pi)
          endif
          sumhx=sumhx+hx

          xtemp(i,j,k) = xtemp(i,j,k)+hx*hy*hz*xinout(ii,jj,kk)
        enddo
        enddo
        enddo
        if ( sumhx*sumhy*sumhz .le. 0.0) write(6,*) 'digitalfilter: ERROR: sumxyz less than zero'
        xtemp(i,j,k) = xtemp(i,j,k)/(sumhx*sumhy*sumhz)
      enddo
    enddo
  enddo
  xinout = xtemp

end subroutine digitalfilter


end module parqvarsmod
