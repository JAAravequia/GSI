module stpuwnd10mmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpuwnd10mmod    module for stpuwnd10m_search and stpuwnd10
!  prgmmr:
!
! abstract: module for stpuwnd10m_search and stpuwnd10
!
! program history log:
!   2016-05-05  pondeca
!   2017-03-19  yang     - modify code to use polymorphic obsNode
!   2019-08-27  kbathmann - split into stpuwnd10m and stpuwnd10m_search
!
! subroutines included:
!   sub stpuwnd10m_search, stpuwnd10
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode    , only: obsNode
use m_uwnd10mNode, only: uwnd10mNode
use m_uwnd10mNode, only: uwnd10mNode_typecast
use m_uwnd10mNode, only: uwnd10mNode_nextcast
implicit none

PRIVATE
PUBLIC stpuwnd10m_search, stpuwnd10m

contains

subroutine stpuwnd10m_search(uwnd10mhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpuwnd10m_search calculate search direction, penalty and contribution to stepsize
!
! abstract: calculate search direction, penalty and contribution to stepsize for 10m-uwind
!            with addition of nonlinear qc
!
! program history log:
!   2019-08-27 kbathmann- split computation of val into its own subroutine
!
!   input argument list:
!     uwnd10mhead
!     ruwnd10m     - search direction for uwnd10m
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional uwnd10m - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  class(obsNode),pointer           ,intent(in   ) :: uwnd10mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) cg_uwnd10m,uwnd10m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_uwnd10m
  real(r_kind),pointer,dimension(:) :: ruwnd10m
  type(uwnd10mNode), pointer :: uwnd10mptr

  out=zero_quad

! If no uwnd10m data return
  if(.not. associated(uwnd10mhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(rval,'uwnd10m',ruwnd10m,istatus);ier=istatus+ier
  if(ier/=0)return

  uwnd10mptr => uwnd10mNode_typecast(uwnd10mhead) 
  do while (associated(uwnd10mptr))
     if(uwnd10mptr%luse)then
        j1=uwnd10mptr%ij(1)
        j2=uwnd10mptr%ij(2)
        j3=uwnd10mptr%ij(3)
        j4=uwnd10mptr%ij(4)
        w1=uwnd10mptr%wij(1)
        w2=uwnd10mptr%wij(2)
        w3=uwnd10mptr%wij(3)
        w4=uwnd10mptr%wij(4)

        uwnd10mptr%val =w1*ruwnd10m(j1)+w2*ruwnd10m(j2)+w3*ruwnd10m(j3)+w4*ruwnd10m(j4)

        do kk=1,nstep
           uwnd10m=uwnd10mptr%val2+sges(kk)*uwnd10mptr%val
           pen(kk)= uwnd10m*uwnd10m*uwnd10mptr%err2
        end do
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. uwnd10mptr%pg > tiny_r_kind .and.  &
                             uwnd10mptr%b  > tiny_r_kind) then
           pg_uwnd10m=uwnd10mptr%pg*varqc_iter
           cg_uwnd10m=cg_term/uwnd10mptr%b
           wnotgross= one-pg_uwnd10m
           wgross = pg_uwnd10m*cg_uwnd10m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*uwnd10mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*uwnd10mptr%raterr2
        end do
     end if

     uwnd10mptr => uwnd10mNode_nextcast(uwnd10mptr)

  end do
  
  return
end subroutine stpuwnd10m_search
subroutine stpuwnd10m(uwnd10mhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpuwnd10m      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for 10m-uwind
!            with addition of nonlinear qc
!
! program history log:
!   2016-05-05  pondeca
!
!   input argument list:
!     uwnd10mhead
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for conventional uwnd10m -
!     sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  class(obsNode),pointer           ,intent(in   ) :: uwnd10mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) kk
  real(r_kind) cg_uwnd10m,uwnd10m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_uwnd10m
  type(uwnd10mNode), pointer :: uwnd10mptr

  out=zero_quad

! If no uwnd10m data return
  if(.not. associated(uwnd10mhead))return

  uwnd10mptr => uwnd10mNode_typecast(uwnd10mhead)
  do while (associated(uwnd10mptr))
     if(uwnd10mptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              uwnd10m=uwnd10mptr%val2+sges(kk)*uwnd10mptr%val
              pen(kk)= uwnd10m*uwnd10m*uwnd10mptr%err2
           end do
        else
           pen(1)=uwnd10mptr%res*uwnd10mptr%res*uwnd10mptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. uwnd10mptr%pg > tiny_r_kind .and.  &
                             uwnd10mptr%b  > tiny_r_kind) then
           pg_uwnd10m=uwnd10mptr%pg*varqc_iter
           cg_uwnd10m=cg_term/uwnd10mptr%b
           wnotgross= one-pg_uwnd10m
           wgross = pg_uwnd10m*cg_uwnd10m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*uwnd10mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*uwnd10mptr%raterr2
        end do
     end if

     uwnd10mptr => uwnd10mNode_nextcast(uwnd10mptr)

  end do

  return
end subroutine stpuwnd10m
end module stpuwnd10mmod
