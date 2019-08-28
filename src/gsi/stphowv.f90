module stphowvmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stphowvmod    module for stphowv_search and stphowv
!  prgmmr:
!
! abstract: module for stphowv_search and stphowv
!
! program history log:
!   2014-04-10  pondeca
!   2015-07-10  pondeca  - force return if no howv data available
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-26  kbathmann- split into stphowv_search and stphowv
!
! subroutines included:
!   sub stphowv_search,stphowv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode , only: obsNode
use m_howvNode, only: howvNode
use m_howvNode, only: howvNode_typecast
use m_howvNode, only: howvNode_nextcast
implicit none

PRIVATE
PUBLIC stphowv_search,stphowv

contains

subroutine stphowv_search(howvhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stphowv_search  calculate search direction, penalty and contribution to stepsize
!
! abstract: calculate search direction penalty and contribution to stepsize for significant
!            wave height with addition of nonlinear qc
!
! program history log:
!   2019-08-26  kbathmann -split the computation of val into its own subroutine
!
!   input argument list:
!     howvhead
!     rhowv     - search direction for howv
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional howv - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: howvhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) cg_howv,howv,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_howv
  real(r_kind),pointer,dimension(:) :: rhowv
  type(howvNode), pointer :: howvptr

  out=zero_quad

! If no howv data return
  if(.not. associated(howvhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(rval,'howv',rhowv,istatus);ier=istatus+ier
  if(ier/=0)return

  howvptr => howvNode_typecast(howvhead)
  do while (associated(howvptr))
     if(howvptr%luse)then
        j1=howvptr%ij(1)
        j2=howvptr%ij(2)
        j3=howvptr%ij(3)
        j4=howvptr%ij(4)
        w1=howvptr%wij(1)
        w2=howvptr%wij(2)
        w3=howvptr%wij(3)
        w4=howvptr%wij(4)

        howvptr%val =w1*rhowv(j1)+w2*rhowv(j2)+w3*rhowv(j3)+w4*rhowv(j4)

        do kk=1,nstep
           howv=howvptr%val2+sges(kk)*howvptr%val
           pen(kk)= howv*howv*howvptr%err2
        end do
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. howvptr%pg > tiny_r_kind .and.  &
                             howvptr%b  > tiny_r_kind) then
           pg_howv=howvptr%pg*varqc_iter
           cg_howv=cg_term/howvptr%b
           wnotgross= one-pg_howv
           wgross = pg_howv*cg_howv/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*howvptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*howvptr%raterr2
        end do
     end if !luse
     howvptr => howvNode_nextcast(howvptr)

  end do !while associated(howvptr)
  
  return
end subroutine stphowv_search

subroutine stphowv(howvhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stphowv      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for significant wave
! height
!            with addition of nonlinear qc
!
! program history log:
!   2014-05-07  pondeca - add howv
!
!   input argument list:
!     howvhead
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for conventional howv -
!     sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  class(obsNode),pointer              ,intent(in   ) :: howvhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) kk
  real(r_kind) cg_howv,howv,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_howv
  type(howvNode), pointer :: howvptr

  out=zero_quad

! If no howv data return
  if(.not. associated(howvhead))return

  howvptr => howvNode_typecast(howvhead)
  do while (associated(howvptr))
     if(howvptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              howv=howvptr%val2+sges(kk)*howvptr%val
              pen(kk)= howv*howv*howvptr%err2
           end do
        else
           pen(1)=howvptr%res*howvptr%res*howvptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. howvptr%pg > tiny_r_kind .and.  &
                             howvptr%b  > tiny_r_kind) then
           pg_howv=howvptr%pg*varqc_iter
           cg_howv=cg_term/howvptr%b
           wnotgross= one-pg_howv
           wgross = pg_howv*cg_howv/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*howvptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*howvptr%raterr2
        end do
     end if !luse
     howvptr => howvNode_nextcast(howvptr)

  end do !while associated(howvptr)

  return
end subroutine stphowv
end module stphowvmod
