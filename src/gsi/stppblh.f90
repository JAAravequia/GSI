module stppblhmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppblhmod    module for stppblh and stppblh_search
!  prgmmr:
!
! abstract: module for stppblh_search and stppblh
!
! program history log:
!   2009-02-24  zhu
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-26  kbathmann - split into stppblh and stppblh_search
!
! subroutines included:
!   sub stppblh_search,stppblh
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode, only: obsNode
use m_pblhNode, only: pblhNode
use m_pblhNode, only: pblhNode_typecast
use m_pblhNode, only: pblhNode_nextcast

implicit none

PRIVATE
PUBLIC stppblh,stppblh_search

contains

subroutine stppblh_search(pblhhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stppblh_search  calculate search direction, penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate search direction, penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2019-08-26 kbathmann - split the computation of val into its own subroutine
!
!   input argument list:
!     pblhhead
!     rpblh     - search direction for pblh
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional pblh - sges(1:nstep)
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
  class(obsNode), pointer             ,intent(in   ) :: pblhhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) cg_pblh,pblh,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_pblh
  real(r_kind),pointer,dimension(:) :: rpblh
  type(pblhNode), pointer :: pblhptr

  out=zero_quad

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(rval,'pblh',rpblh,istatus);ier=istatus+ier
  if(ier/=0)return

  pblhptr => pblhNode_typecast(pblhhead)
  do while (associated(pblhptr))
     if(pblhptr%luse)then
        j1=pblhptr%ij(1)
        j2=pblhptr%ij(2)
        j3=pblhptr%ij(3)
        j4=pblhptr%ij(4)
        w1=pblhptr%wij(1)
        w2=pblhptr%wij(2)
        w3=pblhptr%wij(3)
        w4=pblhptr%wij(4)

        pblhptr%val =w1*rpblh(j1)+w2*rpblh(j2)+w3*rpblh(j3)+w4*rpblh(j4)

        do kk=1,nstep
           pblh=pblhptr%val2+sges(kk)*pblhptr%val
           pen(kk)= pblh*pblh*pblhptr%err2
        end do
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pblhptr%pg > tiny_r_kind .and.  &
                             pblhptr%b  > tiny_r_kind) then
           pg_pblh=pblhptr%pg*varqc_iter
           cg_pblh=cg_term/pblhptr%b
           wnotgross= one-pg_pblh
           wgross = pg_pblh*cg_pblh/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*pblhptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pblhptr%raterr2
        end do
     end if !luse

     pblhptr => pblhNode_nextcast(pblhptr)

  end do !while associated(pblhptr)
  
  return
end subroutine stppblh_search

subroutine stppblh(pblhhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppblh      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2011-02-23  zhu  - update
!
!   input argument list:
!     pblhhead
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for conventional pblh -
!     sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pblhhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) kk
  real(r_kind) cg_pblh,pblh,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_pblh
  type(pblhNode), pointer :: pblhptr

  out=zero_quad

  pblhptr => pblhNode_typecast(pblhhead)
  do while (associated(pblhptr))
     if(pblhptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              pblh=pblhptr%val2+sges(kk)*pblhptr%val
              pen(kk)= pblh*pblh*pblhptr%err2
           end do
        else
           pen(1)=pblhptr%res*pblhptr%res*pblhptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pblhptr%pg > tiny_r_kind .and.  &
                             pblhptr%b  > tiny_r_kind) then
           pg_pblh=pblhptr%pg*varqc_iter
           cg_pblh=cg_term/pblhptr%b
           wnotgross= one-pg_pblh
           wgross = pg_pblh*cg_pblh/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*pblhptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pblhptr%raterr2
        end do
     end if !luse

     pblhptr => pblhNode_nextcast(pblhptr)

  end do !while associated(pblhptr)

  return
end subroutine stppblh

end module stppblhmod
