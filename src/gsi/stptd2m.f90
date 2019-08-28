module stptd2mmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptd2mmod    module for stptd2m_search and stptd2m
!  prgmmr:
!
! abstract: module for stptd2m_search and stptd2m
!
! program history log:
!   2014-04-10  pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-26  kbathmann - split into stptd2m and stptd2m_search
!
! subroutines included:
!   sub stptd2m_search,stptd2m
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
use m_td2mNode, only: td2mNode
use m_td2mNode, only: td2mNode_typecast
use m_td2mNode, only: td2mNode_nextcast
implicit none

PRIVATE
PUBLIC stptd2m_search,stptd2m

contains

subroutine stptd2m_search(td2mhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stptd2m_search  calculate search direction, penalty and contribution to stepsize
!
! abstract: calculate search direction, penalty and contribution to stepsize for 2m-dew point
!            with addition of nonlinear qc
!
! program history log:
!   2019-08-26  kbathmann- split the computation of val into its own subroutine
!
!   input argument list:
!     td2mhead
!     rtd2m     - search direction for td2m
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional td2m - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: td2mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) cg_td2m,td2m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_td2m
  real(r_kind),pointer,dimension(:) :: rtd2m
  type(td2mNode), pointer :: td2mptr

  out=zero_quad

! If no td2m data return
  if(.not. associated(td2mhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0

  call gsi_bundlegetpointer(rval,'td2m',rtd2m,istatus);ier=istatus+ier
  if(ier/=0)return

  td2mptr => td2mNode_typecast(td2mhead)
  do while (associated(td2mptr))
     if(td2mptr%luse)then
        j1=td2mptr%ij(1)
        j2=td2mptr%ij(2)
        j3=td2mptr%ij(3)
        j4=td2mptr%ij(4)
        w1=td2mptr%wij(1)
        w2=td2mptr%wij(2)
        w3=td2mptr%wij(3)
        w4=td2mptr%wij(4)

        td2mptr%val =w1*rtd2m(j1)+w2*rtd2m(j2)+w3*rtd2m(j3)+w4*rtd2m(j4)

        do kk=1,nstep
           td2m=td2mptr%val2+sges(kk)*td2mptr%val
           pen(kk)= td2m*td2m*td2mptr%err2
        end do
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. td2mptr%pg > tiny_r_kind .and.  &
                             td2mptr%b  > tiny_r_kind) then
           pg_td2m=td2mptr%pg*varqc_iter
           cg_td2m=cg_term/td2mptr%b
           wnotgross= one-pg_td2m
           wgross = pg_td2m*cg_td2m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*td2mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*td2mptr%raterr2
        end do
     end if !luse

     td2mptr => td2mNode_nextcast(td2mptr)

  end do !while associated(td2mptr)
  
  return
end subroutine stptd2m_search

subroutine stptd2m(td2mhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptd2m      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for 2m-dew point
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca
!   2015-07-10  pondeca  - force return if no td2m data available
!
!   input argument list:
!     td2mhead
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for conventional td2m -
!     sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  class(obsNode),pointer              ,intent(in   ) :: td2mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) kk
  real(r_kind) cg_td2m,td2m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_td2m
  type(td2mNode), pointer :: td2mptr

  out=zero_quad
       
! If no td2m data return
  if(.not. associated(td2mhead))return

 td2mptr => td2mNode_typecast(td2mhead)
  do while (associated(td2mptr))
     if(td2mptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              td2m=td2mptr%val2+sges(kk)*td2mptr%val
              pen(kk)= td2m*td2m*td2mptr%err2
           end do
        else
           pen(1)=td2mptr%res*td2mptr%res*td2mptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. td2mptr%pg > tiny_r_kind .and.  &
                             td2mptr%b  > tiny_r_kind) then
           pg_td2m=td2mptr%pg*varqc_iter
           cg_td2m=cg_term/td2mptr%b
           wnotgross= one-pg_td2m
           wgross = pg_td2m*cg_td2m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*td2mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*td2mptr%raterr2
        end do
     end if !luse

     td2mptr => td2mNode_nextcast(td2mptr)

  end do !while associated(td2mptr)

  return
end subroutine stptd2m
end module stptd2mmod
