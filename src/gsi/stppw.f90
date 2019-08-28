module stppwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppwmod    module for stppw and stppw
!  prgmmr:
!
! abstract: module for stppw and stppw
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stppw and its tangent linear stppw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stppw_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-20  kbathmann - split into stppw and stppw_search
!
! subroutines included:
!   sub stppw, stppw_search
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: zero,tpwcon,half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode, only: obsNode
use m_pwNode , only: pwNode
use m_pwNode , only: pwNode_typecast
use m_pwNode , only: pwNode_nextcast

implicit none

PRIVATE
PUBLIC stppw,stppw_search

contains

subroutine stppw_search(pwhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppw       calculate search direction, penalty and contribution to
!                            stepsize for precip. water using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate search direction, penalty and contribution to stepsize from
!           precip. water, using nonlinear qc.
!
! program history log:
!   2019-08-20 kbathmann split the computation of val into its own subroutine
!
!   input argument list:
!     pwhead
!     rq       - search direction for q
!     sges     - stepsize estimates(4)
!     nstep    - number of stepsizes ( == 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for precip. water sges(1:nstep)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: latlon11,nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pwhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) ier,istatus
  integer(i_kind) i1,i2,i3,i4,k,kk
  real(r_kind) w1,w2,w3,w4,pg_pw
  real(r_kind) cg_pw,wgross,wnotgross,pwx
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: rq
  type(pwNode), pointer :: pwptr


  out=zero_quad

!  If no pw data return
  if(.not. associated(pwhead))return

! Retrieve pointers
  ier=0
  call gsi_bundlegetpointer(rval,'q',rq, istatus);ier=istatus+ier
  if(ier/=0)return

  pwptr => pwNode_typecast(pwhead)
  do while (associated(pwptr))
     if(pwptr%luse)then
        w1 = pwptr%wij(1)
        w2 = pwptr%wij(2)
        w3 = pwptr%wij(3)
        w4 = pwptr%wij(4)
!   Calculate precipitable water increment and delta precip water increment
        do k=1,nsig
           i1 = pwptr%ij(1)+(k-1)*latlon11
           i2 = pwptr%ij(2)+(k-1)*latlon11
           i3 = pwptr%ij(3)+(k-1)*latlon11
           i4 = pwptr%ij(4)+(k-1)*latlon11
           pwptr%val =pwptr%val +(w1* rq(i1)+w2* rq(i2)&
                    + w3* rq(i3)+w4* rq(i4))*tpwcon * pwptr%dp(k)
        end do
        do kk=1,nstep
           pwx=pwptr%val2+sges(kk)*pwptr%val
           pen(kk)=pwx*pwx*pwptr%err2
        end do

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pwptr%pg > tiny_r_kind .and. &
                             pwptr%b  > tiny_r_kind) then
           pg_pw=pwptr%pg*varqc_iter
           cg_pw=cg_term/pwptr%b
           wnotgross= one-pg_pw
           wgross = pg_pw*cg_pw/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif
        out(1) = out(1)+pen(1)*pwptr%raterr2
        do kk=1,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pwptr%raterr2
        end do
     end if !luse
     pwptr => pwNode_nextcast(pwptr)
  end do !while associated(pwptr)
  return
end subroutine stppw_search
subroutine stppw(pwhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppw       calculate penalty and contribution to stepsize
!                            for precip. water using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from
!           precip. water, using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-25  wu
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist, generalized to use interpolated delta(pressure)
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stppw and stppw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify to output for b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number
!   of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - udpate to use gsi_bundle
!
!   input argument list:
!     pwhead
!     sges     - stepsize estimates(4)
!     nstep    - number of stepsizes ( == 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for precip. water sges(1:nstep)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none
! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pwhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) kk
  real(r_kind) pg_pw
  real(r_kind) cg_pw,wgross,wnotgross,pwx
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: rq
  type(pwNode), pointer :: pwptr


  out=zero_quad

!  If no pw data return
  if(.not. associated(pwhead))return
  pwptr => pwNode_typecast(pwhead)
  do while (associated(pwptr))
     if(pwptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              pwx=pwptr%val2+sges(kk)*pwptr%val
              pen(kk)=pwx*pwx*pwptr%err2
           end do
        else
           pen(1)=pwptr%res*pwptr%res*pwptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pwptr%pg > tiny_r_kind .and. &
                             pwptr%b  > tiny_r_kind) then
           pg_pw=pwptr%pg*varqc_iter
           cg_pw=cg_term/pwptr%b
           wnotgross= one-pg_pw
           wgross = pg_pw*cg_pw/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif
        out(1) = out(1)+pen(1)*pwptr%raterr2
        do kk=1,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pwptr%raterr2
        end do
     end if !luse
     pwptr => pwNode_nextcast(pwptr)
  end do !while associated(pwptr)
  return
end subroutine stppw

end module stppwmod
