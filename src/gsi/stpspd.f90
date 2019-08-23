module stpspdmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpspdmod    module for stpspd_search stpspd
!  prgmmr:
!
! abstract: module for stpspd_search and stpspd
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpspd and its tangent linear stpspd_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpspd_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-23  kbathmann- split into stpspd_search and stpspd
!
! subroutine included:
!   sub stpspd_search,stpspd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
use gsi_4dvar, only: ltlint
use m_obsNode, only: obsNode
use m_spdNode, only: spdNode
use m_spdNode, only: spdNode_typecast
use m_spdNode, only: spdNode_nextcast

implicit none

PRIVATE
PUBLIC stpspd_search,stpspd

contains

subroutine stpspd_search(spdhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpspd_search calculate search direction, penalty and stepsize terms
!                           for wind speed, with nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate search direction, penalty and stepsize terms for wind speed
!
! program history log:
!   2019-08-23  kbathmann- split the computation of val into its own subroutine
!
!   input argument list:
!     spdhead
!     ru       - search direction for u
!     rv       - search direction for v
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list 
!     out(1:nstep)   - contribution to penalty from wind speed sges(1:nstep)
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
  class(obsNode), pointer             ,intent(in   ) :: spdhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) spdnl,spdtl,uu,vv,spd
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) cg_spd,pencur,wgross,wnotgross
  real(r_kind) pg_spd,pentl
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(spdNode), pointer :: spdptr

  out=zero_quad

!  If no spd data return
  if(.not. associated(spdhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  spdptr => spdNode_typecast(spdhead)
  do while (associated(spdptr))

     if(spdptr%luse)then
        j1 = spdptr%ij(1)
        j2 = spdptr%ij(2)
        j3 = spdptr%ij(3)
        j4 = spdptr%ij(4)
        w1 = spdptr%wij(1)
        w2 = spdptr%wij(2)
        w3 = spdptr%wij(3)
        w4 = spdptr%wij(4)
 
        spdptr%valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
        spdptr%valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
        if (ltlint) then
           spdnl=sqrt(spdptr%valu2*spdptr%valu2+spdptr%valv2*spdptr%valv2)
           spd=spdnl-spdptr%res
           pencur=spd*spd*spdptr%err2
           do kk=1,nstep
              spdtl=spdptr%valu2*spdptr%valv+spdptr%valv2*spdptr%valv
              if (spdnl>tiny_r_kind*100._r_kind) then
                 spdtl=spdtl/spdnl
              else
                 spdtl=zero
              endif
              pentl  =two*spdtl*spd*spdptr%err2
              pen(kk)=pencur+sges(kk)*pentl
           end do
        else
           do kk=1,nstep
              uu=spdptr%valu2+sges(kk)*spdptr%valu
              vv=spdptr%valv2+sges(kk)*spdptr%valv
              spd=sqrt(uu*uu+vv*vv)-spdptr%res
              pen(kk)=spd*spd*spdptr%err2
           end do
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and. &
                             spdptr%b  > tiny_r_kind) then
           pg_spd=spdptr%pg*varqc_iter
           cg_spd=cg_term/spdptr%b
           wnotgross= one-pg_spd
           wgross = pg_spd*cg_spd/wnotgross
           pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)  ) + wgross)/(one+wgross))
           enddo
        endif

        out(1) = out(1)+pen(1)*spdptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*spdptr%raterr2
        end do

     end if
    
     spdptr => spdNode_nextcast(spdptr)

  end do
  return
end subroutine stpspd_search

subroutine stpspd(spdhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspd  calculate penalty and stepsize terms
!                for wind speed, with nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and stepsize terms for wind speed
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpspd and stpspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output b1 and b3
!   2007-03-19  tremolet - binning of observations
!   2007-05-10  tremolet - add opt to run as linear procedure
!   2007-06-04  derber  - use quad precision to get reproducability over number
!   of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2009-01-19  todling - re-implement Tremolet's linearization for q1fy10
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling  - update to use gsi_bundle
!   2010-09-25  todling  - fix linearlization
!
!   input argument list:
!     spdhead
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list
!     out(1:nstep)   - contribution to penalty from wind speed sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: spdhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out

! Declare local variables
  integer(i_kind) kk
  real(r_kind) spdnl,spdtl,uu,vv,spd
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) cg_spd,pencur,wgross,wnotgross
  real(r_kind) pg_spd,pentl
  type(spdNode), pointer :: spdptr

  out=zero_quad

!  If no spd data return
  if(.not. associated(spdhead))return

  spdptr => spdNode_typecast(spdhead)
  do while (associated(spdptr))

     if(spdptr%luse)then
        if(nstep > 0)then
           if (ltlint) then
              spdnl=sqrt(spdptr%valu2*spdptr%valu2+spdptr%valv2*spdptr%valv2)
              spd=spdnl-spdptr%res
              pencur=spd*spd*spdptr%err2
              do kk=1,nstep
                 spdtl=spdptr%valu2*spdptr%valv+spdptr%valv2*spdptr%valv
                 if (spdnl>tiny_r_kind*100._r_kind) then
                    spdtl=spdtl/spdnl
                 else
                    spdtl=zero
                 endif
                 pentl  =two*spdtl*spd*spdptr%err2
                 pen(kk)=pencur+sges(kk)*pentl
              end do
           else
              do kk=1,nstep
                 uu=spdptr%valu2+sges(kk)*spdptr%valu
                 vv=spdptr%valv2+sges(kk)*spdptr%valv
                 spd=sqrt(uu*uu+vv*vv)-spdptr%res
                 pen(kk)=spd*spd*spdptr%err2
              end do
           end if
        else
           pen(1)=spdptr%res*spdptr%res*spdptr%err2
        end if !nstep>0

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and. &
                             spdptr%b  > tiny_r_kind) then
           pg_spd=spdptr%pg*varqc_iter
           cg_spd=cg_term/spdptr%b
           wnotgross= one-pg_spd
           wgross = pg_spd*cg_spd/wnotgross
           pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)  ) + wgross)/(one+wgross))
           enddo
        endif

        out(1) = out(1)+pen(1)*spdptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*spdptr%raterr2
        end do

     end if !luse

     spdptr => spdNode_nextcast(spdptr)

  end do
  return
end subroutine stpspd
end module stpspdmod
