module stpswcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpswcpmod    module for stpswcp_search and stpswcp
!  prgmmr:
!
! abstract: module for stpswcp_search and stpswcp
!
! program history log:
!   2019-08-26  kbathmann - split into stpswcp and stpswcp_search
!
! subroutines included:
!   sub stpswcp_search, stpswcp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode, only: obsNode
use m_swcpNode , only: swcpNode
use m_swcpNode , only: swcpNode_typecast
use m_swcpNode , only: swcpNode_nextcast

implicit none

PRIVATE
PUBLIC stpswcp_search, stpswcp

contains

subroutine stpswcp_search(swcphead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpswcp_search  calculate search direciton, penalty and contribution
!                             to stepsize for swcp using nonlinear qc
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: calculate search direciton, penalty and contribution to stepsize from swcp
!           using nonlinear qc.
!
! program history log:
!   2019-08-27 kbathmann- split the computation of val into its own subroutine
!
!   input argument list:
!     swcphead
!     rt       - search direction for t
!     rp       - search direction for p
!     rq       - search direction for q
!     rqi      - search direction for qi
!     rqs      - search direction for qs
!     rqg      - search direction for qg
!     rqh      - search direction for qh
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
  use gridmod, only: nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use obsmod, only: l_wcp_cwm
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: swcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j,kk,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) w1,w2,w3,w4,pg_swcp
  real(r_kind) cg_swcp,wgross,wnotgross,swcpx
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: rt, rp, rq
  real(r_kind),pointer,dimension(:) :: rqi, rqs, rqg, rqh
  real(r_kind) :: rt_TL,rp_TL,rq_TL
  real(r_kind) :: rqi_TL,rqs_TL,rqg_TL,rqh_TL

  type(swcpNode), pointer :: swcpptr


  out=zero_quad

!  If no swcp data return
  if(.not. associated(swcphead))return

! Retrieve pointers
  ier=0

  if (.not.l_wcp_cwm) then
    call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'q'   ,rq,istatus);ier=istatus+ier
  else
    call gsi_bundlegetpointer(rval,'qi',rqi,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qh',rqh,istatus);ier=istatus+ier
  endif ! l_wcp_cwm

  if(ier/=0)return

  swcpptr => swcpNode_typecast(swcphead)
  do while (associated(swcpptr))
     if(swcpptr%luse)then
        do j=1,nsig
          i1(j)=swcpptr%ij(1,j)
          i2(j)=swcpptr%ij(2,j)
          i3(j)=swcpptr%ij(3,j)
          i4(j)=swcpptr%ij(4,j)
        enddo
        w1 = swcpptr%wij(1)
        w2 = swcpptr%wij(2)
        w3 = swcpptr%wij(3)
        w4 = swcpptr%wij(4)
        swcpptr%val=zero

!      Calculate solid-water content path increment and delta swcp increment

        if (.not.l_wcp_cwm) then
           do j=1,nsig
              rt_TL =w1* rt(i1(j))+w2* rt(i2(j))+w3* rt(i3(j))+w4* rt(i4(j))
              rp_TL =w1* rp(i1(j))+w2* rp(i2(j))+w3* rp(i3(j))+w4* rp(i4(j))
              rq_TL =w1* rq(i1(j))+w2* rq(i2(j))+w3* rq(i3(j))+w4* rq(i4(j))
              swcpptr%val  = swcpptr%val+rt_tl*swcpptr%jac_t(j)+rp_tl &
                     *swcpptr%jac_p(j)+rq_tl*swcpptr%jac_q(j)
           enddo
        else
           do j=1,nsig
              rqi_TL =w1* rqi(i1(j))+w2* rqi(i2(j))+w3* rqi(i3(j))+w4* rqi(i4(j))
              rqs_TL =w1* rqs(i1(j))+w2* rqs(i2(j))+w3* rqs(i3(j))+w4* rqs(i4(j))
               rqg_TL =w1* rqg(i1(j))+w2* rqg(i2(j))+w3* rqg(i3(j))+w4* rqg(i4(j))
              rqh_TL =w1* rqh(i1(j))+w2* rqh(i2(j))+w3* rqh(i3(j))+w4* rqh(i4(j))
              swcpptr%val=swcpptr%val+rqi_tl*swcpptr%jac_qi(j)+ rqs_tl*swcpptr%jac_qs(j) &
                          + rqg_tl*swcpptr%jac_qg(j)+ rqh_tl*swcpptr%jac_qh(j)
           enddo        
        endif ! l_wcp_cwm
        do kk=1,nstep
           swcpx=swcpptr%val2+sges(kk)*swcpptr%val
           pen(kk)=swcpx*swcpx*swcpptr%err2
        end do

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. swcpptr%pg > tiny_r_kind .and. &
                             swcpptr%b  > tiny_r_kind) then
           pg_swcp=swcpptr%pg*varqc_iter
           cg_swcp=cg_term/swcpptr%b
           wnotgross= one-pg_swcp
           wgross = pg_swcp*cg_swcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*swcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*swcpptr%raterr2
        end do
     end if !luse

     swcpptr => swcpNode_nextcast(swcpptr)

  end do !while associated(swcpptr)
  return
end subroutine stpswcp_search

subroutine stpswcp(swcphead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpswcp       calculate penalty and contribution to stepsize
!                              for swcp using nonlinear qc
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: calculate penalty and contribution to stepsize from swcp
!           using nonlinear qc.
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in stppw.f90 and stpgps.f90 
!                           - stpswcp.f90 includes 2 stp options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                 operator = f(qi,qs,qg,qh) partition6
!
!   input argument list:
!     swcphead
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
  class(obsNode), pointer             ,intent(in   ) :: swcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) kk
  real(r_kind) cg_swcp,wgross,wnotgross,swcpx,pg_swcp
  real(r_kind),dimension(max(1,nstep))::pen
  type(swcpNode), pointer :: swcpptr


  out=zero_quad

!  If no swcp data return
  if(.not. associated(swcphead))return

  swcpptr => swcpNode_typecast(swcphead)
  do while (associated(swcpptr))
     if(swcpptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              swcpx=swcpptr%val2+sges(kk)*swcpptr%val
              pen(kk)=swcpx*swcpx*swcpptr%err2
           end do
        else
           pen(1)=swcpptr%val2*swcpptr%val2*swcpptr%err2
        end if !nstep>0

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. swcpptr%pg > tiny_r_kind .and. &
                             swcpptr%b  > tiny_r_kind) then
           pg_swcp=swcpptr%pg*varqc_iter
           cg_swcp=cg_term/swcpptr%b
           wnotgross= one-pg_swcp
           wgross = pg_swcp*cg_swcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*swcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*swcpptr%raterr2
        end do
     end if !luse

     swcpptr => swcpNode_nextcast(swcpptr)

  end do !while associated(swcpptr)
  return
end subroutine stpswcp
end module stpswcpmod
