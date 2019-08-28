module stplwcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stplwcpmod    module for stplwcp_search and stplwcp
!  prgmmr:
!
! abstract: module for stplwcp_search and stplwcp
!
! program history log:
!   2019-08-26  kbathmann - split into stpswcp and stpswcp_search
!
! subroutines included:
!   sub stplwcp_search, stplwcp
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
use m_lwcpNode , only: lwcpNode
use m_lwcpNode , only: lwcpNode_typecast
use m_lwcpNode , only: lwcpNode_nextcast

implicit none

PRIVATE
PUBLIC stplwcp_search, stplwcp

contains

subroutine stplwcp_search(lwcphead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stplwcp_search calculate search direction, penalty and contribution
!                            to stepsize for lwcp using nonlinear qc
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: calculate search direction, penalty and contribution to stepsize from lwcp
!           using nonlinear qc.
!
! program history log:
!   2019-08-27 kbathmann- split the computation of val into its own subroutine
!
!   input argument list:
!     lwcphead
!     rt       - search direction for t
!     rp       - search direction for p
!     rq       - search direction for q
!     rql      - search direction for ql
!     rqr      - search direction for qr
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
  class(obsNode), pointer             ,intent(in   ) :: lwcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j,kk,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) w1,w2,w3,w4,pg_lwcp
  real(r_kind) cg_lwcp,wgross,wnotgross,lwcpx
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: rt, rp, rq
  real(r_kind),pointer,dimension(:) :: rql, rqr
  real(r_kind) :: rt_TL,rp_TL,rq_TL
  real(r_kind) :: rql_TL,rqr_TL
  type(lwcpNode), pointer :: lwcpptr


  out=zero_quad

!  If no lwcp data return
  if(.not. associated(lwcphead))return

! Retrieve pointers
  ier=0

  if (.not.l_wcp_cwm) then
    call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'q'   ,rq,istatus);ier=istatus+ier
  else
    call gsi_bundlegetpointer(rval,'ql',rql,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qr',rqr,istatus);ier=istatus+ier
  endif ! l_wcp_cwm

  if(ier/=0)return

  lwcpptr => lwcpNode_typecast(lwcphead)
  do while (associated(lwcpptr))
     if(lwcpptr%luse)then
!KAB        if(nstep > 0)then
           w1 = lwcpptr%wij(1)
           w2 = lwcpptr%wij(2)
           w3 = lwcpptr%wij(3)
           w4 = lwcpptr%wij(4)

           do j=1,nsig
             i1(j)=lwcpptr%ij(1,j)
             i2(j)=lwcpptr%ij(2,j)
             i3(j)=lwcpptr%ij(3,j)
             i4(j)=lwcpptr%ij(4,j)
           enddo
             
           lwcpptr%val=zero

!      Calculate liquid-water content path increment and delta lwcp increment

           if (.not.l_wcp_cwm) then
             do j=1,nsig
                rt_TL=w1* rt(i1(j))+w2* rt(i2(j))+w3* rt(i3(j))+w4* rt(i4(j))
                rp_TL=w1* rp(i1(j))+w2* rp(i2(j))+w3* rp(i3(j))+w4* rp(i4(j))
                rq_TL=w1* rq(i1(j))+w2* rq(i2(j))+w3* rq(i3(j))+w4* rq(i4(j))
                lwcpptr%val=lwcpptr%val+rt_tl*lwcpptr%jac_t(j)+rp_tl &
                            *lwcpptr%jac_p(j)+rq_tl*lwcpptr%jac_q(j)
             enddo
           else
             do j=1,nsig
               rql_TL=w1* rql(i1(j))+w2* rql(i2(j))+w3* rql(i3(j))+w4* rql(i4(j))
               rqr_TL=w1* rqr(i1(j))+w2* rqr(i2(j))+w3* rqr(i3(j))+w4* rqr(i4(j))
               lwcpptr%val=lwcpptr%val+rql_tl*lwcpptr%jac_ql(j)+ rqr_tl*lwcpptr%jac_qr(j)
             enddo        
           endif ! l_wcp_cwm

           do kk=1,nstep
              lwcpx=lwcpptr%val2+sges(kk)*lwcpptr%val
              pen(kk)=lwcpx*lwcpx*lwcpptr%err2
           end do
!        else
!           pen(1)=val2*val2*lwcpptr%err2
!KAB        end if !nstep>0

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. lwcpptr%pg > tiny_r_kind .and. &
                             lwcpptr%b  > tiny_r_kind) then
           pg_lwcp=lwcpptr%pg*varqc_iter
           cg_lwcp=cg_term/lwcpptr%b
           wnotgross= one-pg_lwcp
           wgross = pg_lwcp*cg_lwcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*lwcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*lwcpptr%raterr2
        end do
     end if !luse

     lwcpptr => lwcpNode_nextcast(lwcpptr)

  end do !while associated(lwcpptr)
  return
end subroutine stplwcp_search

subroutine stplwcp(lwcphead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplwcp       calculate penalty and contribution to stepsize
!                              for lwcp using nonlinear qc
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: calculate penalty and contribution to stepsize from lwcp
!           using nonlinear qc.
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in stppw.f90 and stpgps.f90 
!                           - stplwcp.f90 includes 2 stp options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                 operator = f(ql,qr) partition6
!
!   input argument list:
!     lwcphead
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
  class(obsNode), pointer             ,intent(in   ) :: lwcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) kk
  real(r_kind) cg_lwcp,wgross,wnotgross,lwcpx,pg_lwcp
  real(r_kind),dimension(max(1,nstep))::pen
  type(lwcpNode), pointer :: lwcpptr


  out=zero_quad

!  If no lwcp data return
  if(.not. associated(lwcphead))return

  lwcpptr => lwcpNode_typecast(lwcphead)
  do while (associated(lwcpptr))
     if(lwcpptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              lwcpx=lwcpptr%val2+sges(kk)*lwcpptr%val
              pen(kk)=lwcpx*lwcpx*lwcpptr%err2
           end do
        else
           pen(1)=lwcpptr%val2*lwcpptr%val2*lwcpptr%err2
        end if !nstep>0

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. lwcpptr%pg > tiny_r_kind .and. &
                             lwcpptr%b  > tiny_r_kind) then
           pg_lwcp=lwcpptr%pg*varqc_iter
           cg_lwcp=cg_term/lwcpptr%b
           wnotgross= one-pg_lwcp
           wgross = pg_lwcp*cg_lwcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*lwcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*lwcpptr%raterr2
        end do
     end if !luse

     lwcpptr => lwcpNode_nextcast(lwcpptr)

  end do !while associated(lwcpptr)
  return
end subroutine stplwcp
end module stplwcpmod
