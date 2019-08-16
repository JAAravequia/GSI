module stpradmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpradmod    module for stprad_state and stprad
!  prgmmr:
!
! abstract: module for stprad_state and stprad
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stprad and its tangent linear stprad_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprad_tl
!   2009-08-12  lueken - update documentation
!   2011-05-17  todling - add internal routine set_
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-08-14  kbathmann - split into stprad and stprad_state
!
! subroutines included:
!   sub stprad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use radinfo, only: b_rad,pg_rad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
use intradmod, only:lgoback 
use kinds, only: r_kind,r_quad,i_kind
use m_obsNode, only: obsNode
use m_radNode, only: radNode
use m_radNode, only: radNode_typecast
use m_radNode, only: radNode_nextcast

implicit none
real(r_kind) cg_rad,wgross,wnotgross

PRIVATE
PUBLIC stprad_state,stprad


contains
subroutine stprad_state(radhead,dval,rpred,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad_state compute the value of the current state estimate in 
!                radiance space, as well as the contribution to penalty
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute the current state in radiance space
!
! program history log:
!   2019-08-14 kbathmann split the computation of val into its own subroutine
!
!   input argument list:
!     radhead
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     rpred    - search direction for predictors
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero
  use radinfo, only: npred,jpch_rad
  use radinfo, only: nsigradjac
  use gridmod, only: nsig,latlon11
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use intradmod, only: luseu,lusev,luset,luseq,lusecw,luseoz,luseqg,luseqh,luseqi,luseql, &
          luseqr,luseqs
  use intradmod, only: itv,iqv,ioz,icw,ius,ivs,isst,iqg,iqh,iqi,iql,iqr,iqs,lgoback
  implicit none
  
! Declare passed variables
  class(obsNode), pointer                ,intent(in   ) :: radhead
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep))   ,intent(inout) :: out
  real(r_kind),dimension(max(1,nstep))   ,intent(in   ) :: sges
  real(r_kind),dimension(npred,jpch_rad) ,intent(in   ) :: rpred
  type(gsi_bundle),intent(in) :: dval

! Declare local variables
  integer(i_kind) istatus
  integer(i_kind) n,k,nx,j1,j2,j3,j4,mm, ic1
  real(r_kind) w1,w2,w3,w4
  real(r_kind),dimension(nsigradjac):: rdir
  real(r_kind),pointer,dimension(:) :: rt,rq,rcw,roz,ru,rv,rqg,rqh,rqi,rql,rqr,rqs,rst
  real(r_kind),dimension(max(1,nstep)) :: term,rad
  integer(i_kind) nn,kk,ic
  type(radNode), pointer :: radptr
  
  out=zero_quad

!  If no rad data return
  if(.not. associated(radhead))return

  if(lgoback)return

! Retrieve pointers
  call gsi_bundlegetpointer(dval,'u',  ru, istatus)
  call gsi_bundlegetpointer(dval,'v',  rv, istatus)
  call gsi_bundlegetpointer(dval,'tv' ,rt, istatus)
  call gsi_bundlegetpointer(dval,'q',  rq, istatus)
  call gsi_bundlegetpointer(dval,'cw' ,rcw,istatus)
  call gsi_bundlegetpointer(dval,'oz' ,roz,istatus)
  call gsi_bundlegetpointer(dval,'sst',rst,istatus)
  call gsi_bundlegetpointer(dval,'qg' ,rqg,istatus)
  call gsi_bundlegetpointer(dval,'qh' ,rqh,istatus)
  call gsi_bundlegetpointer(dval,'qi' ,rqi,istatus)
  call gsi_bundlegetpointer(dval,'ql' ,rql,istatus)
  call gsi_bundlegetpointer(dval,'qr' ,rqr,istatus)
  call gsi_bundlegetpointer(dval,'qs' ,rqs,istatus)

  rdir=zero

  radptr=> radNode_typecast(radhead)
  do while(associated(radptr))
     if(radptr%luse)then
        j1=radptr%ij(1)
        j2=radptr%ij(2)
        j3=radptr%ij(3)
        j4=radptr%ij(4)
        w1=radptr%wij(1)
        w2=radptr%wij(2)
        w3=radptr%wij(3)
        w4=radptr%wij(4)
        if(luseu) rdir(ius+1)=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)
        if(lusev) rdir(ivs+1)=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4* rv(j4)
        if (isst>=0) rdir(isst+1)=w1*rst(j1)+w2*rst(j2)+w3*rst(j3)+w4*rst(j4) 
        do n=1,nsig
!          Input state vector
!          Input search direction vector
           if (luset ) rdir(itv+n)=w1*rt(j1)+w2*rt(j2)+w3*rt(j3)+w4*rt(j4)
           if (luseq ) rdir(iqv+n)=w1*rq(j1)+w2*rq(j2)+w3*rq(j3)+w4*rq(j4)
           if (luseoz) rdir(ioz+n)=w1*roz(j1)+w2*roz(j2)+ w3*roz(j3)+w4*roz(j4)
           if (lusecw) rdir(icw+n)=w1*rcw(j1)+w2*rcw(j2)+ w3*rcw(j3)+w4*rcw(j4)
           if (luseqg) rdir(iqg+n)=w1*rqg(j1)+w2*rqg(j2)+ w3*rqg(j3)+w4*rqg(j4)
           if (luseqh) rdir(iqh+n)=w1*rqh(j1)+w2*rqh(j2)+ w3*rqh(j3)+w4*rqh(j4)
           if (luseqi) rdir(iqi+n)=w1*rqi(j1)+w2*rqi(j2)+ w3*rqi(j3)+w4*rqi(j4)
           if (luseql) rdir(iql+n)=w1*rql(j1)+w2*rql(j2)+ w3*rql(j3)+w4*rql(j4)
           if (luseqr) rdir(iqr+n)=w1*rqr(j1)+w2*rqr(j2)+ w3*rqr(j3)+w4*rqr(j4)
           if (luseqs) rdir(iqs+n)=w1*rqs(j1)+w2*rqs(j2)+ w3*rqs(j3)+w4*rqs(j4)
           j1 = j1+latlon11
           j2 = j2+latlon11
           j3 = j3+latlon11
           j4 = j4+latlon11
        end do
        do nn=1,radptr%nchan 
           radptr%val(nn) = zero
!          contribution from bias corection
           ic=radptr%icx(nn)
           if (radptr%use_corr_obs) then  
              do mm=1,radptr%nchan 
                 do nx=1,npred
                    ic1=radptr%icx(mm)
                    radptr%val(nn)=radptr%val(nn)+rpred(nx,ic1)*radptr%rsqrtinv(mm,nn)*radptr%pred(nx,mm)
                 end do
              end do
           else
              do nx=1,npred
                 radptr%val(nn)=radptr%val(nn)+rpred(nx,ic)*radptr%pred(nx,nn)
              end do
           end if
!          contribution from atmosphere
           do k=1,nsigradjac
              radptr%val(nn) =radptr%val(nn) +rdir(k)*radptr%dtb_dvar(k,nn)
           end do
!          calculate radiances for each stepsize
           do kk=1,nstep
              rad(kk)=radptr%val2(nn)+sges(kk)*radptr%val(nn)
           end do
!          calculate contribution to J
           do kk=1,max(1,nstep)
              term(kk)  = radptr%err2(nn)*rad(kk)*rad(kk)
           end do

!          Modify penalty term if nonlinear QC
           if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                               b_rad(ic)  > tiny_r_kind)then
              cg_rad=cg_term/b_rad(ic)
              wnotgross= one-pg_rad(ic)*varqc_iter
              wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
              do kk=1,max(1,nstep)
                 term(kk)  = -two*log((exp(-half*term(kk) )+wgross)/(one+wgross))
              end do
           endif

           out(1) = out(1) + term(1)*radptr%raterr2(nn)
           do kk=2,nstep
              out(kk) = out(kk) + (term(kk)-term(1))*radptr%raterr2(nn)
           end do
        enddo !nn=1,nchan
     endif !luse
     radptr => radNode_nextcast(radptr)
  enddo !while associated(radptr)
  return
end subroutine stprad_state

subroutine stprad(radhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad compute contribution to penalty and stepsize
!                from rad, using nonlinear qc.
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute contribution to penalty and stepsize from radiances.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu
!   1998-02-02  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge stprad and stprad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-06-04  derber  - use quad precision to get reproducability over number
!   of processors
!   2008-04-09  safford - rm unused vars and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - use state_vector in the interface;
!                       - add handlings of sst,oz cases; add pointer_state
!   2010-05-13  todling - update to use gsi_bundle
!                       - on-the-spot handling of non-essential vars
!   2010-07-10  todling - remove omp directives (per merge w/ r8741; Derber?)
!   2011-05-04  todling - merge in Min-Jeong Kim's cloud clear assimilation
!   (connect to Metguess)
!   2011-05-16  todling - generalize entries in radiance jacobian
!   2011-05-17  augline/todling - add hydrometeors
!   2016-07-19  kbathmann- adjustment to bias correction when using correlated
!   obs
!
!   input argument list:
!     radhead
!     sges     - step size estimates(nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration value)
!
!   output argument list:
!     out(1:nstep)   - penalty for radiance data sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables
  class(obsNode), pointer                ,intent(in   ) :: radhead
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep))   ,intent(inout) :: out
  real(r_kind),dimension(max(1,nstep))   ,intent(in   ) :: sges
  real(r_kind),dimension(max(1,nstep)) :: term,rad

  integer(i_kind) nn,kk,ic
  type(radNode), pointer :: radptr

  out=zero_quad

!  If no rad data return
  if(.not. associated(radhead))return

  if(lgoback)return
  radptr=> radNode_typecast(radhead)
  do while(associated(radptr))
     if(radptr%luse)then
        do nn=1,radptr%nchan
           ic=radptr%icx(nn)
           if(nstep > 0)then
!             calculate radiances for each stepsize
              do kk=1,nstep
                 rad(kk)=radptr%val2(nn)+sges(kk)*radptr%val(nn)
              end do
           else
              rad(kk)= radptr%val2(nn)
           end if
        
!          calculate contribution to J
           do kk=1,max(1,nstep)
              term(kk)  = radptr%err2(nn)*rad(kk)*rad(kk)
           end do

!          Modify penalty term if nonlinear QC
           if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                               b_rad(ic)  > tiny_r_kind)then
              cg_rad=cg_term/b_rad(ic)
              wnotgross= one-pg_rad(ic)*varqc_iter
              wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
              do kk=1,max(1,nstep)
                 term(kk)  = -two*log((exp(-half*term(kk) ) + wgross)/(one+wgross))
              end do
           endif

           out(1) = out(1) + term(1)*radptr%raterr2(nn)
           do kk=2,nstep
              out(kk) = out(kk) + (term(kk)-term(1))*radptr%raterr2(nn)
           end do

        end do
     end if

     radptr => radNode_nextcast(radptr)
  end do
  return
end subroutine stprad

end module stpradmod
