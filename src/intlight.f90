module intlightmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlightmod    int module for the observation operator for lightning flash rate (LFR)
!                 
!   prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2016-05-04
!
! abstract: module for the tangent linear (flashrate_TL) and adjoint models (flashrate_AD)
!           of LFR
!
! program history log:
!   2016-05-04  apodaca  - implement TL and AD of the LFR observation operator  
!   2018-02-08  apodaca  - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intlight_
!
! variable definitions:
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$ end documentation block
use m_obsNode, only: obsNode
use m_lightNode, only: lightNode
use m_lightNode, only: lightNode_typecast
use m_lightNode, only: lightNode_nextcast
implicit none

PRIVATE
PUBLIC intlight

interface intlight; module procedure &
          intlight_
end interface

contains

subroutine intlight_(lighthead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlight      TL and subsequent AD of the forward observation operator for LFR
!     prgmmr:    k apodaca          
!        org:    CSU/CIRA, Data Assimilation Group             
!       date:    2016-05-04
!
! abstract: In this program, the tangent linear and adjoint models of a 
!           lightning flash rate observation operator are calculated 
!           using a 12-point horizontal grid for interpolation 
!           and to calculate finite-difference derivatives.
! 
!           The tangent linear equations represent a way to map 
!           the perturbation vectors for the control variables 
!           q, t, u, and v.
!
!           Moreover, the adjoint equations map the sensitivity gradient 
!           vectors for the control variables (q, t, u, v), thus providing 
!           a first order approximation to the sesitivity in the nonlinear 
!           LFR model.
!
! program history log:
!     2018-01-18 apodaca revision of AD code
!
!   input argument list:
!     lighthead   - obs type pointer to obs structure
!     sq          - q increment in grid space
!     st          - t increment in grid space
!     su          - u increment in grid space
!     sv          - v increment in grid space
!
!   output argument list:
!     rq, rt, ru, rv       - updates from the lightning flash rate 
!                            assimilation. 
!
!   comments:
!
! attributes:
!   language: Fortran 90 and/or above
!   machine: 
!
!$$$ end subprogram documentation block

  use kinds,         only: r_kind,i_kind
  use obsmod,        only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use gridmod,       only: nsig
  use qcmod,         only: nlnqc_iter,varqc_iter
  use constants,     only: zero,fv,one,half,tiny_r_kind,cg_term
  use jfunc,         only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar,     only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: lighthead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) k,kk,ier,istatus
  integer(i_kind),dimension(nsig)           :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12
  real(r_kind) val,w1,w2,w3,w4
  real(r_kind) cg_light,grad,p0,wnotgross,wgross,pg_light
  real(r_kind),pointer,dimension(:)         :: sq,su,sv,st
  real(r_kind),pointer,dimension(:)         :: rq,ru,rv,rt
  type(lightNode),  pointer             :: lightptr

! Variables for TL and AD of lightning flash rate
  real(r_kind),dimension(1:nsig)            :: z_TL 
  real(r_kind),dimension(1:nsig)            :: horiz_adv_TL
  real(r_kind),dimension(1:nsig)            :: vert_adv_TL
  real(r_kind),dimension(1:nsig)            :: w_TL
  real(r_kind)                              :: wmaxi1_TL,wmaxi2_TL,wmaxi3_TL,wmaxi4_TL
  real(r_kind)                              :: flashrate_TL,flashratei1_TL,flashratei2_TL
  real(r_kind)                              :: flashratei3_TL, flashratei4_TL
  real(r_kind)                              :: flashrate_AD,flashratei1_AD,flashratei2_AD
  real(r_kind)                              :: flashratei3_AD,flashratei4_AD
  real(r_kind)                              :: wmaxi1_AD,wmaxi2_AD,wmaxi3_AD,wmaxi4_AD
   real(r_kind)                             :: wmax_AD     
  real(r_kind),dimension(1:nsig)            :: z_AD
  real(r_kind),dimension(1:nsig)            :: w_AD
  real(r_kind),dimension(1:nsig)            :: vert_adv_AD,horiz_adv_AD
  real(r_kind),dimension(1:nsig)            :: diffq
  real(r_kind),dimension(1:nsig)            :: difft
  real(r_kind),dimension(1:nsig)            :: diffz
!  wmax variables for lightning flash rate
   real(r_kind)                             :: wmax     
!  Output files
!  character :: tlh_file*40
     

!  If no light data return
  if(.not. associated(lighthead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'tsen',st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier

 
  if(ier/=0)return



  lightptr => lightNode_typecast(lighthead)
  do while (associated(lightptr))

! Load location information into local variables

     w1=lightptr%wij(1)
     w2=lightptr%wij(2)
     w3=lightptr%wij(3)
     w4=lightptr%wij(4)

     do k=1,nsig
        i1(k)=lightptr%ij(1,k)
        i2(k)=lightptr%ij(2,k)
        i3(k)=lightptr%ij(3,k)
        i4(k)=lightptr%ij(4,k)
        i5(k)=lightptr%ij(5,k)
        i6(k)=lightptr%ij(6,k)
        i7(k)=lightptr%ij(7,k)
        i8(k)=lightptr%ij(8,k)
        i9(k)=lightptr%ij(9,k)
        i10(k)=lightptr%ij(10,k)
        i11(k)=lightptr%ij(11,k)
        i12(k)=lightptr%ij(12,k)  
     end do 
     
!                .      .    .                                       .

! In the case of lightning observations (e.g. GOES/GLM), the schematic shown below is
! used for the interpolation of background fields to the location of an observation (+)
! and for the finite-difference derivation method used in the calculation of the TL of
! the observation operator for lightning flash rate.
!
!         i6-------i8
!          |       |
!          |       |
! i10-----i2-------i4-----i12
!  |       |       |       |
!  |       |     + |       |
! i9------i1-------i3-----i11
!          |       |
!          |       |
!         i5-------i7
!

!                .      .    .                                       .
     
! In the following section, the tangent linear of the lightning flash rate observation  
! operator is calculated by being broken into parts.                                               

! Tangent linear of height (z)

     z_TL(:)=zero

     do k=2,nsig-1

        z_TL(i1(1))=lightptr%jac_z0i1
        z_TL(i2(1))=lightptr%jac_z0i2
        z_TL(i3(1))=lightptr%jac_z0i3
        z_TL(i4(1))=lightptr%jac_z0i4
        z_TL(i5(1))=lightptr%jac_z0i5
        z_TL(i6(1))=lightptr%jac_z0i6
        z_TL(i7(1))=lightptr%jac_z0i7
        z_TL(i8(1))=lightptr%jac_z0i8
        z_TL(i9(1))=lightptr%jac_z0i9
        z_TL(i10(1))=lightptr%jac_z0i10
        z_TL(i11(1))=lightptr%jac_z0i11
        z_TL(i12(1))=lightptr%jac_z0i12


        z_TL(i1(k))=z_TL(i1(k-1))+lightptr%jac_vertti1(k)*st(i1(k))          &
                   +lightptr%jac_vertqi1(k)*sq(i1(k))

        z_TL(i2(k))=z_TL(i2(k-1))+lightptr%jac_vertti2(k)*st(i2(k))          &
                   +lightptr%jac_vertqi2(k)*sq(i2(k)) 

        z_TL(i3(k))=z_TL(i3(k-1))+lightptr%jac_vertti3(k)*st(i3(k))          &
                   +lightptr%jac_vertqi3(k)*sq(i3(k))
   
        z_TL(i4(k))=z_TL(i4(k-1))+lightptr%jac_vertti4(k)*st(i4(k))          &
                   +lightptr%jac_vertqi4(k)*sq(i4(k))

        z_TL(i5(k))=z_TL(i5(k-1))+lightptr%jac_vertti5(k)*st(i5(k))          &
                   +lightptr%jac_vertqi5(k)*sq(i5(k))

        z_TL(i6(k))=z_TL(i6(k-1))+lightptr%jac_vertti6(k)*st(i6(k))          &
                   +lightptr%jac_vertqi6(k)*sq(i6(k))

        z_TL(i7(k))=z_TL(i7(k-1))+lightptr%jac_vertti7(k)*st(i7(k))          &
                   +lightptr%jac_vertqi7(k)*sq(i7(k))

        z_TL(i8(k))=z_TL(i8(k-1))+lightptr%jac_vertti8(k)*st(i8(k))          &
                   +lightptr%jac_vertqi8(k)*sq(i8(k))

        z_TL(i9(k))=z_TL(i9(k-1))+lightptr%jac_vertti9(k)*st(i9(k))          &
                   +lightptr%jac_vertqi9(k)*sq(i9(k))

        z_TL(i10(k))=z_TL(i10(k-1))+lightptr%jac_vertti10(k)*st(i10(k))      &
                    +lightptr%jac_vertqi10(k)*sq(i10(k))

        z_TL(i11(k))=z_TL(i11(k-1))+lightptr%jac_vertti11(k)*st(i11(k))      & 
                  +lightptr%jac_vertqi11(k)*sq(i11(k))

        z_TL(i12(k))=z_TL(i12(k-1))+lightptr%jac_vertti12(k)*st(i12(k))      &
                  +lightptr%jac_vertqi12(k)*sq(i12(k))

      enddo 

! Tangent Linear of the Horizontal Advection Section

     horiz_adv_TL(:)=zero

     do k=2,nsig-1
 
        horiz_adv_TL(i1(k))=lightptr%jac_zdxi1(k)*su(i1(k))                   &
                           +lightptr%jac_zdyi1(k)*sv(i1(k))                   &
                           +lightptr%jac_udxi1(k)*(z_TL(i3(k))-z_TL(i9(k)))   &
                           +lightptr%jac_vdyi1(k)*(z_TL(i2(k))-z_TL(i5(k)))

        horiz_adv_TL(i2(k))=lightptr%jac_zdxi2(k)*su(i2(k))                   &
                           +lightptr%jac_zdyi2(k)*sv(i2(k))                   &
                           +lightptr%jac_udxi2(k)*(z_TL(i4(k))-z_TL(i10(k))) &
                           +lightptr%jac_vdyi2(k)*(z_TL(i6(k))-z_TL(i1 (k)))

        horiz_adv_TL(i3(k))=lightptr%jac_zdxi3(k)*su(i3(k))                   &
                           +lightptr%jac_zdyi3(k)*sv(i3(k))                   &
                           +lightptr%jac_udxi3(k)*(z_TL(i11(k))-z_TL(i1(k)))  &
                           +lightptr%jac_vdyi3(k)*(z_TL(i4 (k))-z_TL(i7(k)))

        horiz_adv_TL(i4(k))=lightptr%jac_zdxi4(k)*su(i4(k))                   &
                           +lightptr%jac_zdyi4(k)*sv(i4(k))                   &
                           +lightptr%jac_udxi4(k)*(z_TL(i12(k))-z_TL(i2(k)))  &
                           +lightptr%jac_vdyi4(k)*(z_TL(i8 (k))-z_TL(i3(k)))

       enddo

! Tangent Linear of the Vertical Advection Section

     vert_adv_TL(:)=zero

     do k=1,nsig-1

        vert_adv_TL(i1(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti1(k)*    &
                          (((one+fv*lightptr%jac_qi1(k))*st(i1(k)))           & 
                          +(lightptr%jac_ti1(k)*fv*sq(i1(k))))

        vert_adv_TL(i2(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti2(k)*    &
                          (((one+fv*lightptr%jac_qi2(k))*st(i2(k)))           &
                          +(lightptr%jac_ti2(k)*fv*sq(i2(k))))

        vert_adv_TL(i3(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti3(k)*    &
                          (((one+fv*lightptr%jac_qi3(k))*st(i3(k)))           &
                          +(lightptr%jac_ti3(k)*fv*sq(i3(k))))

        vert_adv_TL(i4(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti4(k)*    &
                          (((one+fv*lightptr%jac_qi4(k))*st(i4(k)))           &
                          +(lightptr%jac_ti4(k)*fv*sq(i4(k))))
      
     enddo


! Tangent Linear of Vertical Velocity

     w_TL(:)=0.

     do k=1,nsig-1

        w_TL(i1(k))=horiz_adv_TL(i1(k))+vert_adv_TL(i1(k))
        w_TL(i2(k))=horiz_adv_TL(i2(k))+vert_adv_TL(i2(k))
        w_TL(i3(k))=horiz_adv_TL(i3(k))+vert_adv_TL(i3(k))
        w_TL(i4(k))=horiz_adv_TL(i4(k))+vert_adv_TL(i4(k))

     enddo
 
!                .      .    .                                       .

! Cloud Mask

! If clouds are present, find the maximum value of vertical velocity  
! (wmax_TL) at four points sorounding an observation (+)  
! and amongst all vertical levels, otherwise set wmax_TL to zero.

     wmaxi1_TL=zero
     wmaxi2_TL=zero
     wmaxi3_TL=zero
     wmaxi4_TL=zero

     wmax=-1.e+10 

     if (lightptr%jac_wmaxflagi1) then
        do k=1,nsig-1
           if (w_TL(i1(k)).gt.wmax) then
              lightptr%jac_kverti1=k
              wmaxi1_TL=w_TL(i1(lightptr%jac_kverti1))
           endif
        enddo
     else !jac_wmaxflagi1
              wmaxi1_TL=zero
     endif 

     if (lightptr%jac_wmaxflagi2) then
        do k=1,nsig-1 
           if (w_TL(i2(k)).gt.wmax) then
              lightptr%jac_kverti2=k
              wmaxi2_TL=w_TL(i2(lightptr%jac_kverti2))
           endif
        enddo
     else 
              wmaxi2_TL=zero
     endif

     if (lightptr%jac_wmaxflagi3) then
        do k=1,nsig-1
           if (w_TL(i3(k)).gt.wmax) then
              lightptr%jac_kverti3=k
              wmaxi3_TL=w_TL(i3(lightptr%jac_kverti3))
           endif
        enddo
     else
              wmaxi3_TL=zero
     endif

     if (lightptr%jac_wmaxflagi4) then
        do k=1,nsig-1
           if (w_TL(i4(k)).gt.wmax) then
              lightptr%jac_kverti4=k
              wmaxi4_TL=w_TL(i4(lightptr%jac_kverti4))
           endif
        enddo
     else
              wmaxi4_TL=zero
     endif


! Tangent Linear of Lightning Flash Rate
    
     flashratei1_TL=lightptr%jac_fratei1*wmaxi1_TL
     flashratei2_TL=lightptr%jac_fratei1*wmaxi2_TL
     flashratei3_TL=lightptr%jac_fratei1*wmaxi3_TL
     flashratei4_TL=lightptr%jac_fratei1*wmaxi4_TL

!  Interpolation of lightning flash rate to observation location (2D field)
!  Forward Model

     flashrate_TL = (w1*flashratei1_TL + w2*flashratei2_TL & 
                   +w3*flashratei3_TL + w4*flashratei4_TL)
     val =  flashrate_TL

     if (luse_obsdiag)then
         if (lsaveobsens) then
            grad = val*lightptr%raterr2*lightptr%err2
            lightptr%diags%obssen(jiter) = grad
         else
            if (lightptr%luse) lightptr%diags%tldepart(jiter)=val
         endif
      end if 

!                .      .    .                                       .

! Adjoint test
 
     if (l_do_adjoint) then
! Difference from observation
        if (.not. lsaveobsens) then
        if (.not. ladtest_obs)  val=val-lightptr%res

!       needed for gradient of nonlinear qc operator
           if (nlnqc_iter .and. lightptr%pg > tiny_r_kind .and.  &
                                lightptr%b  > tiny_r_kind) then
              pg_light=lightptr%pg*varqc_iter
              cg_light=cg_term/lightptr%b
              wnotgross= one-pg_light
              wgross = pg_light*cg_light/wnotgross
              p0   = wgross/(wgross+exp(-half*lightptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs) then
              grad = val
           else
              grad = val*lightptr%raterr2*lightptr%err2
           end if
         endif

!                .      .    .                                       .

! Adjoint of the Lightning Flash Rate Observation Operator   

     flashrate_AD=grad
 
     flashratei1_AD=zero
     flashratei2_AD=zero
     flashratei3_AD=zero
     flashratei4_AD=zero

     flashratei1_AD=flashratei1_AD+w1*flashrate_AD
     flashratei2_AD=flashratei2_AD+w2*flashrate_AD 
     flashratei3_AD=flashratei3_AD+w3*flashrate_AD 
     flashratei4_AD=flashratei4_AD+w4*flashrate_AD

! Adjoint of Maximum Vertical Velocity 

     wmaxi1_AD=zero
     wmaxi2_AD=zero
     wmaxi3_AD=zero
     wmaxi4_AD=zero

     wmaxi1_AD=wmaxi1_AD+lightptr%jac_fratei1*flashratei1_AD
     wmaxi2_AD=wmaxi2_AD+lightptr%jac_fratei2*flashratei2_AD
     wmaxi3_AD=wmaxi3_AD+lightptr%jac_fratei3*flashratei3_AD
     wmaxi4_AD=wmaxi3_AD+lightptr%jac_fratei4*flashratei4_AD

     w_AD(:)=zero        
           
     wmax=-1.e+10

     if (lightptr%jac_wmaxflagi1) then
        do k=nsig-1,1,-1
           if(wmaxi1_AD.gt.wmax) then
           lightptr%jac_kverti1=k
           w_AD(i1(lightptr%jac_kverti1))=wmaxi1_AD
           endif
        enddo 
           else 
           w_AD(i1(k))=zero
     endif
                
     if (lightptr%jac_wmaxflagi2) then
        do k=nsig-1,1,-1
           if (wmaxi2_AD.gt.wmax) then 
              lightptr%jac_kverti2=k
              w_AD(i2(lightptr%jac_kverti2))=wmaxi2_AD              
           endif
        enddo    
           else
              w_AD(i2(k))=zero
     endif

     if (lightptr%jac_wmaxflagi3) then
        do k=nsig-1,1,-1
           if (wmaxi3_AD.gt.wmax) then
              lightptr%jac_kverti3=k
              w_AD(i3(lightptr%jac_kverti3))=wmaxi3_AD
           endif
         enddo
           else
              w_AD(i3(k))=zero
     endif
     
     if (lightptr%jac_wmaxflagi4) then
        do k=nsig-1,1,-1
           if (wmaxi4_AD.gt.wmax) then
               lightptr%jac_kverti4=k
               w_AD(i4(lightptr%jac_kverti4))=wmaxi4_AD
           endif
        enddo
           else
               w_AD(i4(k))=zero
     endif

! Adjoint of Vertical Velocity (from Vertical and Horizontal Advection)

     vert_adv_AD(:)=zero
 
     do k=nsig-1,1,-1

        vert_adv_AD(i4(k))=vert_adv_AD(i4(k))+w_AD(i4(k))
        vert_adv_AD(i3(k))=vert_adv_AD(i3(k))+w_AD(i3(k))
        vert_adv_AD(i2(k))=vert_adv_AD(i2(k))+w_AD(i2(k))
        vert_adv_AD(i1(k))=vert_adv_AD(i1(k))+w_AD(i1(k))

     enddo

     horiz_adv_AD(:)=zero

     do k=nsig-1,2,-1

        horiz_adv_AD(i4(k))=horiz_adv_AD(i4(k))+w_AD(i4(k))
        horiz_adv_AD(i4(k))=horiz_adv_AD(i3(k))+w_AD(i3(k))
        horiz_adv_AD(i2(k))=horiz_adv_AD(i2(k))+w_AD(i2(k))
        horiz_adv_AD(i1(k))=horiz_adv_AD(i1(k))+w_AD(i1(k))

     enddo

! Adjoint of q and t from the Vertical Advection Section

     diffq(:)=zero
     difft(:)=zero

        rq(:)=zero
        rt(:)=zero

     do k=nsig-1,1,-1

        diffq(i1(k))=-(lightptr%jac_ti1(k)*fv*lightptr%jac_vert(K)        &
                     *lightptr%jac_sigdoti1(k))*vert_adv_AD(i1(k))
        difft(i1(k))=-((one+fv*lightptr%jac_qi1(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti1(k))*vert_adv_AD(i1(k))
        diffq(i2(k))=-(lightptr%jac_ti2(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti2(k))*vert_adv_AD(i2(k))
        difft(i2(k))=-((one+fv*lightptr%jac_qi2(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti2(k))*vert_adv_AD(i2(k))
        diffq(i3(k))=-(lightptr%jac_ti3(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti3(k))*vert_adv_AD(i3(k))
        difft(i3(k))=-((one+fv*lightptr%jac_qi3(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti3(k))*vert_adv_AD(i3(k))
        diffq(i4(k))=-(lightptr%jac_ti4(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti4(k))*vert_adv_AD(i4(k))
        difft(i4(k))=-((one+fv*lightptr%jac_qi4(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti4(k))*vert_adv_AD(i4(k))

        rq(i1(k))=rq(i1(k))+diffq(i1(k))

        rt(i1(k))=rt(i1(k))+difft(i1(k))

        rq(i2(k))=rq(i2(k))+diffq(i2(k))

        rq(i3(k))=rq(i3(k))+diffq(i3(k))

        rt(i3(k))=rt(i3(k))+difft(i3(k))

        rq(i4(k))=rq(i4(k))+diffq(i4(k))

        rt(i4(k))=rt(i4(k))+difft(i4(k))

     enddo


 
! Adjoint of z, u, and v from the Horizontal Advection Section
   
      diffz(:)=zero
       z_AD(:)=zero
         rv(:)=zero
         ru(:)=zero

     do k=nsig-1,2,-1

        diffz(i5(k))=-lightptr%jac_vdyi1(k)*horiz_adv_AD(i1(k))
        diffz(i9(k))=-lightptr%jac_udxi1(k)*horiz_adv_AD(i1(k))

        z_AD(i5(k))=z_AD(i5(k))+diffz(i5(k))
        z_AD(i2(k))=z_AD(i2(k))+(lightptr%jac_vdyi1(k)*horiz_adv_AD(i1(k)))
        z_AD(i9(k))=z_AD(i9(k))+(diffz(i9(k)))
        z_AD(i3(k))=z_AD(i3(k))+(lightptr%jac_udxi1(k)*horiz_adv_AD(i1(k)))
  
        rv(i1(k))=rv(i1(k))+(lightptr%jac_zdyi1(k)*horiz_adv_AD(i1(k)))
        ru(i1(k))=ru(i1(k))+(lightptr%jac_zdxi1(k)*horiz_adv_AD(i1(k)))

        diffz(i1(k)) =-lightptr%jac_vdyi2(k)*horiz_adv_AD(i2(k))
        diffz(i10(k))=-lightptr%jac_udxi2(k)*horiz_adv_AD(i2(k))

        z_AD(i1(k))=z_AD(i1(k))+(diffz(i1(k)))
        z_AD(i6(k))=z_AD(i6(k))+(lightptr%jac_vdyi2(k)*horiz_adv_AD(i2(k)))
        z_AD(i10(k))=z_AD(i10(k))+(diffz(i10(k)))
        z_AD(i4(k))=z_AD(i4(k))+(lightptr%jac_udxi2(k)*horiz_adv_AD(i2(k)))
        rv(i2(k))=rv(i2(k))+(lightptr%jac_zdyi2(k)*horiz_adv_AD(i2(k)))
        ru(i2(k))=ru(i2(k))+(lightptr%jac_zdxi2(k)*horiz_adv_AD(i2(k)))

        diffz(i7(k))= -lightptr%jac_vdyi3(k)*horiz_adv_AD(i3(k))
        diffz(i1(k))= -lightptr%jac_udxi3(k)*horiz_adv_AD(i3(k))

        z_AD(i7(k)) =  z_AD(i7(k))+diffz(i7(k))
        z_AD(i4(k)) =  z_AD(i4(k))+(lightptr%jac_vdyi3(k)*horiz_adv_AD(i3(k)))
        z_AD(i1(k)) =  z_AD(i1(k))+diffz(i1(k))
        z_AD(i11(k))=  z_AD(i11(k))+(lightptr%jac_udxi3(k)*horiz_adv_AD(i3(k)))
        rv(i3(k)) =  rv(i3(k))+(lightptr%jac_zdyi3(k)*horiz_adv_AD(i3(k)))
        ru(i3(k)) =  ru(i3(k))+(lightptr%jac_zdxi3(k)*horiz_adv_AD(i3(k)))

        diffz(i3(k))=-lightptr%jac_vdyi4(k)*horiz_adv_AD(i4(k))
        diffz(i2(k))=-z_TL(i2(k))-lightptr%jac_udxi4(k)*horiz_adv_AD(i4(k))

        z_AD(i3(k)) =  z_AD(i3(k))+diffz(i3(k))
        z_AD(i8(k)) =  z_AD(i8(k))+(lightptr%jac_vdyi4(k)*horiz_adv_AD(i4(k)))
        z_AD(i2(k)) = z_TL(i2(k))+diffz(i2(k))
        z_AD(i12(k))= z_AD(i12(k))+(lightptr%jac_udxi4(k)*horiz_adv_AD(i4(k)))
        rv(i4(k)) = rv(i4(k))+(lightptr%jac_zdyi4(k)*horiz_adv_AD(i4(k)))
        ru(i4(k)) = ru(i4(k))+(lightptr%jac_zdxi4(k)*horiz_adv_AD(i4(k)))

     enddo

! Adjoint of q and t from the Calculation of Height (z)
  
     do k=nsig-1,2,-1

        rq(i1(k))=rq(i1(k))+lightptr%jac_vertqi1(k)*z_AD(i1(k))
        rt(i1(k))=rt(i1(k))+lightptr%jac_vertti1(k)*z_AD(i1(k)) 
        z_AD(i1(k-1))=z_AD(i1(k-1))+z_AD(i1(k))
        z_AD(i1(k))=zero

        rq(i2(k))=rq(i2(k))+lightptr%jac_vertqi2(k)*z_AD(i2(k)) 
        rt(i2(k))=rt(i2(k))+lightptr%jac_vertti12(k)*z_AD(i2(k))
        z_AD(i2(k-1))=z_AD(i2(k-1))+z_AD(i2(k))
        z_AD(i2(k))=zero

        rq(i3(k))=rq(i3(k))+lightptr%jac_vertqi3(k)*z_AD(i3(k))
        rt(i3(k))=rt(i3(k))+lightptr%jac_vertti3(k)*z_AD(i3(k))
        z_AD(i3(k-1))=z_AD(i3(k-1))+z_AD(i3(k))
        z_AD(i3(k))=zero

        rq(i4(k))=rq(i4(k))+lightptr%jac_vertqi4(k)*z_AD(i4(k))
        rt(i4(k))=rt(i4(k))+lightptr%jac_vertti4(k)*z_AD(i4(k))
        z_AD(i4(k-1))=z_AD(i4(k-1))+z_AD(i4(k))
        z_AD(i4(k))=zero

        rq(i5(k))=rq(i5(k))+lightptr%jac_vertqi5(k)*z_AD(i5(k))
        rt(i5(k))=rt(i5(k))+lightptr%jac_vertti5(k)*z_AD(i5(k))
        z_AD(i5(k-1))=z_AD(i5(k-1))+z_AD(i5(k))
        z_AD(i5(k))=zero

        rq(i6(k))=rq(i6(k))+lightptr%jac_vertqi6(k)*z_AD(i6(k))
        rt(i6(k))=rt(i6(k))+lightptr%jac_vertti6(k)*z_AD(i6(k))
        z_AD(i6(k-1))=z_AD(i6(k-1))+z_AD(i6(k))
        z_AD(i6(k))=zero

        rq(i7(k))=rq(i7(k))+lightptr%jac_vertqi7(k)*z_AD(i7(k))
        rt(i7(k))=rt(i7(k))+lightptr%jac_vertti7(k)*z_AD(i7(k))
        z_AD(i7(k-1))=z_AD(i7(k-1))+z_AD(i7(k))
        z_AD(i7(k))=zero

        rq(i8(k))=rq(i8(k))+lightptr%jac_vertqi8(k)*z_AD(i8(k))
        rt(i8(k))=rt(i8(k))+lightptr%jac_vertti8(k)*z_AD(i8(k))
        z_AD(i8(k-1))=z_AD(i8(k-1))+z_AD(i8(k))
        z_AD(i8(k))=zero

        rq(i9(k))=rq(i9(k))+lightptr%jac_vertqi9(k)*z_AD(i9(k))
        rt(i9(k))=rt(i9(k))+lightptr%jac_vertti9(k)*z_AD(i9(k))
        z_AD(i9(k-1))=z_AD(i9(k-1))+z_AD(i9(k))
        z_AD(i9(k))=zero

        rq(i10(k))=rq(i10(k))+lightptr%jac_vertqi10(k)*z_AD(i10(k))
        rt(i10(k))=rt(i10(k))+lightptr%jac_vertti10(k)*z_AD(i10(k))
        z_AD(i10(k-1))=z_AD(i10(k-1))+z_AD(i10(k))
        z_AD(i10(k))=zero

        rq(i11(k))=rq(i11(k))+lightptr%jac_vertqi11(k)*z_AD(i11(k))   
        rt(i11(k))=rt(i11(k))+lightptr%jac_vertti11(k)*z_AD(i11(k))
        z_AD(i11(k-1))=z_AD(i11(k-1))+z_AD(i11(k))
        z_AD(i11(k))=zero

        rq(i12(k))=rq(i12(k))+lightptr%jac_vertqi12(k)*z_AD(i12(k))
        rt(i12(k))=rt(i12(k))+lightptr%jac_vertti12(k)*z_AD(i12(k))
        z_AD(i12(k-1))=z_AD(i12(k-1))+z_AD(i12(k))
        z_AD(i12(k))=zero

     enddo
 

     endif !Adjoint

     lightptr => lightNode_nextcast(lightptr)

     enddo
    
  
  return
end subroutine intlight_

end module intlightmod
