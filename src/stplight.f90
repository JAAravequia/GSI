module stplightmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stplightmod    module for stplight and its tangent linear stplight_tl
!  prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2016-05-19
!
! abstract: module for calculating  stplight and its tangent linear stplight_tl
!
! program history log:
!   2016-05-19  apodaca - original version, based on stpgps
!
! subroutines included:
!   sub stplight
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stplight

contains

subroutine stplight(lighthead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stplight  compute contribution to penalty and stepsize
!                       from lightning, using nonlinear qc    
!   prgmmr: apodaca     org: CSU/CIRA           date: 2016-05-19
!
! abstract:  This routine applies the (linear) operator of the 
!            lightning flash rate model and finds an optimal estimate 
!            of step size as done in steepest descent or conjugate 
!            gradient algorithms. 
!            This version includes nonlinear qc.
!
! program history log:
!   2016-05-19  k apodaca 
!   2016-06-21  k apodaca - update documentation
!   2018-03-07  k apodaca - replaced ob_type with polymorphic obsNode through type casting 
!
!   input argument list:
!     lighthead
!     rt    - search direction (gradxJ) for virtual temperature
!     rq    - search direction (gradxJ) for specific humidity
!     ru    - search direction (gradxJ) for the u-component of wind
!     rv    - search direction (gradxJ) for the v-component of wind
!     st    - analysis increment (correction) for virtual temperature
!     sq    - analysis increment (correction) for specific humidity
!     su    - analysis increment (correction) for u-component of wind
!     sv    - analysis increment (correction) for the v-component of wind 
!     sges  - stepsize estimates (nstep)
!     nstep - number of stepsize estimates (==0 means use outer iteration values)
!                                         
!   output argument list:
!     out(1:nstep)- contribution to penalty from lightning - sges(1:nstep)
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:  
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,one,two,half,tiny_r_kind,cg_term,zero_quad,r3600,fv
  use gridmod, only: nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode   , only: obsNode
  use m_lightNode , only: lightNode
  use m_lightNode , only: lightNode_typecast
  use m_lightNode , only: lightNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode   ),pointer           ,intent(in   ) :: lighthead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j,k,kk,ier,istatus
  integer(i_kind),dimension(nsig) :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12
  real(r_kind) :: val,val2
  real(r_kind) :: w1,w2,w3,w4
  real(r_kind),pointer,dimension(:) :: st,sq,su,sv
  real(r_kind),pointer,dimension(:) :: rt,rq,ru,rv
  type(lightNode), pointer :: lightptr
  ! Variables for TL and r_TL of lightning flash rate
  real(r_kind),dimension(1:nsig)   :: z_TL 
  real(r_kind),dimension(1:nsig)   :: horiz_adv_TL
  real(r_kind),dimension(1:nsig)   :: vert_adv_TL
  real(r_kind),dimension(1:nsig)   :: w_TL
  real(r_kind)                     :: wmaxi1_TL,wmaxi2_TL,wmaxi3_TL,wmaxi4_TL
  real(r_kind)                     :: flashrate_TL,flashratei1_TL,flashratei2_TL
  real(r_kind)                     :: flashratei3_TL,flashratei4_TL
  real(r_kind)                     :: wmax
  real(r_kind),dimension(1:nsig)   :: rz_TL
  real(r_kind),dimension(1:nsig)   :: rhoriz_adv_TL
  real(r_kind),dimension(1:nsig)   :: rvert_adv_TL
  real(r_kind),dimension(1:nsig)   :: rw_TL
  real(r_kind)                     :: rwmaxi1_tl,rwmaxi2_tl,rwmaxi3_tl,rwmaxi4_tl
  real(r_kind)                     :: rflashrate_tl,rflashratei1_tl,rflashratei2_tl
  real(r_kind)                     :: rflashratei3_tl,rflashratei4_tl
  
  real(r_kind) cg_light,wgross,wnotgross
  real(r_kind) pg_light,nref
  real(r_kind),dimension(max(1,nstep))::pen

! Initialize penalty, b1, and b3 to zero
  out=zero_quad

!  If no light data return
  if(.not. associated(lighthead))return


! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'tv', st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'q',  sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'u',  su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',  sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tv', rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',  rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',  ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',  rv,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over observations
  lightptr => lightNode_typecast(lighthead)
  do while (associated(lightptr))
     if(lightptr%luse)then

        val2=-lightptr%res
        if(nstep > 0)then
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

           w1=lightptr%wij(1)
           w2=lightptr%wij(2)
           w3=lightptr%wij(3)
           w4=lightptr%wij(4)

           val=zero

!                .      .    .                                       .

! In the case of lightning observations (e.g. GOES-16/GLM), the schematic shown below is
! used for the interpolation of background fields to the location of an observation (+)
! and for the finite-difference derivation method used in the calculation of the TL of
! the observation operator for lightning flash rate.
!          
!         i6-------i8
!          |       |
!          |       |       
! i10-----i2-------i4------i12
!  |       |       |       |
!  |       |     + |       |
! i9------i1-------i3------i11
!          |       |
!          |       |
!         i5-------i7
!                

!                .      .    .                                       .
     

! Tangent linear of the lightning flash rate observation 
! operator.


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
     val2 = val2 + flashrate_TL


!                .      .    .                                       .



! Search direction (gradxJ) for lightning flash rate observation
! operator

! gradxJ: Tangent linear of height (z)

     rz_TL(:)=zero

     do k=2,nsig-1

        rz_TL(i1(1))=lightptr%jac_z0i1
        rz_TL(i2(1))=lightptr%jac_z0i2
        rz_TL(i3(1))=lightptr%jac_z0i3
        rz_TL(i4(1))=lightptr%jac_z0i4
        rz_TL(i5(1))=lightptr%jac_z0i5
        rz_TL(i6(1))=lightptr%jac_z0i6
        rz_TL(i7(1))=lightptr%jac_z0i7
        rz_TL(i8(1))=lightptr%jac_z0i8
        rz_TL(i9(1))=lightptr%jac_z0i9
        rz_TL(i10(1))=lightptr%jac_z0i10
        rz_TL(i11(1))=lightptr%jac_z0i11
        rz_TL(i12(1))=lightptr%jac_z0i12


        rz_TL(i1(k))=rz_TL(i1(k-1))+lightptr%jac_vertti1(k)*rt(i1(k))          &
                    +lightptr%jac_vertqi1(k)*rq(i1(k))

        rz_TL(i2(k))=rz_TL(i2(k-1))+lightptr%jac_vertti2(k)*rt(i2(k))          &
                    +lightptr%jac_vertqi2(k)*rq(i2(k))

        rz_TL(i3(k))=rz_TL(i3(k-1))+lightptr%jac_vertti3(k)*rt(i3(k))          &
                    +lightptr%jac_vertqi3(k)*rq(i3(k))

        rz_TL(i4(k))=rz_TL(i4(k-1))+lightptr%jac_vertti4(k)*rt(i4(k))          &
                    +lightptr%jac_vertqi4(k)*rq(i4(k))

        rz_TL(i5(k))=rz_TL(i5(k-1))+lightptr%jac_vertti5(k)*rt(i5(k))          &
                    +lightptr%jac_vertqi5(k)*rq(i5(k))

        rz_TL(i6(k))=rz_TL(i6(k-1))+lightptr%jac_vertti6(k)*rt(i6(k))          &
                    +lightptr%jac_vertqi6(k)*rq(i6(k))

        rz_TL(i7(k))=rz_TL(i7(k-1))+lightptr%jac_vertti7(k)*rt(i7(k))          &
                    +lightptr%jac_vertqi7(k)*rq(i7(k))

        rz_TL(i8(k))=rz_TL(i8(k-1))+lightptr%jac_vertti8(k)*rt(i8(k))          &
                    +lightptr%jac_vertqi8(k)*rq(i8(k))

        rz_TL(i9(k))=rz_TL(i9(k-1))+lightptr%jac_vertti9(k)*rt(i9(k))          &
                    +lightptr%jac_vertqi9(k)*rq(i9(k))

        rz_TL(i10(k))=rz_TL(i10(k-1))+lightptr%jac_vertti10(k)*rt(i10(k))      &
                     +lightptr%jac_vertqi10(k)*rq(i10(k))

        rz_TL(i11(k))=rz_TL(i11(k-1))+lightptr%jac_vertti11(k)*rt(i11(k))      &
                     +lightptr%jac_vertqi11(k)*rq(i11(k))

        rz_TL(i12(k))=rz_TL(i12(k-1))+lightptr%jac_vertti12(k)*rt(i12(k))      &
                     +lightptr%jac_vertqi12(k)*rq(i12(k))

      enddo


! gradxJ: Tangent Linear of the Horizontal Advection Section

     rhoriz_adv_TL(:)=zero

     do k=2,nsig-1

        rhoriz_adv_TL(i1(k))=lightptr%jac_zdxi1(k)*ru(i1(k))                    &
                            +lightptr%jac_zdyi1(k)*rv(i1(k))                    &
                            +lightptr%jac_udxi1(k)*(rz_TL(i3(k))-rz_TL(i9(k)))  &
                            +lightptr%jac_vdyi1(k)*(rz_TL(i2(k))-rz_TL(i5(k)))

        rhoriz_adv_TL(i2(k))=lightptr%jac_zdxi2(k)*ru(i2(k))                    & 
                            +lightptr%jac_zdyi2(k)*rv(i2(k))                    &
                            +lightptr%jac_udxi2(k)*(rz_TL(i4(k))-rz_TL(i10(k)))&
                            +lightptr%jac_vdyi2(k)*(rz_TL(i6(k))-rz_TL(i1 (k)))

        rhoriz_adv_TL(i3(k))=lightptr%jac_zdxi3(k)*ru(i3(k))                    &
                            +lightptr%jac_zdyi3(k)*rv(i3(k))                    &
                            +lightptr%jac_udxi3(k)*(rz_TL(i11(k))-rz_TL(i1(k))) &
                            +lightptr%jac_vdyi3(k)*(rz_TL(i4 (k))-rz_TL(i7(k)))

        rhoriz_adv_TL(i4(k))=lightptr%jac_zdxi4(k)*ru(i4(k))                    &
                            +lightptr%jac_zdyi4(k)*rv(i4(k))                    &
                            +lightptr%jac_udxi4(k)*(rz_TL(i12(k))-rz_TL(i2(k))) &
                            +lightptr%jac_vdyi4(k)*(rz_TL(i8 (k))-rz_TL(i3(k)))

       enddo

! gradxJ: Tangent Linear of the Vertical Advection Section

     vert_adv_TL(:)=zero

     do k=1,nsig-1

        rvert_adv_TL(i1(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti1(k)*    &
                          (((one+fv*lightptr%jac_qi1(k))*rt(i1(k)))            &
                          +(lightptr%jac_ti1(k)*fv*rq(i1(k))))

        rvert_adv_TL(i2(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti2(k)*    &
                          (((one+fv*lightptr%jac_qi2(k))*rt(i2(k)))            &
                          +(lightptr%jac_ti2(k)*fv*rq(i2(k))))

        rvert_adv_TL(i3(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti3(k)*    &
                          (((one+fv*lightptr%jac_qi3(k))*rt(i3(k)))            &
                          +(lightptr%jac_ti3(k)*fv*rq(i3(k))))

        rvert_adv_TL(i4(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti4(k)*    &
                          (((one+fv*lightptr%jac_qi4(k))*rt(i4(k)))            &
                          +(lightptr%jac_ti4(k)*fv*rq(i4(k))))

     enddo


! gradxJ: Tangent Linear of Vertical Velocity

     rw_TL(:)=0.

     do k=1,nsig-1

        rw_TL(i1(k))=rhoriz_adv_TL(i1(k))+rvert_adv_TL(i1(k))
        rw_TL(i2(k))=rhoriz_adv_TL(i2(k))+rvert_adv_TL(i2(k))
        rw_TL(i3(k))=rhoriz_adv_TL(i3(k))+rvert_adv_TL(i3(k))
        rw_TL(i4(k))=rhoriz_adv_TL(i4(k))+rvert_adv_TL(i4(k))

     enddo

! Cloud Mask (gradxJ)

! If clouds are present, find the maximum value of vertical velocity
! (wmax_TL) at four points sorounding an observation (+)
! and amongst all vertical levels, otherwise set wmax_TL to zero.

     rwmaxi1_TL=zero
     rwmaxi2_TL=zero
     rwmaxi3_TL=zero
     rwmaxi4_TL=zero

     wmax=-1.e+10

     if (lightptr%jac_wmaxflagi1) then
        do k=1,nsig-1
           if (rw_TL(i1(k)).gt.wmax) then
              lightptr%jac_kverti1=k
              rwmaxi1_TL=rw_TL(i1(lightptr%jac_kverti1))
           endif
        enddo
     else !jac_wmaxflagi1
              rwmaxi1_TL=zero
     endif

     if (lightptr%jac_wmaxflagi2) then
        do k=1,nsig-1
           if (rw_TL(i2(k)).gt.wmax) then
              lightptr%jac_kverti2=k
              rwmaxi2_TL=rw_TL(i2(lightptr%jac_kverti2))
           endif
        enddo
     else
              rwmaxi2_TL=zero
     endif

     if (lightptr%jac_wmaxflagi3) then
        do k=1,nsig-1
           if (rw_TL(i3(k)).gt.wmax) then
              lightptr%jac_kverti3=k
              rwmaxi3_TL=rw_TL(i3(lightptr%jac_kverti3))
           endif
        enddo
     else
              rwmaxi3_TL=zero
     endif

     if (lightptr%jac_wmaxflagi4) then
        do k=1,nsig-1
           if (rw_TL(i4(k)).gt.wmax) then
              lightptr%jac_kverti4=k
              rwmaxi4_TL=rw_TL(i4(lightptr%jac_kverti4))
           endif
        enddo
     else
              rwmaxi4_TL=zero
     endif


! gradxJ: Tangent Linear of Lightning Flash Rate

     rflashratei1_TL=lightptr%jac_fratei1*wmaxi1_TL
     rflashratei2_TL=lightptr%jac_fratei1*wmaxi2_TL
     rflashratei3_TL=lightptr%jac_fratei1*wmaxi3_TL
     rflashratei4_TL=lightptr%jac_fratei1*wmaxi4_TL

!  Interpolation of lightning flash rate TL to observation location (2D field)
!  Forward Model

     flashrate_TL = (w1*flashratei1_TL + w2*flashratei2_TL &
                    +w3*flashratei3_TL + w4*flashratei4_TL)
     val = val + flashrate_TL


!          penalty and gradient

           do kk=1,nstep
              nref=val2+sges(kk)*val
              pen(kk)=nref*nref*lightptr%err2
           end do
        else
           pen(1)=val2*val2*lightptr%err2
        end if

!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. lightptr%pg > tiny_r_kind .and. lightptr%b > tiny_r_kind) then
           pg_light=lightptr%pg*varqc_iter
           cg_light=cg_term/lightptr%b
           wnotgross= one-pg_light
           wgross = pg_light*cg_light/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif
 
!       Cost function
        out(1) = out(1)+pen(1)*lightptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*lightptr%raterr2
        end do

     endif
  
     lightptr => lightNode_nextcast(lightptr)

  end do


 return
end subroutine stplight


end module stplightmod
