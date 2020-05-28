module grdtest
!$$$ module documentation block
!           .      .    .                                       .
! module:   grdtest
!  prgmmr: todling
!
! abstract: Routines and data to perform gradient test
!
! program history log:
!   2009-01-18 todling
!   2010-02-19 treadon - wrap module
!
! subroutines included:
!   sub grtest
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
private
public grtest

contains

subroutine grtest(pdx,itertest,xhat_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grtest
!   prggmr: todling
!
! abstract: The aim is to characterize mathematically the cost-function J on a
!           line in the KDIM-dimensional space defined by point X and direction H.
!           Arbitrarily H is defined by -gradJ(X), the useful direction for
!           minimization. Let us denote by f the real function f(a)=J(X+a.H). A
!           sequence of characteristic quantities are computed and displayed for
!           various values of a :
!                    a=PDX, PDX*10, PDX*100 ... PDX*10**itertest
!           The sequence is prematuraly terminated if f(a)-f(0) changes of sign,
!           which normally indicates we are overshooting the minimum of f (assuming
!           J is convex and H points downwards the slope of J). The characteristic
!           quantities TC0,T1,TC1,T2... test increasingly high orders of regularity.
!           Refer to the comments below for the explanation of each quantity.
!             Owing to numerical truncation errors, the tests normally fail for very 
!           small perturbations. The maximum quality of the test results, even for a
!           bug-free simulator, is limited to a few digits, depending on the machine.
!
! program history log:
!   2009-01-18 todling - some quad precision changes (incomplete)
!   2010-05-05 treadon - use r_kind constant in huge()
!   2010-08-19 lueken  - add only to module use
!
!   input argument list:
!    xhat
!    pdx
!    itertest
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind, r_quad
use constants, only: zero,zero_quad, one_quad
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv, &
    random_cv,deallocate_cv,dot_product,assignment(=)

implicit none

real(r_quad)        , intent(in   ) :: pdx 
integer(i_kind)     , intent(in   ) :: itertest 
type(control_vector), optional, intent(in   ) :: xhat_in

! Local variables
real(r_quad), parameter :: half_quad=0.5_r_quad
real(r_quad), parameter ::  two_quad=2.0_r_quad
type(control_vector) :: xdir,yhat,grad,xhat
real(r_quad) :: zabuf(itertest),zfabuf(itertest),ztf2buf(itertest)
real(r_quad) :: zfy,zf0,zdf0,za,zfa,zdfa
real(r_quad) :: ZT1,ZB,ZFB,ztco,ZTC1,ZT2,ZTC1A,ZTC2,ZTF2
real(r_quad) :: ZTF2L
real(r_quad) :: ZTC00,ZTC02,ZTC10,ZTC12
real(r_quad) :: ZERMIN,ZT1TST,ZREF
integer(i_kind) :: ibest,idig,jj,nprt,ii
logical :: lsavinc

!-----------------------------------------------------------------------------

if (mype==0) write(6,*)'grtest: starting'
if (pdx<=epsilon(pdx)) then
   if (mype==0) write(6,*)'grtest, pdx=',pdx
   write(6,*)'grtest: pdx too small',pdx
   call stop2(131)
endif
lsavinc=.false.
nprt=1

call allocate_cv(xdir)
call allocate_cv(yhat)
call allocate_cv(grad)
call allocate_cv(xhat)

!       1.0 Initial point
!           -------------

if (present(xhat_in)) then
   xhat=xhat_in
   if (mype==0) write(6,*)'grtest: use input xhat'
else
   call random_cv(xhat)
   if (mype==0) write(6,*)'grtest: use random_cv(xhat)'
endif
yhat=xhat
call evaljgrad(yhat,zfy,grad,lsavinc,nprt,'grtest')
zfy=half_quad*zfy

!          1.1 Define perturbation direction ZH

if (mype==0) write(6,*) 'The test direction is the opposite of the gradient'
do ii=1,xdir%lencv
   xdir%values(ii)=-grad%values(ii)
end do

!          1.2 Set function f value and derivative at origin

zf0=zfy
zdf0=dot_product(grad,xdir,r_quad)
if (mype==0) write(6,*)'grtest: F(0)=',zf0,' DF(0)=',zdf0

if (zdf0>zero_quad.and.mype==0) write(6,*) 'GRTEST Warning, DF should be negative'
if (abs(zdf0) < sqrt(epsilon(zdf0))) then
   if (mype==0) write(6,*) 'GRTEST WARNING, DERIVATIVE IS TOO SMALL'
endif

!       2. Loop on test point
!          ------------------
ztf2buf(1)=zero_quad
do jj=1,itertest

   za=pdx*(10.0_r_quad**(jj-1))

   if (mype==0) write(6,*)'grtest iter=',jj,' alpha=',za

!           2.1 Compute f and df at new point y=x+a.h

   do ii=1,yhat%lencv
      yhat%values(ii) = xhat%values(ii) + za * xdir%values(ii)
   end do

   call evaljgrad(yhat,zfy,grad,lsavinc,nprt,'grtest')
   zfy=half_quad*zfy

   zfa=zfy
   zdfa=dot_product(grad,xdir,r_quad)

   if (mype==0) write(6,*)'grtest: alpha=',za,' F(a)=',zfa,' DF(a)=',zdfa

   zabuf(jj)=za
   zfabuf(jj)=zfa

!          2.2 Quantity tc0=f(a)/f(0)-1

!         if f is continuous then tc0->1 at origin,
!         at least linearly with a.

   if (abs(zf0)<=tiny(zf0)) then
!           do not compute T1 in this unlikely case
      if (mype==0) write(6,*) 'grtest: Warning: zf0 is suspiciously small.'
      if (mype==0) write(6,*) 'grtest: F(a)-F(0)=',zfa-zf0
   else
      ztco=zfa/zf0-one_quad
      if (mype==0) write(6,*)'grtest: continuity TC0=',ztco
   endif

!                           f(a)-f(0)
!          2.3 Quantity t1=-----------
!                            a.df(0)

!         if df is the gradient then t1->1 at origin,
!         linearly with a. t1 is undefined if df(0)=0.
   if (abs(za*zdf0)<=sqrt(tiny(zf0))) then
      if (mype==0) write(6,*)'grtest: Warning: could not compute ',&
         'gradient test T1, a.df(0)=',za*zdf0  
   else
      zt1=(zfa-zf0)/(za*zdf0)
      if (mype==0) write(6,*)'grtest: gradient T1=',zt1
   endif

!         2.4 Quantity tc1=( f(a)-f(0)-a.df(0) )/a

!         if df is the gradient and df is continuous,
!         then tc1->0 linearly with a.
   ztc1=(zfa-zf0-za*zdf0)/za
   if (mype==0) write(6,*)'grtest: grad continuity TC1=',ztc1

!         2.5 Quantity t2=( f(a)-f(0)-a.df(0) )*2/a**2

!         if d2f exists then t2 -> d2f(0) linearly with a.
   zt2=(zfa-zf0-za*zdf0)*two_quad/(za**2)
   if (mype==0) write(6,*)'grtest: second derivative T2=',zt2

!         2.6 Quantity tc1a=df(a)-df(0)

!         if df is the gradient in a and df is continuous,
!         then tc1a->0 linearly with a.
   ztc1a=zdfa-zdf0
   if (mype==0) write(6,*)'grtest: a-grad continuity TC1A=',ztc1a

!         2.7 Quantity tc2=( 2(f(0)-f(a))+ a(df(0)+df(a))/a**2

!         if f is exactly quadratic, then tc2=0, always: numerically
!         it has to -> 0 when a is BIG. Otherwise tc2->0 linearly for
!         small a is trivially implied by tc1a and t2.
   ztc2=(two_quad*(zf0-zfa)+za*(zdf0+zdfa))/(za**2)
   if (mype==0) write(6,*)'grtest: quadraticity TC2=',ztc2

!                           2   f(0)-f(b)   f(a)-f(b)
!         2.8 Quantity tf2=---( --------- + --------- )
!                           a       b          a-b
!         if 0, a and b are distinct and f is quadratic then
!         tf2=d2f, always. The estimate is most stable when a,b are big.
!         This test works only after two loops, but it is immune against
!         gradient bugs. 
   if (jj>=2) then
      zb =zabuf (jj-1)
      zfb=zfabuf(jj-1)
      ztf2=two_quad/za*((zf0-zfb)/zb+(zfa-zfb)/(za-zb))
      if (mype==0) write(6,*)'grtest: convexity ZTF2=',ztf2
      ztf2buf(jj)=ztf2
   endif

! End loop
enddo

call deallocate_cv(xdir)
call deallocate_cv(yhat)
call deallocate_cv(grad)
call deallocate_cv(xhat)

!          3. Comment on the results

!       tc0(0)/tc0(2)<.011 -> df looks continuous
!       item with (t1<1 and 1-t1 is min) = best grad test item
!       reldif(tf2(last),tf2(last-1)) = precision on quadraticity

!          3.1 Fundamental checks

if (mype==0) then
   write(6,*) 'GRTEST: TENTATIVE CONCLUSIONS :'

   ztc00=abs(zfabuf(1)-zf0)
   ztc02=abs(zfabuf(3)-zf0)
   if( ztc00/zabuf(1)  <=  1.5_r_quad*(ztc02/zabuf(3)) )then
      write(6,*) 'GRTEST: function f looks continous.'
   else
      write(6,*) 'GRTEST: WARNING f does not look continuous',&
         ' (perhaps truncation problem)'  
   endif

!          3.2 Gradient quality

   if (abs(zdf0)<=sqrt(tiny(zf0))) then
      write(6,*) 'GRTEST: The gradient is 0, which is unusual !'
      ztc10=abs(zfabuf(1)-zf0)
      ztc12=abs(zfabuf(3)-zf0)
      if( ztc10/zabuf(1)**2  <=  1.1_r_quad*ztc12/zabuf(3)**2)then
         write(6,*)'GRTEST: The gradient looks good anyway.'
      endif
   else
!     Find best gradient test index
      zermin=huge(zero)
      ibest=-1
      do jj=1,itertest
         zt1tst=(zfabuf(jj)-zf0)/(zabuf(jj)*zdf0)
         zt1tst=abs(zt1tst-one_quad)
         if (zt1tst<zermin) then
            ibest=jj
            zermin=zt1tst
         endif
      enddo
      if(ibest == -1)then
         write(6,*)'GRTEST: gradient test problem : bad ',&
            'gradient, non-convex cost, or truncation errors ?'  
      else
         idig=int(-log(zermin+tiny(zermin))/log(10.0_r_quad))
         write(6,*)'GRTEST: the best gradient test found has ',&
            idig,' satisfactory digits.'  
         if(idig <= 1)then
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS VERY BAD.'
         elseif(idig <= 3)then
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS SUSPICIOUS.'
         elseif(idig <= 5)then
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS ACCEPTABLE.'
         else
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS EXCELLENT.'
         endif

         if (ibest<=itertest-2) then
            ztc10=abs(zfabuf(ibest  )-zf0-zabuf(ibest  )*zdf0)/zabuf(ibest  )
            ztc12=abs(zfabuf(ibest+2)-zf0-zabuf(ibest+2)*zdf0)/zabuf(ibest+2)
            if(ztc10/zabuf(ibest) <=  1.1_r_quad*ztc12/zabuf(ibest+2) )then
               write(6,*)'GRTEST: Gradient convergence looks good.'
            else
               write(6,*)'GRTEST: Gradient convergence is suspicious.'
            endif
         else
            write(6,*)'GRTEST: could not check grad convergence.'
         endif
      endif
   endif

!            3.3 Quadraticity
!         finite difference quadraticity test (gradient-free)

   ztf2=ztf2buf(itertest)
   ztf2l=ztf2buf(itertest-1)
   write(6,*) 'GRTEST: finite diff. d2f estimate no1:',ztf2
   write(6,*) 'GRTEST: finite diff. d2f estimate no2:',ztf2l
   zref=(abs(ztf2l)+abs(ztf2))/two_quad
   if (zref<=tiny(zref)) then
      write(6,*) 'GRTEST: they are too small to decide whether ',&
         'they agree or not.'  
   else
      idig=int(-log(abs(ztf2l-ztf2)/zref+tiny(ztf2))/log(10.0_r_quad))
      write(6,*) 'GRTEST: the fin.dif. estimates of d2f ',&
         'have ',idig,' satisfactory digits.'
      if(idig <= 1)then
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS BAD.'
      elseif(idig <= 3)then
         write(6,*) 'GRTEST:: THE FD-QUADRATICITY IS SUSPICIOUS.'
      elseif(idig <= 5)then
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS ACCEPTABLE.'
      else
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS EXCELLENT.'
      endif
   endif

   write(6,*) 'grtest: Goodbye.'
endif

return
end subroutine grtest
! ----------------------------------------------------------------------
end module grdtest
