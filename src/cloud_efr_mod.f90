module cloud_efr_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    cloud_efr
!
! abstract:  This module contains variables and routines related
!            to information for cloud decomposition and effective radius
!
! program history log:
!   2011-06-20 Yanqiu Zhu
!   2011-11-01 Emily Liu 
!   2013-10-19 Todling    - add initialize/finalize routines; move efr_q vars
!                           from guess to this package
!   2014-06-02 Carley     - Move inquire/read routines associated with use of Ferrier microphysics 
!                           lookup tables from EFFRDS to cloud_init to reduce I/O problems
!   2017-07-25 Nebuda     - add microphysics flag and calc_effrad
!
! subroutines included:
!   sub cloud_calc            - cloud composition
!   sub cloud_calc_gfs        - cloud composition (gfs)
!   sub set_cloud_lower_bound - set lower bound for cloud water (gfs)
!   sub effrds                - effective radius
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,three,five,pi,t0c,r0_05,fv,qcmin
  use gridmod, only: lat2,lon2,nsig,regional
  use guess_grids, only: nfldsig
  implicit none
  save

! set subroutines to public
  public :: cloud_init
  public :: cloud_calc
  public :: cloud_calc_gfs
  public :: cloud_final
  public :: set_cloud_lower_bound
  public :: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
  public :: microphysics
  public :: calc_effrad
  public :: set_small_cwmr


  real(r_kind),allocatable,dimension(:,:,:,:):: efr_ql     ! effective radius for cloud liquid water
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qi     ! effective radius for cloud ice
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qr     ! effective radius for rain
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qs     ! effective radius for snow
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qg     ! effective radius for graupel
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qh     ! effective radius for hail
  integer(i_kind) :: microphysics                          ! 0 Current 1 WSM6 2 Thompson cloud_efr computed from guess fields
                                                           ! set in ncepgfs_io.f90
  character(len=*), parameter :: myname = 'cloud_efr_mod'


! local variables to this module (not public)
  logical,save:: cloud_initialized_=.false.

! - Begin specification of microphysics parameters for Ferrier scheme
!     Mean ice diameters
  real(r_kind), parameter :: DMImin=.05e-3_r_kind, DMImax=1.e-3_r_kind,      &
                             XMImin=1.e6_r_kind*DMImin, XMImax=1.e6_r_kind*DMImax
  integer(i_kind), parameter :: MDImin=XMImin, MDImax=XMImax
!     Mean rain drop diameters vary from 50 microns to 450 microns
  real(r_kind), parameter :: DMRmin=.05E-3_r_kind, DMRmax=.45E-3_r_kind,   &
                             XMRmin=1.E6_r_kind*DMRmin, XMRmax=1.E6_r_kind*DMRmax,              &
                             N0r0=8.E6_r_kind, N0rmin=1.e4_r_kind
  integer(i_kind), parameter :: MDRmin=XMRmin, MDRmax=XMRmax
!     Mean mass of precpitation ice particles as functions of their mean
!     size (in microns)
  real(r_kind) :: MASSI(MDImin:MDImax)
!      Lookup tables for rain  
  real(r_kind) :: MASSR(MDRmin:MDRmax)
!
  logical,save :: use_lookup_table=.false.
! - End specification of Ferrier microphysics related variables  
contains

subroutine cloud_init
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cloud_init       initialize cloud mixing ratio and effective radius
!   prgmmr: todling      org: np22                date: 2013-09-30
!
! abstract: allocate variables related to effective cloud radii
!
! program history log:
!   2013-09-30 Todling
!   2014-06-02 Carley - Added implicit none and inquire/read of Ferrier microphysics
!                       lookup tables (for later use via clous_calc)
use gridmod, only: wrf_mass_regional
implicit none
integer(i_kind) i,j,k,n
logical pcexist

!SIMTB - global case with microphysics
!if(.not.regional) return
 if(cloud_initialized_) return

 allocate (efr_ql(lat2,lon2,nsig,nfldsig),efr_qi(lat2,lon2,nsig,nfldsig), &
           efr_qr(lat2,lon2,nsig,nfldsig),efr_qs(lat2,lon2,nsig,nfldsig), &
           efr_qg(lat2,lon2,nsig,nfldsig),efr_qh(lat2,lon2,nsig,nfldsig))
 do n=1,nfldsig
    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             efr_ql(i,j,k,n)=zero
             efr_qi(i,j,k,n)=zero
             efr_qr(i,j,k,n)=zero
             efr_qs(i,j,k,n)=zero
             efr_qg(i,j,k,n)=zero
             efr_qh(i,j,k,n)=zero
          end do
       end do
    end do
 end do
 cloud_initialized_=.true.
 if (.not. wrf_mass_regional) then
!   READ IN MASSI FROM LOOKUP TABLES
    inquire(file='eta_micro_lookup.dat',exist=pcexist)
    if (pcexist) then
       print *,'cloud init: Reading eta_micro_lookup.dat'
       OPEN (UNIT=1,FILE="eta_micro_lookup.dat",FORM="UNFORMATTED")
       DO I=1,3
          READ(1)
       ENDDO
       READ(1) MASSR
       DO I=1,5
          READ(1)
       ENDDO
       READ(1) MASSI
       CLOSE(1)
       use_lookup_table=.true.
    else
       use_lookup_table=.false.
    end if
 else
    use_lookup_table=.false.
 end if
 
end subroutine cloud_init

subroutine cloud_final
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cloud_final      finalize cloud mixing ratio and effective radius
!   prgmmr: todling      org: np22                date: 2013-09-30
!
! abstract: deallocate variables related to effective cloud radii
!
! program history log:
!   2013-09-30 Todling

  if(.not.cloud_initialized_) return
  deallocate(efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh)
  cloud_initialized_=.false.

end subroutine cloud_final

subroutine calc_effrad


!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_effrad     compute effective ratio for WSM6 & Thompson ! microphysics schemes
!                                assumes tsen & prsl are correct

!                                Zhao effective radius could be set here instead of inside 
!                                set_crtm_cloud but then unnecessary interp to ob instead of 
!                                setting to a constant
!                                this should be called after supersaturation check in read_guess

  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: nfldsig, ges_tsen, ges_prsl
  use constants, only: zero
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpimod, only : mype

  implicit none

! Declare local variables
! Needed for met bundle
  real(r_kind),dimension(:,:,:),pointer::g_q =>NULL()  ! water vapor MR, inside time loop
  real(r_kind),dimension(:,:,:),pointer::g_ql=>NULL()  ! Cloud liquid MR
  real(r_kind),dimension(:,:,:),pointer::g_qi=>NULL()  ! Cloud ice MR
  real(r_kind),dimension(:,:,:),pointer::g_qs=>NULL()  ! Cloud snow MR
  real(r_kind),dimension(:,:,:),pointer::g_qr=>NULL()  ! Cloud rain MR
  real(r_kind),dimension(:,:,:),pointer::g_qg=>NULL()  ! Cloud graupel MR
  real(r_kind),dimension(:,:,:),pointer::g_ni=>NULL()  ! Cloud ice number
  real(r_kind),dimension(:,:,:),pointer::g_nr=>NULL()  ! rain ice number

! available from this module 
! microphysics                                         ! flag 0=Zhao,5=WSM6,7=Thompson
!                                                      ! g_ni & g_nr will be zero for WSM6

!output is efr_ql, efr_qi, efr_qr, efr_qs, efr_qg which is available from the module interface

  integer(i_kind):: i,j,k,n,mp, ier, istatus
  real(r_kind)   :: num0

  if (microphysics > 0)  then

  mp =6  ! WSM6 default
  if (microphysics == 7) mp=8  ! Thompson

! Effective radius function of temperature and pressure, CRTm profile on log prs?
  num0 = zero
  do n=1,nfldsig

     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'q' ,g_q ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'qi',g_qi,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'ql',g_ql,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'qs',g_qs,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'qr',g_qr,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'qg',g_qg,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'ni',g_ni,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(n),'nr',g_nr,istatus)
     ier=ier+istatus
     if (ier==0) then

     do k = 1, nsig
        do j = 1, lon2
           do i = 1, lat2
              efr_qi(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
                                           g_q(i,j,k), g_qi(i,j,k), g_ni(i,j,k),'I',mp)
              efr_ql(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
                                           g_q(i,j,k), g_ql(i,j,k), num0,       'C',mp)
              efr_qs(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
                                           g_q(i,j,k), g_qs(i,j,k), num0,       'S',mp)
              efr_qr(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
                                           g_q(i,j,k), g_qr(i,j,k), g_nr(i,j,k),'R',mp)
              efr_qg(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
                                           g_q(i,j,k), g_qg(i,j,k), num0,       'G',mp)
           enddo
        enddo
     enddo
     else ! failed check that met bundle pointer was assigned
       if(mype==0) write(6,*)trim(myname),' calc_effrad: unable to get met bundle ',&
         'to calculate cloud species effective radius'
     endif

  enddo

  endif ! check that microphysics > 0 and effrad needs to be calculated

  return

end subroutine calc_effrad



subroutine cloud_calc(p0d,q1d,t1d,clwmr,fice,frain,frimef,& 
                      ges_ql,ges_qi,ges_qr,ges_qs,ges_qg,ges_qh,&
                      efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cloud_calc       calculate cloud mixing ratio and effective radius
!   prgmmr: zhu          org: np22                date: 2011-06-18
!
! abstract: calculate cloud mixing ratio and effective radius based on Brad Ferrier's CALMICT
!
! program history log:
!   2011-06-18 Yanqiu Zhu

  use gridmod, only: lat2,lon2,wrf_mass_regional
  implicit none

  integer(i_kind) i,j
  real(r_kind) precice,t1,t2,coef1,coef2,coef
  real(r_kind) qi1
  real(r_kind),dimension(lat2,lon2):: p0d      ! pressure (cb)
  real(r_kind),dimension(lat2,lon2):: p1d      ! pressure (pa)
  real(r_kind),dimension(lat2,lon2):: t1d      ! temperature
  real(r_kind),dimension(lat2,lon2):: q1d      ! specific humidity (kg/kg)
  real(r_kind),dimension(lat2,lon2):: clwmr,fice,frain,frimef
  real(r_kind),dimension(lat2,lon2):: ges_ql   ! mixing ratio of cloud liquid water
  real(r_kind),dimension(lat2,lon2):: ges_qi   ! mixing ratio of cloud ice
  real(r_kind),dimension(lat2,lon2):: ges_qr   ! mixing ratio of rain
  real(r_kind),dimension(lat2,lon2):: ges_qs   ! mixing ratio of snow
  real(r_kind),dimension(lat2,lon2):: ges_qg   ! mixing ratio of graupel
  real(r_kind),dimension(lat2,lon2):: ges_qh   ! mixing ratio of hail
  real(r_kind),dimension(lat2,lon2):: efr_ql   ! mixing ratio of cloud liquid water
  real(r_kind),dimension(lat2,lon2):: efr_qi   ! mixing ratio of cloud ice
  real(r_kind),dimension(lat2,lon2):: efr_qr   ! mixing ratio of rain
  real(r_kind),dimension(lat2,lon2):: efr_qs   ! mixing ratio of snow
  real(r_kind),dimension(lat2,lon2):: efr_qg   ! mixing ratio of graupel
  real(r_kind),dimension(lat2,lon2):: efr_qh   ! mixing ratio of hail

  do j=1,lat2
     do i=1,lon2
        ges_ql(j,i)=zero
        ges_qi(j,i)=zero
        ges_qr(j,i)=zero
        ges_qs(j,i)=zero
        ges_qg(j,i)=zero
        ges_qh(j,i)=zero

        efr_ql(j,i)=zero
        efr_qi(j,i)=zero
        efr_qr(j,i)=zero
        efr_qs(j,i)=zero
        efr_qg(j,i)=zero
        efr_qh(j,i)=zero

        p1d(j,i)=1000.0_r_kind*p0d(j,i)
     end do
  end do

  do j=1,lat2
     do i=1,lon2
        if (clwmr(j,i) <= qcmin) then 
           clwmr(j,i)=zero    !According to B. Ferrier
        else
           if (fice(j,i) > one)  fice(j,i)=one
           if (fice(j,i) < zero) fice(j,i)=zero
           if (frain(j,i) > one)  frain(j,i)=one
           if (frain(j,i) < zero) frain(j,i)=zero

!          Determine composition of condensate in the form of cloud water,
!          cloud ice, snow, graupel, hail, and rain
           qi1=clwmr(j,i) * fice(j,i)
           ges_qi(j,i) = 0.05_r_kind * qi1     ! cloud ice
           precice     = 0.95_r_kind * qi1     ! precipitation ice
           if (t1d(j,i) <= t0c-30.0_r_kind) then
              t1=t0c-30.0_r_kind
              t2=t0c-40.0_r_kind
              coef1=0.05_r_kind
              coef2=0.10_r_kind
              coef=(t1d(j,i)-t2)/(t1-t2)*coef1+(t1d(j,i)-t1)/(t2-t1)*coef2
              ges_qi(j,i) = coef * clwmr(j,i) * fice(j,i)
              precice     = (one-coef) * clwmr(j,i) * fice(j,i)
           end if
           ges_qi(j,i)=max(qcmin,ges_qi(j,i))

           if ((frimef(j,i)>=one) .and. (frimef(j,i)<=5.0_r_kind)) &
              ges_qs(j,i)=max(qcmin,precice) ! snow
           if ((frimef(j,i)>5.0_r_kind) .and. (frimef(j,i)<=20.0_r_kind)) &
              ges_qg(j,i)=max(qcmin,precice) ! graupel
           if (frimef(j,i)>20_r_kind) &
              ges_qh(j,i)=max(qcmin,precice) ! hail

           ges_qr(j,i)=max(qcmin,clwmr(j,i)*(one-fice(j,i))*frain(j,i))       ! rain
           ges_ql(j,i)=max(qcmin,clwmr(j,i)*(one-fice(j,i))*(one-frain(j,i))) !cloud liquid water


!          Calculate effective radius
           if (.not. wrf_mass_regional) &
           call effrds(p1d(j,i),t1d(j,i),q1d(j,i),ges_ql(j,i),qi1,ges_qr(j,i),frimef(j,i),&
                       efr_ql(j,i),efr_qi(j,i),efr_qr(j,i),efr_qs(j,i),efr_qg(j,i),efr_qh(j,i))

        end if ! clwmr(j,i)>qcmin
     end do
  end do
  return
end subroutine cloud_calc

subroutine set_small_cwmr(g_cwmr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_small_cwmr     set cloud water MR to qcmin if smaller

  use gridmod, only: lat2,lon2,nsig
  use constants, only: qcmin
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_cwmr ! mixing ratio of total condensates [Kg/Kg]

! Declare local variables
  integer(i_kind):: i,j,k

! set values of cloud water smaller than qcmin to qcmin
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           g_cwmr(i,j,k) =max(qcmin,g_cwmr(i,j,k))
        end do
     end do
  end do
  return
end subroutine set_small_cwmr

subroutine cloud_calc_gfs(g_ql,g_qi,g_cwmr,g_q,g_tv) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cloud_calc_gfs     calculate cloud mixing ratio
!   prgmmr: eliu          org: np22                date: 2011-11-01
!
! abstract: calculate mixing ratio for each hydrometeor from total condensate 
!
! program history log:
!   2011-11-01 eliu   move the calculation of hydrometeors from ncepgfs_io to cloud_efr module 
!                     (rearranged from Min-Jeong's code)  
!   2014-11-28 zhu  - assign cwgues0 in this subroutine;
!                   - set lower bound to cloud after assigning cwgues0,change atrribute of g_cwmr
!   2016-04-28 eliu - remove cwgues0 to read_gfs subroutine in ncegfs_io.f90


  use gridmod, only: lat2,lon2,nsig
  use constants, only: qcmin
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_ql   ! mixing ratio of cloud liquid water [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_qi   ! mixing ratio of cloud ice [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_cwmr ! mixing ratio of total condensates [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ):: g_q    ! specific humidity [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ):: g_tv   ! virtual temperature [K]

! Declare local variables
  integer(i_kind):: i,j,k
  real(r_kind)   :: work

! Set lower bound to cloud
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           g_cwmr(i,j,k) =max(qcmin,g_cwmr(i,j,k))
        end do
     end do
  end do

! Initialize
  g_ql(:,:,:) = zero 
  g_qi(:,:,:) = zero 

! Calculate mixing ratio of cloud liquid water and ice
  do k = 1, nsig
     do j = 1, lon2
        do i = 1, lat2
           work        = -r0_05*(g_tv(i,j,k)/(one+fv*g_q(i,j,k))-t0c)
           work        = max(zero,work)
           work        = min(one,work)    ! 0<=work<=1 
           g_ql(i,j,k) = g_cwmr(i,j,k)*(one-work)
           g_qi(i,j,k) = g_cwmr(i,j,k)*work
        enddo
     enddo
  enddo
  return
end subroutine cloud_calc_gfs

subroutine set_cloud_lower_bound(g_cwmr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_cloud_lower_bound    
!   prgmmr: eliu          org: np22                date: 2011-11-01
!
! abstract: set minimum value for cloud water mixing ratio  
!
! program history log:
!   2011-11-01 eliu   set minimum value for cloud water mixing ratio 

  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_cwmr   ! mixing ratio of cloud liquid water [Kg/Kg]

! Declare local variables
  integer(i_kind):: i,j,k

! Set lower bound for cloud water  mixing ratio (according to B. Ferrier)
  do k = 1, nsig
     do j = 1, lon2
        do i = 1, lat2
           if (g_cwmr(i,j,k) <= qcmin) then
              g_cwmr(i,j,k)=zero   
           endif
        enddo
     enddo
  enddo
  return
end subroutine set_cloud_lower_bound 

      SUBROUTINE EFFRDS(P1D,T1D,Q1D,QW1,QI1,QR1,FS1D, &
                        EFR_QL,EFR_QI,EFR_QR,EFR_QS,EFR_QG,EFR_QH)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    EFFRDS      COMPUTES EFFECTIVE RADIUS
!   PRGRMMR: JIN         ORG: W/NP2      DATE: 01-08-14       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES EFFECTIVE RADIUS. 
!     THE CODE IS BASED ON SUBROUTINE CALMICT.
!     
! PROGRAM HISTORY LOG:
!   01-08-14  YI JIN 
!   02-02-11  Brad Ferrier - Minor changes for consistency w/ NMM model
!   04-11-10  Brad Ferrier - Removed cloud fraction algorithm
!   04-11-17  H CHUANG - WRF VERSION     
!   11-06-20  Yanqiu Zhu - made changes on CALMICT to be called in GSI
!   14-06-02  Jacob Carley - Move lookup table inquire/read to cloud_init
!
! USAGE:    CALL effrds(T1D,Q1D,QW1,QI1,QR1,FS1D,NLICE1)
!   INPUT ARGUMENT LIST:
!     P1D     - PRESSURE (PA)
!     T1D     - TEMPERATURE (K)
!     Q1D     - SPECIFIC HUMIDITY (KG/KG)
!     QW1   - CLOUD WATER MIXING RATIO (KG/KG)
!     QI1   - TOTAL CLOUD ICE (cloud ice & snow) MIXING RATIO (KG/KG)
!     QR1   - RAIN MIXING RATIO (KG/KG)
!     FS1D  - F_RimeF ("Rime Factor", ratio of total ice growth
!                       to deposition growth)
!
!   OUTPUT ARGUMENT LIST:
!     NLICE1
!
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!        FPVSX
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!$$$  
!
      implicit none
 
      INTEGER(i_kind) INDEXS, INDEXR

      real(r_kind),parameter:: d608=0.608_r_kind
      real(r_kind),parameter:: fmw=18.015_r_kind
      real(r_kind),parameter:: fmd=28.964_r_kind
      real(r_kind),parameter:: eps=fmw/fmd
      real(r_kind),parameter:: rd=287.04_r_kind
      real(r_kind),parameter:: oneps=1.0_r_kind-eps
      real(r_kind),parameter:: NLImin=1.0e3_r_kind
      real(r_kind),parameter:: NLImax=5.0e3_r_kind
      real(r_kind),parameter:: RHOL=1000.0_r_kind


      real(r_kind),intent(in) :: P1D,T1D,Q1D
      real(r_kind),intent(in) :: QW1,QI1,QR1,FS1D
      
!     local variables
      real(r_kind) tem4,indexw,indexi
      real(r_kind) N0r,RHgrd,C_N0r0
      real(r_kind) TC,Flimass,Flarge,     &
           Fsmall,RimeF,Xsimass,Qice,Qsat,ESAT,WV,RHO,RRHO,RQR,          &
           Qsigrd,WVQW,Dum,XLi,Qlice,DLI,xlimass,NLICE1

!     Various rain lookup tables
      REAL(R_KIND) RQR_DRmin,RQR_DRmax,CN0r0,CN0r_DMRmin,CN0r_DMRmax

      real(r_kind) rhox  ! assumed density of the large ice in kg m^-3
      real(r_kind) efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh

!************************************************************************
!     liquid water cloud drop size
      tem4=max(zero,(t0c-T1D)*r0_05)
      indexw=five + five * min(one, tem4)

!     cloud ice drop size
      indexi=50.0_r_kind  ! microns

!     effective radius for liquid water cloud and cloud ice
      efr_ql=1.5_r_kind*indexw
      efr_qi=1.5_r_kind*indexi

!     Initialize variables
      efr_qr=zero
      efr_qs=zero
      efr_qg=zero
      efr_qh=zero

!     Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
      TC=T1D-t0c
      WV=Q1D/(one-Q1D)
      ESAT=1000._r_kind*FPVSX(T1D)
      QSAT=EPS*ESAT/(P1D-ESAT)
      RHO=P1D/(RD*T1D*(one+D608*Q1D))  ! air density in kg m^-3
      RRHO=one/RHO

      if (use_lookup_table) then
         ! MASSR and MASSI are read and initialized in cloud_init
         RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
         RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm
         C_N0r0=PI*RHOL*N0r0
         CN0r0=1.E6_r_kind/C_N0r0**.25_r_kind
         CN0r_DMRmin=1.0_r_kind/(PI*RHOL*DMRmin**4)
         CN0r_DMRmax=1.0_r_kind/(PI*RHOL*DMRmax**4)
!         print *,'MICROINIT: MDRmin, MASSR(MDRmin)=',MDRmin,MASSR(MDRmin)
!         print *,'MICROINIT: MDRmax, MASSR(MDRmax)=',MDRmax,MASSR(MDRmax)
!        print *,  'ETA2P:MASSI(50)= ', MASSI(50)
!        print *,  'ETA2P:MASSI(450)= ', MASSI(450)
!        print *,  'ETA2P:MASSI(1000)= ', MASSI(1000)

!        Based on code from GSMCOLUMN in model to determine reflectivity from rain
!        INDEXR is the mean drop size in microns
         IF (QR1 > qcmin) THEN
           RQR=RHO*QR1
           IF (RQR <= RQR_DRmin) THEN
             N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
             INDEXR=MDRmin
           ELSE IF (RQR >= RQR_DRmax) THEN
             N0r=CN0r_DMRmax*RQR
             INDEXR=MDRmax
           ELSE
             N0r=N0r0
             INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25_r_kind, XMRmax) )
           ENDIF
           efr_qr=1.5_r_kind*INDEXR
         ENDIF        !--- End IF (QR1 > qcmin) block


!        Based on code from GSMCOLUMN in model to determine partition of 
!        total ice into cloud ice & snow (precipitation ice)
         IF (QI1 > qcmin) THEN
!          Initialize RHgrd, grid-scale RH for onset of condensation
           RHgrd=ONE

           QICE=QI1
           RHO=P1D/(RD*T1D*(ONE+ONEPS*Q1D))
           RRHO=ONE/RHO
           QSIgrd=RHgrd*QSAT
           WVQW=WV+QW1

!          * FLARGE  - ratio of number of large ice to total (large & small) ice
!          * FSMALL  - ratio of number of small ice crystals to large ice particles
!           ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!           * XSIMASS - used for calculating small ice mixing ratio
!           * XLIMASS - used for calculating large ice mixing ratio
!           * INDEXS  - mean size of snow to the nearest micron (units of microns)
!           * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!                       rimed) ice mass to the unrimed ice mass (>=1)
!           * FLIMASS - mass fraction of large ice
!           * QTICE   - time-averaged mixing ratio of total ice
!           * QLICE   - time-averaged mixing ratio of large ice
!           * NLICE1   - time-averaged number concentration of large ice

           IF (TC>=ZERO .OR. WVQW<QSIgrd) THEN
             FLARGE=ONE
           ELSE
             FLARGE=.03_r_kind   !- was .2, Brad modified this to get better RH score
             IF (TC>=-8.0_r_kind .AND. TC<=-3.0_r_kind) FLARGE=.5_r_kind*FLARGE
           ENDIF
           FSMALL=(ONE-FLARGE)/FLARGE
           XSIMASS=RRHO*MASSI(MDImin)*FSMALL
           DUM=XMImax*EXP(.0536_r_kind*TC)
           INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
           RimeF=MAX(one, FS1D )
           XLIMASS=RRHO*RimeF*MASSI(INDEXS)
           FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
           QLICE=FLIMASS*QICE
           NLICE1=QLICE/XLIMASS
           IF (NLICE1<NLImin .OR. NLICE1>NLImax) THEN

!            Force NLICE1 to be between NLImin and NLImax
             DUM=MAX(NLImin, MIN(NLImax, NLICE1) )
             XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
             IF (XLI<=MASSI(MDImin) ) THEN
               INDEXS=MDImin
             ELSE IF (XLI<=MASSI(450) ) THEN
               DLI=9.5885E5_r_kind*XLI**.42066_r_kind         ! DLI in microns
               INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
             ELSE IF (XLI<=MASSI(MDImax) ) THEN
               DLI=3.9751E6_r_kind*XLI**.49870_r_kind         ! DLI in microns
               INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
             ELSE 
               INDEXS=MDImax
!              8/22/01: Increase density of large ice if maximum limits
!              are reached for number concentration (NLImax) and mean size
!              (MDImax).  Done to increase fall out of ice.
               IF (DUM>=NLImax)                              &
                 RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
             ENDIF             ! End IF (XLI<=MASSI(MDImin) )
             XLIMASS=RRHO*RimeF*MASSI(INDEXS)
             FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
             QLICE=FLIMASS*QICE
             NLICE1=QLICE/XLIMASS
           ENDIF               ! End IF (NLICE<NLImin ...
         ENDIF                 ! End IF (QI1>0.) THEN

      else ! "eta_micro_lookup.dat" not exist
         IF (QR1>qcmin) efr_qr=1.5_r_kind*300_r_kind
         NLICE1=20.0e3_r_kind
         QLICE=0.95_r_kind*QI1
      end if ! pcexist   


!     Calculate effective radius
      IF (QI1>qcmin) THEN
        if (fs1d<=5.0_r_kind) then 
           rhox=100.0_r_kind
           efr_qs=1.5_r_kind*(RHO*QLICE/(PI*RHOX*NLICE1))**(one/three)*1.0e6_r_kind
        end if
        if ((fs1d>5.0_r_kind) .and. (fs1d<=20.0_r_kind)) then 
           rhox=400.0_r_kind
           efr_qg=1.5_r_kind*(RHO*QLICE/(PI*RHOX*NLICE1))**(one/three)*1.0e6_r_kind
        end if
        if (fs1d>20_r_kind) then 
           rhox=900.0_r_kind
           efr_qh=1.5_r_kind*(RHO*QLICE/(PI*RHOX*NLICE1))**(one/three)*1.0e6_r_kind
        end if
      END IF

      RETURN
      END SUBROUTINE EFFRDS


      real(r_kind) function fpvsx(t)
      use constants, only: tmix, xai, xbi, xa, xb, ttp, psatk, init_constants
      implicit none

      real(r_kind) :: t
      real(r_kind) :: tr

      call init_constants(.true.)

      tr=ttp/t
 
      if(t>=ttp)then
        fpvsx=psatk*(tr**xa)*exp(xb*(one-tr))
      else
        fpvsx=psatk*(tr**xai)*exp(xbi*(one-tr))
      endif

      return
      end function fpvsx

REAL FUNCTION get_effrad(pmid,t,q,q_mr,q_num,species,mp_opt)

  use constants, only: zero

!             efr_qi(i,j,k,n) = get_effrad(ges_prsl(i,j,k,n),ges_tsen(i,j,k,n), &
!                                          g_q(i,j,k), g_qi(i,j,k), g_ni(i,j,k),'I',mp)

        real(r_kind)     :: pmid ! mid level pressure (ges_prsl)
        real(r_kind)     :: t    ! sensible temperature (ges_tsen)
        real(r_kind)     :: q    ! Mixing ratio of water vapor
        real(r_kind)     :: q_mr ! Mixing ratio of cloud species
        real(r_kind)     :: q_num ! number of cloud species
        character(LEN=1) :: species ! character flag of which species 
                                    ! C=cloud liquid, I=cloud ice, R=rain, S=snow, G=graupel
        integer(i_kind)  :: mp_opt  ! 6= WSM6, 8= Thompson, 5=Ferrier, 99= Zhao

        real(r_kind)     :: effrad ! effective radius
        real(r_kind)     :: qqw,qqi,qqr,qqs,qqg
        real(r_kind)     :: f_rimef
        real(r_kind)     :: qqnr,qqni,nlice,nrain

        qqw = zero
        qqi = zero
        qqr = zero
        qqs = zero
        qqg = zero
        f_rimef = zero
        nlice = zero
        nrain = zero

        select case(species)
           case("C")
              qqw = q_mr
           case("R")
              qqr = q_mr
              nrain = q_num
              qqnr = q_num
           case("G")
              qqg = q_mr
           case("S")
              qqs = q_mr
           case("I")
              qqi = q_mr
              nlice = q_num
              qqni = q_num
        end select
        get_effrad = EFFR(pmid,t,q,qqw,qqi,qqr,f_rimef, nlice, nrain, &
                     qqs,qqg,qqnr,qqni,mp_opt,species)

return
end function get_effrad

REAL FUNCTION EFFR(pmid,t,q,qqw,qqi,qqr,f_rimef, nlice, nrain, &
                   qqs,qqg,qqnr,qqni,mp_opt,species)

!       JASON OTKIN AND WILLIAM LEWIS
!       09 DECEMBER 2014

! use params_mod, only: pi, rd, d608
  use constants, only: pi
  use kinds, only: r_kind,i_kind

        implicit none

        real(r_kind) :: pmid,t,q,qqw,qqi,qqr,qqs,qqg,f_rimef,nlice,nrain
        real(r_kind) :: qqnr,qqni
        character(LEN=1) :: species

        integer(i_kind)                         :: n,count,count1,mp_opt
        real(r_kind) :: rho, ncc, rhox
        real(r_kind) :: n0_s, n0_r, n0_g

! these are in constants.F90 but have an if (regional) around them
        real(r_kind),parameter:: d608=0.608_r_kind
        real(r_kind),parameter:: rd=287.04_r_kind

!-------------------------------------------------------------------------------
!  GAMMA FUNCTION & RELATED VARIABLES
!-------------------------------------------------------------------------------

        real(r_kind) :: gamma
        real(r_kind) :: gamma_crg, gamma_i, gamma_s

!       real :: WGAMMA, GAMMLN
        real(r_kind)    :: rc,mu_c,am_c,bm_c,cce(3,15),ccg(3,15),ocg1(15),ocg2(15)
        integer(i_kind) :: nu_c

        real(r_kind), dimension(0:15), parameter:: g_ratio = (/6,24,60,120,210, &
     &              336,504,720,990,1320,1716,2184,2730,3360,4080,4896/)

        real(r_kind)    :: rr, mu_r, am_r, bm_r, cre(3), crg(3), ore1, org1, org2
        real(r_kind)    :: mvd_r, ron_sl, ron_r0, ron_c0, ron_c1, ron_c2, obmr

        real(r_kind)    :: ri, mu_i, am_i, bm_i, cie(3), cig(3), oig1, oig2, obmi

        real(r_kind)    :: rs, am_s, oams, cse(3)
        real(r_kind)    :: loga, a, b, tc0, smob, smo2, smoc
        REAL, PARAMETER:: mu_s = 0.6357
        REAL, PARAMETER:: Kap0 = 490.6
        REAL, PARAMETER:: Kap1 = 17.46
        REAL, PARAMETER:: Lam0 = 20.78
        REAL, PARAMETER:: Lam1 = 3.29

!-------------------------------------------------------------------------------
!  MINIMUM/MAXIMUM CONSTANTS FOR ALL SCHEMES
!-------------------------------------------------------------------------------

        real(r_kind), parameter :: eps=0.622, beta_crg=3., beta_i=2.,beta_s=2.4

        real(r_kind), parameter :: min_qc=1.e-7, min_qr=1.e-7, min_qi=1.e-8,min_qs=1.e-8, min_qg=1.e-7
        real(r_kind), parameter :: min_c=2.e-6,  min_r=20.e-6, min_i=4.e-6,min_s=20.e-6, min_g=20.e-6
        real(r_kind), parameter :: max_c=1.e-2,  max_r=1.e-2,  max_i=1.e-3,max_s=2.e-2,  max_g=5.e-0

        real(r_kind)    :: rg, am_g, bm_g, mu_g
        real(r_kind)    :: cgg(3), cge(3), oge1, obmg, ogg1, ogg2

!       double precision :: no_exp, no_min, lm_exp, lamg, lamc, lamr, lami, lams
        real(r_kind) :: no_exp, no_min, lm_exp, lamg, lamc, lamr, lami, lams

!-------------------------------------------------------------------------------
!  WSM6-SPECIFIC ARRAYS
!-------------------------------------------------------------------------------

        real(r_kind) :: wsm6_nci, xmi, xmitemp

!-------------------------------------------------------------------------------
!  CONSTANTS FOR WSM6 MICROPHYSICS SCHEME
!-------------------------------------------------------------------------------
        real(r_kind), parameter :: wsm6_cnp=3.e8, wsm6_rhor=1000.
        real(r_kind), parameter :: wsm6_rhos=100., wsm6_rhog=500.
        real(r_kind), parameter :: wsm6_dimax=500.e-6, wsm6_dicon=11.9
        real(r_kind), parameter :: wsm6_alpha=.12, wsm6_t0c=273.15
        real(r_kind), parameter :: wsm6_n0s=2.e6, wsm6_n0smax=1.e11
        real(r_kind), parameter :: wsm6_n0r=8.e6, wsm6_n0g=4.e6
        real(r_kind), parameter :: wsm6_qmin=1.e-15

!-------------------------------------------------------------------------------
!  CONSTANTS FOR LIN MICROPHYSICS SCHEME
!-------------------------------------------------------------------------------

        real(r_kind), parameter :: lin_rhoi=100., lin_rhor=1000., lin_rhos=100.
        real(r_kind), parameter :: lin_rhog=400., lin_cnp=3.e8
        real(r_kind), parameter :: lin_n0r=8.e6,  lin_n0s=3.e6,   lin_n0g=4.e6

!-------------------------------------------------------------------------------
!  CONSTANTS FOR NEW THOMPSON MICROPHYSICS SCHEME (FOR WRF VERSIONS 3.1 AND UP)
!-------------------------------------------------------------------------------

        real(r_kind), parameter :: nthom_rhor=1000., nthom_rhos=100.
!       WM LEWIS updated rhog to 500 from 400
        real(r_kind), parameter :: nthom_rhog=500.,  nthom_rhoi=890.
        real(r_kind), parameter :: nthom_gon_min=1.e4, nthom_gon_max=3.e6
        real(r_kind), parameter :: nthom_nt_c=100.e6

        real(r_kind), parameter :: nthom_min_nci=5.e2
        real(r_kind), parameter :: nthom_min_ncr=1.e-6

        real(r_kind), parameter :: nthom_bm_s=2.0               !this is important

        real(r_kind) :: nci2, ncc2, ncr2

        real(r_kind), dimension(10), parameter :: &
        nthom_sa = (/ 5.065339, -0.062659, -3.032362, 0.029469, -0.000285, &
                      0.31255,   0.000204,  0.003199, 0.0,      -0.015952/)
        real(r_kind), dimension(10), parameter :: &
        nthom_sb = (/ 0.476221, -0.015896,  0.165977, 0.007468, -0.000141, &
                      0.060366,  0.000079,  0.000594, 0.0,      -0.003577/)


        if(mp_opt.eq.6) then                        !WSM6 SCHEME

          n0_r = wsm6_n0r
          n0_g = wsm6_n0g
          n0_s = wsm6_n0s

        elseif(mp_opt.eq.2)then                     !LIN SCHEME
          n0_r = lin_n0r
          n0_g = lin_n0g
          n0_s = lin_n0s

        endif

!------------------------------------------------------------------------------
!  SET DIAMETER ARRAYS TO ZERO, COMPUTE DENSITY
!------------------------------------------------------------------------------

        effr=0.

        rho=pmid/(rd*t*(1.+D608*q))


 if(mp_opt.eq.6)then

     SELECT CASE(species)

     CASE("C")

     if ( qqw.gt.min_qc ) then !cloud diameter: assume constant # concentration
       effr = 1.0E6*(( 6. * rho * qqw ) / &
       (pi * wsm6_rhor * wsm6_cnp))**(1/3.)

     endif

     CASE("R")

     if ( qqr.gt.min_qr ) then !rain diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqr ) / &
       ( pi * wsm6_rhor * n0_r * gamma_crg ) ) ** (1/(1+beta_crg ) )
     endif

     CASE("G")

     if ( qqg.gt.min_qg ) then !graupel diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqg ) / &
       ( pi * wsm6_rhog * n0_g * gamma_crg ) ) ** (1/(1+beta_crg ) )
     endif

     CASE("S")

     if ( qqs.gt.min_qs ) then !snow diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqs ) / &
       ( pi * wsm6_rhos * n0_s * gamma_s   ) ) ** ( 1/(1+beta_s) )
     endif
!  ICE DIAMETER: CALCULATED USING METHOD OUTLINED IN WRF BROWSER.  Refer to
!  phys/module_mp_wsm6.F (Vice:fallout of ice crystal).

     CASE("I")

     if ( qqi.gt.min_qi ) then !ice diameter
!       wsm6_nci = min(max(5.38e7*(rho*max(qqi,wsm6_qmin)),1.e3),1.e6)
!       xmi = rho * qqi / wsm6_nci
!       effr = 1.0E6*min( sqrt(xmi), wsm6_dimax)
!!      from wsm6, HWRF ver 3.6:
!!          temp = (den(i,k)*max(qci(i,k,2),qmin))
!!          temp = sqrt(sqrt(temp*temp*temp))
!!          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
!!      diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
       xmitemp=rho*max(qqi,wsm6_qmin)
       xmitemp=sqrt(sqrt(xmitemp*xmitemp*xmitemp))
       xmi= min(max(5.38e7*xmitemp,1.e3),1.e6)
       effr = 1.0E6*max(min(wsm6_dicon * sqrt(xmi),wsm6_dimax), 1.e-25)
     endif

     END SELECT

 elseif(mp_opt.eq.2)then

     SELECT CASE(species)

     CASE("C")

     if ( qqw > min_qc ) then !cloud diameter: assume constant # concentration
       effr = 1.0E6*(( 6. * rho * qqw ) / &
       (pi * lin_rhor * lin_cnp))**(1/3.)
     endif

     CASE("R")

     if ( qqr > min_qr ) then !rain diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqr ) / &
       ( pi * lin_rhor * n0_r * gamma_crg ) ) ** (1/(1+beta_crg ) )
     endif

     CASE("I")

     if ( qqi > min_qi ) then !ice diameter: assume constant # concentrtion
       effr = 1.0E6*( ( 6. * rho * qqi ) / &
       ( pi * lin_rhoi * lin_cnp ) ) ** ( 1/3.)
     endif

     CASE("S")

     if ( qqs > min_qs ) then !snow diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqs ) / &
       ( pi * lin_rhos * n0_s * gamma_s   ) ) ** ( 1/(1+beta_s) )
     endif

     CASE("G")

     if ( qqg > min_qg ) then !graupel diameter: assume gamma distribution
       effr = 1.0E6*( ( 6. * rho * qqg ) / &
       ( pi * lin_rhog * n0_g * gamma_crg ) ) ** (1/(1+beta_crg ) )
     endif

     END SELECT

 elseif(mp_opt.eq.8)then

!-----------------------------------
        ! CLOUD DROPLET NUMBER CONCENTRATION
!-----------------------------------

          ncc = nthom_nt_c


!  rain section

          bm_r   = 3.0
          mu_r   = 0.0
          obmr   = 1.0 / bm_r
          am_r   = pi * nthom_rhor / 6.0

          cre(1) = bm_r + 1.
          cre(2) = mu_r + 1.
          cre(3) = bm_r + mu_r + 1.

          crg(1) = WGAMMA(cre(1))
          crg(2) = WGAMMA(cre(2))
          crg(3) = WGAMMA(cre(3))

          ore1   = 1. / cre(1)
          org1   = 1. / crg(1)
          org2   = 1. / crg(2)

!  cloud section

          bm_c   = bm_r
          mu_c   = min(15.,(1000.e6/nthom_nt_c+2.))

          do n = 1, 15
             cce(1,n) = n + 1.             ! Substitute variable value of mu_c
             cce(2,n) = bm_r + n + 1.      ! Substitute variable value of mu_c

             ccg(1,n) = WGAMMA(cce(1,n))
             ccg(2,n) = WGAMMA(cce(2,n))

             ocg1(n)   = 1./ccg(1,n)
             ocg2(n)   = 1./ccg(2,n)
          enddo

!  ice section

          am_i   = pi * nthom_rhoi / 6.0
          bm_i   = 3.0
          mu_i   = 0.

          cie(1) = mu_i + 1.
          cie(2) = bm_i + mu_i + 1.

          cig(1) = WGAMMA(cie(1))
          cig(2) = WGAMMA(cie(2))

          oig1   = 1./cig(1)
          oig2   = 1./cig(2)
          obmi   = 1./bm_i

!  snow section

          am_s   = 0.069

          oams   = 1./am_s

          cse(1) = nthom_bm_s + 1.

!  graupel section

          bm_g   = 3.0
          mu_g   = 0.0
          obmg   = 1.0 / bm_g
          am_g   = pi * nthom_rhog / 6.0

          cge(1) = bm_g + 1.
          cge(2) = mu_g + 1.
          cge(3) = bm_g + mu_g + 1.

          cgg(1) = WGAMMA(cge(1))
          cgg(2) = WGAMMA(cge(2))
          cgg(3) = WGAMMA(cge(3))

          oge1   = 1. / cge(1)
          ogg1   = 1. / cgg(1)
          ogg2   = 1. / cgg(2)

!CLOUD DIAMETER CALCULATIONS

     SELECT CASE (species)

     CASE("C")

            if(qqw .ge. min_qc) then

              rc = MAX(1.E-12, qqw * rho)
              ncc2 = MAX(1.E-6, ncc * rho)
              if (ncc2 .lt. 10.e6) then
                nu_c = 15
              else
                nu_c   = min (15, NINT(1000.e6/ncc2) + 2)
              endif

              lamc = (ncc2/rc)**obmr * (am_r*g_ratio(nu_c))**obmr

              effr = 1.0E6*MAX(5.01E-6, MIN(SNGL(1.0D0*DBLE(3.+nu_c)/lamc),50.E-6))

!           old UPP
!             effr = 2.*10.

            endif

!RAIN DIAMETER CALCULATIONS

     CASE("R")

            if( qqr > min_qr) then

              rr = MAX(1.E-12, qqr * rho)
              ncr2 = MAX(1.E-6, qqnr * rho)
              lamr = (ncr2/rr)**obmr * (am_r*crg(3)*org2)**obmr

              effr = 1.0E6*MAX(50.01E-6, MIN(SNGL(1.0D0*DBLE(3.+mu_r)/lamr),1999.E-6))

!             old UPP
!              effr=2.*200.

!              print*,'effr_rain=',effr/2.

            endif

!ICE DIAMETER CACLULATIONS

     CASE("I")

            if(qqi .ge. min_qi) then

              ri = MAX(1.E-12, qqi * rho)
              nci2 = MAX(1.E-6, qqni * rho)

              lami = (nci2/ri)**obmi * (am_i*cig(2)*oig1)**obmi

              effr = 1.0E6*MAX(10.01E-6, MIN(SNGL(1.0D0*DBLE(3.+mu_i)/lami),250.E-6))

!             old UPP
!               effr=2.*25.

            endif

!SNOW DIAMETER CALCULATIONS

     CASE("S")

            rs = qqs * rho

            if(qqs .ge. min_qs) then

              tc0  = min(-0.1, t-273.15)
              smob = rs*oams

              if (nthom_bm_s.gt.(2.0-1.e-3) .and. nthom_bm_s.lt.(2.0+1.e-3))then
                  smo2 = smob
              else
                  loga = nthom_sa(1) + nthom_sa(2)*tc0 + nthom_sa(3)*nthom_bm_s+               &
                         nthom_sa(4)*tc0*nthom_bm_s + nthom_sa(5)*tc0*tc0 +&
                         nthom_sa(6)*nthom_bm_s*nthom_bm_s +nthom_sa(7)*tc0*tc0*nthom_bm_s +   &
                         nthom_sa(8)*tc0*nthom_bm_s*nthom_bm_s +nthom_sa(9)*tc0*tc0*tc0 +      &
                         nthom_sa(10)*nthom_bm_s*nthom_bm_s*nthom_bm_s

                  a    = 10.0**loga

                  b    = nthom_sb(1) + nthom_sb(2)*tc0 + nthom_sb(3)*nthom_bm_s+               &
                         nthom_sb(4)*tc0*nthom_bm_s + nthom_sb(5)*tc0*tc0 +&
                         nthom_sb(6)*nthom_bm_s*nthom_bm_s +nthom_sb(7)*tc0*tc0*nthom_bm_s +   &
                         nthom_sb(8)*tc0*nthom_bm_s*nthom_bm_s +nthom_sb(9)*tc0*tc0*tc0 +      &
                         nthom_sb(10)*nthom_bm_s*nthom_bm_s*nthom_bm_s
                  smo2 = (smob/a)**(1./b)
              endif

              !Calculate bm_s+1 (th) moment.  Useful for diameter calcs.
              loga      = nthom_sa(1) + nthom_sa(2)*tc0 + nthom_sa(3)*cse(1) +&
                          nthom_sa(4)*tc0*cse(1) + nthom_sa(5)*tc0*tc0 +&
                          nthom_sa(6)*cse(1)*cse(1) + nthom_sa(7)*tc0*tc0*cse(1)+      &
                          nthom_sa(8)*tc0*cse(1)*cse(1) +nthom_sa(9)*tc0*tc0*tc0 +     &
                          nthom_sa(10)*cse(1)*cse(1)*cse(1)

              a       = 10.0**loga

              b       = nthom_sb(1)+ nthom_sb(2)*tc0 + nthom_sb(3)*cse(1) +&
                        nthom_sb(4)*tc0*cse(1) + nthom_sb(5)*tc0*tc0 +&
                        nthom_sb(6)*cse(1)*cse(1) + nthom_sb(7)*tc0*tc0*cse(1) +&
                        nthom_sb(8)*tc0*cse(1)*cse(1) + nthom_sb(9)*tc0*tc0*tc0 +       &
                        nthom_sb(10)*cse(1)*cse(1)*cse(1)

              smoc      = a * smo2**b

              effr = 1.0E6*MAX(50.E-6, MIN(smoc/smob, 1999.E-6))

!              print*,'snow effr=',effr
!             changing snow effr recovers "old" UPP Thompson almost exactly;
!             i.e. the snow effr is the source of the cold discprepancy.

!             old UPP
!              effr=2.*250.

            endif

     CASE("G")

            if(qqg .ge. min_qg) then

                no_min  = nthom_gon_max

                no_exp  = 200. / qqg

                no_exp  = max(dble(nthom_gon_min),min(no_exp,dble(nthom_gon_max)))

                no_min  = min(no_exp,no_min)

                no_exp  = no_min

                lm_exp  = (no_exp*am_g*cgg(1)/rg)**oge1

                lamg    = lm_exp*(cgg(3)*ogg2*ogg1)**obmg

                effr= 1.0E6*(3.0 + mu_g) / lamg

!           old UPP
!            effr=350.

            endif

     END SELECT

  elseif(mp_opt.eq.5.or.mp_opt.eq.85.or.mp_opt.eq.95)then

     SELECT CASE (species)

     CASE("C")

      effr=2.*10.

     CASE("I")

      effr=2.*25.

     CASE("R")
      if( qqr > min_qr) then
      rhox=1000.
      effr=2.*1.0E6*1.5*(rho*qqr/(pi*rhox*nrain))**(1./3.)

!      old UPP    
!      effr=2.*200.
!      effr=min(200.,effr)
!      print*,'effr_rain=',effr/2.
      endif

     CASE("S")

      if(F_RimeF<=5.0)then
        RHOX=100.
          if(NLICE>0.) then
            effr  = 2.*1.0E6*1.5*(RHO*qqs/(PI*RHOX*NLICE))**(1./3.)
          endif
      endif

     CASE("G")

      if(F_RimeF>5.0.and.F_RimeF<=20.0)then
        RHOX=400.
          if(NLICE>0.) then
            effr  = 2.*1.0E6*1.5*(RHO*qqs/(PI*RHOX*NLICE))**(1./3.)
          endif
      endif

     CASE("H")

      if(F_RimeF>20.0)then
        RHOX=900.
          if(NLICE>0.) then
            effr  = 2.*1.0E6*1.5*(RHO*qqs/(PI*RHOX*NLICE))**(1./3.)
          endif
      endif

     END SELECT


  endif

!-----------------------------------------
! DIAMETER -> RADIUS
!-----------------------------------------

  effr = 0.5*effr

end function EFFR

      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL(r_kind), INTENT(IN):: y

!     real(r_kind)    :: GAMMLN

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA


      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL(r_kind), INTENT(IN):: XX
!     DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
!     DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
!              COF = (/76.18009172947146D0, -86.50532032941677D0, &
!                      24.01409824083091D0, -1.231739572450155D0, &
!                     .1208650973866179D-2, -.5395239384953D-5/)
      real(r_kind), PARAMETER:: STP = 2.5066282746310005D0
      real(r_kind), DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      !DOUBLE PRECISION:: SER,TMP,X,Y
      real(r_kind) :: SER,TMP,X,Y
      INTEGER(i_kind):: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN


end module cloud_efr_mod
