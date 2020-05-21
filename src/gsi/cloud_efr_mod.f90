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

  real(r_kind),allocatable,dimension(:,:,:,:):: efr_ql     ! effective radius for cloud liquid water
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qi     ! effective radius for cloud ice
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qr     ! effective radius for rain
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qs     ! effective radius for snow
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qg     ! effective radius for graupel
  real(r_kind),allocatable,dimension(:,:,:,:):: efr_qh     ! effective radius for hail

! local variables to this module (not public)
  logical,save:: cloud_initialized_=.false.

! - Begin specification of microphysics parameters for Ferrier scheme
!     Mean ice diameters
  real(r_kind), parameter :: dmimin=.05e-3_r_kind, dmimax=1.e-3_r_kind,      &
                             xmimin=1.e6_r_kind*dmimin, xmimax=1.e6_r_kind*dmimax
  integer(i_kind), parameter :: mdimin=xmimin, mdimax=xmimax
!     Mean rain drop diameters vary from 50 microns to 450 microns
  real(r_kind), parameter :: dmrmin=.05e-3_r_kind, dmrmax=.45e-3_r_kind,   &
                             xmrmin=1.e6_r_kind*dmrmin, xmrmax=1.e6_r_kind*dmrmax,              &
                             n0r0=8.e6_r_kind, n0rmin=1.e4_r_kind
  integer(i_kind), parameter :: mdrmin=xmrmin, mdrmax=xmrmax
!     Mean mass of precpitation ice particles as functions of their mean
!     size (in microns)
  real(r_kind) :: massi(mdimin:mdimax)
!      Lookup tables for rain  
  real(r_kind) :: massr(mdrmin:mdrmax)
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

 if(.not.regional) return
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
       open (unit=1,file="eta_micro_lookup.dat",form="unformatted")
       do i=1,3
          read(1)
       enddo
       read(1) massr
       do i=1,5
          read(1)
       enddo
       read(1) massi
       close(1)
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

  implicit none
  if(.not.cloud_initialized_) return
  deallocate(efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh)
  cloud_initialized_=.false.

end subroutine cloud_final

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

subroutine cloud_calc_gfs(g_ql,g_qi,g_cwmr,g_q,g_tv,lower_bound,g_cf)  
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
!   2019-06-06 eliu - add handling for cloud fraction 


  use gridmod, only: lat2,lon2,nsig
  use constants, only: qcmin
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_ql   ! mixing ratio of cloud liquid water [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_qi   ! mixing ratio of cloud ice [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: g_cwmr ! mixing ratio of total condensates [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ):: g_q    ! specific humidity [Kg/Kg]
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ):: g_tv   ! virtual temperature [K]
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout), optional:: g_cf   ! cloud fractio   
  logical,intent(in):: lower_bound                                ! If .true., set lower bound to cloud

! Declare local variables
  integer(i_kind):: i,j,k
  real(r_kind)   :: work

! Set lower bound to cloud
  if (lower_bound) then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              g_cwmr(i,j,k) =max(qcmin,g_cwmr(i,j,k))
           end do
        end do
     end do
  endif

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

  if (present(g_cf)) then
      do k=1, nsig
         do j=1, lon2
            do i=1, lat2
               ! set lower bound to hydrometeors 
               g_cf(i,j,k) = min(max(zero,g_cf(i,j,k)),one)
            enddo
         enddo
      enddo
  endif
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

subroutine effrds(p1d,t1d,q1d,qw1,qi1,qr1,fs1d, &
                  efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh)
!$$$  subprogram documentation block
!                .      .    .     
! subprogam:    effrds      computes effective radius
!   prgrmmr: jin         org: w/np2      date: 01-08-14       
!     
! abstract:  
!     This routine computes effective radius. 
!     The code is based on subroutine calmict.
!     
! program history log:
!   01-08-14  Yi Jin 
!   02-02-11  Brad Ferrier - Minor changes for consistency w/ NMM model
!   04-11-10  Brad Ferrier - Removed cloud fraction algorithm
!   04-11-17  H Chuang - WRF version     
!   11-06-20  Yanqiu Zhu - made changes on calmict to be called in GSI
!   14-06-02  Jacob Carley - Move lookup table inquire/read to cloud_init
!
! usage:    call effrds(t1d,q1d,qw1,qi1,qr1,fs1d,nlice1)
!   input argument list:
!     p1d     - pressure (pa)
!     t1d     - temperature (K)
!     q1d     - specific humidity (kg/kg)
!     qw1   - cloud water mixing ratio (kg/kg)
!     qi1   - total cloud ice (cloud ice & snow) mixing ratio (kg/kg)
!     qr1   - rain mixing ratio (kg/kg)
!     fs1d  - f_rimef ("Rime Factor", ratio of total ice growth
!                       to deposition growth)
!
!   output argument list:
!     nlice1
!
!   output files:
!     none
!     
!   subprograms called:
!        fpvsx
!     utilities:
!     library:
!       none
!     
!$$$  
!
  implicit none
 
  integer(i_kind) indexs, indexr

  real(r_kind),parameter:: d608=0.608_r_kind
  real(r_kind),parameter:: fmw=18.015_r_kind
  real(r_kind),parameter:: fmd=28.964_r_kind
  real(r_kind),parameter:: eps=fmw/fmd
  real(r_kind),parameter:: rd=287.04_r_kind
  real(r_kind),parameter:: oneps=1.0_r_kind-eps
  real(r_kind),parameter:: nlimin=1.0e3_r_kind
  real(r_kind),parameter:: nlimax=5.0e3_r_kind
  real(r_kind),parameter:: rhol=1000.0_r_kind


  real(r_kind),intent(in) :: p1d,t1d,q1d
  real(r_kind),intent(in) :: qw1,qi1,qr1,fs1d
  real(r_kind),intent(out) :: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
      
! local variables
  real(r_kind) tem4,indexw,indexi
  real(r_kind) n0r,rhgrd,c_n0r0
  real(r_kind) tc,flimass,flarge,     &
       fsmall,rimef,xsimass,qice,qsat,esat,wv,rho,rrho,rqr,          &
       qsigrd,wvqw,dum,xli,qlice,dli,xlimass,nlice1

! Various rain lookup tables
  real(r_kind) rqr_drmin,rqr_drmax,cn0r0,cn0r_dmrmin,cn0r_dmrmax

  real(r_kind) rhox  ! assumed density of the large ice in kg m^-3

!************************************************************************
! liquid water cloud drop size
  tem4=max(zero,(t0c-t1d)*r0_05)
  indexw=five + five * min(one, tem4)

! cloud ice drop size
  indexi=50.0_r_kind  ! microns

! effective radius for liquid water cloud and cloud ice
  efr_ql=1.5_r_kind*indexw
  efr_qi=1.5_r_kind*indexi

! Initialize variables
  efr_qr=zero
  efr_qs=zero
  efr_qg=zero
  efr_qh=zero

! Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
  tc=t1d-t0c
  wv=q1d/(one-q1d)
  esat=1000._r_kind*fpvsx(t1d)
  qsat=eps*esat/(p1d-esat)
  rho=p1d/(rd*t1d*(one+d608*q1d))  ! air density in kg m^-3
  rrho=one/rho

  if (use_lookup_table) then
     ! massr and massi are read and initialized in cloud_init
     rqr_drmin=n0r0*massr(mdrmin)    ! Rain content for mean drop diameter of .05 mm
     rqr_drmax=n0r0*massr(mdrmax)    ! Rain content for mean drop diameter of .45 mm
     c_n0r0=pi*rhol*n0r0
     cn0r0=1.e6_r_kind/c_n0r0**.25_r_kind
     cn0r_dmrmin=1.0_r_kind/(pi*rhol*dmrmin**4)
     cn0r_dmrmax=1.0_r_kind/(pi*rhol*dmrmax**4)
!     print *,'MICROINIT: MDRmin, MASSR(MDRmin)=',mdrmin,massr(mdrmin)
!     print *,'MICROINIT: MDRmax, MASSR(MDRmax)=',mdrmax,massr(mdrmax)
!    print *,  'ETA2P:MASSI(50)= ', massi(50)
!    print *,  'ETA2P:MASSI(450)= ', massi(450)
!    print *,  'ETA2P:MASSI(1000)= ', massi(1000)

!    Based on code from gsmcolumn in model to determine reflectivity from rain
!    indexr is the mean drop size in microns
     if (qr1 > qcmin) then
       rqr=rho*qr1
       if (rqr <= rqr_drmin) then
         n0r=max(n0rmin, cn0r_dmrmin*rqr)
         indexr=mdrmin
       else if (rqr >= rqr_drmax) then
         n0r=cn0r_dmrmax*rqr
         indexr=mdrmax
       else
         n0r=n0r0
         indexr=max( xmrmin, min(cn0r0*rqr**.25_r_kind, xmrmax) )
       endif
       efr_qr=1.5_r_kind*indexr
     endif        !--- End if (qr1 > qcmin) block


!    Based on code from gsmcolumn in model to determine partition of 
!    total ice into cloud ice & snow (precipitation ice)
     if (qi1 > qcmin) then
!      Initialize rhgrd, grid-scale rh for onset of condensation
       rhgrd=one

       qice=qi1
       rho=p1d/(rd*t1d*(one+oneps*q1d))
       rrho=one/rho
       qsigrd=rhgrd*qsat
       wvqw=wv+qw1

!      * flarge  - ratio of number of large ice to total (large & small) ice
!      * fsmall  - ratio of number of small ice crystals to large ice particles
!       ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!       * xsimass - used for calculating small ice mixing ratio
!       * xlimass - used for calculating large ice mixing ratio
!       * indexs  - mean size of snow to the nearest micron (units of microns)
!       * rimef   - Rime Factor, which is the mass ratio of total (unrimed &
!                   rimed) ice mass to the unrimed ice mass (>=1)
!       * flimass - mass fraction of large ice
!       * qtice   - time-averaged mixing ratio of total ice
!       * qlice   - time-averaged mixing ratio of large ice
!       * nlice1   - time-averaged number concentration of large ice

       if (tc>=zero .or. wvqw<qsigrd) then
         flarge=one
       else
         flarge=.03_r_kind   !- was .2, Brad modified this to get better rh score
         if (tc>=-8.0_r_kind .and. tc<=-3.0_r_kind) flarge=.5_r_kind*flarge
       endif
       fsmall=(one-flarge)/flarge
       xsimass=rrho*massi(mdimin)*fsmall
       dum=xmimax*exp(.0536_r_kind*tc)
       indexs=min(mdimax, max(mdimin, int(dum) ) )
       rimef=max(one, fs1d )
       xlimass=rrho*rimef*massi(indexs)
       flimass=xlimass/(xlimass+xsimass)
       qlice=flimass*qice
       nlice1=qlice/xlimass
       if (nlice1<nlimin .or. nlice1>nlimax) then

!        Force nlice1 to be between nlimin and nlimax
         dum=max(nlimin, min(nlimax, nlice1) )
         xli=rho*(qice/dum-xsimass)/rimef
         if (xli<=massi(mdimin) ) then
           indexs=mdimin
         else if (xli<=massi(450) ) then
           dli=9.5885e5_r_kind*xli**.42066_r_kind         ! dli in microns
           indexs=min(mdimax, max(mdimin, int(dli) ) )
         else if (xli<=massi(mdimax) ) then
           dli=3.9751e6_r_kind*xli**.49870_r_kind         ! dli in microns
           indexs=min(mdimax, max(mdimin, int(dli) ) )
         else 
           indexs=mdimax
!          8/22/01: Increase density of large ice if maximum limits
!          are reached for number concentration (nlimax) and mean size
!          (mdimax).  Done to increase fall out of ice.
           if (dum>=nlimax)                              &
             rimef=rho*(qice/nlimax-xsimass)/massi(indexs)
         endif             ! End if (xli<=massi(mdimin) )
         xlimass=rrho*rimef*massi(indexs)
         flimass=xlimass/(xlimass+xsimass)
         qlice=flimass*qice
         nlice1=qlice/xlimass
       endif               ! End if (nlice<nlimin ...
     endif                 ! End if (qi1>0._r_kind) then

  else ! "eta_micro_lookup.dat" not exist
     if (qr1>qcmin) efr_qr=1.5_r_kind*300_r_kind
     nlice1=20.0e3_r_kind
     qlice=0.95_r_kind*qi1
  end if ! pcexist   


! Calculate effective radius
  if (qi1>qcmin) then
    if (fs1d<=5.0_r_kind) then 
       rhox=100.0_r_kind
       efr_qs=1.5_r_kind*(rho*qlice/(pi*rhox*nlice1))**(one/three)*1.0e6_r_kind
    end if
    if ((fs1d>5.0_r_kind) .and. (fs1d<=20.0_r_kind)) then 
       rhox=400.0_r_kind
       efr_qg=1.5_r_kind*(rho*qlice/(pi*rhox*nlice1))**(one/three)*1.0e6_r_kind
    end if
    if (fs1d>20_r_kind) then 
       rhox=900.0_r_kind
       efr_qh=1.5_r_kind*(rho*qlice/(pi*rhox*nlice1))**(one/three)*1.0e6_r_kind
    end if
  end if

  return
end subroutine effrds


real(r_kind) function fpvsx(t)
  use constants, only: tmix, xai, xbi, xa, xb, ttp, psatk
  implicit none

  real(r_kind),intent(in) :: t
  real(r_kind) :: tr

  tr=ttp/t
 
  if(t>=ttp)then
    fpvsx=psatk*(tr**xa)*exp(xb*(one-tr))
  else
    fpvsx=psatk*(tr**xai)*exp(xbi*(one-tr))
  endif

  return
end function fpvsx

end module cloud_efr_mod
