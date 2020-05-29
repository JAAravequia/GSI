module gsd_update_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    gsd_update_mod module for updating surface, soil, moisture from gsd for rr
!   prgmmr: Hu              org: gsd                date: 2012-01-12
!
! abstract: module for updating surface, soil, moisture from gsd for rr
!
! program history log:
!   2012-01-12  Hu
!   2015-01-12  Hu  fix the bug in coast proximity calculation in subdomain
!   2015-01-14  Hu  do T soil nudging over snow
!   2015-01-15  Hu  move the land/sea mask check to fine grid update step
!
! subroutines included:
!   sub gsd_update_soil_tq  - change surface and soil based on analysis increment
!   sub gsd_limit_ocean_q   - limits to analysis increments over oceans 
!   sub gsd_update_th2      - adjust 2-m t based on analysis increment
!   sub gsd_update_q2       - adjust 2-m q based on analysis increment
!
! Variable Definitions:

  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  implicit none

! set default to private
  private
! set subroutines to public
  public :: gsd_update_soil_tq
  public :: gsd_limit_ocean_q
  public :: gsd_update_th2
  public :: gsd_update_q2
  public :: gsd_gen_coast_prox
! set passed variables to public

contains

subroutine gsd_update_soil_tq(tinc,is_t,qinc,is_q,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_surface    change surface and soil based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-08-18
!
! abstract:  This routine does the following things:
!              1) add lowest level t increment to T2 
! 
! 
! program history log:
!   1990-10-06  parrish - original code
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!    tinc : first level temperature analysis increment
!    qinc : first level moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use jfunc, only:  tsensible,qoption
  use derivsmod, only: qsatg
  use constants, only: zero,one,fv,one_tenth,deg2rad,pi
  use constants, only: partialSnowThreshold,t0c
  use gridmod, only: lat2,lon2,nsig,nsig_soil
  use gridmod, only: regional_time
  use guess_grids, only: ges_tsen,sno,coast_prox
  use wrf_mass_guess_mod, only: ges_xlon,ges_xlat
  use guess_grids, only: ges_prsl
! use guess_grids, only: nfldsig
  use rapidrefresh_cldsurf_mod, only: l_gsd_soiltq_nudge

  implicit none

! Declare passed variables
  integer(i_kind) is_t,is_q
  real(r_kind),dimension(lat2,lon2), intent(in) :: tinc
  real(r_kind),dimension(lat2,lon2), intent(in) :: qinc
  integer,intent(in) ::  it   ! guess time level

! Declare local variables
  real(r_kind),dimension(lat2,lon2) :: csza
  integer(i_kind)  :: gmt,nday,iyear,imonth,iday
  real(r_kind)     :: declin
  real(r_kind)     :: hrang,xxlat
  real(r_kind)     :: sumqc

  logical ice
  integer(i_kind) :: iderivative
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  integer(i_kind) i,j,k,ier,istatus
  real(r_kind) :: ainc, tinct
  real(r_kind) :: coast_fac,temp,temp_fac,dts_min,tincf
  real(r_kind) :: snowthreshold
! 
  real(r_kind), pointer :: ges_qc(:,:,:)  ! cloud water
  real(r_kind), pointer :: ges_qi(:,:,:)  ! could ice
  real(r_kind),dimension(:,:  ),pointer:: ges_tsk   =>null()
  real(r_kind),dimension(:,:  ),pointer:: ges_soilt1=>null()
  real(r_kind),dimension(:,:,:),pointer:: ges_tslb  =>null()
  real(r_kind),dimension(:,:,:),pointer:: ges_smois =>null()
  real(r_kind),dimension(:,:,:),pointer:: ges_q     =>null()

  integer(i_kind) :: itsig
  
!*******************************************************************************
!

  snowthreshold=1.0e-10_r_kind
  itsig=it
  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(itsig),'ql',ges_qc,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(itsig),'qi',ges_qi,istatus);ier=ier+istatus
  if(ier/=0) then
     write(6,*) 'No cloud water and ice for soil nudging!'
     return ! no guess, nothing to do
  endif

!                                                                              
!   calculation solar declination
! 
  iyear=regional_time(1)   
  imonth=regional_time(2)
  iday=regional_time(3)
  call w3fs13(iyear,imonth,iday,nday)
  declin=deg2rad*23.45_r_kind*sin(2.0_r_kind*pi*(284+nday)/365.0_r_kind)
!
!  csza = fraction of solar constant (cos of zenith angle)
  gmt = regional_time(4)   ! utc 
  do j=2,lon2
     do i=1,lat2   
        hrang=15._r_kind*gmt*deg2rad + ges_xlon(i,j,1) - pi
        xxlat=ges_xlat(i,j,1)
        csza(i,j)=sin(xxlat)*sin(declin)                &
                 +cos(xxlat)*cos(declin)*cos(hrang)
     end do
  end do

  if( l_gsd_soiltq_nudge .and. is_t > 0) then
!     --------------------------------------------
! --- Increment top level of soil temp and snow temp
!       only at land points according to
!       sfc temp increment.  
!     --------------------------------------------

! -- modifications to reintroduce soil temperature nudging
!  -- allow cooling of soil only (not warming)
!      - allow only up to 1.0 k (half of negative ainc for temp)
!      - don't allow if snow water is > 6 mm

!     do it=1,nfldsig
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tskn' ,ges_tsk   ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tsoil',ges_soilt1,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'smoist',ges_smois,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tslb' ,ges_tslb  ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q'    ,ges_q     ,istatus)
     ier=ier+istatus
     if(ier/=0) then
!       code doesn't have to die here ... needs attention for generalization
        call die('gsd_update_soil_tq',': cannot find q in guess, ier=',ier)
     endif 
     do j=1,lon2
        do i=1,lat2
           if(tsensible) then
              ainc=tinc(i,j)
           else
              ainc=tinc(i,j)/(one+fv*ges_q(i,j,1))
           endif
           coast_fac = max(0._r_kind,(coast_prox(i,j)-0.5_r_kind))/0.5_r_kind
           temp = ges_tsen(i,j,1,it)

! *** Increase soil adjustment by factor of 2.0 if temps are 
!       25 deg c, and 2.5 if temp is 32.5 c .

           temp_fac  = 1._r_kind+min (1.5_r_kind,max(0._r_kind,(temp-283._r_kind)/15._r_kind))
           dts_min = -2._r_kind
! -- Allow soil temp cooling to be up to 2.5 * 0.6 = 1.5 X 2 c ( = 3k)
           dts_min = dts_min*temp_fac*0.6_r_kind

! move the land/sea masck check to fine grid update step
           tincf = ainc*temp_fac*coast_fac
! do t soil nudging over snow
           if(nsig_soil == 9) then
! - top level soil temp
              ges_tslb(i,j,1) = ges_tslb(i,j,1) +   &
                              min(1._r_kind,max(dts_min,tincf*0.6_r_kind)) 
! - 0-1 cm level -  soil temp
              ges_tslb(i,j,2) = ges_tslb(i,j,2) +   &
                              min(1._r_kind,max(dts_min,tincf*0.55_r_kind))
! - 1-4 cm level -  soil temp
              ges_tslb(i,j,3) = ges_tslb(i,j,3) +   &
                              min(1._r_kind,max(dts_min,tincf*0.4_r_kind))
! - 4-10 cm level -  soil temp
              ges_tslb(i,j,4) = ges_tslb(i,j,4) +   &
                              min(1._r_kind,max(dts_min,tincf*0.3_r_kind))
! - 10-30 cm level -  soil temp
              ges_tslb(i,j,5) = ges_tslb(i,j,5) +   &
                              min(1._r_kind,max(dts_min,tincf*0.2_r_kind))
           else
! - top level soil temp
              ges_tslb(i,j,1) = ges_tslb(i,j,1) +   &
                              min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
! - 0-5 cm level -  soil temp
              ges_tslb(i,j,2) = ges_tslb(i,j,2) +   &
                              min(1._r_kind,max(dts_min,tincf*0.4_r_kind))
! - 5-20 cm level -  soil temp
              ges_tslb(i,j,3) = ges_tslb(i,j,3) +   &
                              min(1._r_kind,max(dts_min,tincf*0.2_r_kind))
           endif
           if (sno(i,j,it) < partialsnowthreshold) then
! partialsnowthreshold (32 mm) is the threshold for partial snow.
! When grid cell is partially covered with snow or snow-free - always update tsk and soilt1
              ges_tsk(i,j)    = ges_tsk(i,j)    + min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
              ges_soilt1(i,j) = ges_soilt1(i,j) + min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
           else  
! grid cell is fully covered with snow
              if(tincf < zero) then
! always adjust tsk and soilt1 when tincf < 0 - cooling
                 ges_tsk(i,j)    = ges_tsk(i,j)    + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind))
                 ges_soilt1(i,j) = ges_soilt1(i,j) + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind))
              else
! if ticnf > 0 - warming, then adjust snow tsk and soilt1 only if tsk < t0c (273 k).
! If tsk > t0c(273 k) most likely due to melting process, then leave tsk and soilt1 unchanged.
                 if(ges_tsk(i,j) < t0c ) then
                    ges_tsk(i,j)    = min(t0c,ges_tsk(i,j)    + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind)))
                    ges_soilt1(i,j) = min(t0c,ges_soilt1(i,j) + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind)))
                 endif ! tsk < 273 k
              endif ! tincf < 0.

           endif ! sno(i,j,it) < 32
        end do
     end do
!     end do ! it

!---------------------------------------------------------
!  Nudge soil moisture
!---------------------------------------------------------

! Compute saturation specific humidity.

     iderivative = 0
     if(qoption == 1 )then
        iderivative = 1
     else
        iderivative = 2
     end if

     ice=.true.
     call genqsat(qsatg,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it), &
                  lat2,lon2,nsig,ice,iderivative)
     allocate(rhgues(lat2,lon2,nsig))

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q,istatus)
     if(istatus/=0) then
!       code doesn't have to die here ... needs attention for generalization
        call die('gsd_update_soil_tq',': cannot find q in guess')
     endif 
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              rhgues(i,j,k)=ges_q(i,j,k)/qsatg(i,j,k)
           end do
        end do
     end do

     if( is_q > 0) then
!     do it=1,nfldsig
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q,istatus)
        ier=istatus
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'smoist',ges_smois,istatus)
        ier=ier+istatus
        if(ier/=0) then
!          code doesn't have to die here ... needs attention for generalization
           call die('gsd_update_soil_tq',': cannot find q in guess')
        endif 
        do j=1,lon2
           do i=1,lat2
              if(tsensible) then
                 tinct=tinc(i,j)
              else
                 tinct=tinc(i,j)/(one+fv*ges_q(i,j,1))
              endif

              ainc=qinc(i,j)/qsatg(i,j,1)  ! analysis increment in rh

! -- use overall limits based on k level
              ainc = max(-0.3_r_kind,min(0.3_r_kind,ainc))

! -- When background is already dry and prh increment
!      is negative (toward drier still), limit ainc further.
              if (rhgues(i,j,1) < 0.2_r_kind .and. ainc < 0.0_r_kind ) then
                 ainc=ainc*rhgues(i,j,1)/0.2_r_kind
              end if
              if (rhgues(i,j,1) < 0.4_r_kind .and. ainc < 0.0_r_kind ) then
                 ainc=ainc*rhgues(i,j,1)/0.4_r_kind
              end if

              ainc = max(-0.15_r_kind,min(0.15_r_kind,ainc))

! - Only do nudging over land, if daytime (defined as
!          cos of sun zenith angle > 0.1), and if
!          sfc temp increment is negative (meaning that
!          background sfc temp was too warm)

! -- some adjustments below to soil moisture adjustment,
!      which seems to have resulted in too much moistening
!      overall.

! move the land/sea masck check to fine grid update step
!              if (isli(i,j,it) == 1 .and. csza(i,j) > 0.3_r_kind) then
              if (csza(i,j) > 0.3_r_kind) then
                 sumqc=0
                 do k=1,nsig
                    sumqc=max(sumqc,max(ges_qc(i,j,k),ges_qi(i,j,k)))
                 enddo
                 sumqc=0  ! trun off cloud
                 if( sumqc < 1.0e-6_r_kind) then
                    if( sno(i,j,it) < snowthreshold ) then  ! don't do the 
                                                         ! moisture adjustment if there is snow     

                       if (tinct < -0.15_r_kind) then

! - top level soil moisture
!      increase moistening from factor of 0.2 to 0.3
                          ges_smois(i,j,1) = min (max(ges_smois(i,j,1),ges_smois(i,j,2)), &
                                      ges_smois(i,j,1) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                          ges_smois(i,j,2) = min (max(ges_smois(i,j,2),ges_smois(i,j,3)), &
                                      ges_smois(i,j,2) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                          if(nsig_soil == 9) then
                             ges_smois(i,j,3) = min (max(ges_smois(i,j,3),ges_smois(i,j,4)), &
                                      ges_smois(i,j,3) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                             ges_smois(i,j,4) = min (max(ges_smois(i,j,4),ges_smois(i,j,5)), &  
                                      ges_smois(i,j,4) + min(0.03_r_kind,max(0._r_kind,(ainc*0.1_r_kind))))
                          endif
! -- above logic
!       previously - min was sm1_p (level 2)
!       now   - min is (max of level 1 and level 2)
!       Implication - If level 1 was already more moist than
!       level 2, don't force level 1 sm back down to level 2.
!       Decrease moistening from factor of 0.2 to 0.1
!       increase moistening from factor of 0.1 to 0.3
                       endif

                       if (tinct >  0.15_r_kind) then
! - top level soil moisture
!     Now also dry soil if tinc is positive (warming)
!      and the rh_inc is negative.
                          ges_smois(i,j,1) = max(0.0_r_kind,ges_smois(i,j,1) + & 
                                                    max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                          ges_smois(i,j,2) = max(0.0_r_kind,ges_smois(i,j,2) + & 
                                                    max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                          if(nsig_soil == 9) then
                             ges_smois(i,j,3) = max(0.0_r_kind,ges_smois(i,j,3) + & 
                                                    max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                             ges_smois(i,j,4) = max(0.0_r_kind,ges_smois(i,j,4) + & 
                                                    max(-0.03_r_kind,min(0._r_kind,(ainc*0.1_r_kind))))

                          endif
                       end if
                    endif  !  sno(i,j,it) < snowthreshold
                 endif  !  sumqc < 1.0e-6_r_kind
              endif
           end do
        end do
!     end do ! it
     endif ! is_q > 0
     deallocate(rhgues)
  endif

  return
end subroutine gsd_update_soil_tq

subroutine gsd_limit_ocean_q(qinc,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_limit_ocean_q rlimits to analysis increments over oceans
!   prgmmr: Hu          org: GSD                date: 2011-08-31
!
! abstract:  This routine does the following things:
! 
! 
! program history log:
!   2011-08-31  Hu - original code
!
!   input argument list:
!    qinc : moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use jfunc, only:  qoption
  use derivsmod, only:  qsatg
  use constants, only: zero,one,one_tenth
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_tsen,ges_prsl
  use guess_grids, only: isli
! use guess_grids, only: nfldsig
  
  implicit none
  
! Declare passed variables
  integer(i_kind) istatus
  real(r_kind),dimension(lat2,lon2,nsig), intent(inout) :: qinc
  integer,intent(in) ::  it   ! guess time level

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,iderivative
  real(r_kind),allocatable,dimension(:,:,:):: rhgues
  real(r_kind) :: qinc_rh
  real(r_kind),dimension(:,:,:),pointer:: ges_q=>null()


! Compute saturation specific humidity.   

  iderivative = 0
  if(qoption == 1 )then
     iderivative = 1 
  else
     iderivative = 2
  end if

  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2,nsig,ice,iderivative)
  allocate(rhgues(lat2,lon2,nsig))

  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q,istatus)
  if(istatus/=0) then
!    code doesn't have to die here ... needs attention for generalization
     call die('gsd_update_soil_tq',': cannot find q in guess')
  endif 
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=ges_q(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do

!  do it=1,nfldsig
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           if (isli(i,j,1) ==  0) then
              qinc_rh=qinc(i,j,k)/qsatg(i,j,k)
! -- limit pseudo-rh change over water to +/- 0.1
              if( k <= 10) then
                 qinc_rh=max(-0.1_r_kind,min(0.1_r_kind,qinc_rh))
              elseif(  k >= 11.and.k <=18 ) then
                 qinc_rh=max(-0.2_r_kind,min(0.2_r_kind,qinc_rh))
              endif
! -- Limit further drying out over water and near surface.
              if(rhgues(i,j,k) < 0.6_r_kind .and. k <=4 .and. qinc_rh < zero) then
                 qinc_rh=qinc_rh*rhgues(i,j,k)/1.0_r_kind
              endif
              qinc(i,j,k)=qinc_rh*qsatg(i,j,k) 
           else
              qinc(i,j,k)=qinc(i,j,k) 
           end if   ! isli(i,j,1)
        end do
     end do
  end do
!  end do

  deallocate(rhgues)
end subroutine gsd_limit_ocean_q 

subroutine gsd_update_th2(tinc,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_update_th2    adjust 2-m t based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-10-04
!
! abstract:  This routine does the following things:
!              1) add lowest level t increment to T2 
! 
! 
! program history log:
!   2011-10-04  parrish - original code
!   2013-10-19  todling - get guess fileds from bundle
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF
!                         MASS core
!
!   input argument list:
!    tinc : first level temperature analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use jfunc, only:  tsensible
  use constants, only: zero,one,fv,rd_over_cp_mass,one_tenth
  use gridmod, only: lat2,lon2,aeta1_ll,pt_ll,aeta2_ll
! use guess_grids, only: nfldsig

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2), intent(in) :: tinc
  integer,intent(in) ::  it   ! guess time level

  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  integer(i_kind) i,j,ier,ihaveq
  real(r_kind) :: dth2, work_prsl,work_prslk

  real(r_kind),dimension(:,:  ),pointer:: ges_ps =>null()
  real(r_kind),dimension(:,:  ),pointer:: ges_th2=>null()
  real(r_kind),dimension(:,:,:),pointer:: ges_q  =>null()

!*******************************************************************************
!
! 2-m temperature
!  do it=1,nfldsig
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'th2m',ges_th2,ier)
  if(ier/=0) return
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'ps',ges_ps,ier)
  if(ier/=0) return
! Note: for some odd reason the orig. code before bundle change was getting q
!       from slot it=1 - to preserve zero diff I left as such
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q' ,ges_q ,ihaveq)
  do j=1,lon2
     do i=1,lat2
        if(tsensible) then
           dth2=tinc(i,j)
        else
           if(ihaveq/=0) cycle
           dth2=tinc(i,j)/(one+fv*ges_q(i,j,1))
        endif
!       Convert sensible temperature to potential temperature
        work_prsl  = one_tenth*(aeta1_ll(1)*(r10*ges_ps(i,j)-pt_ll)+ &
                                aeta2_ll(1) + pt_ll)
        work_prslk = (work_prsl/r100)**rd_over_cp_mass
        ges_th2(i,j) = ges_th2(i,j) + dth2/work_prslk
     end do
  end do
!  end do

  return
end subroutine gsd_update_th2

subroutine gsd_update_q2(qinc,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_update_q2    adjust 2-m q based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-10-04
!
! abstract:  This routine does the following things:
!              1) add lowest level q increment to Q2 
! 
! 
! program history log:
!   2014-01-22  Hu - original code
!   2014-04-04  Todling - ges_q2 now in MetGuess
!
!   input argument list:
!    qinc : first level moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
! use guess_grids, only: nfldsig

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2), intent(in) :: qinc
  integer,intent(in) ::  it   ! guess time level

  real(r_kind),dimension(:,:  ),pointer:: ges_q2=>null()
  integer(i_kind) i,j,ier

!*******************************************************************************
!
! 2-m temperature
!  do it=1,nfldsig
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q2m',ges_q2,ier)
  if(ier/=0) return
  do j=1,lon2
     do i=1,lat2
        ges_q2(i,j) = ges_q2(i,j) + qinc(i,j)
     end do
  end do
!  end do

  return
end subroutine gsd_update_q2


subroutine gsd_gen_coast_prox
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_gen_coast_prox calculate coast proximity based on isli
!   prgmmr: Hu          org: GSD                date: 2015-01-14
!
! abstract:  This routine does the following things:
!              1) calculate coast proximity based on isli 
! 
! 
! program history log:
!   2014-01-22  Hu - original code
!
!   input argument list:
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use gridmod, only: lat2,lon2
  use general_sub2grid_mod, only: general_gather2grid,general_scatter2sub
  use general_commvars_mod, only: g1
  use guess_grids, only: isli,coast_prox
  use rapidrefresh_cldsurf_mod, only: l_gsd_soiltq_nudge

  implicit none

! Declare passed variables

  real(r_kind),dimension(:),allocatable:: worksub
  real(r_kind),allocatable,dimension(:,:,:):: hwork
  real(r_kind),allocatable,dimension(:,:,:):: hcoast_prox

  integer(i_kind) workpe,ii
  integer(i_kind) i,j,ico
  integer(i_kind) ia,ib,ja,jb,ic,jc,nco,nip

!*******************************************************************************
!
  if( l_gsd_soiltq_nudge) then

! water, land, seaice index
     allocate(worksub(g1%inner_vars*g1%nlat*g1%nlon))
     allocate(hwork(g1%inner_vars,g1%nlat,g1%nlon))
     allocate(hcoast_prox(g1%inner_vars,g1%nlat,g1%nlon))
     workpe=0

     ii=0
     do j=1,lon2
        do i=1,lat2
           ii=ii+1
           worksub(ii)=isli(i,j,1)
        end do
     end do

     call general_gather2grid(g1,worksub,hwork,workpe)

     if(mype==workpe) then
!
!  isli = 0 water, =1 land, =2 sea ice (on water)
        hcoast_prox=0.0_r_kind
        ico = 3
        do j=1,g1%nlon
           ja = max(1   ,j-ico)
           jb = min(g1%nlon,j+ico+1)
           do i=1,g1%nlat
             if (abs(hwork(1,i,j)-1.0_r_kind) <0.001_r_kind .or. &
                 abs(hwork(1,i,j)-2.0_r_kind) <0.001_r_kind ) then
                ia = max(1   ,i-ico)
                ib = min(g1%nlat,i+ico+1)
                nco = 0
                nip = 0
                do jc=ja,jb
                do ic=ia,ib
                   if (abs(hwork(1,ic,jc)-1.0_r_kind) <0.001_r_kind .or. &
                       abs(hwork(1,ic,jc)-2.0_r_kind) <0.001_r_kind ) nco = nco+1
                   nip = nip+1
                end do
                end do
                hcoast_prox(1,i,j) = float(nco)/float (nip)
             end if
           end do
        end do
     endif    ! mype==workpe
     deallocate(hwork)
!
     worksub=0.0
     call general_scatter2sub(g1,hcoast_prox,worksub,workpe)
     deallocate(hcoast_prox)

     ii=0
     do j=1,lon2
        do i=1,lat2
           ii=ii+1
           coast_prox(i,j)=worksub(ii)
        end do
     end do
  else
    coast_prox=0.0_r_kind
  endif

  return
end subroutine gsd_gen_coast_prox

end module gsd_update_mod
