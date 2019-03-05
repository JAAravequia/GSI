! MODULE CONTAINING FORWARD OPERATORS FOR RADAR REFLECTIVITY ASSIMILATION
!  FALL VALOCITY ALSO CALCULATED WHEN POSSIBLE
!  Based on code from AJ / XW 2016
!  Heavily Modified by Thomas Jones
!   - Add Thompson Reflectivity and Fall velocity code
!   - Clean up things
!   - to do: Add NSSL reflectivity option

module setupdbz_lib

public hx_wsm6, hx_thomp, jqr_dart, jqs_dart, jqg_dart

INTEGER, PARAMETER, PUBLIC:: nrbins = 50
DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: xxDx
DOUBLE PRECISION, DIMENSION(nrbins), PUBLIC:: xxDs,xdts,xxDg,xdtg
DOUBLE PRECISION, PARAMETER, PUBLIC:: lamda_radar = 0.10           ! in meters
DOUBLE PRECISION, PUBLIC:: K_w, PI5, lamda4
COMPLEX*16, PUBLIC:: m_w_0, m_i_0
DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: simpson
DOUBLE PRECISION, DIMENSION(3), PARAMETER, PUBLIC:: basis =       &
                           (/1.d0/3.d0, 4.d0/3.d0, 1.d0/3.d0/)
DOUBLE PRECISION, PARAMETER:: melt_outside_s = 0.9d0
DOUBLE PRECISION, PARAMETER:: melt_outside_g = 0.9d0
INTEGER, PARAMETER, PUBLIC:: slen = 20
CHARACTER(len=slen), PUBLIC::                                     &
       mixingrulestring_s, matrixstring_s, inclusionstring_s,    &
       hoststring_s, hostmatrixstring_s, hostinclusionstring_s,  &
       mixingrulestring_g, matrixstring_g, inclusionstring_g,    &
       hoststring_g, hostmatrixstring_g, hostinclusionstring_g


contains

! ***** CALCULATE REFLECTIVITY USING WSM6 MICROPHYSICS 
subroutine hx_wsm6(qrgesin0,qggesin0,qsgesin0,rhogesin,tempgesin,rDBZ,debugging)
  use kinds, only: r_kind,r_double,i_kind
  use obsmod, only: static_gsi_nopcp_dbz
  
implicit none

real(r_kind) :: qrgesin0,qsgesin0,qggesin0
real(r_kind) :: qrgesin,qsgesin,qggesin,rhogesin,tempgesin,rDBZ
real(r_kind) :: zqr,zqg,zqs
logical :: debugging
real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) :: n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi

 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0


pi=3.14159
dielectric=0.224
n0r=8e6
n0s=3e6 !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6
rhos=100
rhor=1000
rhog=500 !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20)/(((pi*rhor)**1.75)*(n0r**0.75))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20)/(((pi*rhog)**1.75)*(n0g**0.75))
param_wet_g=(7.2e20)/((((pi*rhog)**1.75)*(n0g**0.75))**0.95)
param_wet_s=(7.2e20)/(((pi*rhos)**1.75)*(n0s**0.75))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s


zqr=param_r*((rhogesin*qrgesin)**1.75)
if (tempgesin .lt. 273.15) then
 zqr=0
 zqg=param_dry_g*((rhogesin*qggesin)**1.75)
 zqs=param_dry_s*((rhogesin*qsgesin)**1.75)
else if(tempgesin .lt. 278.15) then
 zqg=param_wet_g*((rhogesin*qggesin)**1.6675)
 zqs=param_wet_s*((rhogesin*qsgesin)**1.75)
else
 zqg=0
 zqs=0
endif
 rDBZ=zqr+zqg+zqs
if (rdBZ .gt. 1.0e-3) then
 rdBZ=10*log10(rdBZ)
else
!rdBZ=-30
 rdBZ=0
endif
if(rdBZ.lt.static_gsi_nopcp_dbz) rdBZ=static_gsi_nopcp_dbz !notice, static_gsi_nopcp_dbz should be larger than -30

if(debugging) then
 print *, "ZQR=",zqr,zqs,zqg,tempgesin
 print*, 'USING WSM6 REFLECTIVITY'
endif

end subroutine hx_wsm6
! **** END WSM 6

! ***** CALCULATE REFLECTIVITY USING THOMPSON 3.6.1 MICROPHYSICS 


! STUFF NEEDED FOR THOMPSON
subroutine radar_init()
IMPLICIT NONE

INTEGER:: n

PI5 = 3.14159*3.14159*3.14159*3.14159*3.14159
lamda4 = lamda_radar*lamda_radar*lamda_radar*lamda_radar
m_w_0 = m_complex_water_ray (lamda_radar, 0.0d0)
m_i_0 = m_complex_ice_maetzler (lamda_radar, 0.0d0)
K_w = (ABS( (m_w_0*m_w_0 - 1.0) /(m_w_0*m_w_0 + 2.0) ))**2

do n = 1, nrbins+1
    simpson(n) = 0.0d0
enddo
do n = 1, nrbins-1, 2
	simpson(n) = simpson(n) + basis(1)
    simpson(n+1) = simpson(n+1) + basis(2)
    simpson(n+2) = simpson(n+2) + basis(3)
enddo

do n = 1, slen
    mixingrulestring_s(n:n) = char(0)
    matrixstring_s(n:n) = char(0)
    inclusionstring_s(n:n) = char(0)
    hoststring_s(n:n) = char(0)
    hostmatrixstring_s(n:n) = char(0)
    hostinclusionstring_s(n:n) = char(0)
    mixingrulestring_g(n:n) = char(0)
    matrixstring_g(n:n) = char(0)
    inclusionstring_g(n:n) = char(0)
    hoststring_g(n:n) = char(0)
    hostmatrixstring_g(n:n) = char(0)
    hostinclusionstring_g(n:n) = char(0)
enddo

	mixingrulestring_s = 'maxwellgarnett'
    hoststring_s = 'air'
    matrixstring_s = 'water'
    inclusionstring_s = 'spheroidal'
    hostmatrixstring_s = 'icewater'
    hostinclusionstring_s = 'spheroidal'

    mixingrulestring_g = 'maxwellgarnett'
    hoststring_g = 'air'
    matrixstring_g = 'water'
    inclusionstring_g = 'spheroidal'
    hostmatrixstring_g = 'icewater'
    hostinclusionstring_g = 'spheroidal'

!..Create bins of snow (from 100 microns up to 2 cm).
    xxDx(1) = 100.D-6
    xxDx(nrbins+1) = 0.02d0
    do n = 2, nrbins
       xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
               *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
    enddo
    do n = 1, nrbins
        xxDs(n) = DSQRT(xxDx(n)*xxDx(n+1))
        xdts(n) = xxDx(n+1) - xxDx(n)
    enddo

!..Create bins of graupel (from 100 microns up to 5 cm).
    xxDx(1) = 100.D-6
    xxDx(nrbins+1) = 0.05d0
    do n = 2, nrbins
      xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
               *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
    enddo
    do n = 1, nrbins
        xxDg(n) = DSQRT(xxDx(n)*xxDx(n+1))
        xdtg(n) = xxDx(n+1) - xxDx(n)
    enddo

end subroutine radar_init

!MORE STUFF USED BY THOMPSON
      subroutine rayleigh_soak_wetgraupel (x_g, a_geo, b_geo, fmelt,    &
                     meltratio_outside, m_w, m_i, lambda, C_back,       &
                     mixingrule,matrix,inclusion,                       &
                     host,hostmatrix,hostinclusion)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: x_g, a_geo, b_geo, fmelt, lambda,  &
                                     meltratio_outside
      DOUBLE PRECISION, INTENT(out):: C_back
      COMPLEX*16, INTENT(in):: m_w, m_i
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion,     &
                                     host, hostmatrix, hostinclusion

      COMPLEX*16:: m_core, m_air
      DOUBLE PRECISION:: D_large, D_g, rhog, x_w, xw_a, fm, fmgrenz,    &
                         volg, vg, volair, volice, volwater,            &
                         meltratio_outside_grenz, mra
      INTEGER:: error
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0

      call radar_init

!     refractive index of air:
      m_air = (1.0d0,0.0d0)

!     Limiting the degree of melting --- for safety: 
      fm = DMAX1(DMIN1(fmelt, 1.0d0), 0.0d0)
!     Limiting the ratio of (melting on outside)/(melting on inside):
      mra = DMAX1(DMIN1(meltratio_outside, 1.0d0), 0.0d0)

!    ! The relative portion of meltwater melting at outside should increase
!    ! from the given input value (between 0 and 1)
!    ! to 1 as the degree of melting approaches 1,
!    ! so that the melting particle "converges" to a water drop.
!    ! Simplest assumption is linear:
      mra = mra + (1.0d0-mra)*fm

      x_w = x_g * fm

      D_g = a_geo * x_g**b_geo

      if (D_g .ge. 1d-12) then

       vg = PIx/6. * D_g**3
       rhog = DMAX1(DMIN1(x_g / vg, 900.0d0), 10.0d0)
       vg = x_g / rhog
      
       meltratio_outside_grenz = 1.0d0 - rhog / 1000.

       if (mra .le. meltratio_outside_grenz) then
        !..In this case, it cannot happen that, during melting, all the
        !.. air inclusions within the ice particle get filled with
        !.. meltwater. This only happens at the end of all melting.
        volg = vg * (1.0d0 - mra * fm)
 
       else
        !..In this case, at some melting degree fm, all the air
        !.. inclusions get filled with meltwater.
        fmgrenz=(900.0-rhog)/(mra*900.0-rhog+900.0*rhog/1000.)

        if (fm .le. fmgrenz) then
         !.. not all air pockets are filled:
         volg = (1.0 - mra * fm) * vg
        else
         !..all air pockets are filled with meltwater, now the
         !.. entire ice sceleton melts homogeneously:
         volg = (x_g - x_w) / 900.0 + x_w / 1000.
        endif

       endif

       D_large  = (6.0 / PIx * volg) ** (1./3.)
       volice = (x_g - x_w) / (volg * 900.0)
       volwater = x_w / (1000. * volg)
       volair = 1.0 - volice - volwater
      
       !..complex index of refraction for the ice-air-water mixture
       !.. of the particle:
       m_core = get_m_mix_nested (m_air, m_i, m_w, volair, volice,      &
                         volwater, mixingrule, host, matrix, inclusion, &
                         hostmatrix, hostinclusion, error)
       if (error .ne. 0) then
        C_back = 0.0d0
        return
       endif

       !..Rayleigh-backscattering coefficient of melting particle: 
       C_back = (ABS((m_core**2-1.0d0)/(m_core**2+2.0d0)))**2           &
                * PI5 * D_large**6 / lamda4

      else
       C_back = 0.0d0
      endif

      end subroutine rayleigh_soak_wetgraupel

!+---+-----------------------------------------------------------------+


      COMPLEX*16 FUNCTION m_complex_water_ray(lambda,T)

!      Complex refractive Index of Water as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.001,1.0] m; T in [-10.0,30.0] deg C
!      after Ray (1972)

      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: epsinf,epss,epsr,epsi
      DOUBLE PRECISION:: alpha,lambdas,sigma,nenner
      COMPLEX*16, PARAMETER:: i = (0d0,1d0)
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0

      epsinf  = 5.27137d0 + 0.02164740d0 * T - 0.00131198d0 * T*T
      epss    = 78.54d+0 * (1.0 - 4.579d-3 * (T - 25.0)                 &
              + 1.190d-5 * (T - 25.0)*(T - 25.0)                        &
              - 2.800d-8 * (T - 25.0)*(T - 25.0)*(T - 25.0))
      alpha   = -16.8129d0/(T+273.16) + 0.0609265d0
      lambdas = 0.00033836d0 * exp(2513.98d0/(T+273.16)) * 1e-2

      nenner = 1.d0+2.d0*(lambdas/lambda)**(1d0-alpha)*sin(alpha*PIx*0.5) &
             + (lambdas/lambda)**(2d0-2d0*alpha)
      epsr = epsinf + ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)   &
           * sin(alpha*PIx*0.5)+1d0)) / nenner
      epsi = ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)            &
           * cos(alpha*PIx*0.5)+0d0)) / nenner                           &
           + lambda*1.25664/1.88496
      
      m_complex_water_ray = SQRT(CMPLX(epsr,-epsi))
      
      END FUNCTION m_complex_water_ray

!+---+-----------------------------------------------------------------+
      
      COMPLEX*16 FUNCTION m_complex_ice_maetzler(lambda,T)
      
!      complex refractive index of ice as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.0001,30] m; T in [-250.0,0.0] C
!      Original comment from the Matlab-routine of Prof. Maetzler:
!      Function for calculating the relative permittivity of pure ice in
!      the microwave region, according to C. Maetzler, "Microwave
!      properties of ice and snow", in B. Schmitt et al. (eds.) Solar
!      System Ices, Astrophys. and Space Sci. Library, Vol. 227, Kluwer
!      Academic Publishers, Dordrecht, pp. 241-257 (1998). Input:
!      TK = temperature (K), range 20 to 273.15
!      f = frequency in GHz, range 0.01 to 3000
         
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: f,c,TK,B1,B2,b,deltabeta,betam,beta,theta,alfa

      c = 2.99d8
      TK = T + 273.16
      f = c / lambda * 1d-9

      B1 = 0.0207
      B2 = 1.16d-11
      b = 335.0d0
      deltabeta = EXP(-10.02 + 0.0364*(TK-273.16))
      betam = (B1/TK) * ( EXP(b/TK) / ((EXP(b/TK)-1)**2) ) + B2*f*f
      beta = betam + deltabeta
      theta = 300. / TK - 1.
      alfa = (0.00504d0 + 0.0062d0*theta) * EXP(-22.1d0*theta)
      m_complex_ice_maetzler = 3.1884 + 9.1e-4*(TK-273.16)
      m_complex_ice_maetzler = m_complex_ice_maetzler                   &
                             + CMPLX(0.0d0, (alfa/f + beta*f)) 
      m_complex_ice_maetzler = SQRT(CONJG(m_complex_ice_maetzler))
      
      END FUNCTION m_complex_ice_maetzler

!+---+-----------------------------------------------------------------+


      COMPLEX*16 FUNCTION get_m_mix (m_a, m_i, m_w, volair, volice,     &
                     volwater, mixingrule, matrix, inclusion, error)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion
      INTEGER, INTENT(out):: error

      error = 0
      get_m_mix = CMPLX(1.0d0,0.0d0)

      if (mixingrule .eq. 'maxwellgarnett') then
       if (matrix .eq. 'ice') then
        get_m_mix = m_complex_maxwellgarnett(volice, volair, volwater,  &
                           m_i, m_a, m_w, inclusion, error)
       elseif (matrix .eq. 'water') then
        get_m_mix = m_complex_maxwellgarnett(volwater, volair, volice,  &
                           m_w, m_a, m_i, inclusion, error)
       elseif (matrix .eq. 'air') then
        get_m_mix = m_complex_maxwellgarnett(volair, volwater, volice,  &
                           m_a, m_w, m_i, inclusion, error)
       else
        !write(radar_debug,*) 'GET_M_MIX: unknown matrix: ', matrix
        !CALL wrf_debug(150, radar_debug)
        error = 1
       endif

      else
       !write(radar_debug,*) 'GET_M_MIX: unknown mixingrule: ', mixingrule
       !CALL wrf_debug(150, radar_debug)
       error = 2
      endif

      if (error .ne. 0) then
       !write(radar_debug,*) 'GET_M_MIX: error encountered'
       !CALL wrf_debug(150, radar_debug)
      endif

      END FUNCTION get_m_mix

!+---+-----------------------------------------------------------------+
      complex*16 function get_m_mix_nested (m_a, m_i, m_w, volair,      &
                     volice, volwater, mixingrule, host, matrix,        &
                     inclusion, hostmatrix, hostinclusion, cumulerror)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, host, matrix,          &
                     inclusion, hostmatrix, hostinclusion
      INTEGER, INTENT(out):: cumulerror

      DOUBLE PRECISION:: vol1, vol2
      COMPLEX*16:: mtmp
      INTEGER:: error

      !..Folded: ( (m1 + m2) + m3), where m1,m2,m3 could each be
      !.. air, ice, or water

      cumulerror = 0
      get_m_mix_nested = CMPLX(1.0d0,0.0d0)

      if (host .eq. 'air') then

       if (matrix .eq. 'air') then
        !write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        !CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volice / MAX(volice+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, 0.0d0, vol1, vol2,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
          
        if (hostmatrix .eq. 'air') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                         volair, (1.0d0-volair), 0.0d0, mixingrule,     &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'icewater') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                         volair, (1.0d0-volair), 0.0d0, mixingrule,     &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error
        else
         !write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
         !CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'ice') then

       if (matrix .eq. 'ice') then
        !write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        !CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volair+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, 0.0d0, vol2,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'ice') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                         (1.0d0-volice), volice, 0.0d0, mixingrule,     &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airwater') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                         (1.0d0-volice), volice, 0.0d0, mixingrule,     &
                         'air', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
         !write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
         !CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'water') then

       if (matrix .eq. 'water') then
        !write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        !CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volice+volair,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, vol2, 0.0d0,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'water') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airice') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
         !write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
         !CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'none') then

       get_m_mix_nested = get_m_mix (m_a, m_i, m_w,                     &
                       volair, volice, volwater, mixingrule,            &
                       matrix, inclusion, error)
       cumulerror = cumulerror + error
        
      else
       !write(radar_debug,*) 'GET_M_MIX_NESTED: unknown matrix: ', host
       !CALL wrf_debug(150, radar_debug)
       cumulerror = cumulerror + 1
      endif

      IF (cumulerror .ne. 0) THEN
       !write(radar_debug,*) 'GET_M_MIX_NESTED: error encountered'
       !CALL wrf_debug(150, radar_debug)
       get_m_mix_nested = CMPLX(1.0d0,0.0d0)    
      endif

      end function get_m_mix_nested

!+---+-----------------------------------------------------------------+
     COMPLEX*16 FUNCTION m_complex_maxwellgarnett(vol1, vol2, vol3,    &
                     m1, m2, m3, inclusion, error)

      IMPLICIT NONE

      COMPLEX*16 :: m1, m2, m3
      DOUBLE PRECISION :: vol1, vol2, vol3
      CHARACTER(len=*) :: inclusion

      COMPLEX*16 :: beta2, beta3, m1t, m2t, m3t
      INTEGER, INTENT(out) :: error

      error = 0

      if (DABS(vol1+vol2+vol3-1.0d0) .gt. 1d-6) then
       !write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: sum of the ',       &
              !'partial volume fractions is not 1...ERROR'
       !CALL wrf_debug(150, radar_debug)
       m_complex_maxwellgarnett=CMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m1t = m1**2
      m2t = m2**2
      m3t = m3**2

      if (inclusion .eq. 'spherical') then
       beta2 = 3.0d0*m1t/(m2t+2.0d0*m1t)
       beta3 = 3.0d0*m1t/(m3t+2.0d0*m1t)
      elseif (inclusion .eq. 'spheroidal') then
       beta2 = 2.0d0*m1t/(m2t-m1t) * (m2t/(m2t-m1t)*LOG(m2t/m1t)-1.0d0)
       beta3 = 2.0d0*m1t/(m3t-m1t) * (m3t/(m3t-m1t)*LOG(m3t/m1t)-1.0d0)
      else
       !write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: ', 'unknown inclusion: ', inclusion
       !CALL wrf_debug(150, radar_debug)
       m_complex_maxwellgarnett=DCMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m_complex_maxwellgarnett = &
       SQRT(((1.0d0-vol2-vol3)*m1t + vol2*beta2*m2t + vol3*beta3*m3t) / &
       (1.0d0-vol2-vol3+vol2*beta2+vol3*beta3))

      END FUNCTION m_complex_maxwellgarnett

!+---+-----------------------------------------------------------------+
!****** THOMPSON FORWARD OPERATOR
subroutine hx_thomp(qvgesin0,qrgesin0,qggesin0,qsgesin0,qcgesin0,nrgesin0,presgesin,tempgesin,rDBZ, vt_DBZ, debugging)
  use kinds, only: r_kind,r_double,i_kind
  use obsmod, only: static_gsi_nopcp_dbz

implicit none

real(r_kind) :: qvgesin0,qrgesin0,qsgesin0,qggesin0, qcgesin0, nrgesin0,presgesin,tempgesin
real(r_kind) :: qv1, qc1, qr1, nr1, qs1, qg1, t1, p1
real(r_kind) :: rDBZ, vt_DBZ

logical :: debugging

!..Local variables
      REAL :: temp, pres, qv, rho, rhof
      REAL :: rc, rr, nr, rs, rg

      DOUBLE PRECISION :: ilamr, ilamg, N0_r, N0_g
      REAL	:: mvd_r
      REAL  :: smob, smo2, smoc, smoz
      REAL	:: oM3, M0, Mrat, slam1, slam2, xDs
      REAL	:: ils1, ils2, t1_vts, t2_vts, t3_vts, t4_vts
      REAL	:: vtr_dbz_wt, vts_dbz_wt, vtg_dbz_wt

      REAL  :: ze_rain, ze_snow, ze_graupel, ze_snow_fz, ze_graupel_fz

      DOUBLE PRECISION:: N0_exp, N0_min, lam_exp, lamr, lamg
      REAL:: a_, b_, loga_, tc0
      DOUBLE PRECISION:: fmelt_s, fmelt_g

      INTEGER :: i, j, k, k_0, kbot, m, n
      LOGICAL :: melti
      LOGICAL :: L_qr, L_qs, L_qg, L_qc, L_qi

      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL:: xslw1, ygra1, zans1


      LOGICAL:: micro_init

      LOGICAL, PARAMETER:: iiwarm = .false.
      INTEGER, PARAMETER:: IFDRY = 0
      REAL, PARAMETER :: T_0 = 273.15
      REAL, PARAMETER :: PI = 3.1415926536


!..Densities of rain, snow, graupel, and cloud ice.
      REAL, PARAMETER :: rho_w = 1000.0
      REAL, PARAMETER :: rho_s = 100.0
      REAL, PARAMETER :: rho_g = 500.0
      REAL, PARAMETER :: rho_i = 890.0

!..Prescribed number of cloud droplets.  Set according to known data or
!.. roughly 100 per cc (100.E6 m^-3) for Maritime cases and
!.. 300 per cc (300.E6 m^-3) for Continental.  Gamma shape parameter,
!.. mu_c, calculated based on Nt_c is important in autoconversion
!.. scheme.
      REAL, PARAMETER :: Nt_c = 100.E6
      REAL, PARAMETER :: Nt_c_max = 1999.E6

!..Generalized gamma distributions for rain, graupel and cloud ice.
!.. N(D) = N_0 * D**mu * exp(-lamda*D);  mu=0 is exponential.
      REAL, PARAMETER :: mu_r = 0.0
      REAL, PARAMETER :: mu_g = 0.0
      REAL, PARAMETER :: mu_i = 0.0
      REAL :: mu_c
      INTEGER :: nu_c

!..Sum of two gamma distrib for snow (Field et al. 2005).
!.. N(D) = M2**4/M3**3 * [Kap0*exp(-M2*Lam0*D/M3)
!..    + Kap1*(M2/M3)**mu_s * D**mu_s * exp(-M2*Lam1*D/M3)]
!.. M2 and M3 are the (bm_s)th and (bm_s+1)th moments respectively
!.. calculated as function of ice water content and temperature.
      REAL, PARAMETER :: mu_s = 0.6357
      REAL, PARAMETER :: Kap0 = 490.6
      REAL, PARAMETER :: Kap1 = 17.46
      REAL, PARAMETER :: Lam0 = 20.78
      REAL, PARAMETER :: Lam1 = 3.29

!..Y-intercept parameter for graupel is not constant and depends on
!.. mixing ratio.  Also, when mu_g is non-zero, these become equiv
!.. y-intercept for an exponential distrib and proper values are
!.. computed based on same mixing ratio and total number concentration.
      REAL, PARAMETER :: gonv_min = 1.E4
      REAL, PARAMETER :: gonv_max = 3.E6

!..Mass power law relations:  mass = am*D**bm
!.. Snow from Field et al. (2005), others assume spherical form.
      REAL, PARAMETER :: am_r = PI*rho_w/6.0
      REAL, PARAMETER :: bm_r = 3.0
      REAL, PARAMETER :: am_s = 0.069
      REAL, PARAMETER :: bm_s = 2.0
      REAL, PARAMETER :: am_g = PI*rho_g/6.0
      REAL, PARAMETER :: bm_g = 3.0
      REAL, PARAMETER :: am_i = PI*rho_i/6.0
      REAL, PARAMETER :: bm_i = 3.0

!..Fallspeed power laws relations:  v = (av*D**bv)*exp(-fv*D)
!.. Rain from Ferrier (1994), ice, snow, and graupel from
!.. Thompson et al (2008). Coefficient fv is zero for graupel/ice.
      REAL, PARAMETER :: av_r = 4854.0
      REAL, PARAMETER :: bv_r = 1.0
      REAL, PARAMETER :: fv_r = 195.0
      REAL, PARAMETER :: av_s = 40.0
      REAL, PARAMETER :: bv_s = 0.55
      REAL, PARAMETER :: fv_s = 100.0
      REAL, PARAMETER :: av_g = 442.
      REAL, PARAMETER :: bv_g = 0.89
      REAL, PARAMETER :: av_i = 1847.5
      REAL, PARAMETER :: bv_i = 1.0
      REAL, PARAMETER :: av_c = 0.316946E8
      REAL, PARAMETER :: bv_c = 2.0    
      

!..Minimum microphys values
!.. R1 value, 1.E-12, cannot be set lower because of numerical
!.. problems with Paul Field's moments and should not be set larger
!.. because of truncation problems in snow/ice growth.
      REAL, PARAMETER :: R1 = 1.E-12
      REAL, PARAMETER :: R2 = 1.E-6
      REAL, PARAMETER :: R3 = 1.0
      REAL, PARAMETER :: R4 = 1.E-4
      REAL, PARAMETER :: R5 = 1.E-5
      REAL, PARAMETER :: R7 = 1.E-7
      REAL, PARAMETER :: eps = 1.E-15

!..Rho_not used in fallspeed relations (rho_not/rho)**.5 adjustment.
      REAL, PARAMETER :: rho_not = 101325.0/(287.05*298.0)

!..Schmidt number
      REAL, PARAMETER :: Sc = 0.632
      REAL :: Sc3

!..Homogeneous freezing temperature
      REAL, PARAMETER :: HGFR = 235.16

!..Water vapor and air gas constants at constant pressure
      REAL, PARAMETER :: Rv = 461.5
      REAL, PARAMETER :: oRv = 1./Rv
      REAL, PARAMETER :: R = 287.04
      REAL, PARAMETER :: Cp = 1004.0

!..Enthalpy of sublimation, vaporization, and fusion at 0C.
      REAL, PARAMETER :: lsub = 2.834E6
      REAL, PARAMETER :: lvap0 = 2.5E6
      REAL, PARAMETER :: lfus = lsub - lvap0
      REAL, PARAMETER :: olfus = 1./lfus

!..Ice initiates with this mass (kg), corresponding diameter calc.
!..Min diameters and mass of cloud, rain, snow, and graupel (m, kg).
      REAL, PARAMETER :: xm0i = 1.E-12
      REAL, PARAMETER :: D0c = 1.E-6
      REAL, PARAMETER :: D0r = 50.E-6
      REAL, PARAMETER :: D0s = 200.E-6
      REAL, PARAMETER :: D0g = 250.E-6
      REAL :: D0i, xm0s, xm0g

!..Lookup table dimensions
      INTEGER, PARAMETER :: ntb_t = 9

!..For snow moments conversions (from Field et al. 2005)
      REAL, DIMENSION(10), PARAMETER :: &
      sa = (/ 5.065339, -0.062659, -3.032362, 0.029469, -0.000285, &
              0.31255,   0.000204,  0.003199, 0.0,      -0.015952/)
      REAL, DIMENSION(10), PARAMETER :: &
      sb = (/ 0.476221, -0.015896,  0.165977, 0.007468, -0.000141, &
              0.060366,  0.000079,  0.000594, 0.0,      -0.003577/)

!..Temperatures (5 C interval 0 to -40) used in lookup tables.
      REAL, DIMENSION(ntb_t), PARAMETER :: &
      Tc = (/-0.01, -5., -10., -15., -20., -25., -30., -35., -40. /)


!..Variables holding a bunch of exponents and gamma values (cloud water,
!.. cloud ice, rain, snow, then graupel).
      REAL, DIMENSION(7) :: cie, cig
      REAL:: oig1, oig2, obmi
      REAL, DIMENSION(13) :: cre, crg
      REAL:: ore1, org1, org2, org3, obmr
      REAL, DIMENSION(18) :: cse, csg
      REAL :: oams, obms, ocms
      REAL, DIMENSION(12) :: cge, cgg
      REAL :: oge1, ogg1, ogg2, ogg3, oamg, obmg, ocmg



!..Allocate space for lookup tables (J. Michalakes 2009Jun08)
      micro_init = .TRUE.

      if (micro_init) then

!..From Martin et al. (1994), assign gamma shape parameter mu for cloud
!.. drops according to general dispersion characteristics (disp=~0.25
!.. for Maritime and 0.45 for Continental).
!.. disp=SQRT((mu+2)/(mu+1) - 1) so mu varies from 15 for Maritime
!.. to 2 for really dirty air.
      mu_c = MIN(15., (1000.E6/Nt_c + 2.))

!..Schmidt number to one-third used numerous times.
      Sc3 = Sc**(1./3.)

!..Compute min ice diam from mass, min snow/graupel mass from diam.
      D0i = (xm0i/am_i)**(1./bm_i)
      xm0s = am_s * D0s**bm_s
      xm0g = am_g * D0g**bm_g

!..These constants various exponents and gamma() assoc with cloud,
!.. rain, snow, and graupel.


      cie(1) = mu_i + 1.
      cie(2) = bm_i + mu_i + 1.
      cie(3) = bm_i + mu_i + bv_i + 1.
      cie(4) = mu_i + bv_i + 1.
      cie(5) = mu_i + 2.
      cie(6) = bm_i*0.5 + mu_i + bv_i + 1.
      cie(7) = bm_i*0.5 + mu_i + 1.
      cig(1) = WGAMMA(cie(1))
      cig(2) = WGAMMA(cie(2))
      cig(3) = WGAMMA(cie(3))
      cig(4) = WGAMMA(cie(4))
      cig(5) = WGAMMA(cie(5))
      cig(6) = WGAMMA(cie(6))
      cig(7) = WGAMMA(cie(7))
      oig1 = 1./cig(1)
      oig2 = 1./cig(2)
      obmi = 1./bm_i

      cre(1) = bm_r + 1.
      cre(2) = mu_r + 1.
      cre(3) = bm_r + mu_r + 1.
      cre(4) = bm_r*2. + mu_r + 1.
      cre(5) = mu_r + bv_r + 1.
      cre(6) = bm_r + mu_r + bv_r + 1.
      cre(7) = bm_r*0.5 + mu_r + bv_r + 1.
      cre(8) = bm_r + mu_r + bv_r + 3.
      cre(9) = mu_r + bv_r + 3.
      cre(10) = mu_r + 2.
      cre(11) = 0.5*(bv_r + 5. + 2.*mu_r)
      cre(12) = bm_r*0.5 + mu_r + 1.
      cre(13) = bm_r*2. + mu_r + bv_r + 1.
      do n = 1, 13
         crg(n) = WGAMMA(cre(n))
      enddo
      obmr = 1./bm_r
      ore1 = 1./cre(1)
      org1 = 1./crg(1)
      org2 = 1./crg(2)
      org3 = 1./crg(3)

      cse(1) = bm_s + 1.
      cse(2) = bm_s + 2.
      cse(3) = bm_s*2.
      cse(4) = bm_s + bv_s + 1.
      cse(5) = bm_s*2. + bv_s + 1.
      cse(6) = bm_s*2. + 1.
      cse(7) = bm_s + mu_s + 1.
      cse(8) = bm_s + mu_s + 2.
      cse(9) = bm_s + mu_s + 3.
      cse(10) = bm_s + mu_s + bv_s + 1.
      cse(11) = bm_s*2. + mu_s + bv_s + 1.
      cse(12) = bm_s*2. + mu_s + 1.
      cse(13) = bv_s + 2.
      cse(14) = bm_s + bv_s
      cse(15) = mu_s + 1.
      cse(16) = 1.0 + (1.0 + bv_s)/2.
      cse(17) = cse(16) + mu_s + 1.
      cse(18) = bv_s + mu_s + 3.
      do n = 1, 18
         csg(n) = WGAMMA(cse(n))
      enddo
      oams = 1./am_s
      obms = 1./bm_s
      ocms = oams**obms

      cge(1) = bm_g + 1.
      cge(2) = mu_g + 1.
      cge(3) = bm_g + mu_g + 1.
      cge(4) = bm_g*2. + mu_g + 1.
      cge(5) = bm_g*2. + mu_g + bv_g + 1.
      cge(6) = bm_g + mu_g + bv_g + 1.
      cge(7) = bm_g + mu_g + bv_g + 2.
      cge(8) = bm_g + mu_g + bv_g + 3.
      cge(9) = mu_g + bv_g + 3.
      cge(10) = mu_g + 2.
      cge(11) = 0.5*(bv_g + 5. + 2.*mu_g)
      cge(12) = 0.5*(bv_g + 5.) + mu_g
      do n = 1, 12
         cgg(n) = WGAMMA(cge(n))
      enddo
      oamg = 1./am_g
      obmg = 1./bm_g
      ocmg = oamg**obmg
      oge1 = 1./cge(1)
      ogg1 = 1./cgg(1)
      ogg2 = 1./cgg(2)
      ogg3 = 1./cgg(3)


      endif

!CONVERT INPUT VARIABLES NAMES TO LOCAL VARIABLES
qv1=qvgesin0
qr1=qrgesin0
qs1=qsgesin0
qg1=qggesin0
qc1=qcgesin0
nr1=nrgesin0
p1=presgesin
t1=tempgesin

!-- SET DEFAULT VALUE
         rdBZ = 0.0

!+---+-----------------------------------------------------------------+
!..Put column of data into local arrays.
!+---+-----------------------------------------------------------------+
  
         temp = t1
         qv = MAX(1.E-10, qv1)
         pres = p1
         rho = 0.622*pres/(R*temp*(qv+0.622))
         rhof = SQRT(RHO_NOT/rho)
         
         ! TAJ add a check to precent dBZ being calculated from Nr < 1
         if (qr1 .gt. R1 .and. nr1 .gt. R3) then
            rr = qr1*rho
            nr = MAX(R2, nr1*rho)
            L_qr = .true.
            lamr = (am_r*crg(3)*org2*nr/rr)**obmr
            mvd_r = (3.0 + mu_r + 0.672) / lamr
            
            if (mvd_r .gt. 2.5E-3) then
               mvd_r = 2.5E-3
               lamr = (3.0 + mu_r + 0.672) / mvd_r
               nr = crg(2)*org3*rr*lamr**bm_r / am_r
            elseif (mvd_r .lt. D0r*0.75) then
               mvd_r = D0r*0.75
               lamr = (3.0 + mu_r + 0.672) / mvd_r
               nr = crg(2)*org3*rr*lamr**bm_r / am_r
            endif
            ilamr = 1./lamr
            N0_r = nr*org2*lamr**cre(2)
         else
            rr = R1
            nr = R2
            mvd_r = 50.E-6
            L_qr = .false.
         endif
         if (qs1 .gt. R2) then
            rs = qs1*rho
            L_qs = .true.
         else
            rs = R1
            L_qs = .false.
         endif
         if (qg1 .gt. R2) then
            rg = qg1*rho
            L_qg = .true.
         else     
            rg = R1
            L_qg = .false.
         endif


!+---+-----------------------------------------------------------------+
!..Calculate y-intercept, slope, and useful moments for snow.
!+---+-----------------------------------------------------------------+

         tc0 = MIN(-0.1, temp-273.15)
         smob = rs*oams

!..All other moments based on reference, 2nd moment.  If bm_s.ne.2,
!.. then we must compute actual 2nd moment and use as reference.
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2 = smob
         else
            loga_ = sa(1) + sa(2)*tc0 + sa(3)*bm_s &
               + sa(4)*tc0*bm_s + sa(5)*tc0*tc0 &
               + sa(6)*bm_s*bm_s + sa(7)*tc0*tc0*bm_s &
               + sa(8)*tc0*bm_s*bm_s + sa(9)*tc0*tc0*tc0 &
               + sa(10)*bm_s*bm_s*bm_s
            a_ = 10.0**loga_
            b_ = sb(1) + sb(2)*tc0 + sb(3)*bm_s &
               + sb(4)*tc0*bm_s + sb(5)*tc0*tc0 &
               + sb(6)*bm_s*bm_s + sb(7)*tc0*tc0*bm_s &
               + sb(8)*tc0*bm_s*bm_s + sb(9)*tc0*tc0*tc0 &
               + sb(10)*bm_s*bm_s*bm_s
            smo2 = (smob/a_)**(1./b_)
         endif


!..Calculate bm_s+1 (th) moment.  Useful for diameter calcs.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(1) &
               + sa(4)*tc0*cse(1) + sa(5)*tc0*tc0 &
               + sa(6)*cse(1)*cse(1) + sa(7)*tc0*tc0*cse(1) &
               + sa(8)*tc0*cse(1)*cse(1) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(1)*cse(1)*cse(1)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(1) + sb(4)*tc0*cse(1) &
              + sb(5)*tc0*tc0 + sb(6)*cse(1)*cse(1) &
              + sb(7)*tc0*tc0*cse(1) + sb(8)*tc0*cse(1)*cse(1) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(1)*cse(1)*cse(1)
         smoc = a_ * smo2**b_


!..Calculate bm_s*2 (th) moment.  Useful for reflectivity.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3) &
               + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0 &
               + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3) &
               + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(3)*cse(3)*cse(3)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) &
              + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3) &
              + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)
         smoz = a_ * smo2**b_


!+---+-----------------------------------------------------------------+
!..Calculate y-intercept, slope values for graupel.
!+---+-----------------------------------------------------------------+

      N0_min = gonv_max

         if (temp.lt.270.65 .and. L_qr .and. mvd_r.gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r)
         else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, rg))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0_exp = 10.**(zans1)
         N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
         N0_min = MIN(N0_exp, N0_min)
         N0_exp = N0_min
         lam_exp = (N0_exp*am_g*cgg(1)/rg)**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg = 1./lamg
         N0_g = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)


!+---+-----------------------------------------------------------------+
!..Locate K-level of start of melting (k_0 is level above).
!+---+-----------------------------------------------------------------+
      melti = .false.
         if ( (temp.gt.273.15) .and. L_qr  .and. (L_qs.or.L_qg) ) then
            melti=.true.
         endif

!+---+-----------------------------------------------------------------+
!..Assume Rayleigh approximation at 10 cm wavelength. Rain (all temps)
!.. and non-water-coated snow and graupel when below freezing are
!.. simple. Integrations of m(D)*m(D)*N(D)*dD.
!+---+-----------------------------------------------------------------+
         ze_rain = 1.e-22
         ze_snow = 1.e-22
         ze_graupel = 1.e-22
         if (L_qr) ze_rain = N0_r*crg(4)*ilamr**cre(4)
         if (L_qs) ze_snow = (0.176/0.93) * (6.0/PI)*(6.0/PI)     &
                                 * (am_s/900.0)*(am_s/900.0)*smoz
         if (L_qg) ze_graupel = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
                                    * (am_g/900.0)*(am_g/900.0)         &
                                    * N0_g*cgg(4)*ilamg**cge(4)
         ze_graupel_fz = ze_graupel
         ze_snow_fz = ze_snow

 !print *, L_qr, L_qs, L_qg, ze_rain, ze_snow, ze_graupel
 

!+---+-----------------------------------------------------------------+
!..Special case of melting ice (snow/graupel) particles.  Assume the
!.. ice is surrounded by the liquid water.  Fraction of meltwater is
!.. extremely simple based on amount found above the melting level.
!.. Uses code from Uli Blahak (rayleigh_soak_wet graupel and supporting
!.. routines).
!+---+-----------------------------------------------------------------+

      if (.not. iiwarm .and. melti ) then

!..Reflectivity contributed by melting snow
          !if (L_qs(k) .and. L_qs(k_0) ) then
           if ( L_qs ) then

           !fmelt_s = MAX(0.05d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           fmelt_s = 0.25d0

           eta = 0.d0
           oM3 = 1./smoc
           M0 = (smob*oM3)
           Mrat = smob*M0*M0*M0
           slam1 = M0 * Lam0
           slam2 = M0 * Lam1
           do n = 1, nrbins
              x = am_s * xxDs(n)**bm_s

              call rayleigh_soak_wetgraupel (x, DBLE(ocms), DBLE(obms), &
                    fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_s, matrixstring_s,          &
                    inclusionstring_s, hoststring_s,                    &
                    hostmatrixstring_s, hostinclusionstring_s)

              f_d = Mrat*(Kap0*DEXP(-slam1*xxDs(n))                     &
                    + Kap1*(M0*xxDs(n))**mu_s * DEXP(-slam2*xxDs(n)))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow = SNGL(lamda4 / (pi5 * K_w) * eta)
          if ( 10.*log10((ze_snow)*1.d18) .gt. 70.0  ) then
                !print*, "QS", 10.*log10((ze_snow)*1.d18), 10.*log10((ze_snow_fz)*1.d18), qs1, eta, f_d, CBACK
                ze_snow = ze_snow_fz
          endif
          endif

!..Reflectivity contributed by melting graupel
          !if (L_qg(k) .and. L_qg(k_0) ) then
          if ( L_qg  ) then
           !fmelt_g = MAX(0.05d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
            fmelt_g = 0.25d0

           eta = 0.d0
           lamg = 1./ilamg
           do n = 1, nrbins
              x = am_g * xxDg(n)**bm_g

              call rayleigh_soak_wetgraupel (x, DBLE(ocmg), DBLE(obmg), &
                    fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_g, matrixstring_g,          &
                    inclusionstring_g, hoststring_g,                    &
                    hostmatrixstring_g, hostinclusionstring_g)

              f_d = N0_g*xxDg(n)**mu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel = SNGL(lamda4 / (pi5 * K_w) * eta)
          
	  if ( 10.*log10((ze_graupel)*1.d18) .gt. 70.0  ) then
		!print*, 10.*log10((ze_graupel)*1.d18), 10.*log10((ze_graupel_fz)*1.d18), qg1, eta, f_d, CBACK
		ze_graupel = ze_graupel_fz
	  endif

          endif
      endif

!-- CALCULATE FINAL REFLECTIVITY
       rDBZ = 10.*log10((ze_rain+ze_snow+ze_graupel)*1.d18)
       if(rdBZ.lt.static_gsi_nopcp_dbz) rdBZ=static_gsi_nopcp_dbz !notice, static_gsi_nopcp_dbz should be larger than -30

       !print*, 10.*log10(ze_rain*1.d18), 10.*log10(ze_snow*1.d18), 10.*log10(ze_graupel*1.d18)

!-- CALCULATE DBZ-WEIGHTED TERMINAL VELOCITY
        vt_dBZ = 1.E-3
        
        !VT-SNOW
        if (rs.gt.R2) then
         Mrat = smob / smoc
         ils1 = 1./(Mrat*Lam0 + fv_s)
         ils2 = 1./(Mrat*Lam1 + fv_s)
         t1_vts = Kap0*csg(5)*ils1**cse(5)
         t2_vts = Kap1*Mrat**mu_s*csg(11)*ils2**cse(11)
         ils1 = 1./(Mrat*Lam0)
         ils2 = 1./(Mrat*Lam1)
         t3_vts = Kap0*csg(6)*ils1**cse(6)
         t4_vts = Kap1*Mrat**mu_s*csg(12)*ils2**cse(12)
         vts_dbz_wt = rhof*av_s * (t1_vts+t2_vts)/(t3_vts+t4_vts)
         if (temp.ge.273.15 .and. temp.lt.275.15) then
            vts_dbz_wt = vts_dbz_wt*1.5
         elseif (temp.ge.275.15) then
            vts_dbz_wt = vts_dbz_wt*2.0
         endif
        else
         vts_dbz_wt = 1.E-3
         if ( vts_dbz_wt.gt.50.0 ) then
                vts_dbz_wt = 1.E-3
         endif
        endif

        ! VT RAIN
        if (rr.gt.R1) then
         lamr = 1./ilamr
         vtr_dbz_wt = rhof*av_r*crg(13)*(lamr+fv_r)**(-cre(13)) / (crg(4)*lamr**(-cre(4)))
        else
         vtr_dbz_wt = 1.E-3
         if ( vtr_dbz_wt.gt.50.0 ) then
                vtr_dbz_wt = 1.E-3
         endif
        endif

        ! VT GRAUP
        if (rg.gt.R2) then
         lamg = 1./ilamg
         vtg_dbz_wt = rhof*av_g*cgg(5)*lamg**(-cge(5)) / (cgg(4)*lamg**(-cge(4)))
        else
         vtg_dbz_wt = 1.E-3
	 if ( vtg_dbz_wt.gt.50.0 ) then
 		vtg_dbz_wt = 1.E-3
	 endif
        endif

        ! TOTAL VT
        vt_DBZ = (vts_dbz_wt*ze_snow + vtr_dbz_wt*ze_rain + vtg_dbz_wt*ze_graupel) &
     &                / (ze_rain+ze_snow+ze_graupel)
        if (vt_DBZ < 0.0) then
      		vt_DBZ = 0.0
        endif
        if (vt_DBZ > 30.0) then
            vt_DBZ = 30.0
        endif

if(debugging) then
 !print *, "DBZ, VT = ", rDBZ, vt_DBZ
 !print*, 'USING THOMPSON REFLECTIVITY / FALL VELOCITY'
endif

end subroutine hx_thomp

!+---+-----------------------------------------------------------------+
      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

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
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA

!+---+-----------------------------------------------------------------+

! *** END THOMPSON





! *** STUFF FOR ADJOINT FORWARD OPERATOR (not supported by TAJ)
subroutine jqr_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqr)
  use kinds, only: r_kind,r_double,i_kind

implicit none
real(r_kind) :: qrgesin0,qsgesin0,qggesin0
real(r_kind) :: qrgesin,rhogesin,tempgesin,jqr
real(r_kind) :: Ze,rDBZ,zqr,zqg,zqs,qsgesin,qggesin

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi,thisqrgesin
!if(logspace) then
! qrgesin=10**qrgesin0
! qsgesin=10**qsgesin0
! qggesin=10**qggesin0
!else
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0
!endif

pi=3.14159
dielectric=0.224
n0r=8e6
n0s=3e6 !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6
rhos=100
rhor=1000
rhog=500 !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20)/(((pi*rhor)**1.75)*(n0r**0.75))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20)/(((pi*rhog)**1.75)*(n0g**0.75))
param_wet_g=(7.2e20)/((((pi*rhog)**1.75)*(n0g**0.75))**0.95)
param_wet_s=(7.2e20)/(((pi*rhos)**1.75)*(n0s**0.75))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqrgesin=qrgesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75)
if (tempgesin .lt. 273.15) then
zqr=0
thisqrgesin=0
zqg=param_dry_g*((rhogesin*qggesin)**1.75)
zqs=param_dry_s*((rhogesin*qsgesin)**1.75)
else if (tempgesin .lt. 278.15) then
zqg=param_wet_g*((rhogesin*qggesin)**1.6675)
zqs=param_wet_s*((rhogesin*qsgesin)**1.75)
else
zqg=0
zqs=0
endif

Ze = zqr+zqg+zqs 

if (tempgesin .ge. 273.15) then 
!if(.not.logspace) then
jqr=(10*param_r*(rhogesin**1.75)*1.75*(thisqrgesin**0.75))/(log(10.0)*Ze)
!else
!jqr=(10*param_r*(rhogesin**1.75)*1.75*(thisqrgesin**1.75))/(Ze)
!endif
else
jqr=0.0
endif 

end subroutine jqr_dart

subroutine jqs_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqs)
  use kinds, only: r_kind,r_double,i_kind
  
implicit none
real(r_kind) :: qsgesin0,qggesin0,qrgesin0
real(r_kind) :: qsgesin,rhogesin,tempgesin,jqs
real(r_kind) :: Ze,rDBZ,qrgesin,qggesin,zqr,zqs,zqg

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi,thisqsgesin
!if(logspace) then
! qrgesin=10**qrgesin0
! qsgesin=10**qsgesin0
! qggesin=10**qggesin0
!else
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0
!endif


pi=3.14159
dielectric=0.224
n0r=8e6
n0s=3e6 !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6 !values taken from jung et al 2008/lfo83
rhos=100
rhor=1000
rhog=500 !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20)/(((pi*rhor)**1.75)*(n0r**0.75))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20)/(((pi*rhog)**1.75)*(n0g**0.75))
param_wet_g=(7.2e20)/((((pi*rhog)**1.75)*(n0g**0.75))**0.95)
param_wet_s=(7.2e20)/(((pi*rhos)**1.75)*(n0s**0.75))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqsgesin=qsgesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75)
if (tempgesin .lt. 273.15) then
zqr=0
zqg=param_dry_g*((rhogesin*qggesin)**1.75)
zqs=param_dry_s*((rhogesin*qsgesin)**1.75)
else if (tempgesin .lt. 278.15) then
zqg=param_wet_g*((rhogesin*qggesin)**1.6675)
zqs=param_wet_s*((rhogesin*qsgesin)**1.75)
else
zqg=0
zqs=0
thisqsgesin=0.0
endif

Ze = zqr+zqg+zqs 
if (tempgesin .lt. 273.15) then
!if(.not.logspace)then
jqs=(10*param_dry_s*(rhogesin**1.75)*1.75*(thisqsgesin**0.75))/(log(10.0)*Ze)
!else
!jqs=(10*param_dry_s*(rhogesin**1.75)*1.75*(thisqsgesin**1.75))/(Ze)
!endif
else
!if(.not.logspace)then
jqs=(10*param_wet_s*(rhogesin**1.75)*1.75*(thisqsgesin**0.75))/(log(10.0)*Ze)
!else
!jqs=(10*param_wet_s*(rhogesin**1.75)*1.75*(thisqsgesin**1.75))/(Ze)
!endif
endif

end subroutine jqs_dart

subroutine jqg_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqg)
  use kinds, only: r_kind,r_double,i_kind

implicit none
real(r_kind) :: qggesin0,qsgesin0,qrgesin0
real(r_kind) :: qggesin,rhogesin,tempgesin,jqg
real(r_kind) :: Ze,rDBZ,qrgesin,qsgesin,zqr,zqs,zqg,thisqggesin

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi
!if(logspace) then
! qrgesin=10**qrgesin0
! qsgesin=10**qsgesin0
! qggesin=10**qggesin0
!else
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0
!endif


pi=3.14159
dielectric=0.224
n0r=8e6
n0s=3e6 !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6
rhos=100
rhor=1000
rhog=500 !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20)/(((pi*rhor)**1.75)*(n0r**0.75))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20)/(((pi*rhog)**1.75)*(n0g**0.75))
param_wet_g=(7.2e20)/((((pi*rhog)**1.75)*(n0g**0.75))**0.95)
param_wet_s=(7.2e20)/(((pi*rhos)**1.75)*(n0s**0.75))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqggesin=qggesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75)
if (tempgesin .lt. 273.15) then
zqr=0
zqg=param_dry_g*((rhogesin*qggesin)**1.75)
zqs=param_dry_s*((rhogesin*qsgesin)**1.75)
else if (tempgesin .lt. 278.15) then
zqg=param_wet_g*((rhogesin*qggesin)**1.6675)
zqs=param_wet_s*((rhogesin*qsgesin)**1.75)
else
zqg=0
zqs=0
thisqggesin=0.0
endif

Ze = zqr+zqg+zqs 

if (tempgesin .lt. 273.15) then
!if(.not.logspace)then
jqg=(10*param_dry_g*(rhogesin**1.75)*1.75*(thisqggesin**0.75))/(log(10.0)*Ze)
!else
!jqg=(10*param_dry_g*(rhogesin**1.75)*1.75*(thisqggesin**1.75))/(Ze)
!endif
else
!if(.not.logspace) then
!jqg=(10*param_wet_g*(rhogesin**1.6675)*1.6675*(thisqggesin**0.6675))/(log(10.0)*Ze)
!else
jqg=(10*param_wet_g*(rhogesin**1.6675)*1.6675*(thisqggesin**1.6675))/(Ze)
!endif
endif
end subroutine jqg_dart

!hydrometeor first guess values are in g/kg but note that equations use kg/kg
subroutine hx_gaostensrud2012(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rDBZ)
  use kinds, only: r_kind,r_double,i_kind
implicit none
real(r_kind) :: qrgesin,qsgesin,qggesin,rhogesin,tempgesin,rDBZ
real(r_kind) :: zqr,zqg,zqs

!zqr=(3.63e9)*((rhogesin*qrgesin/1000.0)**1.75)
zqr=(3.63e9)*((rhogesin*qrgesin)**1.75)
!zqg=(4.33e10)*((rhogesin*qggesin/1000.0)**1.75)
zqg=(4.33e10)*((rhogesin*qggesin)**1.75)
if(tempgesin .lt. 273.15) then 
!zqs=(9.8e8)*((rhogesin*qsgesin/1000.0)**1.75)
zqs=(9.8e8)*((rhogesin*qsgesin)**1.75)
else
!zqs=(4.26e11)*((rhogesin*qsgesin/1000.0)**1.75)
zqs=(4.26e11)*((rhogesin*qsgesin)**1.75)
endif
rDBZ=zqr+zqg+zqs
if (rdBZ .gt. 1) then
rdBZ=10*log10(rdBZ)
else
rdBZ=0
endif

!print *, "qrgesin,qggesin,qsgesin,rDBZ,zqr,zqg,zqs:"
!print *, qrgesin,qggesin,qsgesin,rdBZ,zqr,zqg,zqs

!reflectivity threshold for no-precip:
if (rdBZ .lt. 0) rdBZ=0

end subroutine hx_gaostensrud2012
end module setupdbz_lib
