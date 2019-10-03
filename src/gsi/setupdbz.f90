subroutine setupdbz(lunin,mype,bwork,awork,nele,nobs,is,radardbz_diagsave,init_pass)
! modified from setupdbz, now dbz is also a state variable
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupdbz     compute rhs of oi for radar reflectivity (dBZ)
!   prgmmr: carley          org: np22                date: 2011-04-05
!           g.zhao               CAPS/OU                   2015-09-xx (based on GSIv3.3)
!           g.zhao               CAPS/OU                   2016-09-xx (based on GSIv3.5)
!                    referenced to setupt/dw/q and Carley's setupdbz code.
!
! abstract: For radar reflectivity observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! CAPS
!   Forward model for reflectivity is only compatible with Lin Microphysics
!    and takes the following form (similar to operator used in arps-3dvar
!    developed by Drs. Jidong Gao and Chengsi Liu:
!
!
!    dBZ =10*LOG_10( Zer + Zes + Zeh)
!
!    Where:
!         Zer is equivalent radar reflectivity factor from rain (mm^6 m^-3)
!         Zes is equivalent radar reflectivity factor from precipiation snow (mm^6 m^-3)
!         Zeg is equivalent radar reflectivity factor from precipiation hail/graupel (mm^6 m^-3)
!         dBZ is simulated radar reflectivity in units of dBZ
!
!    Plugging in the constants yields the following form:
!
!    Zer  = Cr * (rho*qr)^1.75
!    Zes  = Cs * (rho*qs)^1.75
!    Zeg  = Cg * (rho*qg)^(1.75*0.95)
!
!    where:
!           Cr     = 3.6308 * 10^9 (rain drop)
!           Csneg  = 9.5889e+08   (frozen snow)
!           Cspos  = 4.2607e+11   (melting snow)
!           Cg     = 6.1264e+10   (hail/graupel)
!
!    Which yields the forward model:
!
!    dBZ=10*log10(Zer+Zes+Zeg)
!
! program history log:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of twork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  -modified variational qc and diagnostic output
!   2005-10-27  su - correct error in longitude index for diagnostic output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2005-12-20  parrish - add boundary layer forward model option
!   2005-12-20  parrish - correct dimension error in declaration of prsltmp
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - optimize and fix bugs due to virtual temperature
!   2006-04-11  park    - reset land mask for surface data based on observation type
!   2006-04-27  park    - remove sensitivity test for surface TLM routine
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc for surface model
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2006-09-28  treadon - add 10m wind factor to sfc_wtq_fwd call
!   2006-10-28       su - turn off rawinsonde Vqc at south hemisphere
!   2007-03-09      su - modify the observation perturbation 
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the observation gross check error 
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-21  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-07  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-05-19  carley  - Cleaned up fields loaded into dbzptr.
!                         Removed linearization from inner loop routines
!                         and placed it here (see jqr and jqli).
!   2011-08-11  carley  - Turn on gross error checks.
!   2011-09-19  carley  - Include temporary fix from setuprw to prevent out of
!                         bounds array references associated with dpres<zero
!   2012-02-12  carley  - Update to include use of metguess bundle with qr and qli
!   2016-02-15  Johnson, Y. Wang, X. Wang - Develop the reflectivity operator for WRF ARW 
!                                           (Johnson et al. 2015 MWR; Wang and Wang 2017 MWR).
!                                           Two options were developed,
!                                           1) Explicitly apply the operator H(qr, qs, qg) to hydrometeors
!                                           2) Directly use the reflectivity from the wrfout
!                                           POC: xuguang.wang@ou.edu
!   2017-05-12 Y. Wang and X. Wang - Following Guo replacing ob_type with polymorphic obsNode through type casting,
!                                           POC: xuguang.wang@ou.edu
!   2015-09-xx  G.Zhao  - based on setupt, setupdw and setupq, to build up setupdbz        
!                         Inquire about the cloud guess fields  (merged inti vars_init_)       
!   2015-10-xx  G.Zhao  - obs operator is based on log(qr/s/g)
!   2016-11-07  G.Zhao  - using ges_tv, avoiding computing tv from tsen, and avoiding using ges_q             
!   2016-11-17  G.Zhao  - using ges_tsen, and ges_q, not using ges_tv
!                       - skipping geo-potential height to geometric height
!                       (since latitude data(ilate) is not correct from
!                       read_radarref_mosaic2.)
!                       - debugging diag_conv
!                       - put grid index into diag_conv, not lat/lon.
!                       - comment off Ze=Ze+1.0 if Ze < 1.0
!                       - convert specific humidity Q1D to mixign ratio Q1D
!                       - re-set qr/qs/qg to zero (if <=1.0e-8), but set
!                       qrexp/qsexp/qgexp to 1.0e-8 (if <=1.0e-8) to guaruantee
!                       non-zero Ze and Jacobian for TLM/ADM.
!   2017-02-20  G.ZHao  - using log(Qr/s/g) interpolation, not Qr/s/g interpolation
!   2019-02-19  ctong   - modified to comply with new type structure used for GSIv3.7      
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiags, only: dbzhead
  use obsmod, only: rmiss_single,i_dbz_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,&
                    ens_hx_dbz_cut,static_gsi_nopcp_dbz
  use obsmod, only: oberror_tune ! CAPS
  use m_obsNode, only: obsNode
  use m_dbzNode, only: dbzNode
  use m_obsLList, only: obsLList_appendNode
                     
  use hybrid_ensemble_parameters,only: l_hyb_ens
  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: obs_diag ,doradaroneob,oneobddiff,oneobvalue
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close
  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot 
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,&
       ges_lnprsl,ges_rho,ges_tsen
  use gridmod, only: nsig,get_ijk
  use gsi_metguess_mod, only: gsi_metguess_bundle,gsi_metguess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad,&
       r60,tiny_r_kind,cg_term,huge_single
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use obsmod, only   : if_model_dbz, inflate_obserr
  use setupdbz_lib, only:hx_dart,jqr_dart,jqs_dart,jqg_dart 
  use gridmod, only: wrf_mass_regional,nems_nmmb_regional 
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: nsdim
 ! --- CAPS ---
  use oneobmod, only: oneob_type
  use gridmod, only: lat2, lon2
  use constants, only: r10,r100,r1000
  use constants, only: rd
  use qcmod, only: ptopq,pbotq
  use converr, only: ptabl
  use caps_radaruse_mod, only: l_use_log_qx, l_gpht2gmht, lvldbg, l_set_oerr_ratio_dbz                  
  use caps_radaruse_mod, only: i_melt_snow, i_melt_graupel
  use caps_radaruse_mod, only: Cr,     Pr,                     &
                               Cs_dry, Ps_dry, Cs_wet, Ps_wet, &
                               Cg_dry, Pg_dry, Cg_wet, Pg_wet
! modules added by Tim Supinie for dbz operator based on CAPS multi-moments
! microphysics schemes
  use radaremul_cst
  use dualpara, only: t_obs_dual, t_para_dsd, init_refl, init_para_dsd, calcMDR, &
                      qgh_opt, calcConstants
  use caps_radaruse_mod, only: l_use_dbz_caps
! --- CAPS ---
 
  implicit none
! Declare passed variables
  logical                                          ,intent(in   ) :: radardbz_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
  logical                                          ,intent(in   ) :: init_pass ! state of "setup" parameters

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind
! --- CAPS ---
  real(r_kind),parameter:: D608=0.608_r_kind
! real(r_kind),parameter:: RD=287.04_r_kind

!--------------------------------------------------------------------------!
! Note: the following parameters are computed in subroutine coef4dbzfwrd
!-----------PARAMETERS FOR Lin single moment scheme---------!
! real(r_kind), parameter   :: Cr=3.6308e9_r_kind          ! Rain constant coef.
! real(r_kind), parameter   :: Csneg=9.5889e8_r_kind       ! Precip. snow
! constant coef.  (below 273.15K)
! real(r_kind), parameter   :: Cspos=4.2607e11_r_kind      ! Precip. snow
! constant coef.  (above 273.15K)
! real(r_kind), parameter   :: Cg=6.1264e10_r_kind         ! Precip. hail
! constant coef.
!------------------------------------------------!
! real(r_kind)              :: Cs, Cg, Ps, Pg
  real(r_kind)              :: Cs_tmp, Cg_tmp              ! temperary coefficients for check-up               
!--------------------------------------------------------------------------!

! used for multi-moments schemes by Tim Supinie
  integer, external :: get_qgh_opt

  real(r_kind) denom
  real(r_kind) jqr_num,jqs_num,jqg_num
  real(r_kind) wgt_dry, wgt_wet
  real(r_kind) jqg_num_dry, jqg_num_wet
  real(r_kind) qrges,qsges,qgges,rhoges
  real(r_kind) Ze,dbznoise_runits
  real(r_kind) rdBZr,rdBZs,rdBZg
  real(r_kind) Ze_orig, Zer, Zes, Zeg
  real(r_kind) Zeg_dry, Zeg_wet
  real(r_kind) qrexp, qsexp, qgexp
! --- CAPS ---

! Declare external calls for code analysis
  external:: tintrp2a1, tintrp2a11
  external:: tintrp3
  external:: grdcrd
  external:: stop2
! CAPS uses grdcrd1 tintrp31, not tintrp3, grdcrd. However, output is the same. so skip this.
! Declare local variables
  real(r_kind) rlow,rhgh,rsig
!  real(r_kind) dz,denom,jqr_num,jqli_num,jqr,jqli !modified
  real(r_kind) dz,jqr,jqs,jqg
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges
  real(r_kind),dimension(nsig):: zges,hges
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk 
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat
 
  real(r_kind) ratio_errors
  real(r_kind) dbzgesin,qrgesin,qsgesin,qggesin,rhogesin,tempgesin,qligesin
  real(r_kind) qrgesin1,qsgesin1,qggesin1, qligesin1
  real(r_kind) rdBZ,presw,dbznoise
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) i,nchar,nreal,k,k1,ii
  integer(i_kind) mm1,jj,k2            
  integer(i_kind) jsig,ikxx,nn,ibin,ioff, ioff0
  integer(i_kind) ier,ilat,ilon,ihgt,idbzob,ikx,itime,iuse
  integer(i_kind) ielev,id,itilt,iazm,ilone,ilate,irange
  integer(i_kind) ier2,idbznoise,idmiss2opt
! --- CAPS ----
! these integer variables were declared but not used in CAPS. So I do not include..
! j, nii, isli, it, istatus, ier_b, ijk
! i4, j4, k4, n4, kminmin, kmaxmax
  integer(i_kind) nlat_ll,nlon_ll,nsig_ll,nfld_ll
  integer(i_kind) ipres,iqmax,iqc,icat,itemp
  integer(i_kind) istnelv,iobshgt,izz,iprvd,isprvd,iptrb
  integer(i_kind) idomsfc,iskint,isfcr,iff10
  integer(i_kind) nguess

  logical:: in_curbin, in_anybin
! --- CAPS ----
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(80):: string
  character(128):: diag_file
  logical :: diagexist

  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical :: proceed
  logical,dimension(nobs):: luse,muse
  equivalence(rstation_id,station_id)
  real(r_kind) wrange
  integer(i_kind) numequal,numnotequal,istat
 
  logical:: debugging

  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(dbzNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  character(len=*),parameter:: myname='setupdbz'
  integer(i_kind) irefsmlobs, irejrefsmlobs

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z

  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qli
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_dbz

! --- CAPS ---
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
! real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_nr

! variables added by Tim Supinie for dbz operator based on CAPS multi-moments
! micro-physics schemes
  real(r_kind),dimension(nscalar) :: qscalar
  type(t_obs_dual) :: obs_dual
  type(t_para_dsd) :: var_dsd

  real(r_kind) :: presq
  real(r_kind) :: P1D,T1D,Q1D,RHO
! real(r_kind) :: P1D,T1D,RHO
  real(r_kind) :: qges,tsenges       ! used to calculate tv - virtual temperature
! real(r_kind) :: tvges
  real(r_kind) :: lnprslges          ! use log(p) for vertical interpolation
! real(r_kind) :: prslges            ! use     p  for vertical interpolation
  real(r_kind) :: qr_min, qs_min, qg_min

!------------------------------------------------!
! variable used for invocation of multi-momemnt scheme by Tim Supinie
  logical :: firstcalled
  save firstcalled
  data firstcalled/.true./

!------------------------------------------------!
  integer(i_kind) :: iunitdbg
  character(27)   :: fnamedbg
  logical         :: l_dbgout
  logical         :: l_open_dbgout

  integer(i_kind) :: icnt_nouse

!====================================================================================!
!
  if ( l_use_dbz_caps ) then           !! CAPS
! Check to see if required guess fields are available
! vars. list: ps, z, q
! vars. list: qr, qs, qg
    call check_vars_(proceed)
    if(.not.proceed) then
        write(6,*) myname,': some or all necessary variables are not available for dbz obs operator. Quit!'
        return  ! not all vars available, simply return
    end if

! If require guess vars available, extract from bundle ...
    call init_vars_

    qscalar=zero

    write(6,*)myname,'(pe=',mype,') mphyopt for obs forwrd operator:',mphyopt
! Convert log(qr/qs/qg) to qr/qs/qg before the spatial interpolation
    nlat_ll=size(ges_qr,1)
    nlon_ll=size(ges_qr,2)
    nsig_ll=size(ges_qr,3)
    nfld_ll=size(ges_qr,4)
    if (lvldbg >10) then
        write(6,'(1x,A10,A4,I4,A36,4I8)') &
            myname,'(pe=',mype,')  local nlat2 nlon2 nsig nflds =',nlat_ll,nlon_ll,nsig_ll,nfld_ll
        write(6,'(1x,A10,A4,I4,A36,4I8)') &
            myname,'(pe=',mype,')         lat2  lon2 nsig nflds =',lat2,lon2,nsig,nfldsig
    end if
! convert log(Qr/s/g) back to Qr/s/g for Qrsg interpolation ( G.Zhao 2016/11)
    if(lvldbg>1) &
        write(6,*)myname,'(pe=',mype,') No converting logQx to Qx before interpolation in obs forwrd operator.'
  
    iunitdbg=mype+3000
    write(fnamedbg,'(A13,I6.6,A5,I3.3)') 'setupdbz_dbg_',mype,'.txt.',jiter
    l_dbgout = .FALSE.
    l_open_dbgout = .FALSE.

  end if

! --- CAPS ---

  n_alloc(:)=0
  m_alloc(:)=0
 
!******************************************************************************* 
! Flag is appiled since data arrays are different from CAPS and others.
!
  if ( l_use_dbz_caps ) then           !! CAPS
! --- CAPS ---
! --- G. Zhao temperarorily uses prepbufr conventional dataset to store dBZ obs
!             just for single dBZ obs test (so the data array is same as q and
!             qr --c.f. subroutine setupq and setupqr

    ! Read and reformat observations in work arrays.
    read(lunin)data,luse,ioid

    icnt_nouse = 0

    write(6,*)myname,'(pe=',mype,') nele nobs =',nele,nobs,     &
        ' luse_obsdiag=',luse_obsdiag

    ! --- G. Zhao's obs data array
    ier=1       ! index of obs error
    ilon=2      ! index of grid relative obs location (x)
    ilat=3      ! index of grid relative obs location (y)
    ipres=4     ! index of pressure
    idbzob=5     ! index of dbz observation
    id=6        ! index of station id
    itime=7     ! index of observation time in data array
    ikxx=8      ! index of ob type
    iqmax=9     ! index of max error
    itemp=10    ! index of dry temperature
    iqc=11      ! index of quality mark
    ier2=12     ! index of original-original obs error ratio
    iuse=13     ! index of use parameter
    idomsfc=14  ! index of dominant surface type
    iskint=15   ! index of surface skin temperature
    iff10=16    ! index of 10 meter wind factor
    isfcr=17    ! index of surface roughness
    ilone=18    ! index of longitude (degrees)
    ilate=19    ! index of latitude (degrees)
    istnelv=20  ! index of station elevation (m)
    iobshgt=21  ! index of observation height (m)
    izz=22      ! index of surface height
    iprvd=23    ! index of observation provider
    isprvd=24   ! index of observation subprovider
    icat =25    ! index of data level category
    iptrb=26    ! index of dbz perturbation

    do i=1,nobs
       muse(i)=nint(data(iuse,i)) <= jiter

       if ( .not. luse(i) ) then
           icnt_nouse = icnt_nouse + 1
           if (lvldbg >= 100) &
               write(6,*)myname,'(pe:',mype,') Data:',data(idbzob,i),'at ilon ilat:', &
                    data(ilon,i),data(ilat,i),' not use -->',luse(i),' ict:',icnt_nouse
       end if
    end do
    if (lvldbg>1) &
        write(6,*)myname,'(pe=',mype,') number of no use obs on this pe =',icnt_nouse

! Skipping the duplicate observation check (often used in prepbufr conv. obs)

    numequal=0
    numnotequal=0

! --- CAPS ---
  else 
  ! Read and reformat observations in work arrays.
    read(lunin)data,luse, ioid
!    index information for data array (see reading routine)
    ier=1        ! index of obs error
    ilon=2       ! index of grid relative obs location (x)
    ilat=3       ! index of grid relative obs location (y)
    ihgt=4       ! index of obs elevation
    idbzob=5     ! index of radar reflectivity observation (dBZ)
    iazm=6       ! index of azimuth angle in data array
    itime=7      ! index of observation time in data array (hour)        ! Analysis relative time!
    ikxx=8       ! index of obs type in data array                       ! from the convinfo file (order in the list)
    itilt=9      ! index of tilt angle in data array
    ielev=10     ! index of radar elevation
    id=11        ! index of station id
    iuse=12      ! index of use parameter
    ilone=13     ! index of longitude (degrees)
    ilate=14     ! index of latitude (degrees)
    irange=15    ! index of range in m of obs from radar
    ier2=16      ! index of original-original obs error
    idbznoise=17 ! index of noise threshold for reflectivity (dBZ)
    idmiss2opt=18 ! index of if it is converted from the missing value
 
    numequal=0
    numnotequal=0
    irefsmlobs=0
    irejrefsmlobs=0 

  end if

! CAPS uses conv_diagsave, but master uses radardbz_diagsave. the latter is used.
! However, parameters were slightly different from each other.
!
! If requested, save select data for output to diagnostic file
  if(radardbz_diagsave)then
     if ( l_use_dbz_caps ) then           !! CAPS
        ii=0
        nchar=1_i_kind
!       ioff0=24_i_kind               ! 21 + 3 (22->Zr; 23->Zs; 24->Zg)
        ioff0=26_i_kind               ! 21 + 5 (22->Zr; 23->Zs; 24->Zg;25->tsenges; 26->RHO;)
        nreal=ioff0
     else
        ii=0
        nchar=1
        ioff0=25
        nreal=27
     end if
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (.not.allocated(cdiagbuf)) allocate(cdiagbuf(nobs))
     if (.not.allocated(rdiagbuf)) allocate(rdiagbuf(nreal,nobs))
  end if
  mm1=mype+1
  scale=one
  rsig=nsig

! CAPS : these two subroutines were already done in the cpase of CAPS
  if ( .not. l_use_dbz_caps ) then
!   Check to see if required guess fields are available
    call check_vars_(proceed)
    if(.not.proceed) return  ! not all vars available, simply return

!   If require guess vars available, extract from bundle ...
    call init_vars_
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
! - Observation times are checked in read routine - comment out for now

  if ( l_use_dbz_caps ) then
     call dtime_setup()   ! CAPS code  utilize this.
  end if

  do i=1,nobs
     debugging=.false.
     if(doradaroneob) debugging=.true.
     dtime=data(itime,i)
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     if ( l_use_dbz_caps ) then  ! CAPS uses ipres not ihgt
!        dpres=data(ipres,i)                     ! from prepbufr conv. obs setupq            
        dpres=data(ipres,i)                      ! from rdararef_mosaic2: this height abv MSL
     else
        dpres=data(ihgt,i)
        dbznoise=data(idbznoise,i)
        wrange=data(irange,i)
     end if
     ikx = nint(data(ikxx,i))
     error=data(ier2,i)
     slat=data(ilate,i)*deg2rad               ! needed when converting geophgt to 
                                              ! geometric hgh (hges --> zges below)

     if(debugging) then
       print * , "============="
       print *, dlat,dlon,dpres
       print *, data(ilate,i),data(ilone,i)
     endif


!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif

     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin
!    Link obs to diagnostics structure
     if(luse_obsdiag)then
       if (.not.lobsdiag_allocated) then
          if (.not.associated(obsdiags(i_dbz_ob_type,ibin)%head)) then
             obsdiags(i_dbz_ob_type,ibin)%n_alloc = 0
             allocate(obsdiags(i_dbz_ob_type,ibin)%head,stat=istat)
             
             if (istat/=0) then
                write(6,*)'setupdbz: failure to allocate obsdiags',istat
                call stop2(286)
             end if
             obsdiags(i_dbz_ob_type,ibin)%tail => obsdiags(i_dbz_ob_type,ibin)%head
          else
             allocate(obsdiags(i_dbz_ob_type,ibin)%tail%next,stat=istat)
             if (istat/=0) then
                write(6,*)'setupdbz: failure to allocate obsdiags',istat
                call stop2(286)
             end if
             obsdiags(i_dbz_ob_type,ibin)%tail => obsdiags(i_dbz_ob_type,ibin)%tail%next
          end if
          obsdiags(i_dbz_ob_type,ibin)%n_alloc = obsdiags(i_dbz_ob_type,ibin)%n_alloc +1
          allocate(obsdiags(i_dbz_ob_type,ibin)%tail%muse(miter+1))
          allocate(obsdiags(i_dbz_ob_type,ibin)%tail%nldepart(miter+1))
          allocate(obsdiags(i_dbz_ob_type,ibin)%tail%tldepart(miter))
          allocate(obsdiags(i_dbz_ob_type,ibin)%tail%obssen(miter))
          obsdiags(i_dbz_ob_type,ibin)%tail%indxglb=ioid(i) 
          obsdiags(i_dbz_ob_type,ibin)%tail%nchnperobs=-99999
          obsdiags(i_dbz_ob_type,ibin)%tail%luse=.false.
          obsdiags(i_dbz_ob_type,ibin)%tail%muse(:)=.false.
          obsdiags(i_dbz_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
          obsdiags(i_dbz_ob_type,ibin)%tail%tldepart(:)=zero
          obsdiags(i_dbz_ob_type,ibin)%tail%wgtjo=-huge(zero)
          obsdiags(i_dbz_ob_type,ibin)%tail%obssen(:)=zero
          n_alloc(ibin) = n_alloc(ibin) +1
          my_diag => obsdiags(i_dbz_ob_type,ibin)%tail
          my_diag%idv = is
          my_diag%iob = ioid(i)  
          my_diag%ich = 1
          my_diag%elat= data(ilate,i) 
          my_diag%elon= data(ilone,i)

          write(6,*)myname,'(pe=',mype,' obsdiag%tail%tldepart=',  &
              obsdiags(i_dbz_ob_type,ibin)%tail%tldepart(:)

       else
          if (.not.associated(obsdiags(i_dbz_ob_type,ibin)%tail)) then
             obsdiags(i_dbz_ob_type,ibin)%tail => obsdiags(i_dbz_ob_type,ibin)%head
          else
             obsdiags(i_dbz_ob_type,ibin)%tail => obsdiags(i_dbz_ob_type,ibin)%tail%next
          end if
          if (.not.associated(obsdiags(i_dbz_ob_type,ibin)%tail)) then
            call die(myname,'.not.associated(obsdiags(i_dbz_ob_type,ibin)%tail)')
          end if
          if (obsdiags(i_dbz_ob_type,ibin)%tail%indxglb/=ioid(i)) then
             write(6,*)'setupdbz: index error'
             call stop2(288)
          end if
       endif
     endif

!     (The following part is used in subroutine setupdw)
!     Interpolate terrain height(model elevation) to obs location.
     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
!     1. dpres (MRMS obs height is height above MSL) is adjusted by zsges, so it
!        is changed to height relative to model elevation (terrain).
!        because in GSI, geop_hgtl is the height relative to terrain (ges_z) (subroutine guess_grids)
     dpres=dpres-zsges
     if ( .not. l_use_dbz_caps ) then ! CAPS uses pres, not hgt.
        if(dpres > 10000.0_r_kind) cycle !don't need obs above 10 km
     end if 
     if (dpres<zero) then
       cycle  !  temporary fix to prevent out of bounds array reference in zges,prsltmp
     endif

!     Interpolate log(ps) & log(pres) at mid-layers and geo-potential height to obs locations/times
!     Note: geop_hgtl is relative to model terrain, i.e. height - ges_z (ref. to subroutine guess_grids)

     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
    
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
   
! 2. Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23

     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
          ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
          two*flattening*sin2)
     termrg = (termg/grav)*termr


     if ( l_use_dbz_caps ) then           !! CAPS
        if (l_gpht2gmht) then
           do k=1,nsig
              zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
           end do
        else
           do k=1,nsig
              zges(k) = hges(k)
           end do
        end if
     else
        do k=1,nsig
           zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
        end do
     end if

!    Given observation height (1) convert height to grid relative units, (2) compute
!    compute observation pressure (for diagnostic purposes only), and
!    (3) compute location of midpoint of first model layer above surface
!    in grid relative units
!    Convert observation height (in dpres) from meters to grid relative
!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd(dpres,1,zges,nsig,1)  ! CAPS uses grdcrd1
  
!    Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
!    This is improved one according to the Comments of CAPS code.
!    wm - updated so {k1,k2} are at min {1,2} and at max {nsig-1,nsig}
     k1=min(max(1,k),nsig-1)
     k2=min(k1+1,nsig)
!    k1=max(1,k)         - old method
!    k2=min(k+1,nsig)    - old method

!    Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))

     presw  = ten*exp(pobl)

     if ( l_use_dbz_caps ) then           !! CAPS declares presq 
        presq  = presw         
     else
        if( (k1 == k2) .and. (k1 == 1) ) presw=ten*exp(prsltmp(k1)) 
     end if

!    solution to Nan in some members only for EnKF which causes problem?
!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     sfcchk=log(psges)
     call grdcrd(sfcchk,1,prsltmp,nsig,-1)  ! CAPS uses grdcrd1
!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)
!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)
!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if

! CAPS uses l_set_oerr_ratio_dbz. Thus, l_use_dbz_caps flag is used.
! However, ratio_errors were calculated twice.. Need to check.
     if ( l_use_dbz_caps ) then 
!    Adjust observation error.   
!    Observation error currently assumed from user-defined namelist (oe_dbz) 
!    and is *not* adjusted
        if(l_set_oerr_ratio_dbz) then
           ratio_errors = error/(abs(data(ier,i) + 1.0e6_r_kind*rhgh +  &
                 r8*rlow))
        else
           ratio_errors = one
        end if
!
!--- G. Zhao be careful with dpres, especially when it is used in vertical
!    interpolation.  For radar obs, the better way may be use height in vertical
!    interpolation. Anyway, "dpres" used in tintrp3, is not in pressure unit, it
!    is in grid unit.

!    Not adjusting obs error based upon ob vertical location relative to grid box
        if(l_set_oerr_ratio_dbz) then
           ratio_errors = error/(abs(data(ier,i)))
        else
           ratio_errors = one
        end if
     else
     !Not adjusting obs error based upon ob vertical location relative to grid box
        ratio_errors = error/(abs(data(ier,i)))   
     end if
   
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero

     if ( l_use_dbz_caps ) then 
! --- CAPS ---
!----------------------------------------------------------------------------!
!                                                                            !
! Implementation of forward operator for radar dBZ --------------------------!
!                                                                            !
!----------------------------------------------------------------------------!
!    Interpolate guess q, ts, lnprsl, rho, and hydrometeors  to observation
!    location and time.

        call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_tsen,tsenges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
!       call tintrp31(ges_tv,tvges,dlat,dlon,dpres,dtime, &
!          hrdifsig,mype,nfldsig)
!       call tintrp31(ges_prsl,prslges,dlat,dlon,dpres,dtime, &
!          hrdifsig,mype,nfldsig)
        call tintrp31(ges_lnprsl,lnprslges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        P1D=exp(lnprslges)*r1000         ! unit: kPa --> Pa
        Q1D=qges                         ! it is specific humidity
        Q1D=Q1D/(one-Q1D)                ! convert specific humidy to mixing ratio
        T1D=tsenges
        RHO=P1D/(rd*T1D*(one+D608*Q1D))  ! air density in kg m^-3      
!       T1D=tvges                        ! using virtual temperature
!       RHO=P1D/(rd*T1D)                 ! air density in kg m^-3      

        qrges=zero
        call tintrp31(ges_qr,qrges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        qsges=zero
        call tintrp31(ges_qs,qsges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        qgges=zero
        call tintrp31(ges_qg,qgges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        if (mphyopt == 108) then
           call tintrp31(ges_nr,qscalar(P_NR),dlat,dlon,dpres,dtime, &
              hrdifsig,mype,nfldsig)
           qscalar(P_NR)=qscalar(P_NR)*RHO  ! convert qnr to unit demanded by operators
        end if

!-------- debugging pressure
        if (lvldbg > 1) then
           if (NINT(dlat) .eq. 105 .and. NINT(dlon) .eq. 175) then
              write(6,*)'setupdbz (debugging@sclm): dlon dlat dpres presq P1D T1D Q1D qges RHO hgt(amsl) zsges:'
              write(6,*)'setupdbz (debugging):',dlon,dlat,dpres,presq,P1D,T1D,Q1D,qges,RHO,zob+zsges,zsges
              write(6,*)'setupdbz (debugging): max qr, qs, qg, qnr:',maxval(ges_qr),maxval(ges_qs),maxval(ges_qg),maxval(ges_nr)
              write(6,*)'setupdbz (debugging):','hrdifsig=',hrdifsig,' nfldsig=',nfldsig
              write(6,*)'setupdbz (debugging):','latitude=',data(ilate,i),' logitude=',data(ilone,i)
            end if
        end if

!    Convert guess log(qr, qs, qg) on obs point to qr/qs/qg 
!    Note: using log-transformed Qrsg for spatial interpolation
        if ( l_use_log_qx ) then
!        Note: the zero qx or very tiny value of qx is re-set to be non-zero
!        "bigger" tiny value in read_wrf_mass_guess.F90 for log-transformed qx
            qrexp = exp(qrges)
            qsexp = exp(qsges)
            qgexp = exp(qgges)
        else
!        Note: the zero qx or very tiny value of qx needs to be re-set to
!        non-zero
!        "bigger" tiny value here for log10(Zer + Zes + Zeg)
            qr_min = 1.0E-8_r_kind
            qs_min = 1.0E-8_r_kind
            qg_min = 1.0E-8_r_kind
!           qrexp = qrges
            qrexp = max(qrges, qr_min)
!           qsexp = qsges
            qsexp = max(qsges, qs_min)
!           qgexp = qgges
            qgexp = max(qgges, qg_min)

        end if
!    array qscalar is used in dbz forward operator in CAPS radaremul package    
        if ( P_QR > 0 ) qscalar(P_QR) = qrexp
        if ( P_QS > 0 ) qscalar(P_QS) = qsexp
        if ( P_QG > 0 ) qscalar(P_QG) = qgexp
        if ( P_QH > 0 ) qscalar(P_QH) = qgexp             ! no hail for WRF-ARW run

        Zer = zero ;         Zes = zero ;          Zeg = zero ;
        jqr = zero ;         jqs = zero ;          jqg = zero ;
        jqr_num = zero ;     jqs_num = zero ;      jqg_num = zero ;
        wgt_dry = zero ;     wgt_wet = zero ;
        Zeg_dry = zero ;     Zeg_wet = zero ;
        jqg_num_dry = zero ; jqg_num_wet = zero ;
        denom = zero;

!    Compute simulated *equivalent* radar reflectivity
!    also Jacobian used for TLM and ADM

        select case (mphyopt)
        case (2,3,4,5,6,7)
!          mphyopt = 2/3/4/5/6/7 : single moment MicroPhyics scheme
!          rain
!          Zer = Cr  * (RHO * qrexp)**(1.75_r_kind)
           Zer = Cr  * (RHO * qrexp)**(Pr)

!          snow
!          Zes  = Cs * (RHO * qsexp)**(1.75_r_kind)
           if ( i_melt_snow < 0 ) then
!             no melting: dry snow at any temperature
              Zes = Cs_dry * (RHO * qsexp)**(Ps_dry)
              Cs_tmp  = Cs_dry
           else if ( i_melt_snow  .eq. 100 ) then
!             melting: wet snow at any temperature
              Zes = Cs_wet * (RHO * qsexp)**(Ps_wet)
              Cs_tmp  = Cs_wet
           else
!             melting: depending on temperature
              if (T1D < 273.15_r_kind) then
                 Zes = Cs_dry * (RHO * qsexp)**(Ps_dry)
                 Cs_tmp  = Cs_dry
              else
                 Zes = Cs_wet * (RHO * qsexp)**(Ps_wet)
                 Cs_tmp  = Cs_wet
              end if
           end if

!          graupel/hail
!          Zeg  = Cg * (RHO * qgexp)**(1.75_r_kind*0.95_r_kind)
           if ( i_melt_graupel < 0 ) then
!             no melting: dry grauple/hail at any temperature
              Zeg = Cg_dry * (RHO * qgexp)**(Pg_dry)
              Cg_tmp  = Cg_dry
           else if ( i_melt_graupel  .eq. 100 ) then
!             melting: wet graupel at any temperature
              Zeg = Cg_wet * (RHO * qgexp)**(Pg_wet)
              Cg_tmp  = Cg_wet
           else
!             melting: depending on the temperature
              if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                 Zeg = Cg_dry * (RHO * qgexp)**(Pg_dry)
                 Cg_tmp  = Cg_dry
              else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                 Zeg = Cg_wet * (RHO * qgexp)**(Pg_wet)
                 Cg_tmp  = Cg_wet
              else
                 wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                 wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                 Zeg_dry = Cg_dry * (RHO * qgexp)**(Pg_dry)
                 Zeg_wet = Cg_wet * (RHO * qgexp)**(Pg_wet)
                 Zeg     = wgt_dry*Zeg_dry + wgt_wet*Zeg_wet
                 Cg_tmp  = wgt_dry*Cg_dry  + wgt_wet*Cg_wet
              end if
           end if
!
           Ze=Zer+Zes+Zeg

           if(lvldbg>1)then
              if (NINT(dlat) .eq. 105 .and. NINT(dlon) .eq. 175) then
                 write(6,*)'setupdbz (debugging@sclm): qrexp Zer Cr:',qrexp,Zer, Cr
                 write(6,*)'setupdbz (debugging): qsexp Zes T1D Cs:',qsexp,Zes, T1D, Cs_tmp
                 write(6,*)'setupdbz (debugging): qgexp Zeg T1D Cg:',qgexp,Zeg, T1D, Cg_tmp
                 write(6,*)'setupdbz (debugging): Ze:',Ze
              end if
           end if


!          if(Ze <1.0_r_kind) then
!             Ze=Ze + 1.0_r_kind
!          end if
!          Convert to simulated radar reflectivity in units of dBZ
           Ze_orig = Ze
           rdBZ = ten * log10(Ze)
           rdBZr = ten * log10(Zer)
           rdBZs = ten * log10(Zes)
           rdBZg = ten * log10(Zeg)

           if(lvldbg>1)then
              if (NINT(dlat) .eq. 105 .and. NINT(dlon) .eq. 175) then
                 write(6,*)'setupdbz(dbg@sclm): mype  Ze  rdBZ (+r/s/g) :',mype, Ze, rdBZ, rdBZr, rdBZs, rdBZg
              end if
           end if

!          find dqr/ddBZ, dqs/ddBZ, dqg/ddBZ (used in inner loop routine)
!          Jacobian used for TLM and ADM
!          denom=(log(ten))*Ze
           if ( l_use_log_qx ) then
!             rain
!             jqr_num =
!             ten*Cr*((RHO)**1.75_r_kind)*1.75_r_kind*((qrexp)**(1.75_r_kind))             
!             jqr_num = ten*Cr*1.75_r_kind*((RHO*qrexp)**(1.75_r_kind))
!             jqr_num = ten*1.75_r_kind*Zer
              jqr_num = ten*Pr*Zer

!             snow
!             jqs_num =
!             ten*Cs*((RHO)**1.75_r_kind)*1.75_r_kind*((qsexp)**(1.75_r_kind))
!             jqs_num = ten*Cs*1.75_r_kind*((RHO*qsexp)**(1.75_r_kind))
!             jqs_num = ten*1.75_r_kind*Zes
              if ( i_melt_snow < 0 ) then
!                no melting: dry snow at any temperature
                 jqs_num = ten*Ps_dry*Zes
              else if ( i_melt_snow  .eq. 100 ) then
!                melting: wet snow at any temperature
                 jqs_num = ten*Ps_wet*Zes
              else
!                melting: depending on temperature
                 if (T1D < 273.15_r_kind) then
                     jqs_num = ten*Ps_dry*Zes
                 else
                     jqs_num = ten*Ps_wet*Zes
                 end if
              end if

!             graupel/hail
!             jqg_num =
!             ten*Cg*((RHO)**(1.75_r_kind*0.95_r_kind))*(1.75_r_kind*0.95_r_kind)*((qgexp**(1.75_r_kind*0.95_r_kind)))
!             jqg_num = ten*Cg*(1.75_r_kind*0.95_r_kind)*((RHO*qgexp)**(1.75_r_kind*0.95_r_kind))
!             jqg_num = ten*(1.75_r_kind*0.95_r_kind)*Zeg
              if ( i_melt_graupel < 0 ) then
!                no melting: dry grauple/hail at any temperature
                 jqg_num = ten*Pg_dry*Zeg
              else if ( i_melt_graupel  .eq. 100 ) then
!                melting: wet graupel at any temperature
                 jqg_num = ten*Pg_wet*Zeg
              else
!                melting: depending on the temperature
                 if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                     jqg_num = ten*Pg_dry*Zeg
                 else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                     jqg_num = ten*Pg_wet*Zeg
                 else
                     wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                     wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                     jqg_num_dry = ten*Pg_dry*Zeg
                     jqg_num_wet = ten*Pg_wet*Zeg
                     jqg_num = wgt_dry*jqg_num_dry + wgt_wet*jqg_num_wet
                 end if
              end if

           else
!             rain
!             Zer = Cr  * (RHO * qrexp)**(Pr)
!             jqr_num = ten*1.75_r_kind*Zer/qrexp
!             jqr_num = ten*Cr*((RHO)**1.75_r_kind)*1.75_r_kind*((qrexp)**(1.75_r_kind - one))             
              jqr_num = ten*Cr*((RHO)**Pr)*Pr*((qrexp)**(Pr - one))

!             snow
!             Zes  = Cs * (RHO * qsexp)**(1.75_r_kind)
!             jqs_num = ten*1.75_r_kind*Zes/qsexp
!             jqs_num = ten*Cs*((RHO)**1.75_r_kind)*1.75_r_kind*((qsexp)**(1.75_r_kind - one))
              if ( i_melt_snow < 0 ) then
!                no melting: dry snow at any temperature
                 jqs_num = ten*Cs_dry*((RHO)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry - one))
              else if ( i_melt_snow  .eq. 100 ) then
!                melting: wet snow at any temperature
                 jqs_num = ten*Cs_wet*((RHO)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet - one))
              else
!                melting: depending on temperature
                 if (T1D < 273.15_r_kind) then
                    jqs_num = ten*Cs_dry*((RHO)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry - one))
                 else
                    jqs_num = ten*Cs_wet*((RHO)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet - one))
                 end if
              end if

!             graupel/hail
!             jqg_num = ten*Cg*((RHO)**(1.75_r_kind*0.95_r_kind))*(1.75_r_kind*0.95_r_kind)*((qgexp**(1.75_r_kind*0.95_r_kind - one)))
!             jqg_num = ten*(1.75_r_kind*0.95_r_kind)*Zeg/qgexp
              if ( i_melt_graupel < 0 ) then
!                no melting: dry grauple/hail at any temperature
                 jqg_num = ten*Cg_dry*((RHO)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
              else if ( i_melt_graupel  .eq. 100 ) then
!                melting: wet graupel at any temperature
                 jqg_num = ten*Cg_wet*((RHO)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
              else
!                melting: depending on the temperature
                 if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                    jqg_num = ten*Cg_dry*((RHO)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
                 else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                    jqg_num = ten*Cg_wet*((RHO)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
                 else
                    wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                    wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                    jqg_num_dry = ten*Cg_dry*((RHO)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
                    jqg_num_wet = ten*Cg_wet*((RHO)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
                    jqg_num = wgt_dry*jqg_num_dry + wgt_wet*jqg_num_wet
                 end if
              end if

           end if

           denom=(log(ten))*Ze

           jqr  = jqr_num/denom
           jqs  = jqs_num/denom
           jqg  = jqg_num/denom

           if (NINT(dlat) .eq. 100 .and. NINT(dlon) .eq. 100) then
              if (lvldbg>1) then
                 write(6,*)'setupdbz: mype  jqr T1D Cr:',mype, jqr, T1D, Cr
                 write(6,*)'setupdbz: mype  jqs T1D Cs:',mype, jqs, T1D, Cs_tmp
                 write(6,*)'setupdbz: mype  jqg T1D Cg:',mype, jqg, T1D, Cg_tmp
              end if
              if ( .not. l_open_dbgout) then
                 l_open_dbgout = .TRUE.
                 open(iunitdbg,file=trim(fnamedbg),form='formatted')
                 write(6,*) 'setupdbz (debugging output filename):',fnamedbg,' mype=',mype
                 write(iunitdbg,'(1x,(A400))')'dlon,dlat,dpres,presq,P1D,T1D,Q1D,qges,RHO,zob+zsges,zsges,qrexp,Cr,rdBZr,qsexp,Cs,rdBZs,qgexp,Cg,rdBZg,rdBZ,jqr,jqs,jqg,ddiff+rdBZ'
                 write(iunitdbg,'(1x,(A400))')'-----------------------------------------------------------------------------------------------------------------------------------'
              end if
              ! l_dbgout = .TRUE.
              write(iunitdbg,'(1x,25(1x,E12.4))')dlon,dlat,dpres,presq,P1D,T1D,Q1D,qges,RHO,zob+zsges,zsges,qrexp,Cr,rdBZr, &
                 qsexp,Cs_tmp,rdBZs,qgexp,Cg_tmp,rdBZg,rdBZ,jqr,jqs,jqg,ddiff+rdBZ
           end if

        case (108)
!          mphyopt = 108 :   Thompson MP scheme with double moment

           qgh_opt = get_qgh_opt(1, 0)

           call set_dsd_para()
           call calcMDR()
           if (firstcalled) then
              CALL calcConstants()
              firstcalled = .false.
           end if

           obs_dual = init_refl()
           var_dsd = init_para_dsd()
           call rdr_obs(real(RHO), real(qscalar), 0., obs_dual, var_dsd, 1, 1)
           rdBZ = real(obs_dual%T_log_ref, kind=r_kind)
           Ze = 10 ** (rdBZ / 10)

        case default
           write(6,*) 'not recognized mphyopt-->',mphyopt
           call stop2(999)
        end select

! --- CAPS ---
     else

!    Interpolate guess dbz to observation location and time.
        if(if_model_dbz) then
        call tintrp31(ges_dbz,dbzgesin,dlat,dlon,dpres,dtime,& !modified
             hrdifsig,mype,nfldsig)
        endif
!    Interpolate guess qr, qli, and rho to observation location and time.
        call tintrp31(ges_qr,qrgesin,dlat,dlon,dpres,dtime,& !modified
             hrdifsig,mype,nfldsig)
        if( wrf_mass_regional )then
          call tintrp31(ges_qs,qsgesin,dlat,dlon,dpres,dtime,& 
               hrdifsig,mype,nfldsig)
          call tintrp31(ges_qg,qggesin,dlat,dlon,dpres,dtime,& 
               hrdifsig,mype,nfldsig)
        else if(nems_nmmb_regional) then
          call tintrp31(ges_qli,qligesin,dlat,dlon,dpres,dtime,&
               hrdifsig,mype,nfldsig)
        endif
        call tintrp31(ges_rho,rhogesin,dlat,dlon,dpres,dtime,&
             hrdifsig,mype,nfldsig)
        call tintrp31(ges_tsen,tempgesin,dlat,dlon,dpres,dtime,&
             hrdifsig,mype,nfldsig)

        if( nems_nmmb_regional ) then
          qrgesin1  = max(qrgesin,1.e-6_r_kind)
          qligesin1 = max(qligesin,1.e-6_r_kind)
        else if( wrf_mass_regional ) then
          qrgesin1  = max(qrgesin,1.e-6_r_kind)
          qsgesin1  = max(qsgesin,1.e-6_r_kind) 
          qggesin1  = max(qggesin,1.e-5_r_kind) 
        end if

        if(if_model_dbz) then
          rDBZ=dbzgesin
        else
          if( wrf_mass_regional )then
             call hx_dart(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rDBZ,debugging)
          else if( nems_nmmb_regional ) then
             write(6,*) "if_model_dbz should be set as .true."
             STOP
          endif
        endif !if_model_dbz

        if(miter == 0.or.l_hyb_ens) then !ie an enkf run
! DCD 1 March 2019:  changed 0.0 to static_gsi_nopcp_dbz
!          if(rDBZ < 0_r_kind) rDBZ=0.0_r_kind ! should be the same as in the read_dbz when nopcp=.true.
           if(rDBZ < static_gsi_nopcp_dbz) rDBZ=static_gsi_nopcp_dbz ! should be the same as in the read_dbz when nopcp=.true.
        endif
        if(miter == 0.and.ens_hx_dbz_cut) then !ie an enkf run
          if(rDBZ > 60_r_kind) rDBZ=60_r_kind
        endif

        jqr = 0.0_r_kind
        jqs = 0.0_r_kind
        jqg = 0.0_r_kind

        if( .not. if_model_dbz )then
          if( wrf_mass_regional ) then
            call jqr_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqr)
            call jqs_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqs)
            call jqg_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqg)
          else if( nems_nmmb_regional ) then
             write(6,*) "if_model_dbz should be set as .true."
             STOP
          endif
        end if
  
     end if
!
   
     if(rdBZ==data(idbzob,i)) then
        numequal=numequal+1
     else
        numnotequal=numnotequal+1
     end if
    
! Compute innovations
     !--------------Calculate departure from observation----------------!
     ddiff = data(idbzob,i) - rdBZ

     if ( l_use_dbz_caps ) then 
        IF(dlon==105 .and. dlat==28 )WRITE(*,*)'CCT: at (105, 28) Zr,Zs,Zg=',Zer,Zes,Zeg
        IF(dlon==105 .and. dlat==28 )WRITE(*,*)'------------------qr,qs,qg=',qrexp,qsexp,qgexp
        IF(dlon==105 .and. dlat==28 )WRITE(*,*)'------------------Cr,Cs,Cg=',Cr,Cs_tmp,Cg_tmp
        IF(dlon==105 .and. dlat==28 )WRITE(*,*)'------------------Pr,Ps,pg=',Pr,Ps_dry,Pg_dry
        IF(dlon==105 .and. dlat==28 )WRITE(*,*)'-----------------------rho=',RHO

        if(lvldbg>1) then
           if (NINT(dlat) .eq. 105 .and. NINT(dlon) .eq. 175) then
              write(6,*)myname,'(dbg@sclm) mype   data(idbzob,i)  rdBZ ddiff:',mype,data(idbzob,i),rdBZ,ddiff
           end if
        end if
     else

        if(miter > 0.and..not.l_hyb_ens) ddiff = max(min(ddiff,20.0_r_kind),-20.0_r_kind)


        if(debugging) print *, "DDIFF1: ",ddiff,data(idbzob,i),rdBZ

     end if

! If requested, setup for single obs test.
     if ( l_use_dbz_caps ) then    ! CAPS
        if (oneobtest) then
!          Note: do not use this way to run single obs test for dbz in the current version. (g.zhao)
           ddiff=maginnov
!          ddiff=maginnov - rdBZ
!          nonlinear obs (like dbz) needs outer-loop more than once.
           if (trim(adjustl(oneob_type))=='dbz') then
              data(idbzob,i) = maginnov
              ddiff = data(idbzob,i) - rdBZ
           end if
!          if (qsges<tiny_r_kind .and. qliges<tiny_r_kind)then
!             ddiff = maginnov - rdBZ
!             write(6,*)'setupdbz: single obs dBZ test with zero qr/qs/qg
!             :diff=',ddiff
!          end if
           error=one/(magoberr)
           ratio_errors=one
           write(6,*)'  Single ',trim(adjustl(oneob_type)),' obs test:  maginnov ddiff obsdata :',maginnov,ddiff,data(idbzob,i)
        end if
   
     else

        if (oneobtest) then
           ddiff = maginnov
           error=one/magoberr
           ratio_errors=one
        end if

        if (doradaroneob) then
           if(oneobvalue > -900_r_kind) then
              data(idbzob,i) = oneobvalue
              ddiff = data(idbzob,i) - rdBZ
           else
              ddiff = oneobddiff
              data(idbzob,i) = rdBZ+ddiff
           endif
        endif !oneob

        if(rdBZ >= 5_r_kind) irefsmlobs=irefsmlobs+1
 
        if(debugging) print *, "DDIFF2: ",ddiff,data(idbzob,i),rdBZ

     end if

!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)     
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     
     residual = abs(ddiff)
     ratio    = residual/obserrlm


     if ( l_use_dbz_caps ) then    ! CAPS
        if (l_set_oerr_ratio_dbz) then
            if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
!
!!                  obserror = residual/cgross(ikx)
!!                  error = one/obserror
!           
!!              else       
                    if (luse(i)) awork(4) = awork(4)+one
                    error = zero
                    ratio_errors = zero
!!              end if
            end if
         else
            ratio_errors = one
         end if

     else
        if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
           if ( inflate_obserr .and. (ratio-cgross(ikx)) <= cgross(ikx) .and. ratio_errors >= tiny_r_kind) then 
           ! Since radar reflectivity can be very different from the model background
           ! good observations may be rejected during this QC step.  However, if these observations
           ! are allowed through, they can yield problems with convergence.  Therefore the error
           ! is inflated here up to twice the observation error in a manner that is
           ! proportional to the residual.  If this IF-TEST for this inflation fails, the
           ! observation is subsequently rejected.
                    
            obserror = residual/cgross(ikx)
            error = one/obserror
           
           else
              if (luse(i)) awork(4) = awork(4)+one 
              error = zero 
              ratio_errors = zero 
       
              if(rdBZ <= 5_r_kind) irejrefsmlobs=irejrefsmlobs+1
           end if
        end if

     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false. 
     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_dbz_ob_type,ibin)%tail%muse(nobskeep)
     
     val     = error*ddiff
             
!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
       
!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+val2*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if
!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        ress  = scale*ddiff
        ressw = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >=ptop(k) .and. presw<=pbot(k))then  
!          if(presq >=ptopq(k) .and. presq<=pbotq(k))then 
! CAPS uses presq, not presw. However, no difference exists between presq and presw. No change was made here
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
             
           end if
        end do
     end if
     if(luse_obsdiag)then
        obsdiags(i_dbz_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_dbz_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_dbz_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
        if ( l_use_dbz_caps ) then    ! CAPS
           obsdiags(i_dbz_ob_type,ibin)%tail%luse=luse(i) ! CAPS only
        end if
     end if

     
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then             
       
        allocate(my_head)

        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(dbzhead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij(1),my_head%wij(1))   

        my_head%raterr2 = ratio_errors**2
        if ( l_use_dbz_caps ) then           !! CAPS
           my_head%ddiff   = ddiff   ! CAPS uses ddiff, not res
        else
           my_head%res     = ddiff
        end if
        my_head%err2    = error**2
        my_head%time    = dtime
        my_head%luse    = luse(i)
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%jqr     = jqr

        if ( wrf_mass_regional ) then
          my_head%jqs     = jqs
          my_head%jqg     = jqg
        end if


        if ( l_use_dbz_caps ) then    ! CAPS
           if(oberror_tune) then
              my_head%dbzpertb=data(iptrb,i)/error/ratio_errors
              my_head%kx=ikx
              if(presq > ptabl(2))then
                 my_head%k1=1
              else if( presq <= ptabl(33)) then
                 my_head%k1=33
              else
                 k_loop: do k=2,32
                    if(presq > ptabl(k+1) .and. presq <= ptabl(k)) then
                       my_head%k1=k
                       exit k_loop
                    endif
                 enddo k_loop
              endif
           endif
        end if

        if(luse_obsdiag)then
          my_head%diags => obsdiags(i_dbz_ob_type,ibin)%tail
!         write(6,*)myname,'(pe=',mype,'): dbzhead%head%diags%tldepart=', &
!             dbztail(ibin)%head%diags%tldepart(:)
!         write(6,*)myname,'(pe=',mype,'): dbzhead%head%diags%tldepart=', &
!             dbzhead(ibin)%head%diags%tldepart(:)

       
          my_diag => my_head%diags
          if(my_head%idv /= my_diag%idv .or. &
             my_head%iob /= my_diag%iob ) then
             call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                   (/is,ioid(i),ibin/))
             call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
             call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
             call die(myname)
          endif
        endif

        my_head => null()
     endif

!    Save select output for diagnostic file
!    CAPS uses conv_diagsave, but leave this for now.
     if(radardbz_diagsave .and. luse(i) )then


        ii=ii+1
        rstation_id     = data(id,i)

        if ( l_use_dbz_caps ) then    ! CAPS
           cdiagbuf(ii)    = station_id         ! station id

           rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
           rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

           rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
           rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
!          rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
           rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
           rdiagbuf(6,ii)  = presq              ! observation pressure (hPa)
!          rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
           rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
!          rdiagbuf(8,ii)  = (dtime*r60)-time_offset  ! obs time (sec relative to analysis time)
           rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (sec relative to analysis time)

!          rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
           rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
           rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
           rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
           if(muse(i)) then
              rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
           else
              rdiagbuf(12,ii) = -one
           endif
        end if

        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        if ( l_use_dbz_caps ) then    ! CAPS
           rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
           rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
           rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
           rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
           rdiagbuf(17,ii) = data(idbzob,i)       ! radar reflectivity observation (dBZ)
           rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
           rdiagbuf(19,ii) = data(idbzob,i)-rdBZ  ! obs-ges w/o bias correction (dBZ) (future slot)
           rdiagbuf(20,ii)  = data(ilat,i)        ! index of grid relative obs location (y)
           rdiagbuf(21,ii)  = data(ilon,i)        ! index of grid relative obs location (x)
           rdiagbuf(22,ii) = rdBZr                ! dBZ from rain water
           rdiagbuf(23,ii) = rdBZs                ! dBZ from snow
           rdiagbuf(24,ii) = rdBZg                ! dBZ from graupel
           rdiagbuf(25,ii) = T1D                  ! temperature (sensible, K)
           rdiagbuf(26,ii) = RHO                  ! air density (kg/m**3)

           ioff=ioff0
           if (lobsdiagsave) then
              do jj=1,miter
                 ioff=ioff+1
                 if (obsdiags(i_dbz_ob_type,ibin)%tail%muse(jj)) then
                    rdiagbuf(ioff,ii) = one
                 else
                    rdiagbuf(ioff,ii) = -one
                 endif
              enddo
              do jj=1,miter+1
                 ioff=ioff+1
                 rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%nldepart(jj)
              enddo
              do jj=1,miter
                 ioff=ioff+1
                 rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%tldepart(jj)
              enddo
              do jj=1,miter
                 ioff=ioff+1
                 rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%obssen(jj)
              enddo

           endif
        else

           if(binary_diag) call contents_binary_diag_
           if(netcdf_diag) call contents_netcdf_diag_

        end if

     end if
  end do

! Release memory of local guess arrays
  call final_vars_

  if ( l_use_dbz_caps ) then    ! CAPS
     if ( l_open_dbgout ) then
        write(6,*) 'There is output into unit=',iunitdbg,' fname=',fnamedbg,' mype=',mype
        close(iunitdbg)
     else
        write(6,*) 'no output into unit=',iunitdbg,' fname=',fnamedbg,' mype=',mype
     end if
  end if

! Write information to diagnostic file ! again, conv_diagsave -> radardbz_diagsave
  if ( l_use_dbz_caps ) then    ! CAPS
     if(radardbz_diagsave  .and. ii>0 )then
        call dtime_show(myname,'diagsave:dbz',i_dbz_ob_type)
        write(6,*) 'writing diag of DBZ'
        write(7)'dbz',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
  else
     if(radardbz_diagsave  .and. ii>0 )then
        write(string,600) jiter
600  format('radardbz_',i2.2)
        diag_file=trim(dirname) // trim(string)
        if(init_pass) then
           open(66,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
        else
           inquire(file=trim(diag_file),exist=diagexist)
           if (diagexist) then
              open(66,file=trim(diag_file),form='unformatted',status='old',position='append')
           else
              open(66,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
           endif
        endif
        if(init_pass .and. mype == 0) then
           write(66) ianldate
           write(6,*)'SETUPDBZ:   write time record to file ',&
                   trim(diag_file), ' ',ianldate
        endif

        call dtime_show(myname,'diagsave:dbz',i_dbz_ob_type)
        write(66)'dbz',nchar,nreal,ii,mype,ioff0
        write(66)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
        close(66)
     end if
     write(6,*)'mype, irefsmlobs,irejrefsmlobs are ',mype,' ',irefsmlobs, ' ',irejrefsmlobs
  end if
! close(52) !simulated obs
! End of routine
  contains

  subroutine check_vars_ (proceed)

  use radaremul_cst, only: mphyopt ! CAPS
  use caps_radaruse_mod, only: l_use_dbz_caps
  use obsmod, only: if_model_dbz
  use gridmod, only: wrf_mass_regional, nems_nmmb_regional

  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q' , ivar, istatus )
  proceed=proceed.and.ivar>0
  if( .not. l_use_dbz_caps) then ! CAPS
     call gsi_metguess_get ('var::tv', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
  if( if_model_dbz ) then
      call gsi_metguess_get ('var::dbz', ivar, istatus )
      proceed=proceed.and.ivar>0
  end if
  if(wrf_mass_regional)then
     call gsi_metguess_get ('var::qs', ivar, istatus )
     proceed=proceed.and.ivar>0
     call gsi_metguess_get ('var::qg', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
    call gsi_metguess_get ('var::qr', ivar, istatus )
    proceed=proceed.and.ivar>0
  if(nems_nmmb_regional)then
     call gsi_metguess_get ('var::qli', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
! --- CAPS ---
  if ( mphyopt == 108 ) then
      call gsi_metguess_get ('var::qnr', ivar, istatus )
      proceed=proceed.and.ivar>0
  end if
! --- CAPS ---
  end subroutine check_vars_

  subroutine init_vars_

  use radaremul_cst, only: mphyopt ! CAPS
  use caps_radaruse_mod, only: l_use_dbz_caps
  use obsmod, only: if_model_dbz
  use gridmod, only: wrf_mass_regional, nems_nmmb_regional

  real(r_kind),dimension(:,:  ),pointer:: rank2
  real(r_kind),dimension(:,:,:),pointer:: rank3
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

     if(if_model_dbz)then
     !    get dbz ....
         varname='dbz'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
           if(allocated(ges_dbz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
           endif
           allocate(ges_dbz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_dbz(:,:,:,1)=rank3
           do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_dbz(:,:,:,ifld)=rank3
           enddo
         else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
         endif
     endif

!    get qr ...
     varname='qr'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qr))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qr(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qr(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

     if(wrf_mass_regional)then
!    get qs ...
     varname='qs'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qs))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qs(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qs(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qs(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

!    get qg ...
     varname='qg'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qg))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qg(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qg(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif


     end if

     if(nems_nmmb_regional)then
!      get qli ...
       varname='qli'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
       if (istatus==0) then
           if(allocated(ges_qli))then
              write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
              call stop2(999)
           endif
           allocate(ges_qli(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_qli(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
              ges_qli(:,:,:,ifld)=rank3
           enddo
       else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
       endif
     end if

! --- CAPS ---
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get qnr ...
     if ( mphyopt == 108 ) then
         varname='qnr'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus) 
         if (istatus==0) then
             if(allocated(ges_nr))then
                 write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                 call stop2(999)
             endif
             allocate(ges_nr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
             ges_nr(:,:,:,1)=rank3
             do ifld=2,nfldsig
                 call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                 ges_nr(:,:,:,ifld)=rank3
             enddo
         else
             write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
             call stop2(999)
         endif
     end if
! --- CAPS ---
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('conv_dbz_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists. Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_

        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = (dtime*r60)-time_offset  ! obs time (sec relative to analysis time)
        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
        rdiagbuf(17,ii) = data(idbzob,i)       ! radar reflectivity observation (dBZ)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
        rdiagbuf(19,ii) = data(idbzob,i)-rdBZ  ! obs-ges w/o bias correction (dBZ) (future slot)
        rdiagbuf(20,ii)=data(iazm,i)*rad2deg   ! azimuth angle
        rdiagbuf(21,ii)=data(itilt,i)*rad2deg  ! tilt angle
        rdiagbuf(22,ii)=data(irange,i) ! the range in km
        rdiagbuf(23,ii)=data(idmiss2opt,i) ! the range in km

        rdiagbuf(23,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)
        rdiagbuf(24,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)

        if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setupdbz.f90 '
            stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_dbz_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_dbz_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '    dbz'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id          )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata("Latitude",                sngl(data(ilate,i)) )
           call nc_diag_metadata("Longitude",               sngl(data(ilone,i)) )
           call nc_diag_metadata("Station_Elevation",       sngl(data(ielev,i)) )
           call nc_diag_metadata("Pressure",                sngl(presw)         )
           call nc_diag_metadata("Height",                  sngl(data(ihgt,i))  )
           call nc_diag_metadata("Time",                    sngl(dtime-time_offset))
           call nc_diag_metadata("Prep_QC_Mark",            sngl(zero)          )
           call nc_diag_metadata("Prep_Use_Flag",           sngl(data(iuse,i))  )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb   !          )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(rwgt)          )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)          )
           endif

           call nc_diag_metadata("Errinv_Input",            sngl(errinv_input)  )
           call nc_diag_metadata("Errinv_Adjust",           sngl(errinv_adjst)  )
           call nc_diag_metadata("Errinv_Final",            sngl(errinv_final)  )

           call nc_diag_metadata("Observation",             sngl(data(idbzob,i)) )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)   )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(data(idbzob,i)-rdBZ) )

           if (lobsdiagsave) then
              do jj=1,miter
                 if (obsdiags(i_dbz_ob_type,ibin)%tail%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", obsdiags(i_dbz_ob_type,ibin)%tail%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", obsdiags(i_dbz_ob_type,ibin)%tail%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen", obsdiags(i_dbz_ob_type,ibin)%tail%obssen   )
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qr)) deallocate(ges_qr)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_qli)) deallocate(ges_qli)
    if(allocated(ges_dbz)) deallocate(ges_dbz)
! --- CAPS ---
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_nr)) deallocate(ges_nr)
! --- CAPS ---
  end subroutine final_vars_

  subroutine init_qcld(t_cld, qxmin_cld, icat_cld, t_dpnd)
      use kinds, only: r_kind,r_single,r_double,i_kind
      implicit none
      real(r_kind),     intent(in   )   :: t_cld
      real(r_kind),     intent(inout)   :: qxmin_cld
      integer,          intent(in   )   :: icat_cld
      logical,          intent(in   )   :: t_dpnd
!
!     local variables
      real  :: tr_ll, qrmin_ll, tr_hl, qrmin_hl
      real  :: ts_ll, qsmin_ll, ts_hl, qsmin_hl
      real  :: tg_ll, qgmin_ll, tg_hl, qgmin_hl
      real  :: qr_min, qs_min, qg_min
!------------------------------------------------------

      qr_min = 5.0E-6_r_kind
      qs_min = 5.0E-6_r_kind
      qg_min = 5.0E-6_r_kind
      tr_ll = 275.65; qrmin_ll = 5.0E-6_r_kind;
      tr_hl = 270.65; qrmin_hl = 1.0E-8_r_kind;
      ts_ll = 275.65; qsmin_ll = 1.0E-8_r_kind;
      ts_hl = 270.65; qsmin_hl = 5.0E-6_r_kind;
      tg_ll = 275.65; qgmin_ll = 1.0E-6_r_kind;
      tg_hl = 270.65; qgmin_hl = 5.0E-6_r_kind;

      select case (icat_cld)
      case (1)
          if ( t_dpnd ) then
              if (t_cld <= tr_hl) then
                  qxmin_cld = qrmin_hl
              else if (t_cld >= tr_ll) then
                  qxmin_cld = qrmin_ll
              else
                  qxmin_cld = (qrmin_hl + qrmin_ll) * 0.5
              end if
          else
              qxmin_cld = qr_min
          end if
      case default
          write(6,*) 'wrong cloud hydrometer category ID',icat_cld
      end select

      return

  end subroutine init_qcld

end subroutine setupdbz
