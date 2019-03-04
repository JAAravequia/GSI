!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupdlw --- Compute rhs of oi for Dopper Lidar wind component obs
! !PROGRAMMER: JJH  CIMMS/NSSL
! !INTERFACE:
!

subroutine setupdlw(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)

! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiags, only: whead
  use obsmod, only: rmiss_single,perturb_obs,oberror_tune,&
       i_w_ob_type,obsdiags,obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset,bmiss
  use m_obsNode, only: obsNode
  use m_wNode, only: wNode
  use m_obsLList, only: obsLList_appendNode
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1,qc_satwnds,njqc,vqc
  use oneobmod, only: oneobtest,oneob_type,magoberr,maginnov 
  use gridmod, only: get_ijk,nsig,twodvar_regional,regional,wrf_nmm_regional,&
      rotate_wind_xy2ll
  use guess_grids, only: nfldsig,hrdifsig,geop_hgtl,sfcmod_gfs
  use guess_grids, only: tropprs,sfcmod_mm5
  use guess_grids, only: ges_lnprsl,comp_fact10,pt_ll,pbl_height
  use constants, only: zero,half,one,tiny_r_kind,two,cg_term, &
           three,rd,grav,four,five,huge_single,r1000,wgtlim,r10,r400
  use constants, only: grav_ratio,flattening,deg2rad, &
       grav_equator,somigliana,semi_major_axis,eccentricity
  use jfunc, only: jiter,last,jiterstart,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use converr_uv, only: ptabl_uv
  use converr, only: ptabl
  use rapidrefresh_cldsurf_mod, only: l_PBL_pseudo_SurfobsUV, pblH_ration,pps_press_incr
  use rapidrefresh_cldsurf_mod, only: l_closeobs, i_gsdqc

  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  implicit none
  
! !INPUT PARAMETERS:

   integer(i_kind)                                  ,intent(in   ) :: lunin ! unit from which to read observations
   integer(i_kind)                                  ,intent(in   ) :: mype  ! mpi task id
   integer(i_kind)                                  ,intent(in   ) :: nele  ! number of data elements per observation
   integer(i_kind)                                  ,intent(in   ) :: nobs  ! number of observations
   integer(i_kind)                                  ,intent(in   ) :: is    ! ndat index
   logical                                          ,intent(in   ) :: conv_diagsave ! logical to save innovation dignostics
   
! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
   real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork ! data counts and gross checks

!
! !DESCRIPTION:  For wind component observations, this routine
!  \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
!  \end{enumerate}
!
! !REVISION HISTORY:
! 2017-01-13 JJH  - Add Doppler Wind Lidar option!

! REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_1=1.0_r_kind
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: r7=7.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r50=50.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r0_1_bmiss=0.1_r_kind*bmiss

  character(len=*),parameter:: myname='setupdlw'

! Declare external calls for code analysis
  external:: intrp2a11,tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  real(r_double) rstation_id
  real(r_kind) qcu,qcv,trop5,tfact,fact
  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,ressw,ress,val,val2,valqc2,dudiff,dvdiff
  real(r_kind) valqc,valu,valv,dx10,rlow,rhgh,drpx,prsfc,var_jb
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2,qcgross
  real(r_kind) presw,factw,dpres,ugesin,vgesin,rwgt,dpressave
  real(r_kind) sfcchk,prsln2,error,dtime,dlon,dlat,r0_001,rsig,thirty,rsigp
  real(r_kind) ratio_errors,goverrd,spdges,spdob,ten,psges,zsges
  real(r_kind) slat,sin2,termg,termr,termrg,pobl,uob,vob
  real(r_kind) uob_reg,vob_reg,uob_e,vob_e,dlon_e,uges_e,vges_e,dudiff_e,dvdiff_e
  real(r_kind) dz,zob,z1,z2,p1,p2,dz21,dlnp21,spdb,dstn
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,skint,sfcr
  real(r_kind) dudiff_opp, dvdiff_opp, vecdiff, vecdiff_opp
  real(r_kind) dudiff_opp_rs, dvdiff_opp_rs, vecdiff_rs, vecdiff_opp_rs
  real(r_kind) oscat_vec,ascat_vec,rapidscat_vec
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges,zges
  real(r_kind) wdirob,wdirgesin,wdirdiffmax
  real(r_kind),dimension(34)::ptabluv
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) i,nchar,nreal,k,j,l,ii,itype,ijb
! Variables needed for new polar winds QC based on Log Normalized Vector Departure (LNVD)
  real(r_kind) LNVD_wspd
  real(r_kind) LNVD_omb
  real(r_kind) LNVD_ratio
  real(r_kind) LNVD_threshold

  integer(i_kind) jsig,mm1,iptrbu,iptrbv,jj,icat
  integer(i_kind) k1,k2,ikxx,nn,isli,ibin,ioff,ioff0
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx,ielev,iqc
  integer(i_kind) ihgt,ier2,iuse,ilate,ilone,istat
  integer(i_kind) izz,iprvd,isprvd
  integer(i_kind) idomsfc,isfcr,iskint,iff10

  real(r_kind) error_1,error_2,rep_err ! for Dopper Lidar, ajust obs err by adding the representation error

  integer(i_kind) num_bad_ikx

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical z_height,sfc_data
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical lowlevelsat
  logical proceed

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(wNode),pointer :: my_head
  type(obs_diag),pointer :: my_diag
  real(r_kind) :: thisPBL_height,ratio_PBL_height,prest,prestsfc,dudiffsfc,dvdiffsfc
  real(r_kind) :: hr_offset

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0
!******************************************************************************
! Read and reformat observations in work arrays.
  spdb=zero

  read(lunin)data,luse,ioid

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of height
  iuob=6      ! index of u observation
  ivob=7      ! index of v observation
  id=8        ! index of station id
  itime=9     ! index of observation time in data array
  ikxx=10     ! index of ndex ob type in convinfo file
  ielev=11    ! index of station elevation
  iqc=12      ! index of quality mark
  ier2=13     ! index of original-original obs error ratio
  iuse=14     ! index of use parameter
  idomsfc=15  ! index of dominant surface type
  iskint=16   ! index of surface skin temperature
  iff10=17    ! index of 10 meter wind factor
  isfcr=18    ! index of surface roughness
  ilone=19    ! index of longitude (degrees)
  ilate=20    ! index of latitude (degrees)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  ijb=25      ! index of non linear qc parameter
  iptrbu=26   ! index of u perturbation
  iptrbv=27   ! index of v perturbation

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  ten = 10.0_r_kind
  r0_001=0.001_r_kind
  rsigp=rsig+one
  goverrd=grav/rd
  var_jb=zero

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=23
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+7*miter+2
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

!  handle multiple-report observations at a station
  hr_offset=min_offset/60.0_r_kind
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k) == data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then

           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
              endif
!              write(*,'(a,2f10.5,2I8,2L10)') 'chech wind obs time==',&
!              data(itime,k)-hr_offset,data(itime,l)-hr_offset,k,l,&
!                           muse(k),muse(l)
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do

  call dtime_setup()
  num_bad_ikx=0
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        rstation_id     = data(id,i)
        error=data(ier2,i)
        ikx=nint(data(ikxx,i))
        var_jb=data(ijb,i)
        if(ikx < 1 .or. ikx > nconvtype) then
           num_bad_ikx=num_bad_ikx+1
           if(num_bad_ikx<=10) write(6,*)' in setupdlw, bad ikx, ikx,i,nconvtype=',ikx,i,nconvtype
           cycle
        end if
        isli = data(idomsfc,i)
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     do jj=1,2
        if (luse_obsdiag) then
           if (.not.lobsdiag_allocated) then
              if (.not.associated(obsdiags(i_w_ob_type,ibin)%head)) then
                 obsdiags(i_w_ob_type,ibin)%n_alloc = 0
                 allocate(obsdiags(i_w_ob_type,ibin)%head,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setupdlw: failure to allocate obsdiags',istat
                    call stop2(304)
                 end if
                 obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%head
              else
                 allocate(obsdiags(i_w_ob_type,ibin)%tail%next,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setupdlw: failure to allocate obsdiags',istat
                    call stop2(305)
                 end if
                 obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%tail%next
              end if
              obsdiags(i_w_ob_type,ibin)%n_alloc = obsdiags(i_w_ob_type,ibin)%n_alloc +1
    
              allocate(obsdiags(i_w_ob_type,ibin)%tail%muse(miter+1))
              allocate(obsdiags(i_w_ob_type,ibin)%tail%nldepart(miter+1))
              allocate(obsdiags(i_w_ob_type,ibin)%tail%tldepart(miter))
              allocate(obsdiags(i_w_ob_type,ibin)%tail%obssen(miter))
              obsdiags(i_w_ob_type,ibin)%tail%indxglb=ioid(i)
              obsdiags(i_w_ob_type,ibin)%tail%nchnperobs=-99999
              obsdiags(i_w_ob_type,ibin)%tail%luse=luse(i)
              obsdiags(i_w_ob_type,ibin)%tail%muse(:)=.false.
              obsdiags(i_w_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
              obsdiags(i_w_ob_type,ibin)%tail%tldepart(:)=zero
              obsdiags(i_w_ob_type,ibin)%tail%wgtjo=-huge(zero)
              obsdiags(i_w_ob_type,ibin)%tail%obssen(:)=zero
    
              n_alloc(ibin) = n_alloc(ibin) +1
              my_diag => obsdiags(i_w_ob_type,ibin)%tail
              my_diag%idv = is
              my_diag%iob = ioid(i)
              my_diag%ich = jj
              my_diag%elat= data(ilate,i)
              my_diag%elon= data(ilone,i)
    
           else
              if (.not.associated(obsdiags(i_w_ob_type,ibin)%tail)) then
                 obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%head
              else
                 obsdiags(i_w_ob_type,ibin)%tail => obsdiags(i_w_ob_type,ibin)%tail%next
              end if
              if (.not.associated(obsdiags(i_w_ob_type,ibin)%tail)) then
                 call die(myname,'.not.associated(obsdiags(i_w_ob_type,ibin)%tail)')
              end if
              if (obsdiags(i_w_ob_type,ibin)%tail%indxglb/=ioid(i)) then
                 write(6,*)'setupdlw: index error'
                 call stop2(306)
              end if
           endif
           if (jj==1) obsptr => obsdiags(i_w_ob_type,ibin)%tail
        endif
     enddo

     if(.not.in_curbin) cycle

!    Load observation error and values into local variables
     !obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i))) ! seems no use, commented out by JJH
     uob = data(iuob,i)
     vob = data(ivob,i)
     spdob=sqrt(uob*uob+vob*vob)
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     itype=ictype(ikx)

!    Type 221=pibal winds contain a mixture of wind observations reported
!    by pressure and others by height.  Those levels only reported by 
!    pressure have a missing value (ie, large) value for the reported 
!    height.  The logic below determines whether to process type 221 
!    wind observations using height or pressure as the vertical coordinate.
!    If height is not bad (less than r0_1_bmiss), we use height in the
!    forward model.  Otherwise, use reported pressure.

     z_height = .false.
     if (itype==262 .and. (data(ihgt,i)<r0_1_bmiss)) z_height = .true.

!    Process observations reported with height differently than those
!    reported with pressure.   

     sfc_data = (itype >=280 .and. itype < 300) .and. (.not.twodvar_regional)
     if (z_height .or. sfc_data) then

        drpx = zero
        dpres = data(ihgt,i)
        dstn = data(ielev,i)
        call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
!       Subtract off combination of surface station elevation and
!       model elevation depending on how close to surface
        fact = zero
        if(dpres-dstn > 10._r_kind)then
           if(dpres-dstn > r1000)then
              fact = one
           else
              fact=(dpres-dstn)/990._r_kind
           end if
        end if
        dpres=dpres-(dstn+fact*(zsges-dstn))

!       Get guess surface elevation and geopotential height profile 
!       at observation location.
        call tintrp2a1(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)


!       Given observation height, (1) adjust 10 meter wind factor if
!       necessary, (2) convert height to grid relative units, (3) compute
!       compute observation pressure (for diagnostic purposes only), and
!       (4) compute location of midpoint of first model layer above surface
!       in grid relative units

!       Adjust 10m wind factor if necessary.  Rarely do we have a
!       profiler/vad obs within 10 meters of the surface.  Almost always,
!       the code below resets the 10m wind factor to 1.0 (i.e., no
!       reduction in wind speed due to surface friction).

!       Convert observation height (in dpres) from meters to grid relative
!       units.  Save the observation height in zob for later use.
        zob = dpres
        call grdcrd1(dpres,zges,nsig,1)

!       Interpolate guess u and v to observation location and time.
 
        call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)

        if (zob > zges(1)) then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if

           if (zob <= ten) then
              if(zob < ten)then
                 term = max(zob,zero)/ten
                 factw = term*factw
              end if
           else
              term = (zges(1)-zob)/(zges(1)-ten)
              factw = one-term+factw*term
           end if
 
           ugesin=factw*ugesin
           vgesin=factw*vgesin

        endif

        if(sfc_data .or. dpres < one) then
           drpx=0.005_r_kind*abs(dstn-zsges)*(one-fact)
        end if
 
!       Compute observation pressure (only used for diagnostics)

!       Set indices of model levels below (k1) and above (k2) observation.
        if (dpres<one) then
           z1=zero;    p1=log(psges)
           z2=zges(1); p2=prsltmp(1)
        elseif (dpres>nsig) then
           z1=zges(nsig-1); p1=prsltmp(nsig-1)
           z2=zges(nsig);   p2=prsltmp(nsig)
           drpx = 1.e6_r_kind
        else
           k=dpres
           k1=min(max(1,k),nsig)
           k2=max(1,min(k+1,nsig))
           z1=zges(k1); p1=prsltmp(k1)
           z2=zges(k2); p2=prsltmp(k2)
        endif
       
        dz21     = z2-z1
        dlnp21   = p2-p1
        dz       = zob-z1
        pobl     = p1 + (dlnp21/dz21)*dz
        presw    = ten*exp(pobl)
        if (itype==262) then ! for Doppler Lidar, add the representation error to the original obs error 
        ! the representation err is calculated according radiosonde (from errtable)
           error_1 = data(ier,i)
           error_2 = data(ier2,i)
           if(presw>=1075.0)  then ! 1100 mb select the closest level 
              rep_err = 1.85
           elseif(presw>=1025.0 .and. presw<1075.0) then ! 1050mb
              rep_err = 2.06
           elseif(presw>=975.0 .and. presw<1025.0) then ! 1000mb
              rep_err = 2.28
           elseif(presw>=925.0 .and. presw<975.0) then ! 950mb
              rep_err = 2.42
           elseif(presw>=875.0 .and. presw<925.0) then ! 900mb
              rep_err = 2.49
           elseif(presw>=825.0 .and. presw<875.0) then ! 850mb
              rep_err = 2.52
           elseif(presw>=775.0 .and. presw<825.0) then ! 800mb
              rep_err = 2.50
           elseif(presw>=725.0 .and. presw<775.0) then ! 750mb
              rep_err = 2.48
           elseif(presw>=675.0 .and. presw<725.0) then ! 700mb
              rep_err = 2.45
           elseif(presw>=625.0 .and. presw<675.0) then ! 650mb
              rep_err = 2.44
           elseif(presw>=575.0 .and. presw<625.0) then ! 600mb
              rep_err = 2.44
           elseif(presw>=525.0 .and. presw<575.0) then ! 550mb
              rep_err = 2.46
           elseif(presw>=475.0 .and. presw<525.0) then ! 500mb
              rep_err = 2.50
           elseif(presw>=425.0 .and. presw<475.0) then ! 450mb
              rep_err = 2.57
           elseif(presw>=375.0 .and. presw<425.0) then ! 400mb
              rep_err = 2.64
           elseif(presw>=325.0 .and. presw<375.0) then ! 350mb
              rep_err = 2.69
           elseif(presw>=275.0 .and. presw<325.0) then ! 300mb
              rep_err = 2.77
           elseif(presw>=225.0 .and. presw<275.0) then ! 250mb
              rep_err = 2.90
            elseif(presw>=175.0 .and. presw<225.0) then ! 200mb
              rep_err = 3.05
           elseif(presw>=125.0 .and. presw<175.0) then ! 150mb
              rep_err = 3.11
           elseif(presw>=87.5 .and. presw<125.0) then ! 100mb
              rep_err = 3.02
           elseif(presw>=62.5 .and. presw<87.5) then ! 75mb
              rep_err = 2.87
           elseif(presw>=45.0 .and. presw<62.5) then ! 50mb
              rep_err = 2.71
           elseif(presw>=35.0 .and. presw<45.0) then ! 40mb
              rep_err = 2.55
           elseif(presw>=25.0 .and. presw<35.0) then ! 30mb
              rep_err = 2.44
           elseif(presw>=15.0 .and. presw<25.0) then ! 20mb
              rep_err = 2.37
           elseif(presw>=7.5 .and. presw<15.0) then ! 10mb
              rep_err = 2.31
           elseif(presw>=4.5 .and. presw<7.5) then ! 5mb
              rep_err = 2.24
           elseif(presw>=3.5 .and. presw<4.5) then ! 4mb
              rep_err = 2.18
           elseif(presw>=2.5 .and. presw<3.5) then ! 3mb
              rep_err = 2.13
           elseif(presw>=1.5 .and. presw<2.5) then ! 2mb
              rep_err = 2.11
           elseif(presw<1.5) then ! 0-1mb
              rep_err = 2.10
           endif
           error_1 = error_1 + rep_err
           error_2 = error_2 + rep_err
           error = error_2 ! the value for error should be ajusted also (line 343)
        endif

!       Determine location in terms of grid units for midpoint of
!       first layer above surface
        sfcchk=zero
!       call grdcrd1(sfcchk,zges,nsig,1)


!    Process observations with reported pressure
     else
        dpres = data(ipres,i)
        presw = ten*exp(dpres)
        dpres = dpres-log(psges)
        drpx=zero

        prsfc=psges
        prsln2=log(exp(prsltmp(1))/prsfc)
        dpressave=dpres

!       Put obs pressure in correct units to get grid coord. number
        dpres=log(exp(dpres)*prsfc)
        call grdcrd1(dpres,prsltmp(1),nsig,-1)
 
!       Interpolate guess u and v to observation location and time.
 
        call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        if(dpressave <= prsln2)then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if
 
           call tintrp2a1(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
              nsig,mype,nfldsig)
!          Apply 10-meter wind reduction factor to guess winds
           dx10=-goverrd*ten/tges(1)
           if (dpressave < dx10)then
              term=(prsln2-dpressave)/(prsln2-dx10)
              factw=one-term+factw*term
           end if
           ugesin=factw*ugesin   
           vgesin=factw*vgesin
 
        end if
       
!       Get approx k value of sfc by using surface pressure
        sfcchk=log(psges)
        call grdcrd1(sfcchk,prsltmp(1),nsig,-1)
 
     endif


!    Checks based on observation location relative to model surface and top
     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsigp,zero)
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     if(itype == 262) then ! Doppler Lidar 
        ratio_errors=error/(error_1+drpx+1.0e6_r_kind*rhgh+four*rlow)
     else
        ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+four*rlow)
     endif

!    Invert observation error
     error=one/error

!    Check to see if observation below model surface or above model top.
!    If so, don't use observation
     if (dpres > rsig )then
        if( regional .and. presw > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

     if(itype==262 .and. dpres<zero) ratio_errors=zero 

!    Compute innovations
     dudiff=uob-ugesin
     dvdiff=vob-vgesin
     spdb=sqrt(uob**2+vob**2)-sqrt(ugesin**2+vgesin**2)

2000 format(a9,1x,2(f8.2,1x),4(f8.2,1x),3x,i3)
2001 format(a6,1x,2(f8.2,1x),4(f8.2,1x),3x,i3,3x,f8.2)

!    If requested, setup for single obs test.
     if (oneobtest) then
        if (oneob_type=='u') then
           dudiff=maginnov
           dvdiff=zero
        elseif (oneob_type=='v') then
           dudiff=zero
           dvdiff=maginnov
        endif
        error=one/magoberr
        ratio_errors=one
     end if
 
!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = sqrt(dudiff**2+dvdiff**2)
     ratio    = residual/obserrlm
!!   modify cgross depending on the quality mark, qcmark=3, cgross=0.7*cgross
!!   apply asymetric gross check for satellite winds
     qcgross=cgross(ikx)
     if(data(iqc,i) == three  ) then
        qcgross=r0_7*cgross(ikx)
     endif

     if (ratio>qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if

     if (twodvar_regional) then
        wdirdiffmax=100000._r_kind
        if (spdob > zero .and. (spdob-spdb) > zero) then
           call getwdir(uob,vob,wdirob)
           call getwdir(ugesin,vgesin,wdirgesin)
           if ( min(abs(wdirob-wdirgesin),abs(wdirob-wdirgesin+r360), &
                          abs(wdirob-wdirgesin-r360)) > wdirdiffmax ) then
               error = zero
               ratio_errors = zero
           endif
        endif
     endif

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_w_ob_type,ibin)%tail%muse(nobskeep)

!    Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
              dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
           dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
        endif
     endif
 
     valu     = error*dudiff
     valv     = error*dvdiff

!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        val      = valu*valu+valv*valv
        exp_arg  = -half*val
        rat_err2 = ratio_errors**2
        if(njqc  .and. var_jb>tiny_r_kind .and. var_jb<10.0_r_kind .and. error >tiny_r_kind) then
           if(exp_arg  == zero) then
              wgt=one
           else
              wgt=sqrt(dudiff*dudiff+dvdiff*dvdiff)*error/sqrt(two*var_jb)
              wgt=tanh(wgt)/wgt
           endif
           term=-two*var_jb*rat_err2*log(cosh((sqrt(val))/sqrt(two*var_jb)))
           rwgt = wgt/wgtlim
           valqc = -two*term
        else if (vqc .and. cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        else
           term = exp_arg
           wgt  = one 
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        endif


!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(4*nsig+jsig+100)=awork(4*nsig+jsig+100)+valu*valu*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+valv*valv*rat_err2
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

!       Loop over pressure level groupings and obs to accumulate statistics
!       as a function of observation type.
        ress  = scale*sqrt(dudiff**2+dvdiff**2)
        ressw = ress*ress
        val2    = half*(valu*valu+valv*valv)
        valqc2  = half*valqc
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+spdb           ! speed bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc2         ! nonlin qc penalty
 
           end if
        end do
     end if


!    Fill obs to diagnostics structure
     if (luse_obsdiag) then
!    U
        obsptr%muse(jiter)=muse(i)
        obsptr%nldepart(jiter)=dudiff
        obsptr%wgtjo= (error*ratio_errors)**2
!    V
        obsdiags(i_w_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_w_ob_type,ibin)%tail%nldepart(jiter)=dvdiff
        obsdiags(i_w_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
 
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(whead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        my_head%dlev = dpres
        my_head%factw= factw
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)

        do j=1,8
           my_head%wij(j)=factw*my_head%wij(j)
        end do

        my_head%ures=dudiff
        my_head%vres=dvdiff
        my_head%err2=error**2
        my_head%raterr2=ratio_errors **2  
        my_head%time = dtime
        my_head%b=cvar_b(ikx)
        my_head%pg=cvar_pg(ikx)
        my_head%jb=var_jb
        my_head%luse=luse(i)

        if (luse_obsdiag) then
           my_head%diagu => obsptr

           my_diag => my_head%diagu
           if(my_head%idv/=my_diag%idv .or. &
              my_head%iob/=my_diag%iob .or. &
                        1/=my_diag%ich ) then
              call perr(myname,'mismatched %[head,diag], (idv,iob,ich,ibin) =',&
                        (/is,ioid(i),1,ibin/))
              call perr(myname,'head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,1/))
              call perr(myname,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
              call die(myname)
           endif
        endif ! (luse_obsdiag)

        if(oberror_tune) then
           my_head%upertb=data(iptrbu,i)/error/ratio_errors
           my_head%vpertb=data(iptrbv,i)/error/ratio_errors
           my_head%kx=ikx
           if (njqc) then
              ptabluv=ptabl_uv
           else
              ptabluv=ptabl
           endif
           if(presw > ptabluv(2))then
              my_head%k1=1
           else if( presw <= ptabluv(33)) then
              my_head%k1=33
           else
              k_loop: do k=2,32
                 if(presw > ptabluv(k+1) .and. presw <= ptabluv(k)) then
                    my_head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        if (luse_obsdiag) then
           my_head%diagv => obsdiags(i_w_ob_type,ibin)%tail

           my_diag => my_head%diagv
           if(my_head%idv/=my_diag%idv .or. &
              my_head%iob/=my_diag%iob .or. &
                        2/=my_diag%ich ) then
              call perr(myname,'mismatched %[head,diag], (idv,iob,ich,ibin) =',&
                        (/is,ioid(i),2,ibin/))
              call perr(myname,'head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,2/))
              call perr(myname,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
              call die(myname)
           endif
        endif ! (luse_obsdiag)
 
        my_head => null()
     end if

!    Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters) 
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = var_jb             ! non linear qc parameter
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        if(ictype(ikx) == 262) then  
           err_input = error_2
           err_adjst = error_1
        else
           err_input = data(ier2,i)
           err_adjst = data(ier,i)
        endif

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
 
        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1

        rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
        rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(20,ii) = data(ivob,i)       ! v wind component observation (m/s)
        rdiagbuf(21,ii) = dvdiff             ! v obs-ges used in analysis (m/s)
        rdiagbuf(22,ii) = vob-vgesin         ! v obs-ges w/o bias correction (m/s) (future slot)

        if(regional) then

!           replace positions 17-22 with earth relative wind component information

           uob_reg=data(iuob,i)
           vob_reg=data(ivob,i)
           dlon_e=data(ilone,i)*deg2rad
           call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(dudiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)
           rdiagbuf(17,ii) = uob_e         ! earth relative u wind component observation (m/s)
           rdiagbuf(18,ii) = dudiff_e      ! earth relative u obs-ges used in analysis (m/s)
           rdiagbuf(19,ii) = uob_e-uges_e  ! earth relative u obs-ges w/o bias correction (m/s) (future slot)

           rdiagbuf(20,ii) = vob_e         ! earth relative v wind component observation (m/s)
           rdiagbuf(21,ii) = dvdiff_e      ! earth relative v obs-ges used in analysis (m/s)
           rdiagbuf(22,ii) = vob_e-vges_e  ! earth relative v obs-ges w/o bias correction (m/s) (future slot)
        end if

        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor

        ioff=ioff0
        if (lobsdiagsave) then
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_w_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%nldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%tldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsptr%obssen(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_w_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

        if (twodvar_regional) then
           rdiagbuf(ioff+1,ii) = data(idomsfc,i) ! dominate surface type
           rdiagbuf(ioff+2,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

     endif

  end do
! End of loop over observations
  if(num_bad_ikx > 0) write(6,*)' in setupdlw, num_bad_ikx ( ikx<1 or ikx>nconvtype ) = ',num_bad_ikx

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:w',i_w_ob_type)
     write(7)' uv',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     if (twodvar_regional) then
        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
     endif
  end if


! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
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
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_u(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_u(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_v(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_v(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupdlw
