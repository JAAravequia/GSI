subroutine setupcwp(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
! Forward operator to assimilate GOES-Imager CWP retrievals
!$$$  subprogram documentation block
!                .      .    .                                       .
! 
! abstract: For CWP observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
!
! program history log:
!   2016-06-21  Thomas Jones  - Initial version of CWP forward operator
!                               "kind" information from DART file...replace later
!   2018-06-26  Thomas Jones  - Add updates corresponding to new obs processing
!                               "obkind" now comes from obs processing program
!                               Includes IWP, LWP, ZERO and day/night flags
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
!   language: f90 (ftn)
!   machine:  Cray
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use obsmod, only:  obsdiags, lobsdiagsave,nobskeep,lobsdiag_allocated, &
                     rmiss_single, time_offset, obs_diag, i_cwp_ob_type,luse_obsdiag

  use m_obsdiags, only: cwphead
  use m_obsNode, only: obsNode
  use m_cwpNode, only: cwpNode
  use m_obsLList, only: obsLList_appendNode

  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: obs_diag ,doradaroneob,oneobddiff,oneobvalue
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close

  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot 
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,&
       ges_lnprsl,ges_rho,ges_tsen,ges_prsl,ges_qsat                                 
  use gridmod, only: nsig,get_ijk,lat2,lon2,istart,jstart
  use gsi_metguess_mod, only: gsi_metguess_bundle,gsi_metguess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad,&
       r60,tiny_r_kind,cg_term,huge_single
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gridmod, only: wrf_mass_regional
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: nsdim
 
  implicit none
! Declare passed variables
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
  
! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind
  real(r_kind),parameter:: gravity    = 9.81_r_kind
  real(r_kind) :: r,rr,dqr,iters,dqs,dqg
  
! Declare external calls for code analysis
  external:: tintrp2a11, tintrp2a1
  external:: tintrp3
  external:: grdcrd1
  external:: stop2
  
! Declare local variables
  real(r_kind) rCWP, iwpmax, lwpmax, iwpmax_night, lwpmax_night, layer_rh
  real(r_kind) rlow,rhgh,rsig
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


  real(r_kind),dimension(nsig) :: qvgesin,qrgesin,qsgesin,qigesin,qggesin,qcgesin,psgesin
  real(r_kind),dimension(nsig) :: qsatgesin, relhum
! real(r_kind),dimension(nsig) :: qhgesin
  real(r_kind) dlat,dlon,dtime,ddiff,error,slat,satctp,satcbp,presw
  real(r_kind) ratio_errors
  
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,qrexp,qsexp,qgexp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) i,nchar,nreal,k,j,k1,ii
  integer(i_kind) mm1,jj,k2,isli
  integer(i_kind) jsig,ikxx,nn,ibin,ioff,ioff0
  integer(i_kind) mink, maxk, bbb, ttt
  integer(i_kind) ier,ilat,ilon,ihgt,icwpob,ikx,itime,iuse
  integer(i_kind) icbp,ictp,ikind,ilone,ilate, obkind
  integer(i_kind) ier2, idmiss2opt,it,istatus
  real(r_kind) :: effectiverhoqr,minrhoqr
  
  character(8),allocatable,dimension(:):: cdiagbuf

  logical :: proceed
  logical :: usenight, usenight_clear, uselwp_clear
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  real(r_kind) wrange
  integer(i_kind) numequal,numnotequal,kminmin,kmaxmax,istat
 
  logical:: in_curbin, in_anybin,debugging,save_jacobian
  type(sparr2) :: dhx_dx
  real(r_single), dimension(nsdim) :: dhx_dx_array
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(cwpNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  character(len=*),parameter:: myname='setupcwp'
  character(len=8) :: cpe

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qv
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qc
  !real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_ql
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qi
  !real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qh  

  n_alloc(:)=0
  m_alloc(:)=0

!*********** SET DAY / NIGHT USE FLAG: DEFAULT = FALSE ****************
  usenight = .false.
  usenight_clear = .false.
!*********** SET USE LWP = 0 FLAG: DEFAULT = TRUE ****************
  uselwp_clear = .false.
 
!******************************************************************************* 
  ! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid
!    index information for data array (see reading routine)
  ier=1        ! index of obs error
  ilon=2       ! index of grid relative obs location (x)
  ilat=3       ! index of grid relative obs location (y)
  ihgt=4       ! index of obs pressure height (hPa)
  icwpob=5     ! index of CWP observation (kg / m2)
  itime=7      ! index of observation time in data array (hour)        ! Analysis relative time!
  ikxx=8       ! index of obs type in data array                       ! from the convinfo file (order in the list)
  icbp=9       ! index of CBP
  ictp=10      ! index of CTP
  ikind=11     ! index of obkind: 0 = CWP=zero DAY; 1 = LWP DAY; 2 = IWP DAY; 3 = CWP=zero NIGHT; 4 = LWP NIGHT; 5 = IWP NIGHT
  iuse=12      ! index of use parameter
  ilone=13     ! index of longitude (degrees)
  ilate=14     ! index of latitude (degrees)
  ier2=16      ! index of original-original obs error
  
  numequal=0
  numnotequal=0

!
! If requested, save select data for output to diagnostic file
  print *, '*** BEGIN CWP FORWARD OPERATOR'
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=24
     nreal=ioff0                                                             
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if
  mm1=mype+1
  scale=one
  rsig=nsig


! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
! - Observation times are checked in read routine - comment out for now
 
!  call dtime_setup()
  do i=1,nobs
      debugging=.false.
        dtime=data(itime,i)
        dlat=data(ilat,i)
        dlon=data(ilon,i)    
        presw=data(ihgt,i)
        ikx = nint(data(ikxx,i))
        obkind = data(ikind,i)
        error=data(ier2,i)
        slat=data(ilate,i)*deg2rad
        satcbp=data(icbp,i)
        satctp=data(ictp,i) 

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
        if (.not.associated(obsdiags(i_cwp_ob_type,ibin)%head)) then
           allocate(obsdiags(i_cwp_ob_type,ibin)%head,stat=istat)
           if (istat/=0) then
              write(6,*)'setupcwp: failure to allocate obsdiags',istat
              call stop2(286)
           end if
           obsdiags(i_cwp_ob_type,ibin)%tail => obsdiags(i_cwp_ob_type,ibin)%head
        else
           allocate(obsdiags(i_cwp_ob_type,ibin)%tail%next,stat=istat)
           if (istat/=0) then
              write(6,*)'setupcwp: failure to allocate obsdiags',istat
              call stop2(286)
           end if
           obsdiags(i_cwp_ob_type,ibin)%tail => obsdiags(i_cwp_ob_type,ibin)%tail%next
        end if

        
        allocate(obsdiags(i_cwp_ob_type,ibin)%tail%muse(miter+1))
        allocate(obsdiags(i_cwp_ob_type,ibin)%tail%nldepart(miter+1))
        allocate(obsdiags(i_cwp_ob_type,ibin)%tail%tldepart(miter))
        allocate(obsdiags(i_cwp_ob_type,ibin)%tail%obssen(miter))
        obsdiags(i_cwp_ob_type,ibin)%tail%indxglb=ioid(i)
        obsdiags(i_cwp_ob_type,ibin)%tail%nchnperobs=-99999
        obsdiags(i_cwp_ob_type,ibin)%tail%luse=.false.
        obsdiags(i_cwp_ob_type,ibin)%tail%muse(:)=.false.
        obsdiags(i_cwp_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
        obsdiags(i_cwp_ob_type,ibin)%tail%tldepart(:)=zero
        obsdiags(i_cwp_ob_type,ibin)%tail%wgtjo=-huge(zero)
        obsdiags(i_cwp_ob_type,ibin)%tail%obssen(:)=zero
        n_alloc(ibin) = n_alloc(ibin) +1
        my_diag => obsdiags(i_cwp_ob_type,ibin)%tail
        my_diag%idv = is
        my_diag%iob = ioid(i)
        my_diag%ich = 1
     else

        if (.not.associated(obsdiags(i_cwp_ob_type,ibin)%tail)) then
           obsdiags(i_cwp_ob_type,ibin)%tail => obsdiags(i_cwp_ob_type,ibin)%head
        else
           obsdiags(i_cwp_ob_type,ibin)%tail => obsdiags(i_cwp_ob_type,ibin)%tail%next
        end if
        if (obsdiags(i_cwp_ob_type,ibin)%tail%indxglb/=ioid(i)) then
           write(6,*)'setupcwp: index error'
           call stop2(288)
        end if
      endif
     endif

!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig, mype,nfldsig)
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp,nsig,-1)
!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-presw,zero)
!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(presw-r0_001-nsig,zero)
!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if

     !Not adjusting obs error based upon ob vertical location relative to grid box
     ratio_errors = error/(abs(data(ier,i)))   
     error = one/error


!    Interpolate guess press, qr, qs, qg, qi, qc to observation location and time for all model levels
    call tintrp2a1(ges_prsl,psgesin,dlat,dlon,dtime,& 
            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qv,qvgesin,dlat,dlon,dtime,&
            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qr,qrgesin,dlat,dlon,dtime,& 
            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qs,qsgesin,dlat,dlon,dtime,& 
            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qg,qggesin,dlat,dlon,dtime,& 
            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qc,qcgesin,dlat,dlon,dtime,& 
!    call tintrp2a1(ges_ql,qcgesin,dlat,dlon,dtime,&   !FV3 FIX (qc = ql)
            hrdifsig,nsig,mype,nfldsig) 
    call tintrp2a1(ges_qi,qigesin,dlat,dlon,dtime,& 
            hrdifsig,nsig,mype,nfldsig)                        
!    call tintrp2a1(ges_qh,qigesin,dlat,dlon,dtime,&
!            hrdifsig,nsig,mype,nfldsig)
    call tintrp2a1(ges_qsat,qsatgesin,dlat,dlon,dtime,&
            hrdifsig,nsig,mype,nfldsig)

! Threshold small hydrometeor values
    qvgesin(:)  = max(qvgesin(:),1.e-6_r_kind)
    qrgesin(:)  = max(qrgesin(:),1.e-6_r_kind)
    qsgesin(:)  = max(qsgesin(:),1.e-9_r_kind)
    qggesin(:)  = max(qggesin(:),1.e-8_r_kind)
    qcgesin(:)  = max(qcgesin(:),1.e-8_r_kind)
    qigesin(:)  = max(qigesin(:),1.e-8_r_kind)
!    qhgesin(:)  = max(qhgesin(:),1.e-6_r_kind)
    qsatgesin(:)  = max(qsatgesin(:),1.e-8_r_kind)

   ! Calculate relative humidity for each model level
    relhum(:) = qvgesin(:) / qsatgesin(:)
    
! Convert P to pressure in (Pa)
    psgesin(:) = psgesin(:)*1000.0_r_kind

! ************* FORWARD OPERATOR TO CALCUALTE IWP/LWP/CWP FROM MODEL H(x) **********
!  LOOP THROUGH EACH MODEL LEVEL FROM SFC TO MODEL TOP

  ! CALCULATE MIN AND MAX LEVEL TO INTERGRATE OVER USING CBP/CTP
  mink=2
  maxk=nsig-2
  bbb=0
  ttt=0
  if (satcbp > 0.0 .and. satctp > 0.0) then
	do k=1, nsig-2
	  if ( psgesin(k) > 10000.0) then 

		! CLOUD BASE LEVEL
		if (satcbp > psgesin(k) .and. bbb == 0) then
			mink=k
			bbb=1
		endif
		! CLOUD TOP LEVEL
		if (satctp > psgesin(k) .and. ttt == 0) then
			maxk=k
			ttt=1
		endif
	  endif
	end do
  endif


  ! COMBINE HYDROMETEOR CONCENTRATIONS FROM CBP to CTP MODEL LEVELS
  do k=mink,maxk

     ! check that pressure decreases with height
     if ( psgesin(k) > psgesin(k+1) ) then

      !IWP/CWP/ZERO
       if (obkind == 0 .or. obkind == 1 .or. obkind == 3 .or. obkind == 5 ) then
        rCWP = rCWP + 0.5_r_kind * ( (qcgesin(k) + qcgesin(k+1)) + (qigesin(k) + qigesin(k+1)) + & 
      			(qggesin(k) + qggesin(k+1)) + (qrgesin(k) + qrgesin(k+1)) + &
      			(qsgesin(k) + qsgesin(k+1))  ) * (psgesin(k) - psgesin(k+1))
                        ! (qhgesin(k) + qhgesin(k+1))
      endif
      
      !LWP
      if (obkind == 2 .or. obkind == 4  ) then
        rCWP = rCWP + 0.5_r_kind * ( (qcgesin(k) + qcgesin(k+1)) + (qrgesin(k) + qrgesin(k+1)) ) * &
        	 (psgesin(k) - psgesin(k+1))
      endif
     else
     print*, "*** WARNING: PRESSURE INCREASES WITH Z: ", psgesin(k), psgesin(k+1)

     endif
     
  end do

  !APPLY CWP QC
  if (rCWP < 0.0 ) rCWP = 0.0
  !CONVERT TO PATH UNITS (kg/m2)
  if (rCWP > 0.0 ) rCWP = 1.0 * rCWP /(gravity)   ! -> kg/m2

  !print*, 'CWP TEST:', i, obkind, satcbp, satctp, presw, rCWP, data(icwpob,i), data(ier2,i)

  ! DAY SATURATION ADJUSTMENT
  iwpmax = 5.0
  lwpmax = 3.0
  if (obkind == 0 .or. obkind == 1 ) then
   if (rCWP > iwpmax ) rCWP = iwpmax
  endif
  if (obkind == 2) then
   if (rCWP > lwpmax ) rCWP = lwpmax
  endif  

  ! NIGHT SATURATION ADJUSTMENT
  iwpmax_night = 2.0
  lwpmax_night = 2.0
  if (obkind == 3 .or. obkind == 4 ) then
   if (rCWP > iwpmax_night ) rCWP = iwpmax_night
  endif
  if (obkind == 5) then
   if (rCWP > lwpmax_night ) rCWP = lwpmax_night
  endif

  !CHECK WHERE SATURATED CLOUD LAYER EXISTS IN MODEL ANALYSYS (from cloud base to cloud top)
  layer_rh = sum(relhum(mink:maxk)) / (max(1,size(relhum(mink:maxk))))
  if ( obkind == 2 .and. layer_rh > 0.975 ) then 
    print*, 'CWP RH THRESHOLD DAY:', sum(relhum(mink:maxk)), (max(1,size(relhum(mink:maxk)))), layer_rh
    rCWP = -998.0
    muse(i)= .false.
  endif

  if ( obkind == 4 .and. layer_rh > 0.975 ) then 
    print*, 'CWP RH THRESHOLD NIGHT:', sum(relhum(mink:maxk)), (max(1,size(relhum(mink:maxk)))), layer_rh
    rCWP = -998.0
    muse(i)= .false.
  endif

  !CHECK IF WANT TO USE NIGHT-TIME DATA
  if (usenight == .false. .and. (obkind == 4 .or. obkind == 5)) then
    muse(i)= .false.
  endif  
  if (usenight_clear == .false. .and. (obkind == 3 .or. obkind == 7 )) then
    muse(i)= .false.
  endif

  !CHECK IF WANT TO USE LWP = 0 DATA (zeros over low-level clouds)
  if (uselwp_clear == .false. .and. obkind == 6 ) then
    muse(i)= .false.
  endif  

! END FORWARD OPERATOR PART


!--------------Calculate departure from observation----------------!
  ddiff = data(icwpob,i) - rCWP

! --------------   Gross error checks ----------------!
     obserror = one/max(ratio_errors*error,tiny_r_kind)     
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if(miter == 0) then !keep all obs so we can add reflectivity where background is -120 DBZ
       if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if ( (ratio-cgross(ikx)) <= cgross(ikx) .and. ratio_errors >= tiny_r_kind) then      !commented before modified  
          !Since radar reflectivity can be very different from the model background
          ! good observations may be rejected during this QC step.  However, if these observations
          ! are allowed through, they can yield problems with convergence.  Therefore the error
          ! is inflated here up to twice the observation error in a manner that is
          ! proportional to the residual.  If this IF-TEST for this inflation fails, the
          ! observation is subsequently rejected.
                    
           obserror = residual/cgross(ikx)!commented before modified  
           error = one/obserror!commented before modified  
           
        else       !commented before modified  
           if (luse(i)) awork(4) = awork(4)+one 
           error = zero 
           ratio_errors = zero 
       
        end if!commented before modified  
     end if
   endif

     !if (ratio_errors*error <=tiny_r_kind) muse(i)=.false. 
     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_cwp_ob_type,ibin)%tail%muse(nobskeep)
     
     val     = error*ddiff


!  **** VAR STUFF...ditch ???             
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
           jsig = presw
           jsig=max(1,min(jsig,nsig))
           
           !print*, jsig, val2, rat_err2, valqc
           
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
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
             
           end if
        end do

     end if


     if(luse_obsdiag)then
       obsdiags(i_cwp_ob_type,ibin)%tail%luse=luse(i)
       obsdiags(i_cwp_ob_type,ibin)%tail%muse(jiter)=muse(i)
       obsdiags(i_cwp_ob_type,ibin)%tail%nldepart(jiter)=ddiff 
       obsdiags(i_cwp_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then             


        allocate(my_head)
        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(cwphead(ibin),my_node)
        my_node => null()
       
        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)        


!       Set (i,j,k) indices of guess gridpoint that bound obs location
        my_head%dlev = presw
!        my_head%factw = factw
        call get_ijk(mm1,dlat,dlon,presw,my_head%ij,my_head%wij)
 
        my_head%raterr2 = ratio_errors**2 
        my_head%res     = ddiff               ! Observation - ges
        my_head%err2    = error**2
        my_head%time    = dtime
        my_head%luse    = luse(i)
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
 
        if (luse_obsdiag) then
          my_head%diags => obsdiags(i_cwp_ob_type,ibin)%tail
       
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
     endif
     
     
!    Save select output for diagnostic file
     if(conv_diagsave .and. luse(i) )then

        ii=ii+1
        !cdiagbuf(ii)    = 'GOES-R'         ! satellite ID id
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
        
        if(binary_diag) call contents_binary_diag_
        if(netcdf_diag) call contents_netcdf_diag_

     end if
  end do


! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
       !call dtime_show(myname,'diagsave:cwp',i_cwp_ob_type)
       write(7)'cwp',nchar,nreal,ii,mype,ioff0
       write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       deallocate(cdiagbuf,rdiagbuf)
     endif
  end if
  !write(6,*)'mype, icwpsmlobs,irejcwpsmlobs are ',mype,' ',irefsmlobs, ' ',irejrefsmlobs
  print*, 'END SETUPCWP'

! End of routine
  contains


  subroutine check_vars_ (proceed)
  
   logical,intent(inout) :: proceed
   integer(i_kind) ivar, istatus
 ! Check to see if required guess fields are available
   call gsi_metguess_get ('var::qv', ivar, istatus )
   proceed=ivar>0
   call gsi_metguess_get ('var::qr', ivar, istatus )
   proceed=ivar>0
   call gsi_metguess_get ('var::qs', ivar, istatus )
   proceed=proceed.and.ivar>0
   call gsi_metguess_get ('var::qg', ivar, istatus )
   proceed=proceed.and.ivar>0
   call gsi_metguess_get ('var::qi', ivar, istatus )
   proceed=proceed.and.ivar>0  
   call gsi_metguess_get ('var::ql', ivar, istatus )
   proceed=proceed.and.ivar>0 
   !call gsi_metguess_get ('var::qh', ivar, istatus )
   !proceed=proceed.and.ivar>0 
   call gsi_metguess_get ('var::ps', ivar, istatus )
   proceed=proceed.and.ivar>0       
  
  end subroutine check_vars_



  subroutine init_vars_

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

!    get QV
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

!    get qr ...
     varname='qr'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     print *, '*** MAX-MIN QR ***', MAXVAL(rank3),MINVAL(rank3)
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

!    get qs ...
     varname='qs'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     print *, '*** MAX-MIN QS ***', MAXVAL(rank3),MINVAL(rank3)
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
     print *, '*** MAX-MIN QG ***', MAXVAL(rank3),MINVAL(rank3)
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

!    get qi ...
     varname='qi'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     print *, '*** MAX-MIN QI ***', MAXVAL(rank3),MINVAL(rank3)
     if (istatus==0) then
         if(allocated(ges_qi))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qi(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qi(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qi(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif


!    get ql ... (=qc)
!     varname='ql'
!     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     !print *, '*** MAX-MIN QC ***', MAXVAL(rank3),MINVAL(rank3)
!     if (istatus==0) then
!         if(allocated(ges_ql))then
!            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
!            call stop2(999)
!         endif
!         allocate(ges_ql(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
!         ges_ql(:,:,:,1)=rank3
!         do ifld=2,nfldsig
!            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
!            ges_ql(:,:,:,ifld)=rank3
!         enddo
!     else
!         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
!         call stop2(999)
!     endif

!     get qc ...
     varname='ql'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     print *, '*** MAX-MIN QC ***', MAXVAL(rank3),MINVAL(rank3)
     if (istatus==0) then
         if(allocated(ges_qc))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qc(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qc(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qc(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

!    get qh ...
!     varname='qh'
!     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
!     if (istatus==0) then
!         if(allocated(ges_qh))then
!            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
!            call stop2(999)
!         endif
!         allocate(ges_qh(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
!         ges_qh(:,:,:,1)=rank3
!         do ifld=2,nfldsig
!            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
!            ges_qh(:,:,:,ifld)=rank3
!         enddo
!     else
!         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
!         call stop2(999)
!     endif

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
900  format('conv_cwp_',i2.2,'.nc4')
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

        cdiagbuf(ii)    = 'GOES-R'         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = zero               ! dummy
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = zero               ! dummy
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (CWP)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (CWP)**-1
        rdiagbuf(16,ii) = errinv_input         ! final inverse observation error (CWP)**-1
        rdiagbuf(17,ii) = data(icwpob,i)       ! radar reflectivity observation (cWP)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (CWP)
        rdiagbuf(19,ii) = data(icwpob,i)-rCWP  ! obs-ges w/o bias correction (CWP)

       if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setupcwp.f90 '
            stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_cwp_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_cwp_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_cwp_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_cwp_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '    cwp'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              'GOES-R'         )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata("Latitude",                sngl(data(ilate,i)) )
           call nc_diag_metadata("Longitude",               sngl(data(ilone,i)) )
!           call nc_diag_metadata("Station_Elevation",       sngl(data(ielev,i)) )
           call nc_diag_metadata("Pressure",                sngl(presw)         )
!           call nc_diag_metadata("Height",                  sngl(data(ihgt,i))  )
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
           call nc_diag_metadata("Errinv_Final",            sngl(errinv_input)  )

           call nc_diag_metadata("Observation",             sngl(data(icwpob,i)) )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)   )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(data(icwpob,i)-rCWP) )

           if (lobsdiagsave) then
              do jj=1,miter
                 if (obsdiags(i_cwp_ob_type,ibin)%tail%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", obsdiags(i_cwp_ob_type,ibin)%tail%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", obsdiags(i_cwp_ob_type,ibin)%tail%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen", obsdiags(i_cwp_ob_type,ibin)%tail%obssen   )
           endif
           if (save_jacobian) then
              call fullarray(dhx_dx, dhx_dx_array)
              call nc_diag_data2d("Observation_Operator_Jacobian", dhx_dx_array)
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qv)) deallocate(ges_qv)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qr)) deallocate(ges_qr)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_qc)) deallocate(ges_qc)
!    if(allocated(ges_ql)) deallocate(ges_ql)  
    if(allocated(ges_qi)) deallocate(ges_qi)  
    !if(allocated(ges_qh)) deallocate(ges_qh) 
  end subroutine final_vars_
end subroutine setupcwp
