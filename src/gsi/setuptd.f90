!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
!ROUTINE:  setuptd --- Compute rhs of oi for dew point temperature obs
! 
! abstract: For dew point observations
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2017-09-10 Junjun Hu, CIMMS/OU/NOAA/NSSL 
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
!
subroutine setuptd(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)

! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind

  use m_obsdiags, only: tdhead
  use obsmod, only: sfcmodel,perturb_obs,oberror_tune,&
       i_td_ob_type,obsdiags,lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use m_obsNode, only: obsNode
  use m_tdNode, only: tdNode
  use m_obsLList, only: obsLList_appendNode
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset

  use qcmod, only: npres_print,dfact,dfact1,ptop,pbot,ptopq,pbotq 
  use qcmod, only: njqc,vqc

  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: obs_diag ,doradaroneob,oneobddiff,oneobvalue

  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close

  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr

  use gridmod, only: nsig,twodvar_regional,regional
  use gridmod, only: get_ijk
  use jfunc, only: jiter,last,jiterstart,miter

  use guess_grids, only: nfldsig, hrdifsig,ges_lnprsl,&
       geop_hgtl,ges_tsen,pt_ll,pbl_height

  use constants, only: zero, one, four,t0c,rd_over_cp,three,rd_over_cp_mass,ten
  use constants, only: tiny_r_kind,half,two,cg_term
  use constants, only: huge_single,r1000,wgtlim,r10,fv
  use constants, only: one_quad
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,icsubtype
  use converr_t, only: ptabl_t ! currently use t errtable, td can be included in errtable in future JJH
  use converr, only: ptabl 
  use rapidrefresh_cldsurf_mod, only: l_sfcobserror_ramp_t,l_sfcobserror_ramp_q 
  use rapidrefresh_cldsurf_mod, only: i_use_2mq4b,l_closeobs !

  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  use met_mod, only: qv_to_relh, rh_and_temp_to_dewpoint,qv_to_dewpoint

  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: nsdim

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind)                                  , intent(in   ) :: lunin   ! file unit from which to read observations
  integer(i_kind)                                  , intent(in   ) :: mype    ! mpi task id
  integer(i_kind)                                  , intent(in   ) :: nele    ! number of data elements per observation
  integer(i_kind)                                  , intent(in   ) :: nobs    ! number of observations
  integer(i_kind)                                  , intent(in   ) :: is      ! ndat index
  logical                                          , intent(in   ) :: conv_diagsave   ! logical to save innovation dignostics


! !INPUT/OUTPUT PARAMETERS:

! array containing information ...
  real(r_kind),dimension(npres_print,nconvtype,5,3), intent(inout) :: bwork !  about o-g stats
  real(r_kind),dimension(100+7*nsig)               , intent(inout) :: awork !  for data counts and gross checks

!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind

  character(len=*),parameter :: myname='setuptd'

! Declare external calls for code analysis
  external:: SFC_WTQ_FWD
  external:: get_tlm_tsfc
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  
  real(r_double) rstation_id
  real(r_kind) rsig,drpx,rsigp
  real(r_kind) psges,sfcchk,pres_diff,rlow,rhgh,ramp
  real(r_kind) pof_idx,poaf,effective
  real(r_kind) tges,qges
  real(r_kind) obserror,ratio,val2,obserrlm,ratiosfc
  real(r_kind) residual,ressw2,scale,ress,ratio_errors,tdob,ddiff
  real(r_kind) val,valqc,dlon,dlat,dtime,dpres,error,prestd,rwgt,var_jb !JJH prest>prestd
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind) cg_td,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2,qcgross
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_kind) tgges,roges
  real(r_kind) tdges,rhges 
  real(r_kind),dimension(nsig):: tvtmp,qtmp,utmp,vtmp,hsges
  real(r_kind) u10ges,v10ges,t2ges,q2ges,psges2,f10ges

  real(r_kind),dimension(nsig):: prsltmp2

  real(r_kind),dimension(34) :: ptabltd
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  real(r_kind) :: hr_offset

  integer(i_kind) i,j,nchar,nreal,k,ii,jj,l,nn,ibin,idia,idia0,ix,ijb
  integer(i_kind) mm1,jsig,iqt
  integer(i_kind) itype,msges
  integer(i_kind) ier,ilon,ilat,ipres,itdob,id,itime,ikx,iqc,iptrb,icat,ipof,ivvlc,idx,itemp
  integer(i_kind) ier2,iuse,ilate,ilone,ikxx,istnelv,iobshgt,izz,iprvd,isprvd
  integer(i_kind) regime,istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr

  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical,dimension(nobs):: luse,muse
  logical sfctype
  logical iqtflg
  logical aircraftobst

  logical:: in_curbin, in_anybin,save_jacobian

  type(sparr2) :: dhx_dx
  real(r_single), dimension(nsdim) :: dhx_dx_array

  logical proceed
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(tdNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  real(r_kind) :: thisPBL_height,ratio_PBL_height,prestsfc,diffsfc,dthetav

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_q2
  real(r_kind),allocatable,dimension(:,:,:) :: ges_td2m
  logical usetd2m

  n_alloc(:)=0
  m_alloc(:)=0

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...  
  usetd2m = .false.
  call init_vars_
!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

! index information for data array (see reading routine)
  ier=1       ! index of obs(td) error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  itdob=5     ! index of td observation
  id=6        ! index of station id
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of ob type
  iqt=9       ! qtflg (virtual temperature flag for t)
  iqc=10      ! index of quality mark
  ier2=11     ! index of original-original obs error ratio
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  istnelv=19  ! index of station elevation (m)
  iobshgt=20  ! index of observation height (m)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  itemp=25    ! index of temperature (t) obs 
  ijb=26      ! index of non linear qc parameter
  iptrb=27    ! index of td perturbation

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  var_jb=zero

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
 !              write(*,'(a,2f10.5,2I8,2L10)') 'chech obs time==',data(itime,k)-hr_offset,data(itime,l)-hr_offset,k,l,&
 !                           muse(k),muse(l)
           else
               tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
               dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
               dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do


! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=19
    
     idia0=nreal
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     rdiagbuf=zero
     if(netcdf_diag) call init_netcdf_diag_
  end if
  scale=one
  rsig=float(nsig)
  mm1=mype+1

  rsigp=rsig+one
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        ! Convert obs lats and lons to grid coordinates
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        dpres=data(ipres,i)
        error=data(ier2,i)
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx)
        rstation_id     = data(id,i)
        prestd=r10*exp(dpres)     ! in mb JJH
        sfctype=(itype>179.and.itype<190).or.(itype>=192.and.itype<=199)
  
        iqtflg=nint(data(iqt,i)) == 0 ! always sensible temperature, iqtflg always false
        var_jb=data(ijb,i)
!       write(6,*) 'SETUPTD:itype,var_jb,ijb=',itype,var_jb,ijb

!       Load observation value and observation error into local variables
        tdob=data(itdob,i)
        obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
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
           if (.not.associated(obsdiags(i_td_ob_type,ibin)%head)) then
              allocate(obsdiags(i_td_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuptd: failure to allocate obsdiags',istat
                 call stop2(298)
              end if
              obsdiags(i_td_ob_type,ibin)%tail => obsdiags(i_td_ob_type,ibin)%head
           else
              allocate(obsdiags(i_td_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuptd: failure to allocate obsdiags',istat
                 call stop2(298)
              end if
              obsdiags(i_td_ob_type,ibin)%tail => obsdiags(i_td_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_td_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_td_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_td_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_td_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_td_ob_type,ibin)%tail%indxglb=ioid(i)
           obsdiags(i_td_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_td_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_td_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_td_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_td_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_td_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_td_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_td_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = ioid(i)
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_td_ob_type,ibin)%tail)) then
              obsdiags(i_td_ob_type,ibin)%tail => obsdiags(i_td_ob_type,ibin)%head
           else
              obsdiags(i_td_ob_type,ibin)%tail => obsdiags(i_td_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_td_ob_type,ibin)%tail%indxglb/=ioid(i)) then
              write(6,*)'setuptd: index error'
              call stop2(300)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

! Interpolate log(ps) & log(pres) at mid-layers to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     drpx=zero
     if(sfctype .and. .not.twodvar_regional) then
        drpx=abs(one-((one/exp(dpres-log(psges))))**rd_over_cp)*t0c 
     end if

!    Put obs pressure in correct units to get grid coord. number
     call grdcrd1(dpres,prsltmp(1),nsig,-1)

! Implementation of forward model ----------

     if(sfctype.and.sfcmodel) then
        tgges=data(iskint,i)
        roges=data(isfcr,i)

        msges = 0
        if(itype == 180 .or. itype == 182 .or. itype == 183 .or. itype == 199) then    !sea
           msges=0
        elseif(itype == 181 .or. itype == 187 .or. itype == 188) then  !land
           msges=1
        endif

        call tintrp2a1(ges_tv,tvtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_q,qtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_u,utmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_v,vtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(geop_hgtl,hsges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
  
        psges2  = psges          ! keep in cb
        prsltmp2 = exp(prsltmp)  ! convert from ln p to cb
        call SFC_WTQ_FWD (psges2, tgges,&
             prsltmp2(1), tvtmp(1), qtmp(1), utmp(1), vtmp(1), &
             prsltmp2(2), tvtmp(2), qtmp(2), hsges(1), roges, msges, &
             f10ges,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)
        qges = q2ges 

     else
        if(sfctype .and. i_use_2mq4b>0) then ! ges_th2 available only if i_use_2mt4b>0 and i_use_2mq4b>0
          ! Interpolate guess td2m to observation location and time ! from setuptd2m 
           if(usetd2m) then
              call tintrp2a11(ges_td2m,tdges,dlat,dlon,dtime,hrdifsig,&
                   mype,nfldsig)
           else
              ! Interpolate 2-m q to obs locations/times
              call tintrp2a11(ges_q2,q2ges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)

              ! Interpolate guess moisture to observation location and time
              call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
                  hrdifsig,mype,nfldsig) 

              if(i_use_2mq4b==1)then !from setup q
                  qges=0.33_r_single*qges+0.67_r_single*q2ges
              elseif(i_use_2mq4b==2) then
                  if(q2ges >= qges) then
                     q2ges=min(q2ges, 1.15_r_single*qges)
                  else
                     q2ges=max(q2ges, 0.85_r_single*qges)
                  end if
                  qges=q2ges
              else
                  write(6,*) 'Invalid i_use_2mq4b number=',i_use_2mq4b
                  call stop2(100)
              endif
           endif
        else
          ! Interpolate guess moisture to observation location and time
          call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
               hrdifsig,mype,nfldsig) 

        endif

     endif

     if(.not. usetd2m) then 
          ! from qges calculate tdges  
          if(qges < 0.0_r_kind .or. qges >= 1.0_r_kind) then
             tdges = -1.0e+10_r_kind
             muse(i)=.false. ! don't use it
          else
             call qv_to_dewpoint(qges,prestd,tdges) !   JJH
          endif
     endif


!    Get approximate k value of surface by using surface pressure
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp(1),nsig,-1)

!    Check to see if observations is above the top of the model (regional mode)
     if(sfctype)then
        if(abs(dpres)>four) drpx=1.0e10_r_kind
        pres_diff=prestd-r10*psges !JJH
        if (twodvar_regional .and. abs(pres_diff)>=r1000) drpx=1.0e10_r_kind
! linear variation of observation ramp [between grid points 1(~3mb) and 15(~45mb) below the surface]
     endif
     rlow=max(sfcchk-dpres,zero)
     if(l_sfcobserror_ramp_t .and. l_sfcobserror_ramp_q) then   
        ramp=min(max(((rlow-1.0_r_kind)/(15.0_r_kind-1.0_r_kind)),0.0_r_kind),1.0_r_kind)
     else
        ramp=rlow
     endif

     rhgh=max(zero,dpres-rsigp-r0_001)

     if(sfctype.and.sfcmodel)  dpres = one     ! place sfc T obs at the model sfc

     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     
     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+r8*ramp)
     error=one/error
     if (dpres > rsig )then
        if( regional .and. prestd > pt_ll )then !JJH
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

! Compute innovation
     ddiff = tdob-tdges

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross error checks

     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     ratiosfc = ddiff/obserrlm

 ! modify gross check limit for quality mark=3
     if(data(iqc,i) == three ) then
        qcgross=r0_7*cgross(ikx)
     else
        qcgross=cgross(ikx)
     endif

     if (twodvar_regional) then                  
        if ( (data(iuse,i)-real(int(data(iuse,i)),kind=r_kind)) == 0.25_r_kind )then 
            qcgross=three*qcgross                    ! Terrain aware modification
                                                     ! to gross error check
        end if  
     endif


     if (ratio > qcgross .or. ratio_errors < tiny_r_kind) then
         if (luse(i)) awork(4) = awork(4)+one
         error = zero
         ratio_errors = zero
     else
         ratio_errors = ratio_errors/sqrt(dup(i))
     end if
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_td_ob_type,ibin)%tail%muse(nobskeep)

!    Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              ddiff=ddiff+data(iptrb,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           ddiff=ddiff+data(iptrb,i)/error/ratio_errors
        endif
     endif

!    Compute penalty terms
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2

        if(njqc .and. var_jb>tiny_r_kind .and. var_jb < 10.0_r_kind .and. error >tiny_r_kind)  then
           if(exp_arg  == zero) then
              wgt=one
           else
              wgt=ddiff*error/sqrt(two*var_jb)
              wgt=tanh(wgt)/wgt
           endif
           term=-two*var_jb*rat_err2*log(cosh((val)/sqrt(two*var_jb)))
           rwgt = wgt/wgtlim
           valqc = -two*term
        else if (vqc .and. cvar_pg(ikx)> tiny_r_kind .and. error >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_td=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_td*wnotgross)
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
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
           awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
           awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2*rat_err2
        end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
        ress   = ddiff*scale
        ressw2 = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(prestd >ptopq(k) .and. prestd <= pbotq(k))then          ! JJH
              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress           ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2         ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
              
           end if
        end do
     end if

!    Fill obs diagnostics structure
     if(luse_obsdiag)then
        obsdiags(i_td_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_td_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_td_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
!    if ( .not. last .and. muse(i)) then
     if (muse(i)) then

!!!!!!!!!!!!!!!!!!!!!! JJH added
        allocate(my_head)
        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(tdhead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)


!       Set (i,j,k) indices of guess gridpoint that bound obs location
        my_head%dlev= dpres
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)

        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%raterr2 = ratio_errors**2
        my_head%time    = dtime
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%jb      = var_jb
        my_head%luse    = luse(i)

        if(oberror_tune) then
           my_head%kx=ikx
           my_head%tdpertb=data(iptrb,i)/error/ratio_errors
           if (njqc) then
              ptabltd=ptabl_t ! currently use t errtable, td can be included in errtable in future JJH
           else
              ptabltd=ptabl
           endif

           if(prestd > ptabltd(2))then
              my_head%k1=1
           else if( prestd <= ptabltd(33)) then
              my_head%k1=33
           else
              k_loop: do k=2,32
                 if(prestd > ptabltd(k+1) .and. prestd <= ptabltd(k)) then
                    my_head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        if(luse_obsdiag)then
           write(*,*) '------------22222' !JJH
           my_head%diags => obsdiags(i_td_ob_type,ibin)%tail
           write(*,*) '------------22223'!JJH

           my_diag => my_head%diags
           write(*,*) '------------22224'!JJH
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

!!!!!!!!!!!!!!!!!!!!!! JJH added


! Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

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
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

        rdiagbuf(13,ii) = var_jb*1.0e+6 + rwgt               ! nonlinear qc relative weight JJH var_jb added
       

        if(binary_diag) call contents_binary_diag_
        if(netcdf_diag) call contents_netcdf_diag_

     end if

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     if(netcdf_diag) call nc_diag_write
      if(binary_diag .and. ii>0)then
       write(7)' td',nchar,nreal,ii,mype,idia0
       write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       deallocate(cdiagbuf,rdiagbuf)

       if (twodvar_regional) then
        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
       endif
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
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q', ivar, istatus )
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
         if(allocated(ges_z))then
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
     if(i_use_2mq4b) then
!    get q2m ...
        varname='q2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_z))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_q2(size(rank2,1),size(rank2,2),nfldsig))
            ges_q2(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_q2(:,:,ifld)=rank2
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            call stop2(999)
        endif

!    get td2m ...
        varname='td2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_td2m))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_td2m(size(rank2,1),size(rank2,2),nfldsig))
            ges_td2m(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_td2m(:,:,ifld)=rank2
            enddo
            usetd2m = .true.
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            !call stop2(999)
        endif
     endif
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
900  format('conv_td_',i2.2,'.nc4')
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
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = prestd              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = data(iqt,i)        ! setup qc or event mark (currently qtflg only)
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = var_jb*1.0e+6 + rwgt                  ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (K)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (K)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (K)**-1
        rdiagbuf(17,ii) = data(itdob,i)       ! dew point temperature observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = tdob-tdges           ! obs-ges w/o bias correction (K) (future slot)
        idia=idia0        
        if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setuptd.f90 '
            stop
           do jj=1,miter
              idia=idia+1
              if (obsdiags(i_td_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,ii) = one
              else
                 rdiagbuf(idia,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_td_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_td_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_td_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(idia+1:nreal,ii))
           idia = idia + size(dhx_dx)
        endif

        if (twodvar_regional) then
           rdiagbuf(idia+1,ii) = data(idomsfc,i) ! dominate surface type
           rdiagbuf(idia+2,ii) = data(izz,i)     ! model terrain at observation location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

  end subroutine contents_binary_diag_

  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '    td'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id          )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata("Latitude",                sngl(data(ilate,i)) )
           call nc_diag_metadata("Longitude",               sngl(data(ilone,i)) )
           call nc_diag_metadata("Station_Elevation",       sngl(data(istnelv,i)) )
           call nc_diag_metadata("Pressure",                sngl(prestd)         )
           call nc_diag_metadata("Height",                  sngl(data(iobshgt,i))  )
           call nc_diag_metadata("Time",                    sngl(dtime-time_offset))
           call nc_diag_metadata("Prep_QC_Mark",            sngl(zero)          )
           call nc_diag_metadata("Prep_Use_Flag",           sngl(data(iuse,i))  )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb   !          )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(var_jb*1.0e+6 + rwgt )          )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)          )
           endif

           call nc_diag_metadata("Errinv_Input",            sngl(errinv_input)  )
           call nc_diag_metadata("Errinv_Adjust",           sngl(errinv_adjst)  )
           call nc_diag_metadata("Errinv_Final",            sngl(errinv_final)  )

           call nc_diag_metadata("Observation",             sngl(data(itdob,i)) )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)   )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(data(itdob,i)-tdges) )

           if (lobsdiagsave) then
              do jj=1,miter
                 if (obsdiags(i_td_ob_type,ibin)%tail%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", obsdiags(i_td_ob_type,ibin)%tail%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", obsdiags(i_td_ob_type,ibin)%tail%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen", obsdiags(i_td_ob_type,ibin)%tail%obssen   )
           endif
           if (save_jacobian) then
              call fullarray(dhx_dx, dhx_dx_array)
              call nc_diag_data2d("Observation_Operator_Jacobian", dhx_dx_array)
           endif

  end subroutine contents_netcdf_diag_


  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_td2m)) deallocate(ges_td2m)
  end subroutine final_vars_

end subroutine setuptd
