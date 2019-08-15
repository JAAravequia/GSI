!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  rtma_comp_fact10 --- Compute the factw with similarity theory
!
! !INTERFACE:
!

subroutine rtma_comp_fact10(dlat,dlon,dtime,dpres,mype,factw)

! !USES:

  use mpeu_util, only: die,perr,getindex
  use state_vectors, only: svars3d, levels, nsdim
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiags, only: whead
  use obsmod, only: rmiss_single,perturb_obs,oberror_tune,lobsdiag_forenkf,&
       i_w_ob_type,obsdiags,obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset,bmiss,ianldate
  use m_obsNode, only: obsNode
  use m_wNode, only: wNode
  use m_obsLList, only: obsLList_appendNode
  use obsmod, only: obs_diag,luse_obsdiag
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1,qc_satwnds,njqc,vqc
  use oneobmod, only: oneobtest,oneob_type,magoberr,maginnov 
  use gridmod, only: get_ijk,nsig,twodvar_regional,regional,wrf_nmm_regional,&
      rotate_wind_xy2ll
  use guess_grids, only: nfldsig,hrdifsig,geop_hgtl,sfcmod_gfs
  use guess_grids, only: tropprs,sfcmod_mm5
  use guess_grids, only: ges_lnprsl,comp_fact10,pt_ll,pbl_height
  use constants, only: zero,half,one,tiny_r_kind,two,cg_term, &
           three,rd,grav,four,five,huge_single,r1000,wgtlim,r10,r400,&
           r100,fv !for similarity
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
  use sparsearr, only: sparr2, new, size, writearray, fullarray

  implicit none
  
! !INPUT PARAMETERS:

   integer(i_kind)                                  ,intent(in   ) :: mype  ! mpi task id
   
! !INPUT/OUTPUT PARAMETERS:


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
!
!   2018-08-12  zhang - creat the code 
!
!
!

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

  character(len=*),parameter:: myname='rtma_comput_fact10'

! Declare external calls for code analysis
  external:: intrp2a11,tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2
  external:: SFC_WTQ_FWD

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

  integer(i_kind) i,nchar,nreal,k,j,l,ii,itype,ijb
  integer(i_kind) jsig,mm1,iptrbu,iptrbv,jj,icat
  integer(i_kind) k1,k2,ikxx,nn,isli,ibin,ioff,ioff0
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx,ielev,iqc
  integer(i_kind) ihgt,ier2,iuse,ilate,ilone,istat
  integer(i_kind) izz,iprvd,isprvd
  integer(i_kind) idomsfc,isfcr,iskint,iff10

  integer(i_kind) :: iz, u_ind, v_ind, nnz, nind
  real(r_kind) :: delz
  logical z_height,sfc_data
  logical lowlevelsat,duplogic
  logical proceed



  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

! for similarity theory
  integer(i_kind) :: msges
  integer(i_kind) :: iza
  logical iqtflg
  real(r_kind) :: h10
  real(r_kind) :: tgges,roges,regime
  real(r_kind) :: tv1,tv2,psit2,psit
  real(r_kind) :: u10ges,v10ges,t2ges,q2ges,psges2,f10ges
  real(r_kind) :: u_elev_ges,v_elev_ges
  real(r_kind) :: pres1,pres2,tmp1,tmp2,qq1,qq2,uu1,vv1,hgt1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_presgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_presgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::ges_tmpgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_tmpgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::ges_qgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_qgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::ges_ugrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_vgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_hgtgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::ges_sfrgrid
  real(r_kind),allocatable,dimension(:,:,:  )::ges_tggrid



! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

! for similarity
  call init_vars11_


  msges = 1 !for land
  call tintrp2a11(ges_presgrid1,pres1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_presgrid2,pres2,dlat,dlon,dtime,hrdifsig,&
                   mype,nfldsig)
  !x convert input pressure variables from Pa to cb.
  pres1  = 0.001_r_kind*pres1
  pres2  = 0.001_r_kind*pres2
  print*,'xyz pres1,pres2=', pres1,pres2

  call tintrp2a11(ges_tmpgrid1,tmp1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_tmpgrid2,tmp2,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_qgrid1,qq1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_qgrid2,qq2,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_ugrid1,uu1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_vgrid1,vv1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_hgtgrid1,hgt1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_sfrgrid,roges,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(ges_tggrid,tgges,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  !convert sensible temperature to virtual temperature
  tv1=tmp1*((one+fv*qq1))
  tv2=tmp2*((one+fv*qq2))
  tgges=tgges*((one+fv*qq2))

  !unit change: originally m --> cm
  roges=roges*r100

  psges2  = psges          ! keep in cb 

  h10 = 10.0_r_kind
  call SFC_WTQ_FWD (psges2, tgges,&
       pres1, tmp1, qq1, uu1, vv1, &
       pres2, tmp2, qq2, hgt1, roges, msges, h10,&
       !output variables
       f10ges,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)

  h10=dpres     
  call SFC_WTQ_FWD (psges2, tgges,&
       pres1, tmp1, qq1, uu1, vv1, &
       pres2, tmp2, qq2, hgt1, roges, msges, h10,&
       !output variables
       f10ges,u_elev_ges,v_elev_ges, t2ges, q2ges, regime, iqtflg,h10)

  factw=sqrt(u_elev_ges**2+v_elev_ges**2)/sqrt(u10ges**2+v10ges**2)

! Release memory of local guess arrays
  call final_vars_
  call final_vars11_

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

  subroutine init_vars11_

   real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
   real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
   character(len=10) :: varname
   integer(i_kind) ifld, istatus

!    Assimilation of LST required ges variables
!    get presgrid1 ...
     varname='presgrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_presgrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_presgrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_presgrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_presgrid1(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get presgrid2 ...
     varname='presgrid2'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_presgrid2))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_presgrid2(size(rank2,1),size(rank2,2),nfldsig))
         ges_presgrid2(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_presgrid2(:,:,ifld)=rank2
         enddo

      else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tmpgrid1 ...
     varname='tmpgrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_tmpgrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tmpgrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_tmpgrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_tmpgrid1(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tmpgrid2 ...
     varname='tmpgrid2'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_tmpgrid2))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tmpgrid2(size(rank2,1),size(rank2,2),nfldsig))
         ges_tmpgrid2(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_tmpgrid2(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get qgrid1 ...
     varname='qgrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_qgrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qgrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_qgrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_qgrid1(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get qgrid2 ...
     varname='qgrid2'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_qgrid2))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qgrid2(size(rank2,1),size(rank2,2),nfldsig))
         ges_qgrid2(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_qgrid2(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get ugrid1 ...
     varname='ugrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ugrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc'
            call stop2(999)
         endif
         allocate(ges_ugrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_ugrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ugrid1(:,:,ifld)=rank2
         enddo
      else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get vgrid1 ...
     varname='vgrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_vgrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_vgrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_vgrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_vgrid1(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get hgtgrid1 ...
     varname='hgtgrid1'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_hgtgrid1))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_hgtgrid1(size(rank2,1),size(rank2,2),nfldsig))
         ges_hgtgrid1(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_hgtgrid1(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get sfrgrid ...
     varname='sfrgrid'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_sfrgrid))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_sfrgrid(size(rank2,1),size(rank2,2),nfldsig))
         ges_sfrgrid(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_sfrgrid(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tggrid ...
     varname='tggrid'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_tggrid))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tggrid(size(rank2,1),size(rank2,2),nfldsig))
         ges_tggrid(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_tggrid(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  end subroutine init_vars11_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

  subroutine final_vars11_
    if(allocated(ges_presgrid1)) deallocate(ges_presgrid1)
    if(allocated(ges_presgrid2)) deallocate(ges_presgrid2)
    if(allocated(ges_tmpgrid1)) deallocate(ges_tmpgrid1)
    if(allocated(ges_tmpgrid2)) deallocate(ges_tmpgrid2)
    if(allocated(ges_qgrid1)) deallocate(ges_qgrid1)
    if(allocated(ges_qgrid2)) deallocate(ges_qgrid2)
    !if(allocated(ges_ugrid1)) deallocate(ges_ugrid1)
    if(allocated(ges_vgrid1)) deallocate(ges_vgrid1)
    if(allocated(ges_hgtgrid1)) deallocate(ges_hgtgrid1)
    if(allocated(ges_sfrgrid)) deallocate(ges_sfrgrid)
    if(allocated(ges_tggrid)) deallocate(ges_tggrid)

  end subroutine final_vars11_

  end subroutine rtma_comp_fact10
