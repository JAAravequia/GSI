subroutine setuplight(lunin,mype,bwork,awork,nele,nobs,is,light_diagsave,eps)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuplight     compute rhs of oi for lightning flash rate
!   prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2015-07-06
!
! abstract:  For assimilation of lightning flash rate observations 
!            (GOES/GLM)
!            this routine:
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2015-07-06  k apodaca  -  first version of setuplight: 
!                             add lightflashrate, i.e. the subroutine including 
!                             the nonlinear lightning flash rate operator 
!   2015-07-08  m zupanski -  few updates regarding the subroutine calls
!   2015-07-08  m zupanski -  original calls for online bias correction
!
!   2016-05-01  k apodaca  -  updates regarding compatibility with the GFS model
!   2016-09-10  k apodaca  -  updates to the online bias correction procedure 
!   2017-02-28  k apodaca  -  updates for reading both WRF-ARW and GFS background 
!                             fields 
!   2018-02-07  k apodaca  -  replaced ob_type with polymorphic obsNode through type casting
!
!---
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
!   language: Fortran 90 and/or above
!   machine:  
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use guess_grids, only: hrdifsig,nfldsig,ntguessig
  use gridmod, only: dx_gfs
  use gridmod, only: region_dx,region_dy      ! dx, dy (:,:)
  use gridmod, only: region_dxi,region_dyi    ! inverse dx,dy (:,:)
  use gridmod, only: wrf_nmm_regional,wrf_mass_regional,nems_nmmb_regional
!--
  use gridmod, only: lat2,lon2,get_ij,nlat_sfc,nlon_sfc
  use gridmod, only: regional,nlat_regional,nlon_regional,nsig, &
                     eta1_ll,pt_ll,aeta1_ll
  use gridmod, only: latlon11
!--
  use gfs_stratosphere, only: nsig_save,deta1_save,aeta1_save
  use m_obsdiags, only: lighthead
  use obsmod, only: rmiss_single,i_light_ob_type,obsdiags,lobsdiagsave,&
                    nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: obs_diag,luse_obsdiag
  use m_obsNode, only: obsNode
  use m_lightNode, only: lightNode
  use m_obsLList, only: obsLList_appendNode
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use constants, only: zero,one,fv,grav,r1000, &
       tiny_r_kind,three,half,two,cg_term,huge_single,&
       wgtlim, rd, qcmin
  use constants, only: one_tenth,qmin,ten,t0c,five,r0_05
  use jfunc, only: jiter,jiterstart,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use lightinfo, only: nlighttype,nulight,gross_light,glermax,&
                       glermin,b_light,iuse_light,pg_light
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
!--
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle

  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_itype,mpi_sum
!--
!--

  implicit none

! Declare passed variables
  logical                                           ,intent(in   ) :: light_diagsave
  integer(i_kind)                                   ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)                ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nlighttype,5,3),intent(inout) :: bwork
  integer(i_kind)                                   ,intent(in   ) :: is ! ndat index

! Declare local parameter
  character(len=*),parameter:: myname="setuplight"


! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11,tintrp2a11_indx
  external:: stop2

! Declare local variables
  real(r_kind):: lightges0,lightges,grsmlt,dlat,dlon,dtime,obserror, &
                 obserrlm,residual,ratio,dlight
  real(r_kind) error,ddiff, light_diff, newdiff
  real(r_kind) ressw2,ress,scale,val2,val,valqc
  real(r_kind) rat_err2,exp_arg,term,ratio_errors,rwgt
  real(r_kind) cg_light,wgross,wnotgross,wgt,arg
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind),dimension(nsig_save) ::  deltasigma !For GFS
  real(r_kind),dimension(nsig_save) ::  sigma !For GFS
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(lat2,lon2,nfldsig)::rp2
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_single),allocatable,dimension(:,:)::diagbuf
  real(r_kind) tem4,indexw
! Local variables
  integer(i_kind)                   :: it,k,istatus,ier,nsig_read
  real(r_kind), pointer             :: flashrate  (:,:,:)  ! lightning flash rate
  real(r_kind), pointer             :: flashrate_h(:,:,:)  ! lightning flash rate
  real(r_kind), pointer             :: flashrate_tmp(:,:,:)  !
  real(r_kind), pointer             :: dx  (:,:)  ! 
  real(r_kind), pointer             :: dy  (:,:)  ! 
  logical,allocatable               :: wmaxflag(:,:,:)
  real(r_kind),allocatable          :: sigmadot(:,:,:,:)  !! vert. vel in sigma
!----
! Coefficients for derivative calculations

  real(r_kind),allocatable          :: jac_udx(:,:,:,:)
  real(r_kind),allocatable          :: jac_vdy(:,:,:,:)
  real(r_kind),allocatable          :: jac_zdx(:,:,:,:)
  real(r_kind),allocatable          :: jac_zdy(:,:,:,:)
  real(r_kind),allocatable          :: jac_frate(:,:,:)
  real(r_kind),allocatable          :: jac_vert(:)
  real(r_kind),allocatable          :: jac_vertt(:,:,:,:)
  real(r_kind),allocatable          :: jac_vertq(:,:,:,:)

  real(r_kind),allocatable          :: kvert(:,:,:)
  real(r_kind)                      :: sum_loc,sum_gbl
  integer(i_kind)                   :: nobs_loc,nobs_gbl
  real(r_kind)                      :: r0,w0
  real(r_kind),intent(inout)        :: eps
  real(r_kind)                      :: eps0
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)      :: cwgues

  real(r_kind),allocatable          :: wij(:)
  integer(i_kind),allocatable       :: ij(:)
  integer(i_kind),dimension(12)     :: light_ij
  integer(i_kind)                   :: ix,ixp,iy,iyp
  integer(i_kind)                   :: jtime,jtimep
!---
  integer(i_kind) ikxx,nn,ibin,ioff
  integer(i_kind) i,nchar,nreal,j,jj,ii,l,mm1,im,jm,km
  integer(i_kind) iret,iret_cw,nguess,ilon,ilat,ilight,id,itime,ikx,ilightmax,iqc
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,iobshgt,iobsprs
  integer(i_kind) idomsfc,iskint,iff10,isfcr

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed

  external:: mpi_allreduce

! File(s) for postprocessing
  character :: post_file*40
  character :: post_file2*40

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  integer(i_kind) :: istat
  class(obsNode),pointer:: my_node
  type(lightNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag


! Guess fields
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

! Guess of cloud fields

!-- Regional

  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qv
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_ql
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qi
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qg

!-- Global

  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr_it 

!--

  n_alloc(:)=0
  m_alloc(:)=0

  grsmlt=three  ! multiplier factor for gross check, an appropriate magnitude
                ! is yet to be determined.
  mm1=mype+1
  scale=one

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if (.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!--
! Retrieve cloud guess_tracer fields for the cloud mask applied in the 
! nonlinear lightning flash rate observation operator.
!--
     
! Regional 

     if (wrf_mass_regional.or.nems_nmmb_regional.or.regional) then
        nsig_read=nsig 

     if (ier==zero) then
       do jj=1,nfldsig
         do k=1,nsig
           do j=1,lon2
             do i=1,lat2
                cwgues(i,j,k,jj)=ges_ql(i,j,k,jj)+ges_qi(i,j,k,jj)+ges_qr(i,j,k,jj)+&
                              ges_qs(i,j,k,jj)+ges_qg(i,j,k,jj)
             enddo
           enddo
         enddo
        enddo
     end if

     do k=1,nsig_read
        deltasigma(k)=eta1_ll(k)-eta1_ll(k+1)
        sigma(k)=aeta1_ll(k)
     enddo

     
     dx(:,:)=region_dx(:,:)
     dy(:,:)=region_dy(:,:)
      
! Global
 
     else !If Global i.e. "if not (wrf_mass_regional.or.nems_nmmb_regional.or.regional)"

       nsig_read=nsig_save ! for GFS

       do jj=1,nfldsig
         do j=1,lon2
           do i=1,lat2
             do k=1,nsig
                cwgues(i,j,k,jj)=ges_cwmr_it(i,j,k,jj)
             enddo
           enddo
         enddo
       enddo

!--
! Define local indices
!--
     im=nlon_sfc
     jm=nlat_sfc
     km=nsig_read 
    
!--  
! Retrieve the model's sigma levels and the values for the difference between them
!--

     do k=1,nsig_read

        deltasigma(k)=deta1_save(k)
        sigma(k)=aeta1_save(k)

     enddo
!--
!  Resolution of the GFS grid in degrees for both, the latitudinal 
!  and longitudinal directions
!--
     allocate(dx(1:im,1:jm))
     allocate(dy(1:im,1:jm))
   
     do j=2,nlat_sfc/2
 
        dx(:,j)=dx_gfs(j)
        dy(:,j)=dx_gfs(j)

     enddo


     endif !  end regional/global block

!-- 
! Allocate local variables
!--

     allocate(flashrate  (1:im,1:jm,1:nfldsig))
     allocate(flashrate_h(1:im,1:jm,1:nfldsig))
     allocate(jac_frate  (1:im,1:jm,1:nfldsig))
     allocate(kvert      (1:im,1:jm,1:nfldsig))
     allocate(wmaxflag   (1:im,1:jm,1:nfldsig))
     allocate(sigmadot   (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_vert   (1:km-1))
     allocate(jac_zdx    (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_zdy    (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_udx    (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_vdy    (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_vertt  (1:im,1:jm,1:km-1,1:nfldsig))
     allocate(jac_vertq  (1:im,1:jm,1:km-1,1:nfldsig))

!******************************************************************************
! Read and reformat lightning observations in work arrays.
! Forward model for lightning flash rate
!-- loop over FGAT time
     do it=1,nfldsig
        call lightflashrate(im,jm,km,km-1,nfldsig,pt_ll,sigma(1:km-1),&
             deltasigma(1:km-1),dx(:,:),dy(:,:),ges_ps(:,:,it),&
             ges_z(:,:,it),cwgues(:,:,:,it),ges_tv(:,:,:,it),&
             ges_q(:,:,:,it),ges_u(:,:,:,it),ges_v(:,:,:,it),&
             jac_frate(:,:,it),jac_vert(:),jac_vertt(:,:,:,it),&
             jac_vertq(:,:,:,it),jac_zdx(:,:,:,it),jac_zdy(:,:,:,it),&
             jac_udx(:,:,:,it),jac_vdy(:,:,:,it),sigmadot(:,:,:,it),&
             kvert(:,:,it),wmaxflag(:,:,it),flashrate_h(:,:,it))
     enddo  

!-- 
! Prepare observed and modeled lightning flash rate at obs location
!--
  read(lunin)data,luse,ioid

!        index information for data array (see reading routine)

  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ilight=4    ! index of lightning observations
  itime=5     ! index of observation time in data array
  ikxx=6      ! index of ob type
  ilightmax=7 ! index of light max error
  iqc=8       ! index of quality mark
  ier2=9      ! index of original-original obs error ratio
  iuse=10     ! index of use parameter
  ilone=11    ! index of longitude (degrees)
  ilate=12    ! index of latitude (degrees)


     do i=1,nobs
        muse(i)=nint(data(11,i)) <= jiter
     enddo

     dup=one
     do k=1,nobs
       do l=k+1,nobs
         if (data(ilat,k) == data(ilat,l) .and.  &
            data(ilon,k) == data(ilon,l) .and. &
            data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
            muse(k) .and. muse(l)) then
            tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
            dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
            dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
         end if
       enddo
     enddo

! If requested, save selected data output into a diagnostic file
     if (light_diagsave) then
        nchar=1
        nreal=16
     if (lobsdiagsave) nreal=nreal+4*miter+1
        allocate(diagbuf(nreal,nobs))
        ii=0
     end if
!--
! Save some lightning flash rate values (observed, guess, no. of obs.)
! to compute the local sums inside "sumlightbias.f90," These are used 
! for bias correction.
!--
     write(post_file,199)mype
199 format('sums_lfr_',i3.3,'.bin')
     open(unit=200,file=trim(post_file),form='formatted',action='write')

!--
! Interpolation to obs location, for each observation
!--

     do i=1,nobs
        dtime=data(itime,i)
        call dtime_check(dtime, in_curbin, in_anybin)
        if (.not.in_anybin) cycle

        if (in_curbin) then
           dlat=data(ilat,i)
           dlon=data(ilon,i)

! Only for post-processing in real earth coordinates
!           dlon=data(11,i)
!           dlat=data(12,i)

           dlight=data(ilight,i)
           ikx = nint(data(ikxx,i))
           error=data(ier2,i)

           ratio_errors=error/data(ier,i)
           error=one/error

        endif ! (in_curbin)

        if (.not.in_curbin) cycle


! Interpolate (horizontally) model lightning flash rate to obs location
! (before bias correction)
 
            call tintrp2a11(flashrate_h,lightges0,dlat,dlon,dtime, &
                            hrdifsig,mype,nfldsig)

! Write lightning output to a file for bias correction

        write(200,*)i,dlight,lightges0
!--
! Optimal bias correction parameter for the lightning flash rate.
! The calculation is done inside the "setuprhsall.f90" subroutine.
!--

        if (miter.eq.1) then
           eps0=1.
        else
           eps0=eps
        endif

!      Uncomment for testing

!        if (mype.eq.0) then
!           write(*,*)miter,"setuplight: eps=",eps
!        endif

!--
! Bias-corrected flashrate: Use epsilon to adjust flash rate 
! from of the min/max values from the nonlinear lightning flash rate
! observation operator.
!-- 
        flashrate(:,:,:)=eps0*flashrate_h(:,:,:)

     enddo ! end loop over observations

! Interpolation to obs location, for each observation

     call dtime_setup()
     do i=1,nobs
        dtime=data(itime,i)
        call dtime_check(dtime, in_curbin, in_anybin)
        if (.not.in_anybin) cycle

        if (in_curbin) then
           dlat=data(ilat,i)
           dlon=data(ilon,i)

! Only for post-processing in real earth coordinates
!           dlon=data(11,i)
!           dlat=data(12,i)

           dlight=data(ilight,i)
           ikx = nint(data(ikxx,i))
           error=data(ier2,i)

           ratio_errors=error/data(ier,i)
           error=one/error


        endif ! (in_curbin)


!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,"Error nobs_bins,ibin= ",nobs_bins,ibin

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_light_ob_type,ibin)%head)) then 
              obsdiags(i_light_ob_type,ibin)%n_alloc = 0
              allocate(obsdiags(i_light_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)"setuplight: failure to allocate obsdiags",istat
                 call stop2(342)
              end if
              obsdiags(i_light_ob_type,ibin)%tail => obsdiags(i_light_ob_type,ibin)%head
           else
              allocate(obsdiags(i_light_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)"setuplight: failure to allocate obsdiags",istat
                 call stop2(343)
              end if
              obsdiags(i_light_ob_type,ibin)%tail => obsdiags(i_light_ob_type,ibin)%tail%next
           end if
           obsdiags(i_light_ob_type,ibin)%n_alloc = obsdiags(i_light_ob_type,ibin)%n_alloc +1

           allocate(obsdiags(i_light_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_light_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_light_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_light_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_light_ob_type,ibin)%tail%indxglb=ioid(i)
           obsdiags(i_light_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_light_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_light_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_light_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_light_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_light_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_light_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_light_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = ioid(i)
           my_diag%ich = 1
           my_diag%elat= data(ilate,i)
           my_diag%elon= data(ilone,i)
 
        else
           if (.not.associated(obsdiags(i_light_ob_type,ibin)%tail)) then
              obsdiags(i_light_ob_type,ibin)%tail => obsdiags(i_light_ob_type,ibin)%head
           else
              obsdiags(i_light_ob_type,ibin)%tail => obsdiags(i_light_ob_type,ibin)%tail%next
           end if
           if (.not.associated(obsdiags(i_light_ob_type,ibin)%tail)) then
              call die(myname,'.not.associated(obsdiags(i_light_ob_type,ibin)%tail)')
           end if
           if (obsdiags(i_light_ob_type,ibin)%tail%indxglb/=ioid(i)) then
              write(6,*)"SETUPLIGHT: index error"
              call stop2(344)
           end if
        endif
     endif
    
     if (.not.in_curbin) cycle

!-- Interpolate bias-corrected model Lightning flash rate to obs location
     call tintrp2a11(flashrate,lightges,dlat,dlon,dtime,&
                     hrdifsig,mype,nfldsig)

!------------------------------------------------------------------
! Write information into a file for post-processing.
!------------------------------------------------------------------
!     post_file2='mod_lfr2.bin'
!     write(post_file2,198)mype
!     198 format('mod_lfr2_ ',i3.3,'.bin')
!     open(unit=201,file=trim(post_file2),form='formatted',action='write')
!       write(201,*)dlat,dlon,lightges
!     close(unit=201,status='keep')
!------------------------------------------------------------------

!--
! Calculation of the innovation (OBS-GUESS)
!--
     ddiff = dlight - lightges

!--
!    Gross checks using the innovation
!-- 
     residual = abs(ddiff)
     if (residual>grsmlt*data(ilightmax,i)) then
        error = zero
        ratio_errors=zero
        if (luse(i)) awork(7) = awork(7)+one
     end if
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(glermin(ikx),min(glermax(ikx),obserror))
     ratio    = residual/obserrlm
     if (ratio > gross_light(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if
! 
     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false. 
     if (nobskeep>0.and.luse_obsdiag) muse(i)=obsdiags(i_light_ob_type,ibin)%tail%muse(nobskeep)

         val = error*ddiff

     if (luse(i)) then

!    Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (pg_light(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-pg_light(ikx)
           cg_light=b_light(ikx)
           wgross = cg_term*pg_light(ikx)/(cg_light*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

! Accumulate statistics as a function of observation type
        ress  = ddiff*scale
        ressw2= ress*ress
        val2  = val*val
        rat_err2 = ratio_errors**2
!       Accumulate statistics for obs belonging to this task
        if (muse(i) ) then
           if(rwgt < one) awork(21) = awork(21)+one
           awork(5) = awork(5)+val2*rat_err2
           awork(4) = awork(4)+one
           awork(22)=awork(22)+valqc
           nn=1
        else
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one             ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress            ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2          ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2   ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc           ! nonlin qc penalty
     
     end if


!    Fill obs diagnostics structure

     if (luse_obsdiag) then
        obsdiags(i_light_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_light_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_light_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     
     if ( .not. last .and. muse(i)) then 

        allocate(my_head)
        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head
        call obsLList_appendNode(lighthead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)
!                .      .    .                                       .

! In the case of lightning observations (e.g. GOES/GLM), the schematic shown below is
! used for the interpolation of background fields to the location of an observation (+)
! and for the finite-difference derivation method used in the calculation of the TL of
! the observation operator for lightning flash rate. Calculations are done at each
! quadrant (i.e. central, north, south, east, and west).
!
!         i6-------i8
!          |       |
!          |       |
! i10-----i2-------i4------i12
!  |       |       |       |
!  |       |     + |       |
! i9------i1-------i3------i11
!          |       |
!          |       |
!         i5-------i7
!

!                .      .    .                                       .

! Begin preparing information for intlight

        allocate(my_head%jac_z0i1, my_head%jac_z0i2, my_head%jac_z0i3, &
                 my_head%jac_z0i4, my_head%jac_z0i5, my_head%jac_z0i6, &
                 my_head%jac_z0i7, my_head%jac_z0i8, my_head%jac_z0i9, &
                 my_head%jac_z0i10,my_head%jac_z0i11,my_head%jac_z0i12,&
                 my_head%jac_vertqi1(nsig), my_head%jac_vertqi2(nsig), &
                 my_head%jac_vertqi3(nsig), my_head%jac_vertqi4(nsig), &
                 my_head%jac_vertqi5(nsig), my_head%jac_vertqi6(nsig), &
                 my_head%jac_vertqi7(nsig), my_head%jac_vertqi8(nsig), &
                 my_head%jac_vertqi9(nsig), my_head%jac_vertqi10(nsig),&
                 my_head%jac_vertqi11(nsig),my_head%jac_vertqi12(nsig),&
                 my_head%jac_vertti1(nsig), my_head%jac_vertti2(nsig), &
                 my_head%jac_vertti3(nsig), my_head%jac_vertti4(nsig), &
                 my_head%jac_vertti5(nsig), my_head%jac_vertti6(nsig), &
                 my_head%jac_vertti7(nsig), my_head%jac_vertti8(nsig), &
                 my_head%jac_vertti9(nsig), my_head%jac_vertti10(nsig),&
                 my_head%jac_vertti11(nsig),my_head%jac_vertti12(nsig),&
                 my_head%jac_zdxi1(nsig),   my_head%jac_zdxi2(nsig),   &
                 my_head%jac_zdxi3(nsig),   my_head%jac_zdxi4(nsig),   &
                 my_head%jac_zdyi1(nsig),   my_head%jac_zdyi2(nsig),   &
                 my_head%jac_zdyi3(nsig),   my_head%jac_zdyi4(nsig),   &
                 my_head%jac_udxi1(nsig),   my_head%jac_udxi2(nsig),   &
                 my_head%jac_udxi3(nsig),   my_head%jac_udxi4(nsig),   &
                 my_head%jac_vdyi1(nsig),   my_head%jac_vdyi2(nsig),   &
                 my_head%jac_vdyi3(nsig),   my_head%jac_vdyi4(nsig),   &
                 my_head%jac_vert(nsig),    my_head%jac_sigdoti1(nsig),&
                 my_head%jac_sigdoti2(nsig),my_head%jac_sigdoti3(nsig),&
                 my_head%jac_sigdoti4(nsig),my_head%jac_qi1(nsig),     &
                 my_head%jac_qi2(nsig),     my_head%jac_qi3(nsig),     & 
                 my_head%jac_qi4(nsig),     my_head%jac_qi5(nsig),     &
                 my_head%jac_qi6(nsig),     my_head%jac_qi7(nsig),     &
                 my_head%jac_qi8(nsig),     my_head%jac_qi9(nsig),     &
                 my_head%jac_qi10(nsig),    my_head%jac_qi11(nsig),    &
                 my_head%jac_qi12(nsig),    my_head%jac_ti1(nsig),     &
                 my_head%jac_ti2(nsig),     my_head%jac_ti3(nsig),     &
                 my_head%jac_ti4(nsig),     my_head%jac_ti5(nsig),     &
                 my_head%jac_ti6(nsig),     my_head%jac_ti7(nsig),     &
                 my_head%jac_ti8(nsig),     my_head%jac_ti9(nsig),     &
                 my_head%jac_ti10(nsig),    my_head%jac_ti11(nsig),    &
                 my_head%jac_ti12(nsig),    my_head%jac_kverti1,       &
                 my_head%jac_kverti2,       my_head%jac_kverti3,       &
                 my_head%jac_kverti4,       my_head%jac_fratei1,       &
                 my_head%jac_fratei2,       my_head%jac_fratei3,       &
                 my_head%jac_fratei4,       my_head%jac_wmaxflagi1,    &
                 my_head%jac_wmaxflagi2,    my_head%jac_wmaxflagi3,    &
                 my_head%jac_wmaxflagi4,    my_head%ij(12,nsig),stat=istat)
        if (istatus/=0) write(6,*)" setuplight: failure to allocate lighttail_jacs, istat=",istat

!       Set (i,j) indices of guess gridpoint that bound obs location

        call get_ij(mm1,dlat,dlon,light_ij,my_head%wij(1))

        do k=1,nsig
           my_head%ij(1,k)=light_ij(1)+(k-1)*latlon11
           my_head%ij(2,k)=light_ij(2)+(k-1)*latlon11
           my_head%ij(3,k)=light_ij(3)+(k-1)*latlon11
           my_head%ij(4,k)=light_ij(4)+(k-1)*latlon11
        enddo

        call get_ij(mm1,dlat-one,dlon,light_ij,my_head%wij(1))

        do k=1,nsig
           my_head%ij(5,k)=light_ij(1)+(k-1)*latlon11
           my_head%ij(7,k)=light_ij(3)+(k-1)*latlon11
        enddo

        call get_ij(mm1,dlat+one,dlon,light_ij,my_head%wij(1))

        do k=1,nsig
           my_head%ij(6,k)=light_ij(2)+(k-1)*latlon11
           my_head%ij(8,k)=light_ij(4)+(k-1)*latlon11
        enddo

        call get_ij(mm1,dlat,dlon-one,light_ij,my_head%wij(1))

        do k=1,nsig
           my_head%ij(9,k)=light_ij(1)+(k-1)*latlon11
           my_head%ij(10,k)=light_ij(2)+(k-1)*latlon11
        enddo

        call get_ij(mm1,dlat,dlon+one,light_ij,my_head%wij(1))

        do k=1,nsig
           my_head%ij(11,k)=light_ij(3)+(k-1)*latlon11
           my_head%ij(12,k)=light_ij(4)+(k-1)*latlon11
        enddo

!-- Find indices at each quadrant surrounding each observation.
!-- Interpolate the "Jacobian" coefficients to any given observation 
!   location and for all quadrants. These are used in the tangent
!   linear and adjoint of the lightning flash rate observation
!   operator.
!----------------------

!-- (1) central quadrant

        call tintrp2a11_indx(dlat,dlon,dtime,hrdifsig,mype,&
                             nfldsig,ix,ixp,iy,iyp,jtime,jtimep)

!-- save coefficients

        my_head%jac_vert(:)=zero
        do k=1,nsig_read
             my_head%jac_vert(k)=jac_vert(k)
        enddo ! k=1,nsig_read

!- the variables below are only needed at 4 central points

        my_head%jac_z0i1=ges_z(ix ,iy ,jtime)
        my_head%jac_z0i2=ges_z(ix ,iyp,jtime)
        my_head%jac_z0i3=ges_z(ixp,iy ,jtime)
        my_head%jac_z0i4=ges_z(ixp,iyp,jtime)

        my_head%jac_wmaxflagi1=wmaxflag(ix ,iy ,jtime)
        my_head%jac_wmaxflagi2=wmaxflag(ix ,iyp,jtime)
        my_head%jac_wmaxflagi3=wmaxflag(ixp,iy ,jtime)
        my_head%jac_wmaxflagi4=wmaxflag(ixp,iyp,jtime)

        my_head%jac_kverti1=kvert(ix ,iy ,jtime)
        my_head%jac_kverti2=kvert(ix ,iyp,jtime)
        my_head%jac_kverti3=kvert(ixp,iy ,jtime)
        my_head%jac_kverti4=kvert(ixp,iyp,jtime)

        my_head%jac_fratei1=jac_frate(ix ,iy ,jtime)
        my_head%jac_fratei2=jac_frate(ix ,iyp,jtime)
        my_head%jac_fratei3=jac_frate(ixp,iy ,jtime)
        my_head%jac_fratei4=jac_frate(ixp,iyp,jtime)

!---
!--- Initialize some variables

        my_head%jac_qi1(:)=zero
        my_head%jac_qi2(:)=zero
        my_head%jac_qi3(:)=zero
        my_head%jac_qi4(:)=zero

        my_head%jac_ti1(:)=zero
        my_head%jac_ti2(:)=zero
        my_head%jac_ti3(:)=zero
        my_head%jac_ti4(:)=zero

        my_head%jac_zdxi1(:)=zero
        my_head%jac_zdxi2(:)=zero
        my_head%jac_zdxi3(:)=zero
        my_head%jac_zdxi4(:)=zero

        my_head%jac_zdyi1(:)=zero
        my_head%jac_zdyi2(:)=zero
        my_head%jac_zdyi3(:)=zero
        my_head%jac_zdyi4(:)=zero

        my_head%jac_udxi1(:)=zero
        my_head%jac_udxi2(:)=zero
        my_head%jac_udxi3(:)=zero
        my_head%jac_udxi4(:)=zero

        my_head%jac_vdyi1(:)=zero
        my_head%jac_vdyi2(:)=zero
        my_head%jac_vdyi3(:)=zero
        my_head%jac_vdyi4(:)=zero

        my_head%jac_vertti1(:)=zero
        my_head%jac_vertti2(:)=zero
        my_head%jac_vertti3(:)=zero
        my_head%jac_vertti4(:)=zero

        my_head%jac_vertqi1(:)=zero
        my_head%jac_vertqi2(:)=zero
        my_head%jac_vertqi3(:)=zero
        my_head%jac_vertqi4(:)=zero


     do k=1,nsig_read
        my_head%jac_qi1(k)=ges_q(ix ,iy ,k,jtime)
        my_head%jac_qi2(k)=ges_q(ix ,iyp,k,jtime)
        my_head%jac_qi3(k)=ges_q(ixp,iy ,k,jtime)
        my_head%jac_qi4(k)=ges_q(ixp,iyp,k,jtime)
        my_head%jac_ti1(k)=ges_tv(ix ,iy ,k,jtime)
        my_head%jac_ti2(k)=ges_tv(ix ,iyp,k,jtime)
        my_head%jac_ti3(k)=ges_tv(ixp,iy ,k,jtime)
        my_head%jac_ti4(k)=ges_tv(ixp,iyp,k,jtime)
        my_head%jac_sigdoti1(k)=sigmadot(ix ,iy ,k,jtime)
        my_head%jac_sigdoti2(k)=sigmadot(ix ,iyp,k,jtime)
        my_head%jac_sigdoti3(k)=sigmadot(ixp,iy ,k,jtime)
        my_head%jac_sigdoti4(k)=sigmadot(ixp,iyp,k,jtime)
        my_head%jac_zdxi1(k)=jac_zdx(ix ,iy ,k,jtime)
        my_head%jac_zdxi2(k)=jac_zdx(ix ,iyp,k,jtime)
        my_head%jac_zdxi3(k)=jac_zdx(ixp,iy ,k,jtime)
        my_head%jac_zdxi4(k)=jac_zdx(ixp,iyp,k,jtime)
        my_head%jac_zdyi1(k)=jac_zdy(ix ,iy ,k,jtime)
        my_head%jac_zdyi2(k)=jac_zdy(ix ,iyp,k,jtime)
        my_head%jac_zdyi3(k)=jac_zdy(ixp,iy ,k,jtime)
        my_head%jac_zdyi4(k)=jac_zdy(ixp,iyp,k,jtime)
        my_head%jac_udxi1(k)=jac_udx(ix ,iy ,k,jtime)
        my_head%jac_udxi2(k)=jac_udx(ix ,iyp,k,jtime)
        my_head%jac_udxi3(k)=jac_udx(ixp,iy ,k,jtime)
        my_head%jac_udxi4(k)=jac_udx(ixp,iyp,k,jtime)
        my_head%jac_vdyi1(k)=jac_vdy(ix ,iy ,k,jtime)
        my_head%jac_vdyi2(k)=jac_vdy(ix ,iyp,k,jtime)
        my_head%jac_vdyi3(k)=jac_vdy(ixp,iy ,k,jtime)
        my_head%jac_vdyi4(k)=jac_vdy(ixp,iyp,k,jtime)
        my_head%jac_vertti1(k)=jac_vertt(ix ,iy ,k,jtime)
        my_head%jac_vertti2(k)=jac_vertt(ix ,iyp,k,jtime)
        my_head%jac_vertti3(k)=jac_vertt(ixp,iy ,k,jtime)
        my_head%jac_vertti4(k)=jac_vertt(ixp,iyp,k,jtime)
     enddo ! k=1,nsig_read

     do k=1,nsig_read-1
        my_head%jac_vertqi1(k)=jac_vertq(ix ,iy ,k,jtime)
        my_head%jac_vertqi2(k)=jac_vertq(ix ,iyp,k,jtime)
        my_head%jac_vertqi3(k)=jac_vertq(ixp,iy ,k,jtime)
        my_head%jac_vertqi4(k)=jac_vertq(ixp,iyp,k,jtime)
     enddo ! k=1,nsig_read-1

!-- (2) south quadrant

     call tintrp2a11_indx(dlat-one,dlon,dtime, &
          hrdifsig,mype,nfldsig,ix,ixp,iy,iyp,jtime,jtimep)


!-- save coefficients

     do k=1,nsig_read-1
        my_head%jac_z0i5=ges_z(ix ,iy ,jtime)
        my_head%jac_z0i7=ges_z(ixp,iyp ,jtime)
        my_head%jac_vertti5(k)=jac_vertt(ix ,iy, k,jtime)
        my_head%jac_vertti7(k)=jac_vertt(ixp ,iy, k,jtime)
        my_head%jac_vertqi5(k)=jac_vertq(ix ,iy, k,jtime)
        my_head%jac_vertqi7(k)=jac_vertq(ixp ,iy, k,jtime)
        my_head%jac_qi5(k)=ges_q(ix ,iy ,k,jtime)
        my_head%jac_qi7(k)=ges_q(ixp,iy ,k,jtime)
        my_head%jac_ti5(k)=ges_tv(ix ,iy ,k,jtime)
        my_head%jac_ti7(k)=ges_tv(ixp,iy ,k,jtime)
     enddo ! k=1,nsig_read-1

!----------------------
!-- (3) north quadrant

     call tintrp2a11_indx(dlat+one,dlon,dtime, &
          hrdifsig,mype,nfldsig,ix,ixp,iy,iyp,jtime,jtimep)


!-- save coefficients

     do k=1,nsig_read-1
        my_head%jac_z0i6=ges_z(ix ,iyp,jtime)
        my_head%jac_z0i8=ges_z(ixp,iyp,jtime)
        my_head%jac_vertti6(k)=jac_vertt(ix ,iyp,k,jtime)
        my_head%jac_vertti8(k)=jac_vertt(ixp,iyp,k,jtime)
        my_head%jac_vertqi6(k)=jac_vertq(ix ,iyp,k,jtime)
        my_head%jac_vertqi8(k)=jac_vertq(ixp,iyp,k,jtime)
        my_head%jac_qi6(k)=ges_q(ix ,iyp,k,jtime)
        my_head%jac_qi8(k)=ges_q(ixp,iyp,k,jtime)
        my_head%jac_ti6(k)=ges_tv(ix ,iyp,k,jtime)
        my_head%jac_ti8(k)=ges_tv(ixp,iyp,k,jtime)
     enddo ! k=1,nsig_read-1

!----------------------
!-- (4) west quadrant

     call tintrp2a11_indx(dlat,dlon-one,dtime, &
          hrdifsig,mype,nfldsig,ix,ixp,iy,iyp,jtime,jtimep)

!-- save coefficients

     do k=1,nsig_read-1
        my_head%jac_z0i9 =ges_z(ix ,iy ,jtime)
        my_head%jac_z0i10=ges_z(ix ,iyp,jtime)
        my_head%jac_vertti9(k)=jac_vertt(ix ,iy,k ,jtime)
        my_head%jac_vertti10(k)=jac_vertt(ix ,iy,k ,jtime)
        my_head%jac_vertqi9(k)=jac_vertq(ix ,iy,k ,jtime)
        my_head%jac_vertqi10(k)=jac_vertq(ix ,iy,k ,jtime)
        my_head%jac_qi9 (k)=ges_q(ix ,iy ,k,jtime)
        my_head%jac_qi10(k)=ges_q(ix ,iyp,k,jtime)
        my_head%jac_ti9 (k)=ges_tv(ix ,iy ,k,jtime)
        my_head%jac_ti10(k)=ges_tv(ix ,iyp,k,jtime)
     enddo ! k=1,nsig_read-1

!----------------------
!-- (5) east quadrant

     call tintrp2a11_indx(dlat,dlon+one,dtime, &
          hrdifsig,mype,nfldsig,ix,ixp,iy,iyp,jtime,jtimep)

!-- save coefficients

     do k=1,nsig_read-1
        my_head%jac_z0i11=ges_z(ixp,iy ,jtime)
        my_head%jac_z0i12=ges_z(ixp,iyp,jtime)
        my_head%jac_vertti11(k)=jac_vertt(ixp,iy,k ,jtime)
        my_head%jac_vertti12(k)=jac_vertt(ixp,iyp,k,jtime)
        my_head%jac_vertqi11(k)=jac_vertq(ixp,iy,k ,jtime)
        my_head%jac_vertqi12(k)=jac_vertq(ixp,iyp,k,jtime)
        my_head%jac_qi11(k)=ges_q(ixp,iy ,k,jtime)
        my_head%jac_qi12(k)=ges_q(ixp,iyp,k,jtime)
        my_head%jac_ti11(k)=ges_tv(ixp,iy ,k,jtime)
        my_head%jac_ti12(k)=ges_tv(ixp,iyp,k,jtime)
     enddo ! k=1,nsig_read-1

!--------------------------------------------------
        my_head%res    = ddiff
        my_head%err2   = error**2
        my_head%raterr2= ratio_errors**2
        my_head%time   = dtime
        my_head%b      = b_light(ikx)
        my_head%pg     = pg_light(ikx)
        my_head%luse   = luse(i)

! End preparing observation information for intlight
!                .      .    .                                       .
        if (luse_obsdiag) then
           my_head%diags => obsdiags(i_light_ob_type,ibin)%tail

           my_diag => my_head%diags
           if (my_head%idv /= my_diag%idv .or. &
               my_head%iob /= my_diag%iob ) then
               call perr(myname,"mismatching %[head,diags]%(idv,iob,ibin) =", &
                        (/is,i,ibin/))
               call perr(myname,"my_head%(idv,iob) =",(/my_head%idv,my_head%iob/))
               call perr(myname,"my_diag%(idv,iob) =",(/my_diag%idv,my_diag%iob/))
               call die(myname)
           endif
        endif

        my_head => null()
    endif  !( .not. last .and. muse(i))

!    Save selected output to a diagnostic file
     if (light_diagsave .and. luse(i)) then
        ii=ii+1

        diagbuf(1,ii)  = data(ier,i)        ! observation error
        diagbuf(2,ii)  = data(ilate,i)      ! observation latitude (degrees)
        diagbuf(3,ii)  = data(ilone,i)      ! observation longitude (degrees)
        diagbuf(4,ii)  = dlight             ! total lightning obs (#hits/km**2*hr)
        diagbuf(5,ii)  = dtime              ! observation time
        diagbuf(6,ii)  = data(iqc,i)        ! input glmbufr qc or event mark
        diagbuf(7,ii) = data(ier2,i)        ! index of original-original obs error
        diagbuf(8,ii) = data(iuse,i)        ! read_glmbufr data usage flag-Changed from 11 to 13
        if(muse(i)) then
           diagbuf(9,ii) = one              ! analysis usage flag (1=use, -1=not used)
        else                                 ! changed order from 12 to 11
           diagbuf(9,ii) = -one
        endif

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

        diagbuf(10,ii) = rwgt               ! nonlinear qc relative weight
        diagbuf(11,ii) = errinv_input       ! glmbufr inverse obs error
        diagbuf(12,ii) = errinv_adjst       ! read_glmbufr inverse obs error
        diagbuf(13,ii) = errinv_final       ! final inverse observation error

        diagbuf(14,ii) = ddiff              ! obs-ges used in analysis (#hits/km2*hr)
        diagbuf(15,ii) = dlight-lightges0   ! obs-ges w/o bias correction (#hits/km2*hr) 
        if (lobsdiagsave) then
           ioff=16
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_light_ob_type,ibin)%tail%muse(jj)) then
                 diagbuf(ioff,ii) = one
              else
                 diagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              diagbuf(ioff,ii) = obsdiags(i_light_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              diagbuf(ioff,ii) = obsdiags(i_light_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              diagbuf(ioff,ii) = obsdiags(i_light_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

     end if


  !    End of loop over observations
  enddo !nobs   

! Release memory of local guess arrays
  call final_vars_


! Close file with lightning information for bias correction

     close(unit=200,status='keep')
   
! Write information to a diagnostics file

  if(light_diagsave .and. ii>0)then
     call dtime_show(myname,"diagsave:goes_glm",i_light_ob_type)
     write(55)" light",nchar,nreal,ii,mype
     write(55)diagbuf(:,1:ii)
     deallocate(diagbuf)
  end if

      deallocate(flashrate)
      deallocate(flashrate_h)
      deallocate(jac_frate)
      deallocate(kvert)
      deallocate(wmaxflag)
      deallocate(sigmadot)
      deallocate(dx)
      deallocate(dy)

      deallocate(jac_vertt)
      deallocate(jac_vertq)
      deallocate(jac_zdx)
      deallocate(jac_zdy)
      deallocate(jac_udx)
      deallocate(jac_vdy)
      
! End of routine

  return
  contains

 subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0

!--
! Retrieve cloud guess_tracer fields for the cloud mask applied in the
! nonlinear lightning flash rate observation operator.
!--

! Get the pointer to cloud  mixing ratios from the guess at time index "it"

! Regional and/or 6-class microphysics

     if (wrf_mass_regional.or.nems_nmmb_regional.or.regional) then
       call gsi_metguess_get ('var::qv' , ivar, istatus )
       proceed=ivar>0
       call gsi_metguess_get ('var::ql' , ivar, istatus )
       proceed=ivar>0
       call gsi_metguess_get ('var::qr', ivar, istatus )
       proceed=proceed.and.ivar>0
       call gsi_metguess_get ('var::qi' , ivar, istatus )
       proceed=ivar>0
       call gsi_metguess_get ('var::qs', ivar, istatus )
       proceed=proceed.and.ivar>0
       call gsi_metguess_get ('var::qg', ivar, istatus )
       proceed=proceed.and.ivar>0

! Global 

     else
       call gsi_metguess_get ('var::cw', ivar, istatus )
       proceed=proceed.and.ivar>0

     endif 

  end subroutine check_vars_
  
  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
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

! Regional

     if (wrf_mass_regional.or.nems_nmmb_regional.or.regional) then
       !    get qv ...
       varname='qv'
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
       
       !    get ql ...
       varname='ql'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
       if (istatus==0) then
           if(allocated(ges_ql))then
              write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
              call stop2(999)
           endif
           allocate(ges_ql(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_ql(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
              ges_ql(:,:,:,ifld)=rank3
           enddo
       else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
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
       !    get qi ...
       varname='qi'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
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

! Global

     else
       !    get cw ...
       varname='cw'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
       if (istatus==0) then
           if(allocated(ges_cwmr_it))then
              write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
              call stop2(999)
           endif
           allocate(ges_cwmr_it(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_cwmr_it(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
              ges_cwmr_it(:,:,:,ifld)=rank3
           enddo
       else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
       endif       
     endif

  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_qv)) deallocate(ges_qv )
    if(allocated(ges_ql)) deallocate(ges_ql )
    if(allocated(ges_qr)) deallocate(ges_qr )
    if(allocated(ges_qi)) deallocate(ges_qi )
    if(allocated(ges_qs)) deallocate(ges_qs )    
    if(allocated(ges_qv)) deallocate(ges_qv )
    if(allocated(ges_cwmr_it)) deallocate(ges_cwmr_it )
  end subroutine final_vars_

end subroutine setuplight


!                .      .    .                                       .

  subroutine lightflashrate(imax,jmax,kmax,kmax_q,ntime,pt_ll,sigma,deltasigma, &
                           dx,dy,ps,z0,cwm,t,q,u,v,jac_frate,jac_vert,jac_vertt,&
                           jac_vertq,jac_zdi,jac_zdy,jac_udx,jac_vdy,sigmadot,  &
                           kvert,wmaxflag,flashrate)

!$$$  documentation block
!                .      .    .                                       .
! subroutine:    lightflashrate     nonlinear lightning flash rate model
!   prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group 
!     date: 2015-07-06
!
! abstract:  Model for the calculation of lightning flash rate. 
!            The calculation starts with the derivation of vertical 
!            velocity from a modified version of the continuity equation 
!            (Janjic et al, 2010), as in Apodaca et al. (2014).
!            The final regresion formula for lightning flash rate is a
!            function of maximum vertical velocity and it is based on 
!            Barthe et al. (2010).   

  use kinds, only: r_kind,r_single,r_double,i_kind
  use constants, only: zero,one,one_tenth,two,three,half
  use constants, only: fv,rd,grav,qmin,ten,t0c,five,r0_05

  implicit none

!------------------------------------------------------
! Define constants, parameters, and variables
!------------------------------------------------------

!-- input
  integer(i_kind)                                           :: imax,jmax,kmax
  integer(i_kind)                                           :: kmax_q
  integer(i_kind)                                           :: ntime
  real(r_kind),intent(out),dimension(1:imax,1:jmax)         :: kvert
  real(r_kind),intent(in),dimension(1:imax,1:jmax,1:kmax_q) :: cwm    !! Total cloud condensate
  real(r_kind),intent(in),dimension(1:imax,1:jmax,1:kmax_q) :: t      !! Temperature
  real(r_kind),intent(in),dimension(1:imax,1:jmax,1:kmax_q) :: q      !! Specific humidity
  real(r_kind),intent(in),dimension(1:imax,1:jmax,1:kmax_q) :: u      !! U-component of the wind
  real(r_kind),intent(in),dimension(1:imax,1:jmax,1:kmax_q) :: v      !! V-component of the wind

  real(r_kind),intent(in),dimension(1:imax,1:jmax)          :: dx     !! Latitudinal grid distance
  real(r_kind),intent(in),dimension(1:imax,1:jmax)          :: dy     !! Longitudinal grid distance
  real(r_kind),intent(in),dimension(1:imax,1:jmax)          :: z0     !! surface height
  real(r_kind),intent(in),dimension(1:imax,1:jmax)          :: ps     !! surface pressure

  real(r_kind),intent(in)                                  :: pt_ll   !! hydrostatic top pressure 
  real(r_kind),intent(in),dimension(1:kmax_q)              :: sigma       !! Sigma levels
  real(r_kind),intent(in),dimension(1:kmax_q)              :: deltasigma  !! Difference between sigma levels

!-- output
  real(r_kind),intent(out),dimension(1:imax,1:jmax)        :: flashrate   !! Lightning flash rate
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_udx
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_vdy
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_zdi
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_zdy


  real(r_kind),intent(out),dimension(1:imax,1:jmax)           :: jac_frate
  real(r_kind),intent(out),dimension(1:kmax_q)                :: jac_vert
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_vertt
  real(r_kind),intent(out),dimension(1:imax,1:jmax,1:kmax_q)  :: jac_vertq

!-----------------------------------------------

  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: horiz_adv !! Horizontal advection 
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: vert_adv  !! Vertical advection
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: z
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: w        !! Vertical velocity

  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: sigmadot

  real(r_kind),dimension(1:imax,1:jmax)                     :: ddx
  real(r_kind),dimension(1:imax,1:jmax)                     :: ddy
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: pu1
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: pu2
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: pv1
  real(r_kind),dimension(1:imax,1:jmax,1:kmax_q)            :: pv2

  real(r_kind)                                              :: sum1      !! Integral1 in sigmadot
  real(r_kind)                                              :: sum2      !! Integral2 in sigmadot

  integer(i_kind)                                :: ismooth,jsmooth
  integer(i_kind)                                :: istart,iend
  integer(i_kind)                                :: jstart,jend
  integer(i_kind)                                :: nsig_read
!------------------------------------------------------
! Variable declaration for the cloud mask flag
!------------------------------------------------------

  integer(i_kind)        :: i,j,k
  integer(i_kind)        :: ii,jj,kk

!-- parameters

  integer(i_kind), parameter :: idiff=2  !for avg and cloud detec. (=0=>no averaging)
  integer(i_kind), parameter :: jdiff=2  !for avg and cloud detec. (=0=>no averaging)
  integer(i_kind)            :: kdiff    !for avg and cloud detec. (=0=>no averaging)

  !real(r_kind), parameter    :: cwm_threshold=1.e-5 !threshold condition for cloud det.
  real(r_kind), parameter    :: cwm_threshold=1.e-15 !threshold condition for cloud det.
  integer(i_kind),dimension(1:imax,1:jmax,1:kmax_q)    :: cldflag
  logical,intent(out),dimension(1:imax,1:jmax)   :: wmaxflag
  integer(i_kind) :: numcld
!------------------------------------------------------
!  wmax, obs_ges

  real(r_kind),parameter :: wpower=4.5      !! regression power parameter
  real(r_kind),parameter :: wcnst=5.e-6     !! regression multiplication parameter
  real(r_kind)           :: wmax

!  Optional output file(s)
  character :: nonlh_file*40

!-- prepare some coefficients

     do i=1,imax
       do j=1,jmax
          ddx(i,j)=one/(two*dx(i,j))
          ddy(i,j)=one/(two*dy(i,j))
       enddo !! do j=1,jmax
     enddo !! do i=1,imax


     jac_vert(:)=zero

     do k=1,kmax_q
        jac_vert(k)=(rd/grav)*(deltasigma(k)/sigma(k))
     enddo  !! do k=1,kmax_q

     jac_vertt(:,:,:)=zero
     jac_vertq(:,:,:)=zero

     do i=1,imax
       do j=1,jmax
         do k=1,kmax_q
            jac_vertt(i,j,k)=jac_vert(k)*(one+fv*q(i,j,k))
            jac_vertq(i,j,k)=jac_vert(k)*(fv*t(i,j,k))
         enddo  !! do k=1,kmax_q
       enddo !! do j=1,jmax
     enddo !! do i=1,imax

! Virtual Temperature (Tv) is given by: tv=t*(1+0.61*q)
! Discretization of the height derivative

     z(:,:,:)=zero

     do i=1,imax
       do j=1,jmax
          z(i,j,1) = z0(i,j)
         do k=2,kmax_q
            z(i,j,k) = z(i,j,k-1)+jac_vert(k)*t(i,j,k)*(one+fv*q(i,j,k))
         enddo
       enddo !! do j=1,jmax
     enddo !! do i=1,imax

     ismooth=1
     jsmooth=1
     istart=1+ismooth
     iend=imax-ismooth
     jstart=1+jsmooth
     jend=jmax-jsmooth

! Horizontal advection in the vertical velocity calculation

     horiz_adv(:,:,:)=zero
     do i=istart,iend
       do j=jstart,jend
         do k=2,kmax_q
            horiz_adv(i,j,k)=(u(i,j,k)*ddx(i,j))*(z(i+1,j,k)-z(i-1,j,k)) &
                            +(v(i,j,k)*ddy(i,j))*(z(i,j+1,k)-z(i,j-1,k))
         enddo  !! do k=1,kmax_q
         horiz_adv(i,j,1) = horiz_adv(i,j,2)
       enddo  !! do j=jstart,jend
     enddo  !! do i=istart,iend
         horiz_adv(1,:,:) = horiz_adv(2,:,:)
         horiz_adv(imax,:,:) = horiz_adv(imax-1,:,:)
         horiz_adv(:,1,:) = horiz_adv(:,2,:)
         horiz_adv(:,jmax,:) = horiz_adv(:,jmax-1,:)

! Additional coefficients

     jac_zdi(:,:,:)=zero
     jac_zdy(:,:,:)=zero
     jac_udx(:,:,:)=zero
     jac_vdy(:,:,:)=zero

     do i=istart,iend
       do j=jstart,jend
         do k=2,kmax_q
            jac_zdi(i,j,k)=(z(i+1,j,k)-z(i-1,j,k))*ddx(i,j)
            jac_zdy(i,j,k)=(z(i,j+1,k)-z(i,j-1,k))*ddy(i,j)
            jac_udx(i,j,k)=u(i,j,k)*ddx(i,j)
            jac_vdy(i,j,k)=v(i,j,k)*ddy(i,j)
            jac_zdi(i,j,1)=jac_zdi(i,j,2)
            jac_zdy(i,j,1)=jac_zdy(i,j,2)
            jac_udx(i,j,1)=jac_udx(i,j,2)
            jac_vdy(i,j,1)=jac_vdy(i,j,2)
         enddo  !! do k=1,kmax_q
       enddo  !! do j=jstart,jend
     enddo  !! do i=istart,iend
            jac_zdi(1,:,:)=jac_zdi(2,:,:)
            jac_zdi(imax,:,:)=jac_zdi(imax-1,:,:)
            jac_zdi(:,1,:)=jac_zdi(:,2,:)
            jac_zdi(:,jmax,:)=jac_zdi(:,jmax-1,:)
            jac_zdy(1,:,:)=jac_zdy(2,:,:)
            jac_zdy(imax,:,:)=jac_zdy(imax-1,:,:)
            jac_zdy(:,1,:)=jac_zdy(:,2,:)
            jac_zdy(:,jmax,:)=jac_zdy(:,jmax-1,:)
            jac_udx(1,:,:)=jac_udx(2,:,:)
            jac_udx(imax,:,:)=jac_udx(imax-1,:,:)
            jac_udx(:,1,:)=jac_udx(:,2,:)
            jac_udx(:,jmax,:)=jac_udx(:,jmax-1,:)
            jac_vdy(1,:,:)=jac_vdy(2,:,:)
            jac_vdy(imax,:,:)=jac_vdy(imax-1,:,:)
            jac_vdy(:,1,:)=jac_vdy(:,2,:)
            jac_vdy(:,jmax,:)=jac_vdy(:,jmax-1,:)

! Sigmadot calculation: 2 integrals in Sigmadot

     do j=jstart,jend
       do i=istart,iend

!--  Sum 1 in sigmadot

          sum1=zero
          do k=1,kmax_q
             pu1(i,j,k)=((ps(i+1,j)*1000)-(pt_ll*100))*u(i+1,j,k)
             pu2(i,j,k)=((ps(i-1,j)*1000)-(pt_ll*100))*u(i-1,j,k)
             pv1(i,j,k)=((ps(i,j+1)*1000)-(pt_ll*100))*v(i,j+1,k)
             pv2(i,j,k)=((ps(i,j-1)*1000)-(pt_ll*100))*v(i,j-1,k)
             sum1=sum1+((((pu1(i,j,k)-pu2(i,j,k))*ddx(i,j))+&
                          ((pv1(i,j,k)-pv2(i,j,k))*ddy(i,j)))*deltasigma(k))
          enddo  ! k=1,kmax_q loop

!--  Sum 2 in sigmadot

          sum2=zero
          do k=kmax_q,1,-1
             sum2=sum2+((((pu1(i,j,k)-pu2(i,j,k))*ddx(i,j))+&
                         ((pv1(i,j,k)-pv2(i,j,k))*ddy(i,j)))*deltasigma(k))
          enddo


!--  Sigmadot

          do k=1,kmax_q
             sigmadot(i,j,k)=((sigma(k)/((ps(i,j)*1000)-(pt_ll*100)))*sum1)-&
                             ((1/((ps(i,j)*1000)-(pt_ll*100)))*sum2)

             sigmadot(i,j,1)=sigmadot(i,j,2)
          enddo
             sigmadot(1,:,:)=sigmadot(2,:,:)
             sigmadot(imax,:,:)=sigmadot(imax-1,:,:)
             sigmadot(:,1,:)=sigmadot(:,2,:)
             sigmadot(:,jmax,:)=sigmadot(:,jmax-1,:)


! Vertical advection

          do k=1,kmax_q
             vert_adv(i,j,k)=-sigmadot(i,j,k)*jac_vert(k)*t(i,j,k)*(one+fv*q(i,j,k))
          enddo   ! k loop   
             vert_adv(i,j,1)=vert_adv(i,j,2)

       enddo  !! do i=istart,iend
     enddo  ! do j=jstart,jend
             vert_adv(1,:,:)=vert_adv(2,:,:)
             vert_adv(imax,:,:)=vert_adv(imax-1,:,:)
             vert_adv(:,1,:)=vert_adv(:,2,:)
             vert_adv(:,jmax,:)=vert_adv(:,jmax-1,:)
!----
! Vertical velocity calculation
!----

      w(:,:,:)=zero
      do i=istart,iend
        do j=jstart,jend
          do k=1,kmax_q
             w(i,j,k)=horiz_adv(i,j,k)+vert_adv(i,j,k)
          enddo
             w(i,j,1)=w(i,j,2)
        enddo  !! do i=istart,iend
      enddo  ! do j=jstart,jend
            w(1,:,:)=w(2,:,:)
            w(imax,:,:)=w(imax-1,:,:)
            w(:,1,:)=w(:,2,:)
            w(:,jmax,:)=w(:,jmax-1,:)

!------------------------------------------------------
! Cloud mask flag
!------------------------------------------------------

     ismooth=1
     jsmooth=1
     istart=1+ismooth
     iend=imax-ismooth
     jstart=1+jsmooth
     jend=jmax-jsmooth

     do j=jstart,jend
       do i=istart,iend
          wmaxflag(i,j)=.false.
          numcld=0
         do ii=max(1,i-idiff),min(imax,i+idiff)
           do jj=max(1,j-jdiff),min(jmax,j+jdiff)
             do kk=1,kmax_q
                if(cwm(ii,jj,kk) .gt. cwm_threshold) then
                  numcld= numcld+1
                endif
             enddo  !! kk
           enddo  !! jj
         enddo  !! ii
        if(numcld .gt. 1) then     !! if clouds exist
           wmaxflag(i,j)=.true.
        else
           wmaxflag(i,j)=.false.
        endif
       enddo  !! do i=istart,iend
     enddo  !! do j=jstart,jend

           wmaxflag(1,:)=wmaxflag(2,:)
           wmaxflag(imax,:)=wmaxflag(imax-1,:)
           wmaxflag(:,1)=wmaxflag(:,2)
           wmaxflag(:,jmax)=wmaxflag(:,jmax-1)

!------------------------------------------------------
!------------------------------------------------------
! Calculate lightning flash rate  
!------------------------------------------------------
!------------------------------------------------------

     do i=1,imax
       do j=1,jmax
          if (wmaxflag(i,j)) then
             wmax=-1.e+10
            do k=1,kmax_q
               if (w(i,j,k).gt.wmax) then
                  wmax=w(i,j,k)
                  kvert(i,j)=k
               endif
               if (wmax .lt. 0.) then
                  wmax=0.
               endif
            enddo ! k loop
             jac_frate(i,j)=wcnst*wpower*(wmax**(wpower-1))
             flashrate(i,j)=wcnst*(wmax**wpower)
             flashrate(i,j)=abs(flashrate(i,j))
          else   ! wmaxflag
             jac_frate(i,j)=zero
             flashrate(i,j)=zero
          endif  ! wmaxflag
        enddo ! j loop
      enddo ! i loop




     end subroutine lightflashrate


!-----
