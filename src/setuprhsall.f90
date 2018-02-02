subroutine setuprhsall(ndata,mype,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setuprhsall   sets up rhs of oi 
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: This routine sets up the right hand side (rhs) of the 
!           analysis equation.  Functions performed in this routine
!           include:
!             a) calculate increments between current solutions and obs,
!             b) generate statistical summaries of quality control and innovations,
!             c) generate diagnostic files (optional), and
!             d) prepare/save information for use in inner minimization loop
!
! program history log:
!   2003-05-22  derber
!   2003-12-23  kleist  - ozone calculation modified to use guess pressure
!   2004-06-17  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase dimension of work arrays for nonlin qc
!   2004-12-08  xu li   - replace local logical flag retrieval with that in radinfo
!   2004-12-22  treadon - restructure code to compute and write out 
!                         innovation information on select outer iterations
!   2005-01-20  okamoto - add ssmi/amsre/ssmis
!   2005-03-30  lpchang - statsoz call was passing ozmz var unnecessarily
!   2005-04-18  treadon - deallocate fbias
!   2005-05-27  derber  - level output change
!   2005-07-06  derber  - include mhs and hirs/4
!   2005-06-14  wu      - add OMI oz
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - simplify data file info handling
!   2005-10-20  kazumori - modify for real AMSR-E data process
!   2005-12-01  cucurull - add GPS bending angle
!   2005-12-21  treadon  - modify processing of GPS data
!   2006-01-09  derber - move create/destroy array, compute_derived, q_diag
!                        from glbsoi outer loop into this routine
!   2006-01-12  treadon - add channelinfo
!   2006-02-03  derber  - modify for new obs control and obs count- clean up!
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - add code to generate optional observation perturbations
!   2006-07-28  derber  - modify code for new inner loop obs data structure
!   2006-07-29  treadon - remove create_atm_grids and destroy_atm_grids
!   2006-07-31  kleist - change call to atm arrays routines
!   2007-02-21  sienkiewicz - add MLS ozone changes
!   2007-03-01  treadon - add toss_gps and toss_gps_sub
!   2007-03-10      su - move the observation perturbation to each setup routine 
!   2007-03-19  tremolet - Jo table
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_conv_file
!   2007-07-09  tremolet - observation sensitivity
!   2007-06-20  cucurull - changes related to gps diagnostics
!   2007-06-29  jung - change channelinfo to array
!   2007-09-30  todling  - add timer
!   2007-10-03  todling  - add observer split option
!   2007-12-15  todling  - add prefix to diag filenames
!   2008-03-28    wu - move optional randon seed for perturb_obs to read_obs
!   2008-04-14  treadon - remove super_gps, toss_gps (moved into genstats_gps)
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-08  todling - move 3dprs/geop-hght calculation from compute_derivate into here
!   2009-01-17  todling - update interface to intjo
!   2009-03-05  meunier - add call to lagragean operator
!   2009-08-19  guo     - moved all rhs related statistics variables to m_rhs
!                         for multi-pass setuprhsall();
!                       - added control arguments init_pass and last_pass for
!                         multi-pass setuprhsall().
!   2009-09-14  guo     - invoked compute_derived() even under lobserver.  This is
!                         the right way to do it.  It trigged moving of statments
!                         from glbsoi() to observer_init().
!                       - cleaned up redandent calls to setupyobs() and inquire_obsdiags().
!   2009-10-22     shen - add high_gps and high_gps_sub
!   2009-12-08  guo     - fixed diag_conv output rewind while is not init_pass, with open(position='rewind')
!   2010-04-09  cucurull - remove high_gps and high_gps_sub
!   2010-04-01  tangborn - start adding call for carbon monoxide data. 
!   2010-04-28      zhu - add ostats and rstats for additional precoditioner
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-10-14  pagowski - added pm2_5 conventional obs
!   2010-10-20  hclin   - added aod
!   2011-02-16      zhu - add gust,vis,pblh
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-09-17  todling - automatic sizes definition for mpi-reduce calls
!   2012-01-11  Hu      - add load_gsdgeop_hgt to compute 2d subdomain pbl heights from the guess fields
!   2012-04-08  Hu      - add code to skip the observations that are not used in minimization
!   2013-02-22  Carley  - Add call to load_gsdgeop_hgt for NMMB/WRF-NMM if using
!                         PBL pseudo obs
!   2013-10-19  todling - metguess now holds background
!   2013-05-24      zhu - add ostats_t and rstats_t for aircraft temperature bias correction
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2014-0?-16  carley/zhu - add tcamt and lcbas
!   2015-07-10  pondeca - add cldch
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-05  pondeca - add uwnd10m, vwund10m
!
!   input argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!     mype     - mpi task id
!
!   output argument list:
!
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad,r_single
  use constants, only: zero,one,fv,zero_quad
  use guess_grids, only: load_prsges,load_geop_hgt,load_gsdpbl_hgt
  use guess_grids, only: ges_tsen,nfldsig
! use obsmod, only: mype_diaghdr
  use obsmod, only: nsat1,iadate,nobs_type,obscounts,&
       ndat,obs_setup,&
       dirname,write_diag,ditype,obsdiags,lobserver,&
       destroyobs,inquire_obsdiags,lobskeep,nobskeep,lobsdiag_allocated, &
       luse_obsdiag
  use obsmod, only: lobsdiagsave
  use obs_sensitivity, only: lobsensfc, lsensrecompute
  use radinfo, only: newpc4pred
  use radinfo, only: mype_rad,diag_rad,jpch_rad,retrieval,fbias,npred,ostats,rstats
! use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,ostats_t,rstats_t,npredt,max_tail
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,ostats_t,rstats_t,npredt,ntail
  use pcpinfo, only: diag_pcp
  use ozinfo, only: diag_ozone,mype_oz,jpch_oz,ihave_oz
  use coinfo, only: diag_co,mype_co,jpch_co,ihave_co
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum
  use gridmod, only: nsig,twodvar_regional,wrf_mass_regional,nems_nmmb_regional
  use gridmod, only: cmaq_regional
  use gsi_4dvar, only: nobs_bins,l4dvar
  use gsi_4dvar, only: mPEs_observer
  use jfunc, only: jiter,jiterstart,miter,first,last
  use qcmod, only: npres_print
  use convinfo, only: nconvtype,diag_conv
  use timermod, only: timer_ini,timer_fnl
  use lag_fields, only: lag_presetup,lag_state_write,lag_state_read,lag_destroy_uv
  use state_vectors, only: svars2d
  use mpeu_util, only: getindex
  use mpl_allreducemod, only: mpl_allreduce
  use aeroinfo, only: diag_aero
  use berror, only: reset_predictors_var
  use rapidrefresh_cldsurf_mod, only: l_PBL_pseudo_SurfobsT,l_PBL_pseudo_SurfobsQ,&
                                      l_PBL_pseudo_SurfobsUV
  use m_rhs, only: rhs_alloc
  use m_rhs, only: rhs_dealloc
  use m_rhs, only: rhs_allocated
  use m_rhs, only: awork  => rhs_awork
  use m_rhs, only: bwork  => rhs_bwork
  use m_rhs, only: aivals => rhs_aivals
  use m_rhs, only: stats    => rhs_stats
  use m_rhs, only: stats_co => rhs_stats_co
  use m_rhs, only: stats_oz => rhs_stats_oz
  use m_rhs, only: toss_gps_sub => rhs_toss_gps
  use setupbend_mod, only: setupbend_class
  use setupco_mod, only: setupco_class
  use setupcldch_mod, only: setupcldch_class
  use setupdw_mod, only: setupdw_class
  use setupgust_mod, only: setupgust_class
  use setuphowv_mod, only: setuphowv_class
  use setuplcbas_mod, only: setuplcbas_class
  use setupmitm_mod, only: setupmitm_class
  use setupmxtm_mod, only: setupmxtm_class
  use setupoz_mod, only: setupoz_class
  use setuppblh_mod, only: setuppblh_class
  use setuppcp_mod, only: setuppcp_class
  use setuppmsl_mod, only: setuppmsl_class
  use setuppm10_mod, only: setuppm10_class
  use setuppm2_5_mod, only: setuppm2_5_class
  use setupps_mod, only: setupps_class
  use setuppw_mod, only: setuppw_class
  use setupq_mod, only: setupq_class
  use setupref_mod, only: setupref_class
  use setuprw_mod, only: setuprw_class
  use setupspd_mod, only: setupspd_class
  use setupt_mod, only: setupt_class
  use setuptcamt_mod, only: setuptcamt_class
  use setuptcp_mod, only: setuptcp_class
! use setuptd2m_mod, only: setuptd2m_class
  use setupvis_mod, only: setupvis_class
  use setupw_mod, only: setupw_class
  use setupwspd10m_mod, only: setupwspd10m_class

  use m_gpsStats, only: gpsStats_genstats       ! was genstats_gps()
  use m_gpsStats, only: gpsStats_destroy        ! was done by genstats_gps()

  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use m_obsdiags, only: obsdiags_reset
  use m_obsdiags, only: obsdiags_read
  use m_obsdiags, only: obsdiags_sort
  use m_obsdiags, only: obsdiags_write

  use mpeu_util, only: die,warn,perr
  use mpeu_util, only: basename
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  integer(i_kind),dimension(ndat,3),intent(in   ) :: ndata
  logical                          ,intent(in   ) :: init_pass, last_pass   ! state of "setup" processing
  type(setupbend_class) :: bend
  type(setupcldch_class) :: cldch
  type(setupco_class) :: co  
  type(setupdw_class) :: dw
  type(setupgust_class) :: gust
  type(setuphowv_class) :: howv
  type(setuplcbas_class) :: lcbas
  type(setupmitm_class) :: mitm
  type(setupmxtm_class) :: mxtm
  type(setupoz_class) :: oz
  type(setuppblh_class) :: pblh
  type(setuppcp_class) :: pcp
  type(setuppm10_class) :: pm10
  type(setuppm2_5_class) :: pm2_5
  type(setuppmsl_class) :: pmsl
  type(setupps_class) :: ps
  type(setuppw_class) :: pw
  type(setupq_class) :: q
  type(setupref_class) :: ref
  type(setuprw_class) :: rw
  type(setupspd_class) :: spd
  type(setupt_class) :: t
  type(setuptcamt_class) :: tcamt
  type(setuptcp_class) :: tcp
! type(setuptd2m_class) :: td2m
  type(setupvis_class) :: vis
  type(setupw_class) :: w
  type(setupwspd10m_class) :: wspd10m

! Declare external calls for code analysis
  external:: compute_derived
  external:: evaljo
  !external:: genstats_gps
  external:: mpi_allreduce
  external:: mpi_finalize
  external:: mpi_reduce
  !external:: read_obsdiags
  external:: setupaod
  external:: setupbend
! external:: setupdw
  external:: setuplag
  external:: setupozlay
  external:: setupozlev
  external:: setuppcp
  external:: statsrad
  external:: stop2
  external:: w3tage

! Delcare local variables
  logical rad_diagsave,ozone_diagsave,pcp_diagsave,conv_diagsave,llouter,getodiag,co_diagsave
  logical aero_diagsave

  character(80):: string
  character(10)::obstype
  character(20)::isis
  character(128):: diag_conv_file
  character(len=12) :: clfile

  integer(i_kind) lunin,nobs,nchanl,nreal,nele,&
       is,idate,i_dw,i_rw,i_sst,i_tcp,i_gps,i_uv,i_ps,i_lag,&
       i_t,i_pw,i_q,i_co,i_gust,i_vis,i_ref,i_pblh,i_wspd10m,i_td2m,&
       i_mxtm,i_mitm,i_pmsl,i_howv,i_tcamt,i_lcbas,i_cldch,i_uwnd10m,i_vwnd10m,iobs,nprt,ii,jj
  integer(i_kind) it,ier,istatus

  real(r_quad):: zjo
  real(r_kind),dimension(40,ndat):: aivals1
  real(r_kind),dimension(7,jpch_rad):: stats1
  real(r_kind),dimension(9,jpch_oz):: stats_oz1
  real(r_kind),dimension(9,jpch_co):: stats_co1
  real(r_kind),dimension(npres_print,nconvtype,5,3):: bwork1
  real(r_kind),allocatable,dimension(:,:):: awork1

  real(r_kind),dimension(:,:,:),pointer:: ges_tv_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q_it =>NULL()
  character(len=*),parameter:: myname='setuprhsall'
   
  logical,parameter:: OBSDIAGS_RELOAD = .false.
  !logical,parameter:: OBSDIAGS_RELOAD = .true.
  logical:: opened
  character(len=256):: tmpname,tmpaccess,tmpform

  if(.not.init_pass .and. .not.lobsdiag_allocated) call die('setuprhsall','multiple lobsdiag_allocated',lobsdiag_allocated)
!******************************************************************************
! Initialize timer
  call timer_ini('setuprhsall')



! Initialize variables and constants.
  first = jiter == jiterstart   ! .true. on first outer iter
  last  = jiter == miter+1      ! .true. following last outer iter
  llouter=.true.

! Set diagnostic output flag

  rad_diagsave  = write_diag(jiter) .and. diag_rad
  pcp_diagsave  = write_diag(jiter) .and. diag_pcp
  conv_diagsave = write_diag(jiter) .and. diag_conv
  ozone_diagsave= write_diag(jiter) .and. diag_ozone .and. ihave_oz
  co_diagsave   = write_diag(jiter) .and. diag_co    .and. ihave_co
  aero_diagsave = write_diag(jiter) .and. diag_aero

  i_ps = 1
  i_uv = 2
  i_t  = 3
  i_q  = 4
  i_pw = 5
  i_rw = 6
  i_dw = 7
  i_gps= 8
  i_sst= 9 
  i_tcp= 10
  i_lag= 11
  i_co = 12
  i_gust=13
  i_vis =14
  i_pblh=15
  i_wspd10m=16
  i_td2m=17
  i_mxtm=18
  i_mitm=19
  i_pmsl=20
  i_howv=21
  i_tcamt=22
  i_lcbas=23
  i_cldch=24
  i_uwnd10m=26
  i_vwnd10m=27
  i_ref =i_vwnd10m

  allocate(awork1(7*nsig+100,i_ref))
  if(.not.rhs_allocated) call rhs_alloc(aworkdim2=size(awork1,2))

! Reset observation pointers
  if(init_pass) call destroyobs
  if(init_pass) call obsdiags_reset(obsdiags_keep=lobsdiagsave)   ! replacing destroyobs()

! Read observation diagnostics if available
  if (l4dvar) then
     getodiag=(.not.lobserver) .or. (lobserver.and.jiter>1)
     clfile='obsdiags.ZZZ'
     if (lobsensfc .and. .not.lsensrecompute) then
        write(clfile(10:12),'(I3.3)') miter
        !call read_obsdiags(clfile)
        call obsdiags_read(clfile,mPEs=mPEs_observer)      ! replacing read_obsdiags()
        call inquire_obsdiags(miter)
     else if (getodiag) then
        if (.not.lobserver) then
           write(clfile(10:12),'(I3.3)') jiter
           !call read_obsdiags(clfile)
           call obsdiags_read(clfile,mPEs=mPEs_observer)   ! replacing read_obsdiags()
           call inquire_obsdiags(miter)
        endif
     endif
  endif

  if(init_pass) then
     if (allocated(obscounts)) then
        write(6,*)'setuprhsall: obscounts allocated'
        call stop2(285)
     end if
     allocate(obscounts(nobs_type,nobs_bins))
  endif

  if (jiter>1.and.lobskeep) then
     nobskeep=1
  else
     nobskeep=0
  endif

! The 3d pressure and geopotential grids are initially loaded at
! the end of the call to read_guess.  Thus, we don't need to call 
! load_prsges and load_geop_hgt on the first outer loop.  We need 
! to update these 3d pressure arrays on all subsequent outer loops.
! Hence, the conditional call to load_prsges and load_geop_hgt

  if (lobserver .or. jiter>jiterstart) then

!    Get sensible temperature (after bias correction's been applied)
     do it=1,nfldsig
        ier=0
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tv',ges_tv_it,istatus);ier=ier+istatus
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'q' ,ges_q_it ,istatus);ier=ier+istatus
        if(ier/=0) exit
        ges_tsen(:,:,:,it)= ges_tv_it(:,:,:)/(one+fv*max(zero,ges_q_it(:,:,:)))
     enddo

!    Load 3d subdomain pressure arrays from the guess fields
     call load_prsges

!    Compute 3d subdomain geopotential heights from the guess fields
     call load_geop_hgt

!    if (sfcmod_gfs .or. sfcmod_mm5) then
!       if (mype==0) write(6,*)'COMPUTE_DERIVED:  call load_fact10'
!       call load_fact10
!    endif
  endif

! Compute 2d subdomain pbl heights from the guess fields
   if (wrf_mass_regional) then
      call load_gsdpbl_hgt(mype)
   else if (nems_nmmb_regional) then
      if (l_PBL_pseudo_SurfobsT .or. l_PBL_pseudo_SurfobsQ .or. l_PBL_pseudo_SurfobsUV) then
         call load_gsdpbl_hgt(mype)
      end if
   endif   


! Compute derived quantities on grid
  if(.not.cmaq_regional) call compute_derived(mype,init_pass)

  ! ------------------------------------------------------------------------

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then


     ! Init for Lagrangian data assimilation (gather winds and NL integration)
     call lag_presetup()
     ! Save state for inner loop if in 4Dvar observer mode
     if (l4dvar.and.lobserver) then
        call lag_state_write()
     end if

!    Reset observation pointers.  This is assumed by setup*() routines.
     do ii=1,size(obsdiags,2)
        do jj=1,size(obsdiags,1)
           obsdiags(jj,ii)%tail => NULL()
        enddo
     enddo

     lunin=1
     open(lunin,file=obs_setup,form='unformatted')
     rewind lunin
 
  
!    If requested, create conventional diagnostic files
     if(conv_diagsave)then
        write(string,900) jiter
900     format('conv_',i2.2)
        diag_conv_file=trim(dirname) // trim(string)
        if(init_pass) then
           open(7,file=trim(diag_conv_file),form='unformatted',status='unknown',position='rewind')

        else
           !  open(7,file=trim(diag_conv_file),form='unformatted',status='old',position='append')

                ! Without a close(7) until the last_pass=.true., the same file
                ! is expected to remain open "asis", equivalent to an "append"
                ! position through a re-open().  Therefore, a sequence of
                ! verification steps are taken to replace the earlier open()
                ! statement, to avoid re-open() without a close().

           inquire(unit=7,opened=opened)
           if(opened) then
             inquire(unit=7,name=tmpname,form=tmpform,access=tmpaccess)
             tmpname=basename(tmpname)
             if(trim(tmpname)/=trim(diag_conv_file)) then
               call perr(myname,'unexpectly occupied, unit =',7)
               call perr(myname,'           diag_conv_file =',trim(diag_conv_file))
               call perr(myname,'   inquire(unit=7,  name= )',trim(tmpname))
               call perr(myname,'   inquire(unit=7,  form= )',trim(tmpform))
               call perr(myname,'   inquire(unit=7,access= )',trim(tmpaccess))
               call  die(myname)
             endif

           else
             call perr(myname,'unexpectly closed, unit =',7)
             call perr(myname,'         diag_conv_file =',trim(diag_conv_file))
             call  die(myname)
           endif
        endif
        idate=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000
        if(init_pass .and. mype == 0)write(7)idate
     end if

     if (newpc4pred) then
        ostats=zero
        rstats=zero_quad
     end if

     if (aircraft_t_bc_pof .or. aircraft_t_bc) then
        ostats_t=zero_quad
        rstats_t=zero_quad
     end if


!    Loop over data types to process
     do is=1,ndat
        nobs=nsat1(is)
 
        if(nobs > 0)then
           read(lunin,iostat=ier) obstype,isis,nreal,nchanl
!          if(mype == mype_diaghdr(is)) then
!             write(6,300) obstype,isis,nreal,nchanl
!300          format(' SETUPALL:,obstype,isis,nreal,nchanl=',a12,a20,i5,i5)
!          endif
           if(ier/=0) call die('setuprhsall','read(), iostat =',ier)
           nele=nreal+nchanl

!          Set up for radiance data
           if(ditype(is) == 'rad')then
 
              call setuprad(lunin,&
                 mype,aivals,stats,nchanl,nreal,nobs,&
                 obstype,isis,is,rad_diagsave,init_pass,last_pass)

!          Set up for aerosol data
           else if(ditype(is) == 'aero')then
              call setupaod(lunin,&
                 mype,nchanl,nreal,nobs,&
                 obstype,isis,is,aero_diagsave,init_pass)

!          Set up for precipitation data
           else if(ditype(is) == 'pcp')then
!             call setuppcp(lunin,mype,&
              call pcp%setuppcp(lunin,mype,&
                 aivals,nele,nobs,obstype,isis,is,pcp_diagsave,init_pass)
 
!          Set up conventional data
           else if(ditype(is) == 'conv')then
!             Set up temperature data
              if(obstype=='t')then
                 write(6,*) 'setting up t'
                 call t%setupp(('setupt'),(/ 'var::v', 'var::u', 'var::q', 'var::ps', 'var::tv' /),&
                      lunin,mype,bwork,awork(1,i_t),nele,nobs,is,conv_diagsave)
!                call setupt(lunin,mype,bwork,awork(1,i_t),nele,nobs,is,conv_diagsave)

!             Set up uv wind data
              else if(obstype=='uv')then
                 write(6,*) 'setting up uv'
                 call w%setupp(('setupw'),(/ 'var::v', 'var::u', 'var::z', 'var::ps', 'var::tv' /),&
                      lunin,mype,bwork,awork(1,i_uv),nele,nobs,is,conv_diagsave)
!                call setupw(lunin,mype,bwork,awork(1,i_uv),nele,nobs,is,conv_diagsave)

!             Set up wind speed data
              else if(obstype=='spd')then
                 write(6,*) 'setting up spd'
                 call spd%setupp(('setupspd'),(/ 'var::v', 'var::u', 'var::z', 'var::ps', 'var::tv' /),&
                      lunin,mype,bwork,awork(1,i_uv),nele,nobs,is,conv_diagsave)
!                call setupspd(lunin,mype,bwork,awork(1,i_uv),nele,nobs,is,conv_diagsave)

!             Set up surface pressure data
              else if(obstype=='ps')then
                 write(6,*) 'setting up ps'
                 call ps%setupp(('setupps'),(/'var::ps','var::z','var::tv'/),&
                      lunin,mype,bwork,awork(1,i_ps),nele,nobs,is,conv_diagsave)
!                call setupps(lunin,mype,bwork,awork(1,i_ps),nele,nobs,is,conv_diagsave)
 
!             Set up tc-mslp data
              else if(obstype=='tcp')then
                 write(6,*) 'setting up tcp'
                 call tcp%setupp(('setuptcp'),(/'var::ps','var::z','var::tv'/),&
                      lunin,mype,bwork,awork(1,i_tcp),nele,nobs,is,conv_diagsave)
!                call setuptcp(lunin,mype,bwork,awork(1,i_tcp),nele,nobs,is,conv_diagsave)

!             Set up moisture data
              else if(obstype=='q') then
                 write(6,*) 'setting up q'
                 call q%setup(lunin,mype,bwork,awork(1,i_q),nele,nobs,is,conv_diagsave)
!                call setupq(lunin,mype,bwork,awork(1,i_q),nele,nobs,is,conv_diagsave)

!             Set up lidar wind data
              else if(obstype=='dw')then
                 write(6,*) 'setting up dw'
                 call dw%setupp(('setupdw'),(/ 'var::ps', 'var::z', 'var::u', 'var::v' /),&
                     lunin,mype,bwork,awork(1,i_dw),nele,nobs,is,conv_diagsave)
!                call setupdw(lunin,mype,bwork,awork(1,i_dw),nele,nobs,is,conv_diagsave)

!             Set up radar wind data
              else if(obstype=='rw')then
                 write(6,*) 'setting up rw'
                 call rw%setup(lunin,mype,bwork,awork(1,i_rw),nele,nobs,is,conv_diagsave)
!                call setuprw(lunin,mype,bwork,awork(1,i_rw),nele,nobs,is,conv_diagsave)

!             Set up total precipitable water (total column water) data
              else if(obstype=='pw')then
                 write(6,*) 'setting up pw'
                 call pw%setupp(('setuppw'),(/ 'var::z', 'var::tv', 'var::q' /),&
                      lunin,mype,bwork,awork(1,i_pw),nele,nobs,is,conv_diagsave)
!                call setuppw(lunin,mype,bwork,awork(1,i_pw),nele,nobs,is,conv_diagsave)
!             Set up conventional sst data
              else if(obstype=='sst' .and. getindex(svars2d,'sst')>0) then 
                 call setupsst(lunin,mype,bwork,awork(1,i_sst),nele,nobs,is,conv_diagsave)

!             Set up conventional lagrangian data
              else if(obstype=='lag') then 
                 call setuplag(lunin,mype,bwork,awork(1,i_lag),nele,nobs,is,conv_diagsave)
              else if(obstype == 'pm2_5')then 
                 write(6,*) 'setting up pm2_5'
                 call pm2_5%setuppm2_5(lunin,mype,nele,nobs,isis,is,conv_diagsave)
!                call setuppm2_5(lunin,mype,nele,nobs,isis,is,conv_diagsave)
              else if(obstype == 'pm10')then 
                 write(6,*) 'setting up pm10'
                 call pm10%setuppm10(lunin,mype,nele,nobs,isis,is,conv_diagsave)
!                call setuppm10(lunin,mype,nele,nobs,isis,is,conv_diagsave)
!             Set up conventional wind gust data
              else if(obstype=='gust' .and. getindex(svars2d,'gust')>0) then
                 write(6,*) 'setting up gust'
                 call gust%setupp(('setupgust'), (/ 'var::ps', 'var::z', 'var::gust' /),&
                    lunin,mype,bwork,awork(1,i_gust),nele,nobs,is,conv_diagsave)
!                call setupgust(lunin,mype,bwork,awork(1,i_gust),nele,nobs,is,conv_diagsave)
!             Set up conventional visibility data
              else if(obstype=='vis' .and. getindex(svars2d,'vis')>0) then
                 write(6,*) 'setting up vis'
                 call vis%setupp(('setupvis'), (/ 'var::z', 'var::ps', 'var::vis' /),&
                      lunin,mype,bwork,awork(1,i_vis),nele,nobs,is,conv_diagsave)
!                call setupvis(lunin,mype,bwork,awork(1,i_vis),nele,nobs,is,conv_diagsave)

!             Set up conventional pbl height data
              else if(obstype=='pblh' .and. getindex(svars2d,'pblh')>0) then
                 write(6,*) 'setting up pblh'
                 call pblh%setupp(('setuppblh'),(/ 'var::ps', 'var::z' /),&
                      lunin,mype,bwork,awork(1,i_pblh),nele,nobs,is,conv_diagsave)
!                call setuppblh(lunin,mype,bwork,awork(1,i_pblh),nele,nobs,is,conv_diagsave)

!             Set up conventional wspd10m data
              else if(obstype=='wspd10m' .and. getindex(svars2d,'wspd10m')>0) then
                 write(6,*) 'setting up wspd10m'
                 call wspd10m%setupp(('setupwspd10m'),(/ 'var::wspd10m', 'var::ps', 'var::z', 'var::u', 'var::v', 'var::tv' /),&
                      lunin,mype,bwork,awork(1,i_wspd10m),nele,nobs,is,conv_diagsave)
!                call setupwspd10m(lunin,mype,bwork,awork(1,i_wspd10m),nele,nobs,is,conv_diagsave)

!             Set up conventional td2m data
              else if(obstype=='td2m' .and. getindex(svars2d,'td2m')>0) then
                 write(6,*) 'setting up td2m'
!                call td2m%setup(lunin,mype,bwork,awork(1,i_td2m),nele,nobs,is,conv_diagsave)
                 call setuptd2m(lunin,mype,bwork,awork(1,i_td2m),nele,nobs,is,conv_diagsave)

!             Set up conventional mxtm data
              else if(obstype=='mxtm' .and. getindex(svars2d,'mxtm')>0) then
!                call setupmxtm(lunin,mype,bwork,awork(1,i_mxtm),nele,nobs,is,conv_diagsave)
                 call mxtm%setupp(('setupmxtm'),(/'var::z', 'var::ps', 'var::mxtm'/),lunin,mype,bwork,awork(1,i_mxtm),&
                            nele,nobs,is,conv_diagsave)

!             Set up conventional mitm data
              else if(obstype=='mitm' .and. getindex(svars2d,'mitm')>0) then
                 write(6,*) 'setting up mitm'
                 call mitm%setupp(('setupmitm'),(/ 'var::ps', 'var::z', 'var::mitm' /),&
                      lunin,mype,bwork,awork(1,i_mitm),nele,nobs,is,conv_diagsave)
!                call setupmitm(lunin,mype,bwork,awork(1,i_mitm),nele,nobs,is,conv_diagsave)

!             Set up conventional pmsl data
              else if(obstype=='pmsl' .and. getindex(svars2d,'pmsl')>0) then
                 write(6,*) 'setting up pmsl'
                 call pmsl%setupp(('setuppmsl'),(/ 'var::ps', 'var::z', 'var::pmsl' /),&
                      lunin,mype,bwork,awork(1,i_pmsl),nele,nobs,is,conv_diagsave)
!                call setuppmsl(lunin,mype,bwork,awork(1,i_pmsl),nele,nobs,is,conv_diagsave)

!             Set up conventional howv data
              else if(obstype=='howv' .and. getindex(svars2d,'howv')>0) then
!                call setuphowv(lunin,mype,bwork,awork(1,i_howv),nele,nobs,is,conv_diagsave)
                 call howv%setupp(('setuphowv'),(/ 'var::ps', 'var::z', 'var::howv' /),&
                       lunin,mype,bwork,awork(1,i_howv),nele,nobs,is,conv_diagsave)

!             Set up total cloud amount data
              else if(obstype=='tcamt' .and. getindex(svars2d,'tcamt')>0) then
                 write(6,*) 'setting up tcamt'
                 call tcamt%setupp(('setuptcamt'),(/ 'var::tcamt' /),&
                      lunin,mype,bwork,awork(1,i_tcamt),nele,nobs,is,conv_diagsave)
!                call setuptcamt(lunin,mype,bwork,awork(1,i_tcamt),nele,nobs,is,conv_diagsave)

!             Set up base height of lowest cloud seen
              else if(obstype=='lcbas' .and. getindex(svars2d,'lcbas')>0) then
                 write(6,*) 'setting up lcbas'
                 call lcbas%setupp(('setuplcbas'),(/ 'var::lcbas', 'var::z' /),&
                     lunin,mype,bwork,awork(1,i_lcbas),nele,nobs,is,conv_diagsave)
!                call setuplcbas(lunin,mype,bwork,awork(1,i_lcbas),nele,nobs,is,conv_diagsave)

!             Set up conventional cldch data
              else if(obstype=='cldch' .and. getindex(svars2d,'cldch')>0) then
                 write(6,*) 'setting up cldch'
!                call setupcldch(lunin,mype,bwork,awork(1,i_cldch),nele,nobs,is,conv_diagsave)
                 call cldch%setupp(('setupcldch'),(/ 'var::ps', 'var::z', 'var::cldch' /),&
                     lunin,mype,bwork,awork(1,i_cldch),nele,nobs,is,conv_diagsave)

!             Set up conventional uwnd10m data
              else if(obstype=='uwnd10m' .and. getindex(svars2d,'uwnd10m')>0) then
                 call setupuwnd10m(lunin,mype,bwork,awork(1,i_uwnd10m),nele,nobs,is,conv_diagsave)

!             Set up conventional vwnd10m data
              else if(obstype=='vwnd10m' .and. getindex(svars2d,'vwnd10m')>0) then
                 call setupvwnd10m(lunin,mype,bwork,awork(1,i_vwnd10m),nele,nobs,is,conv_diagsave)

!             skip this kind of data because they are not used in the var analysis
              else if(obstype == 'mta_cld' .or. obstype == 'gos_ctp' .or. &
                      obstype == 'rad_ref' .or. obstype=='lghtn' .or. &
                      obstype == 'larccld' .or. obstype == 'larcglb') then
                 read(lunin,iostat=ier)
                 if(ier/=0) call die('setuprhsall','read(), iostat =',ier)

!
              else
                 write(6,*) 'Warning, unknown data type in setuprhsall,', obstype

              end if

!          set up ozone (sbuv/omi/mls) data
           else if(ditype(is) == 'ozone' .and. ihave_oz)then
              if (obstype == 'o3lev' .or. index(obstype,'mls')/=0 ) then
!                call setupozlev(lunin,mype,stats_oz,nchanl,nreal,nobs,&
                 call oz%setupozlev(lunin,mype,stats_oz,nchanl,nreal,nobs,&
                      obstype,isis,is,ozone_diagsave,init_pass)
              else
!                call setupozlay(lunin,mype,stats_oz,nchanl,nreal,nobs,&
                 call oz%setupozlay(lunin,mype,stats_oz,nchanl,nreal,nobs,&
                      obstype,isis,is,ozone_diagsave,init_pass)
              end if

!          Set up co (mopitt) data
           else if(ditype(is) == 'co')then 
!             call setupco(lunin,mype,stats_co,nchanl,nreal,nobs,&
              call co%setupco(lunin,mype,stats_co,nchanl,nreal,nobs,&
                   obstype,isis,is,co_diagsave,init_pass)

!          Set up GPS local refractivity data
           else if(ditype(is) == 'gps')then
              if(obstype=='gps_ref')then
!                call setupref(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub,is,init_pass,last_pass)
                 call ref%setupref(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub,is,init_pass,last_pass)

!             Set up GPS local bending angle data
              else if(obstype=='gps_bnd')then
!                call setupbend(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub,is,init_pass,last_pass)
                 call bend%setupbend(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub,is,init_pass,last_pass)
              end if
           end if

        end if

     end do
     close(lunin)

  else

     ! Init for Lagrangian data assimilation (read saved parameters)
     call lag_state_read()

  endif ! < lobserver >
  lobsdiag_allocated=.true.

  if(.not.last_pass) then
     call timer_fnl('setuprhsall')
     return
  endif

! Deallocate wind field array for Lagrangian data assimilation
  call lag_destroy_uv()

! Finalize qc and accumulate statistics for GPSRO data
  call gpsStats_genstats(bwork,awork(:,i_gps),toss_gps_sub,conv_diagsave,mype)
  call gpsStats_destroy()       ! replacing ...
  ! -- call genstats_gps(bwork,awork(1,i_gps),toss_gps_sub,conv_diagsave,mype)

  if (conv_diagsave) close(7)

  if(l_PBL_pseudo_SurfobsT.or.l_PBL_pseudo_SurfobsQ.or.l_PBL_pseudo_SurfobsUV) then
  else
     call obsdiags_sort()
  endif

! for temporary testing purposes, _write and _read.
  if(OBSDIAGS_RELOAD) then
    call obsdiags_write('obsdiags.ttt',force=.true.)
        ! call Barrier() before obsdiags_read(), to make sure all PEs have
        ! finished their obsdiags_write().
    if(mPEs_observer>0) call MPI_Barrier(mpi_comm_world,ier)

    call obsdiags_read('obsdiags.ttt',mPEs=mPEs_observer,force=.true., &
        ignore_iter=.true.)
    call inquire_obsdiags(miter)
  endif

! call inquire_obsdiags(miter)

! Collect information for preconditioning
  if (newpc4pred) then
     call mpl_allreduce(jpch_rad,rpvals=ostats)
     call mpl_allreduce(npred,jpch_rad,rstats)
  end if

! Collect information for aircraft data
  if (aircraft_t_bc_pof .or. aircraft_t_bc) then
!    call mpl_allreduce(npredt,max_tail,ostats_t)
!    call mpl_allreduce(npredt,max_tail,rstats_t)
     call mpl_allreduce(npredt,ntail,ostats_t)
     call mpl_allreduce(npredt,ntail,rstats_t)
  end if

  if (newpc4pred .or. aircraft_t_bc_pof .or. aircraft_t_bc) then
     call reset_predictors_var
  end if

! Collect satellite and precip. statistics
  call mpi_reduce(aivals,aivals1,size(aivals1),mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats,stats1,size(stats1),mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  if (ihave_oz) call mpi_reduce(stats_oz,stats_oz1,size(stats_oz1),mpi_rtype,mpi_sum,mype_oz, &
       mpi_comm_world,ierror)

  if (ihave_co) call mpi_reduce(stats_co,stats_co1,size(stats_co1),mpi_rtype,mpi_sum,mype_co, &
       mpi_comm_world,ierror)

! Collect conventional data statistics
  
  call mpi_allreduce(bwork,bwork1,size(bwork1),mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
  call mpi_allreduce(awork,awork1,size(awork1),mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

! Compute and print statistics for radiance, precipitation, and ozone data.
! These data types are NOT processed when running in 2dvar mode.  Hence
! the check on the 2dvar flag below.

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then

     if (.not.twodvar_regional) then

!       Compute and print statistics for radiance data
        if(mype==mype_rad) call statsrad(aivals1,stats1,ndata)

!       Compute and print statistics for precipitation data
        if(mype==mype_rad) call statspcp(aivals1,ndata)

!       Compute and print statistics for ozone
        if (mype==mype_oz .and. ihave_oz) call statsoz(stats_oz1,ndata)

!       Compute and print statistics for carbon monoxide
!????   if (mype==mype_co .and. ihave_co) call statsco(stats_co1,bwork1,awork1(1,i_co),ndata)

     endif

!    Compute and print statistics for "conventional" data
     call statsconv(mype,&
          i_ps,i_uv,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag, &
          i_gust,i_vis,i_pblh,i_wspd10m,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv, &
          i_tcamt,i_lcbas,i_cldch,i_uwnd10m,i_vwnd10m,i_ref,bwork1,awork1,ndata)

  endif  ! < .not. lobserver >

  deallocate(awork1)
  call rhs_dealloc()   ! destroy the workspace: awork, bwork, etc.
! Print Jo table
  nprt=2
  llouter=.true.
  if(luse_obsdiag)call evaljo(zjo,iobs,nprt,llouter)

! If only performing sst retrieval, end program execution
  if(retrieval)then
     deallocate(fbias)
     if(mype==0)then
        write(6,*)'SETUPRHSALL:  normal completion for retrieval'
        call w3tage('GLOBAL_SSI')
     end if
     call mpi_finalize(ierror)
     stop
  end if

! Finalize timer
  call timer_fnl('setuprhsall')

  return
end subroutine setuprhsall
