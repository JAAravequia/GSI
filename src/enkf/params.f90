module params
!$$$  module documentation block
!
! module: params                       read namelist for EnKF from file
!                                      enkf.nml.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module holds the namelist parameters (and some derived
! parameters) read in from enkf.nml (by the module subroutine
! read_namelist) on each MPI task.
!
! Public Subroutines:
!   read_namelist: initialize namelist parameter defaults, read namelist
!    (over-riding defaults for parameters supplied in namelist), compute
!    some derived parameters.  Sets logical variable params_initialized
!    to .true.
!   cleanup_namelist: deallocate memory allocated in read_namelist
!
! Public Variables: (see comments in subroutine read_namelist)
!
! Modules Used: mpisetup, constants, kinds
!
! program history log:
!   2009-02-23  Initial version.
!   2016-05-02  shlyaeva - Modification for reading state vector from table
!   2016-11-29  shlyaeva - added nhr_state (hours for state fields to 
!                          calculate Hx; nhr_anal is for IAU)
!   2018-05-31  whitaker - added modelspace_vloc (for model-space localization using
!                          modulated ensembles), nobsl_max (for ob selection
!                          in LETKF)
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use constants, only: rearth, deg2rad, init_constants, init_constants_derived
use kinds, only: r_single,i_kind
use radinfo, only: adp_anglebc,angord,use_edges,emiss_bc,newpc4pred

implicit none
private
public :: read_namelist,cleanup_namelist
!  nsats_rad: the total number of satellite data types to read.
!  sattypes_rad:  strings describing the satellite data type (which form part
!   of the diag* filename).
!  dsis:  strings corresponding to sattypes_rad which correspond to the names
!   in the NCEP global_satinfo file.
!  sattypes_oz :  strings describing the ozone satellite data type (which form
!   part of the diag* filename).
integer(i_kind), public, parameter :: nsatmax_rad = 200
integer(i_kind), public, parameter :: nsatmax_oz = 100
character(len=20), public, dimension(nsatmax_rad) ::sattypes_rad, dsis
character(len=20), public, dimension(nsatmax_oz) ::sattypes_oz
! forecast times for first-guess forecasts to be updated (in hours)
integer,dimension(7),public ::  nhr_anal  = (/6,-1,-1,-1,-1,-1,-1/)
integer,dimension(7),public ::  nhr_state = (/6,-1,-1,-1,-1,-1,-1/)
! forecast hour at middle of assimilation window
real(r_single),public :: fhr_assim=6.0
! character string version of nhr_anal with leading zeros.
character(len=2),dimension(7),public :: charfhr_anal
character(len=2),dimension(7),public :: charfhr_state
! prefix for background and analysis file names (mem### appended)
! For global, default is "sfg_"//datestring//"_fhr##_" and
! "sanl_"//datestring//"_fhr##_". If only one time level
! in background, default for analysis is "sanl_"//datestring//"_"
! For regional, default is "firstguess_fhr##." and
! "analysis_fhr##." If only one time level
! in background, default is "firstguess." and "analysis.".
character(len=120),dimension(7),public :: fgfileprefixes
character(len=120),dimension(7),public :: fgsfcfileprefixes
character(len=120),dimension(7),public :: statefileprefixes
character(len=120),dimension(7),public :: statesfcfileprefixes
character(len=120),dimension(7),public :: anlfileprefixes
! analysis date string (YYYYMMDDHH)
character(len=10), public ::  datestring
! filesystem path to input files (first-guess, GSI diagnostic files).
character(len=500),public :: datapath
logical, public :: pseudo_rh, cliptracers
logical, public :: lupp
logical, public :: cnvw_option
integer(i_kind),public ::  nlevs,nanals,numiter,&
                           nlons,nlats,nbackgrounds,nstatefields,&
                           nanals_per_iotask, ntasks_io
integer(i_kind),public, allocatable, dimension(:) ::  nanal1,nanal2
integer(i_kind),public :: nsats_rad,nsats_oz,imp_physics
real(r_single),public ::  covinflatemax,covinflatemin,smoothparm
real(r_single),public ::  corrlengthnh,corrlengthtr,corrlengthsh
real(r_single),public :: analpertwtnh,analpertwtsh,analpertwttr,sprd_tol,saterrfact
real(r_single),public :: analpertwtnh_rtpp,analpertwtsh_rtpp,analpertwttr_rtpp
real(r_single),public :: letkf_rtps=0.0
real(r_single),public :: latbound,delat,p5delat,delatinv
real(r_single),public :: latboundpp,latboundpm,latboundmp,latboundmm

real(r_single),public :: covinflatenh,covinflatesh,covinflatetr,lnsigcovinfcutoff
! for LETKF, max number of obs in local volume.
! default is -1, which means take all obs within
! specified localization radius.  if nobsl_max > 0,
! only the first nobsl_max closest obs within the
! localization radius will be used.
integer,public :: nobsl_max = -1
! do model-space vertical localization
! if .true., eigenvectors of the localization
! matrix are read from a file called 'vlocal_eig.dat'
! (created by an external python utility).
! use correlated obs errors
! if T, extra fields read from diag file and innovation stats
! are in transformed space (R**{-1/2}).
logical,public :: use_correlated_oberrs=.false.
! number of eigenvectors of vertical localization
! used.  Zero if modelspace_vloc=.false., read from
! file 'vlocal_eig.dat' if modelspace_vloc=.true.
integer,public :: neigv = 0
real(r_double) :: vlocal_eval
real(r_double),public,dimension(:,:), allocatable :: vlocal_evecs
logical,public :: params_initialized = .true.
logical,public :: save_inflation = .false.
logical,public :: getkf = .false.
logical, public :: getkf_inflation=.false.
! use DEnKF approx to EnKF perturbation update.
! Implies getkf=T
! See Sakov and Oke 2008 https://doi.org/10.1111/j.1600-0870.2007.00299.x
logical,public :: denkf=.false.
logical,public :: reducedgrid = .false.
logical,public :: regional = .false.
logical,public :: use_gfs_nemsio = .false.
logical,public :: use_gfs_ncio = .false.
logical,public :: arw = .false.
logical,public :: nmm = .true.
logical,public :: nmm_restart = .true.
logical,public :: nmmb = .false.
! use brute force search in LETKF instead of kdtree
logical,public :: letkf_bruteforce_search=.false.

! if true, use ensemble mean qsat in definition of
! normalized humidity analysis variable (instead of
! qsat for each member, which is the default behavior
! when pseudo_rh=.true.  If pseudo_rh=.false, use_qsatensmean
! is ignored.
logical,public :: use_qsatensmean = .false.
logical,public :: write_spread_diag = .false.

! use fv3 cubed-sphere tiled restart files
logical,public :: fv3_native = .false.
character(len=500),public :: fv3fixpath = ' '
integer(i_kind),public :: ntiles=6
integer(i_kind),public :: nx_res=0,ny_res=0
logical,public ::l_pres_add_saved 

! not used anymore
real(r_single),public :: paoverpb_thresh,biasvar
real(r_single),public :: obtimelnh,obtimeltr,obtimelsh
real(r_single),public :: zhuberleft,zhuberright
real(r_single),public :: covl_minfact, covl_efold
real(r_single),public :: lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh,&
               lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh,&
               lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh
integer, public :: nvars, iassim_order, npefiles
logical, public :: deterministic, sortinc,letkf_novlocal,  &
                   huber,univaroz
logical, public :: letkf_flag=.true.
logical, public :: modelspace_vloc=.true.
logical, public :: lupd_obspace_serial=.false.
logical, public :: readin_localization=.false.
logical, public :: lupd_satbiasc=.false.
logical, public :: varqc=.false.
logical, public :: netcdf_diag=.true.
logical, public :: lobsdiag_forenkf=.true.

namelist /nam_enkf/datestring,datapath,&
                   covinflatemax,covinflatemin,&
                   corrlengthnh,corrlengthtr,corrlengthsh,&
                   reducedgrid,nlons,nlats,smoothparm,use_qsatensmean,&
                   fgfileprefixes,fgsfcfileprefixes,anlfileprefixes, &
                   statefileprefixes,statesfcfileprefixes, &
                   analpertwtnh,analpertwtsh,analpertwttr,sprd_tol,&
                   analpertwtnh_rtpp,analpertwtsh_rtpp,analpertwttr_rtpp,letkf_rtps,&
                   nlevs,nanals,saterrfact,regional,use_gfs_nemsio,use_gfs_ncio,&
                   latbound,delat,pseudo_rh,numiter,&
                   cliptracers,adp_anglebc,angord,&
                   newpc4pred,nmmb,nhr_anal,nhr_state, fhr_assim,nbackgrounds,nstatefields, &
                   save_inflation,nobsl_max,&
                   use_edges,emiss_bc,&
                   getkf,getkf_inflation,denkf,write_spread_diag,&
                   covinflatenh,covinflatesh,covinflatetr,lnsigcovinfcutoff,letkf_bruteforce_search,&
                   imp_physics,lupp,cnvw_option,use_correlated_oberrs,&
                   fv3_native,&
! these are not used, included for backwards compatibility
                   lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh,&
                   lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh,&
                   lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh,&
                   covl_minfact,covl_efold,lupd_obspace_serial,letkf_novlocal,&
                   varqc,huber,iassim_order,nvars,sortinc,deterministic,&
                   readin_localization, zhuberleft,zhuberright,&
                   modelspace_vloc,obtimelnh,obtimeltr,obtimelsh,&
                   lupd_satbiasc,paoverpb_thresh,biasvar,npefiles,letkf_flag,&
                   lobsdiag_forenkf,netcdf_diag,univaroz
namelist /nam_wrf/arw,nmm,nmm_restart
namelist /nam_fv3/fv3fixpath,nx_res,ny_res,ntiles,l_pres_add_saved
namelist /satobs_enkf/sattypes_rad,dsis
namelist /ozobs_enkf/sattypes_oz

contains

subroutine read_namelist()
integer i,j,nb,np
logical fexist
real(r_single) modelspace_vloc_cutoff, modelspace_vloc_thresh
! have all processes read namelist from file enkf.nml

! defaults
! time (analysis time YYYYMMDDHH)
datestring = "0000000000" ! if 0000000000 will not be used.
! corrlength (length for horizontal localization in km)
! this corresponding GSI parameter is s_ens_h.
! corrlength is the distance at which the Gaspari-Cohn
! polynomial goes to zero.  s_ens_h is the scale of a 
! Gaussian exp(-0.5*(r/L)**2) so
! corrlength ~ sqrt(2/0.15)*s_ens_h
corrlengthnh = 2800_r_single
corrlengthtr = 2800_r_single
corrlengthsh = 2800_r_single
! min and max inflation.
covinflatemin = 1.0_r_single
covinflatemax = 1.e30_r_single
! path to data directory
datapath = " " ! mandatory
! tolerance for background check.
! obs are not used if they are more than sqrt(S+R) from mean,
! where S is ensemble variance and R is observation error variance.
sprd_tol = 9.9e31_r_single
! definition of tropics and mid-latitudes (for inflation).
latbound = 25._r_single ! this is where the tropics start
delat = 10._r_single    ! width of transition zone.
! RTPS inflation coefficients.
analpertwtnh = 0.0_r_single ! no inflation (1 means inflate all the way back to prior spread)
analpertwtsh = 0.0_r_single
analpertwttr = 0.0_r_single
! RTPP inflation coefficients.
analpertwtnh_rtpp = 0.0_r_single ! no inflation (1 means inflate all the way back to prior perturbation)
analpertwtsh_rtpp = 0.0_r_single
analpertwttr_rtpp = 0.0_r_single
! lnsigcovinfcutoff (length for vertical taper in inflation in ln(sigma))
lnsigcovinfcutoff = 1.0e30_r_single
! if ob space posterior variance divided by prior variance
! less than this value, ob is skipped during serial processing.
paoverpb_thresh = 1.0_r_single! don't skip any obs
! use 'pseudo-rh' analysis variable, as in GSI.
pseudo_rh = .false.
! type of GFS microphyics.
! 99: Zhao-Carr, 11: GFDL
imp_physics = 99
! lupp, if true output extra variables (deprecated, does not do anything)
lupp = .false.
! these are all mandatory.
! nlons and nlats are # of lons and lats
nlons = 0
nlats = 0
! total number of levels
nlevs = 0
! number of ensemble members
nanals = 0
! background error variance for rad bias coeffs  (used in radbias.f90)
! default is (old) GSI value.
! if negative, bias coeff error variace is set to -biasvar/N, where
! N is number of obs per instrument/channel.
! if newpc4pred is .true., biasvar is not used - the estimated
! analysis error variance from the previous cycle is used instead
! (same as in the GSI).
biasvar = 0.1_r_single

! factor to multiply sat radiance errors.
saterrfact = 1._r_single
! number of times to iterate state/bias correction update.
! (numiter = 1 means no iteration, but update done in both observation and model
! space)
! (for LETKF, numiter = 0 shuts off update in observation space)
numiter = 1

! smoothing paramater for inflation (-1 for no smoothing)
smoothparm = -1
! if true, tracers are clipped to zero when read in, and just
! before they are written out.
cliptracers = .true.

! Initialize satellite files to ' '
sattypes_rad=' '
sattypes_oz=' '
dsis=' '

! Initialize first-guess and analysis file name prefixes.
! (blank means use default names)
fgfileprefixes = ''; anlfileprefixes=''; statefileprefixes=''
fgsfcfileprefixes = ''; statesfcfileprefixes=''

! option for including convective clouds in the all-sky 
cnvw_option=.false.

l_pres_add_saved=.true.

! read from namelist file, doesn't seem to work from stdin with mpich
open(912,file='enkf.nml',form="formatted")
read(912,nam_enkf)
read(912,satobs_enkf)
read(912,ozobs_enkf)
if (regional) then
  read(912,nam_wrf)
endif
if (fv3_native) then
  read(912,nam_fv3)
  nlons = nx_res; nlats = ny_res ! (total number of pts = ntiles*res*res)
endif
close(912)

! only letkf with model space vertical localization supported in this vertion
if (.not. letkf_flag .or. .not. modelspace_vloc) then
   if (nproc .eq. 0) then
      print *, 'error: only letkf with model space vert localization supported'
   endif
   call stop2(19)
endif
if (.not. lobsdiag_forenkf) then
   if (nproc .eq. 0) then
      print *, 'error: only lobsdiag_forenkf=T supported'
   endif
   call stop2(19)
endif
if (.not. netcdf_diag) then
   if (nproc .eq. 0) then
      print *, 'error: only netcdf_diag=T supported'
   endif
   call stop2(19)
endif
if (lupd_obspace_serial .and. nproc == 0) then
   print *,'warning: lupd_obspace_serial ignored - no serial enkf support'
endif
if (varqc .and. nproc == 0) then
   print *,'warning: varqc ignored - no serial enkf support'
endif
if (lupd_satbiasc .and. nproc == 0) then
   print *,'warning: lupd_satbiasc ignored - no serial enkf support'
endif
if (readin_localization .and. nproc == 0) then
   print *,'warning: read_localization ignored'
   print *,'corrlength*, nobsl_max, vlocal_eig.dat used to set localization'
endif

! find number of satellite files
nsats_rad=0
do i=1,nsatmax_rad
  if(sattypes_rad(i) == ' ') cycle
  nsats_rad=nsats_rad+1
end do
if(nproc == 0)write(6,*) 'number of satellite radiance files used',nsats_rad

! find number of satellite files
nsats_oz=0
do i=1,nsatmax_oz
  if(sattypes_oz(i) == ' ') cycle
  nsats_oz=nsats_oz+1
end do
if(nproc == 0)write(6,*) 'number of satellite ozone files used',nsats_oz


p5delat=0.5_r_single*delat
latboundpp=latbound+p5delat
latboundpm=latbound-p5delat
latboundmp=-latbound+p5delat
latboundmm=-latbound-p5delat
delatinv=1.0_r_single/delat

! read in eigenvalues/vectors of vertical localization matrix on all tasks
! (text file vlocal_eig.dat must exist)
inquire(file='vlocal_eig.dat',exist=fexist)
if ( fexist ) then
   open(7,file='vlocal_eig.dat',status="old",action="read")
   read(7,*) neigv,modelspace_vloc_thresh,modelspace_vloc_cutoff
   if (neigv < 1) then
      if (nproc .eq. 0) print *, 'error: neigv must be greater than zero'
      call stop2(19)
   endif
   allocate(vlocal_evecs(neigv,nlevs+1))
   if (nproc .eq. 0) then
      print *,'model-space vertical localization enabled'
      print *,'neigv = ',neigv
      print *,'vertical localization cutoff distance (lnp units) =',&
             modelspace_vloc_cutoff
      print *,'eigenvector truncation threshold = ',modelspace_vloc_thresh
      print *,'vertical localization eigenvalues'
   endif
   do i = 1,neigv
      read(7,*) vlocal_eval
      if (nproc .eq. 0) print *,i,vlocal_eval
      do j = 1,nlevs
         read(7,*) vlocal_evecs(i,j)
      enddo
      ! nlevs+1 same as level 1 (2d variables treated as surface)
      vlocal_evecs(i,nlevs+1) = vlocal_evecs(i,1)
   enddo
else
   if (nproc == 0) print *,'no vertical localization, no modulated ensemble'
endif
close(7)

if (nanals <= numproc) then
   ! one ensemble member read in on each of first nanals tasks.
   ntasks_io = nanals
   nanals_per_iotask = 1
   allocate(nanal1(0:ntasks_io-1),nanal2(0:ntasks_io-1))
   do np=0,ntasks_io-1
      nanal1(np) = np+1
      nanal2(np) = np+1
   enddo
else
   nanals_per_iotask = 1
   do
      ntasks_io = nanals/nanals_per_iotask
      if (ntasks_io <= numproc .and. mod(nanals,nanals_per_iotask) .eq. 0) then
         exit
      else
         nanals_per_iotask = nanals_per_iotask + 1
      end if
   end do  
   allocate(nanal1(0:ntasks_io-1),nanal2(0:ntasks_io-1))
   do np=0,ntasks_io-1
      nanal1(np) = 1 + np*nanals_per_iotask
      nanal2(np) = (np+1)*nanals_per_iotask
   enddo
endif

if (nproc == 0) then

   print *,'namelist parameters:'
   print *,'--------------------'
   write(6,nam_enkf)
   write(6,nam_fv3)
   print *,'--------------------'

! check for mandatory namelist variables

   if (nlons == 0 .or. nlats == 0 .or. nlevs == 0 .or. nanals == 0) then
      print *,'must specify nlons,nlats,nlevs,nanals in namelist'
      print *,nlons,nlats,nlevs,nanals
      call stop2(19)
   end if
   if (numproc .lt. ntasks_io) then
      print *,'total number of mpi tasks must be >= ntasks_io'
      print *,'tasks, nanals, ntasks_io = ',numproc,nanals,ntasks_io
      call stop2(19)
   endif
   print *,'ntasks_io = ',ntasks_io
   print *,'nanals_per_iotask = ',nanals_per_iotask
   !do np=0,ntasks_io-1
   !   print *,'task,nanal1,nanal2',np+1,nanal1(np),nanal2(np)
   !enddo
   if (trim(datapath) == '') then
      print *,'need to specify datapath in namelist!'
      call stop2(19)
   end if
   if(regional .and. .not. arw .and. .not. nmm .and. .not. nmmb) then
      print *, 'must select either arw, nmm or nmmb regional dynamical core'
      call stop2(19)
   endif
   if (fv3_native .and. (trim(fv3fixpath) == '' .or. nx_res == 0 .or. ny_res == 0 )) then
      print *, 'must specify nx_res,ny_res and fv3fixpath when fv3_native is true'
      call stop2(19)
   endif

   print *, trim(adjustl(datapath))
   if (datestring .ne. '0000000000') print *, 'analysis time ',datestring
   if (neigv > 0) then
      print *,nanals,' (unmodulated) members'
      print *,neigv,' eigenvectors for vertical localization'
      print *,nanals*neigv,' modulated ensemble members'
   else
      print *,nanals,' members'
   endif

end if

! background forecast time for analysis
nbackgrounds=0
do while (nhr_anal(nbackgrounds+1) > 0)
   write(charfhr_anal(nbackgrounds+1),'(i2.2)') nhr_anal(nbackgrounds+1)
   if (trim(fgfileprefixes(nbackgrounds+1)) .eq. "") then
     ! default first-guess file prefix
     if (regional) then
      if (nbackgrounds > 1) then
        fgfileprefixes(nbackgrounds+1)="firstguess_fhr"//charfhr_anal(nbackgrounds+1)//"."
      else
        fgfileprefixes(nbackgrounds+1)="firstguess."
      endif
     else  ! global
      fgfileprefixes(nbackgrounds+1)="sfg_"//datestring//"_fhr"//charfhr_anal(nbackgrounds+1)//"_"
     endif
   endif
   if (trim(fgsfcfileprefixes(nbackgrounds+1)) .eq. "") then
      fgsfcfileprefixes(nbackgrounds+1)="sfgsfc_"//datestring//"_fhr"//charfhr_anal(nbackgrounds+1)//"_"
   end if
   nbackgrounds = nbackgrounds+1
end do

! state fields
nstatefields=0
do while (nhr_state(nstatefields+1) > 0)
   write(charfhr_state(nstatefields+1),'(i2.2)') nhr_state(nstatefields+1)
   if (trim(statefileprefixes(nstatefields+1)) .eq. "") then
     ! default first-guess file prefix
     if (regional) then
      if (nstatefields > 1) then
        statefileprefixes(nstatefields+1)="firstguess_fhr"//charfhr_state(nstatefields+1)//"."
      else
        statefileprefixes(nstatefields+1)="firstguess."
      endif
     else  ! global
      statefileprefixes(nstatefields+1)="sfg_"//datestring//"_fhr"//charfhr_state(nstatefields+1)//"_"
     endif
   endif
   if (trim(statesfcfileprefixes(nstatefields+1)) .eq. "") then
      statesfcfileprefixes(nstatefields+1)="sfgsfc_"//datestring//"_fhr"//charfhr_state(nstatefields+1)//"_"
   end if
   nstatefields = nstatefields+1
end do

do nb=1,nbackgrounds
   if (trim(anlfileprefixes(nb)) .eq. "") then
     ! default analysis file prefix
     if (regional) then
      if (nbackgrounds > 1) then
        anlfileprefixes(nb)="analysis_fhr"//charfhr_anal(nb)//"."
      else
        anlfileprefixes(nb)="analysis."
      endif
     else ! global
!      if (nbackgrounds > 1) then
        anlfileprefixes(nb)="sanl_"//datestring//"_fhr"//charfhr_anal(nb)//"_"
!      else
!        anlfileprefixes(nb)="sanl_"//datestring//"_"
!      endif
     endif
   endif
enddo

if (nproc .eq. 0) then
  print *,'number of background forecast times to be used for H(x) = ',nstatefields
  print *,'first-guess forecast hours for observation operator = ',&
  charfhr_state(1:nstatefields)
endif

if (nproc .eq. 0) then
  print *,'number of background forecast times to be updated = ',nbackgrounds
  print *,'first-guess forecast hours for analysis = ',&
  charfhr_anal(1:nbackgrounds)
endif

call init_constants(.false.) ! initialize constants.
call init_constants_derived()

if (nproc == 0) then
    if (analpertwtnh > 0) then
       print *,'using multiplicative inflation based on Pa/Pb'
    else if (analpertwtnh < 0) then
       print *,'using relaxation-to-prior inflation'
    else
       print *,'no inflation'
    endif
end if

! rescale covariance localization length
corrlengthnh = corrlengthnh * 1.e3_r_single/rearth
corrlengthtr = corrlengthtr * 1.e3_r_single/rearth
corrlengthsh = corrlengthsh * 1.e3_r_single/rearth

! this var is .false. until this routine is called.
params_initialized = .true.

if (datapath(len_trim(datapath):len_trim(datapath)) .ne. '/') then
   ! add trailing slash if needed
   if (nproc .eq. 0) print *,'adding trailing slash to datapath..'
   datapath = trim(datapath)//'/'
endif

end subroutine read_namelist

subroutine cleanup_namelist
 if (allocated(nanal1)) deallocate(nanal1)
 if (allocated(nanal2)) deallocate(nanal2)
 if (allocated(vlocal_evecs)) deallocate(vlocal_evecs)
end subroutine cleanup_namelist

end module params
