 module gridio_efsoi
!$$$  module documentation block
!
! module: gridio_efsoi               subroutines for reading and writing nemsio
!                                    analysis and forecast files using EnKF
!                                    internal format.  A separate program must
!                                    be run before and after the EnKF analysis to 
!                                    convert to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
! prgmmr: groff            org: emc                    date: 2018-05-24
!
! abstract: Functionality to read nemsio analysis and forecast files.
!
! Public Functions:
!  readgriddata_efsoi
!
! this version reads nemsio files for EFSOI applications.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Initial version.
!   2015-06-29  Add ability to read/write multiple time levels
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP, DPDT
!               removed)
!   2016-11-29  shlyaeva: Add reading/calculating tsen, qi, ql. Pass filenames and
!               hours to read routine to read separately state and control files. 
!               Pass levels and dimenstions to read/write routines (dealing with
!               prse: nlevs + 1 levels). Pass "reducedgrid" parameter.
!   2017-06-14  Adding functionality to optionally write non-inflated ensembles,  
!               a required input for EFSO calculations
!   2018-05-24  Pruning available EnKF nemsio read functionality for EFSOI
!               application.  Add additional routines to compute EFSOI relevant
!               quantities from files read.
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,fv,rd,tiny_r_kind,max_varname_length,t0c,r0_05,tref,pref,constants_initialized,hvap
 use params, only: nlons,nlats,nlevs,use_gfs_nemsio,pseudo_rh,wmoist,andataname,datehr, &
                   cliptracers,datapath,imp_physics,nvars,datestring ! reducedgrid
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo_efsoi, only: ntrunc,npts,latsgrd,ncdim  ! getgridinfo_efsoi must be called first!
 use specmod, only: sptezv_s, sptez_s, init_spec_vars, ndimspec => nc, &
                    isinitialized, gaulats
 use reducedgrid_mod, only: regtoreduced, reducedtoreg, lonsperlat, nlonsfull
 use mpisetup, only: nproc
 use mpeu_util, only: getindex
 use sigio_module
 use nemsio_module
 use loadbal!, only: numptsperproc
 implicit none
 private
 public :: readgriddata_efsoi, get_weight, destroy_weight, divide_weight
 real(r_kind), allocatable, dimension(:,:), save :: weight
 real(r_kind), allocatable, dimension(:), save :: grweight
 contains

 subroutine get_weight 
  use nemsio_module 
  use sigio_module
  implicit none 
 
  character(len=500) :: filename 
 
  real(r_kind), dimension(npts,nlevs+1) :: pressi 
!  real(r_kind), dimension(nlons*nlats) :: ug 
  real(r_single), dimension(npts) :: tmpgrd 
 ! type(sigio_data) sigdata 
  type(nemsio_gfile) :: gfile
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk
  real(r_kind), dimension(nlons*nlats) :: psfc
  real(r_single),allocatable,dimension(:,:,:)   :: nems_vcoord 
  real(r_kind), allocatable, dimension(:) :: ak,bk 
  real(r_kind) :: sumcoslat 
 
  integer(i_kind) :: nlevsin,nlonsin,nlatsin,idvc 
  integer(i_kind) :: i,j,k,iret!iunitsig,iret 
 
  allocate(weight(npts,nlevs)) 
  allocate(grweight(npts)) 
  filename = trim(adjustl(datapath))//andataname 
  ! Read analysis data 
  call nemsio_init(iret=iret)
  if(iret/=0) then
     write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_init,iret=',iret
     call stop2(23)
  end if
  call nemsio_open(gfile,filename,'READ',iret=iret)
  if (iret/=0) then
     write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_open,iret=',iret
     call stop2(23)
  endif
  call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                          dimz=nlevsin,idvc=idvc)
  if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
     print *,'incorrect dims in nemsio file'
     print *,'expected',nlons,nlats,nlevs
     print *,'got',nlonsin,nlatsin,nlevsin
     call stop2(23)
  end if
  allocate(ak(nlevs+1))
  allocate(bk(nlevs+1))
  if (.not. constants_initialized) then
     print *,'constants not initialized (with init_constants, init_constants_derived)'
     call stop2(23)
  end if

  ! calculate weight on the grid point
  sumcoslat = zero
 ! if(reducedgrid) then
     k=0
     do i=1,nlats 
        do j=1,lonsperlat(i) 
           k=k+1 
           grweight(k) = cos(gaulats(i)) * real(nlonsfull,r_kind) & 
                / real(lonsperlat(i),r_kind) 
           sumcoslat = sumcoslat + grweight(k) 
        end do 
     end do 
 ! else 
 !    do i=1,nlons*nlats 
 !       grweight(i) = cos(latsgrd(i)) 
 !       sumcoslat = sumcoslat + grweight(i) 
 !    end do 
 ! end if 
  sumcoslat = 1.0_r_kind / sumcoslat 
  grweight(:) = sqrt(grweight(:)*sumcoslat)
 
  ! Extract surface pressure in pa
  ! to aid in quantifying mass
  call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
  if (iret/=0) then
     write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(ps), iret=',iret
     call stop2(23)
  endif
  ! Assign surface pressure in pa
  psfc = nems_wrk  

  ! Extract surface pressure
  ! on reduced gaussian grid
!  if (reducedgrid) then
     call regtoreduced(psfc,tmpgrd)
!  else
!     tmpgrd(:) = ug(:)
!  end if

  ! calculate half level pressures 
  if (allocated(nems_vcoord))     deallocate(nems_vcoord)
  allocate(nems_vcoord(nlevs+1,3,2))
  call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
  if ( iret /= 0 ) then
     write(6,*)' gridio:  ***ERROR*** problem reading header ', &
        'vcoord, Status = ',iret
     call stop2(99)
  endif    

  if ( idvc == 0 ) then ! sigma coordinate, old file format.
      ak = zero
      bk = nems_vcoord(1:nlevs+1,1,1)
  else if ( idvc == 1 ) then ! sigma coordinate
      ak = zero
      bk = nems_vcoord(1:nlevs+1,2,1)
  else if ( idvc == 2 .or. idvc == 3 ) then ! hybrid coordinate
      ak = nems_vcoord(1:nlevs+1,1,1)
      ak = nems_vcoord(1:nlevs+1,2,1)
  else
      write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
      call stop2(85)
  end if
  !==> pressure at interfaces. 
  do k=1,nlevs+1 
     pressi(:,k)=ak(k)+bk(k)*tmpgrd 
  end do 
  deallocate(ak,bk) 
!$omp parallel do private(k) shared(weight,pressi,grweight,nlevs) 
  do k=1,nlevs 
     ! sqrt(dp)*sqrt(area) 
     weight(:,k)=sqrt((pressi(:,k)-pressi(:,k+1))/tmpgrd(:))*grweight(:) 
  end do 
!$omp end parallel do 
  return 
 end subroutine get_weight

 subroutine destroy_weight 
  implicit none 
  if(allocated(weight)) deallocate(weight) 
  if(allocated(grweight)) deallocate(grweight) 
 end subroutine destroy_weight 
 
 subroutine divide_weight(grdin) 
  implicit none 
  real(r_single), dimension(npts_max,ncdim), intent(inout) :: grdin 
  real(r_single) cptr,qweight,rdtrpr 
  integer(i_kind) :: k,npt 
  cptr = real(sqrt(tref/cp),r_kind) 
  qweight = real(sqrt(cp*tref/wmoist)/hvap,r_kind) 
  rdtrpr = real(sqrt(pref/(rd*tref)),r_kind) 
  do npt=1,numptsperproc(nproc+1) 
     do k=1,nlevs 
        grdin(npt,k) = grdin(npt,k) / weight(indxproc(nproc+1,npt),k) 
        grdin(npt,nlevs+k) = grdin(npt,nlevs+k) / weight(indxproc(nproc+1,npt),k) 
        grdin(npt,2*nlevs+k) = grdin(npt,2*nlevs+k) * cptr / weight(indxproc(nproc+1,npt),k) 
        if (nvars .gt. 3) then 
           grdin(npt,3*nlevs+k) = grdin(npt,3*nlevs+k) * qweight / weight(indxproc(nproc+1,npt),k) 
        end if 
     end do 
     grdin(npt,nvars*nlevs+1) = grdin(npt,nvars*nlevs+1) & 
          & * rdtrpr / grweight(indxproc(nproc+1,npt)) 
  end do 
  return 
 end subroutine divide_weight 

 subroutine readgriddata_efsoi(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,mode,grdin,ft,infilename,qsat)
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getfilehead,nemsio_getheadvar,nemsio_realkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  implicit none

  integer, intent(in) :: nanal
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  character(len=7) charnanal
  integer, intent(in) :: n2d, n3d
  integer, dimension(0:n3d), intent(in) :: levels
  integer, intent(in) :: ndim, ntimes
  integer, intent(in) :: ft
  integer, intent(in) :: mode ! 0: first guess, 1: analysis
  character(*), intent(in), optional :: infilename
 ! logical, intent(in) :: reducedgrid
  real(r_single), dimension(npts,ndim,ntimes), intent(out) :: grdin
  real(r_double), dimension(npts,nlevs,ntimes), intent(out), optional :: qsat
  real(r_kind) cptr,qweight,rdtrpr

  character(len=500) :: filename
 ! character(len=7) charnanal
  character(len=2) charft

  real(r_kind) :: kap,kapr,kap1,clip,qi_coef

  real(r_kind), allocatable, dimension(:,:)     :: vmassdiv
  real(r_single), allocatable, dimension(:,:)   :: pressi,pslg
  real(r_kind), dimension(nlons*nlats)          :: ug,vg
  real(r_single), dimension(npts,nlevs)         :: tv, q, cw
  real(r_kind), dimension(ndimspec)             :: vrtspec,divspec
  real(r_kind), allocatable, dimension(:)       :: psg,pstend,ak,bk
  
  real(r_single),allocatable,dimension(:,:,:)   :: nems_vcoord
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
! type(sigio_head)   :: sighead
 ! type(sigio_data)   :: sigdata
  type(nemsio_gfile) :: gfile

  integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer(i_kind) :: tsen_ind, ql_ind, qi_ind, prse_ind
  integer(i_kind) :: ps_ind, pst_ind, sst_ind

  integer(i_kind) :: k,iret,nb,i,idvc,nlonsin,nlatsin,nlevsin
  logical ice

  ! Construct the File name based on
  ! input arguments
  write(charft, '(i3.3)') ft
  if (nanal > 0) then
    write(charnanal,'(a3, i3.3)') 'mem', nanal
  else
    charnanal = 'ensmean'
  endif

  !if(analysis_impact == .true.) then
     if(present(infilename)) then
        filename=trim(adjustl(datapath))//infilename
     else if(mode == 0) then
        filename = trim(adjustl(datapath))//"gdas.t"//datehr//"z.atmf"//charft//".ensmean.nemsio"
     else if(mode == 1) then
        if(ft == 0) then 
           filename = trim(adjustl(datapath))//"gdas.t"//datehr//"z.atmanl.ensmean.nemsio"
        else
           filename = trim(adjustl(datapath))//trim(adjustl(charnanal))//"/"// &
                "gdas.t"//datehr//"z.atmf"//charft//".nemsio"
        end if 
     else
        print *,'This mode is not supported: mode=',mode
        call stop2(23)
     end if
 ! else
 !    if(present(infilename)) then
 !       filename=trim(adjustl(datapath))//infilename
 !    else if(mode == 0) then
 !       filename = trim(adjustl(datapath))//"gdas.t"//datehr//"z.atmf"//ft//".ensmean.nemsio"
 !    else if(mode == 1) then
 !       if(ft == 0) then
 !          filename = trim(adjustl(datapath))//"gdas.t"//datehr//"z.atmanl.ensmean.nemsio"
 !       else
 !          filename = trim(adjustl(datapath))//trim(adjustl(charnanal))//"/"// &
 !               "gdas.t"//datehr//"z.atmf"//ft//".nemsio"
 !       end if
 !    else
 !       print *,'This mode is not supported: mode=',mode
 !       call stop2(23)
 !    end if
 ! end if
           
  call nemsio_init(iret=iret)
  if(iret/=0) then
     write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_init, iret=',iret
     call stop2(23)
  end if
  call nemsio_open(gfile,filename,'READ',iret=iret)
  if (iret/=0) then
     write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_open, iret=',iret
     call stop2(23)
  endif
  call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                          dimz=nlevsin,idvc=idvc)
  if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
     print *,'incorrect dims in nemsio file'
     print *,'expected',nlons,nlats,nlevs
     print *,'got',nlonsin,nlatsin,nlevsin
     call stop2(23)
  end if

  ice = .false. ! calculate qsat w/resp to ice?
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one
  cptr = sqrt(cp/tref)
  qweight = sqrt(wmoist/(cp*tref))*hvap
  rdtrpr = sqrt(rd*tref)/pref

  u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
  ql_ind  = getindex(vars3d, 'ql')
  qi_ind  = getindex(vars3d, 'qi')
  prse_ind = getindex(vars3d, 'prse')

  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero
  sst_ind = getindex(vars2d, 'sst')

!  if (nproc == 0) then
!    print *, 'indices: '
!    print *, 'u: ', u_ind, ', v: ', v_ind, ', tv: ', tv_ind, ', tsen: ', tsen_ind
!    print *, 'q: ', q_ind, ', oz: ', oz_ind, ', cw: ', cw_ind, ', qi: ', qi_ind
!    print *, 'ql: ', ql_ind, ', prse: ', prse_ind
!    print *, 'ps: ', ps_ind, ', pst: ', pst_ind, ', sst: ', sst_ind
!  endif

  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pslg(npts,nlevs))
  allocate(psg(nlons*nlats))
  if (pst_ind > 0) allocate(vmassdiv(nlons*nlats,nlevs),pstend(nlons*nlats))
 
  call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
  if (iret/=0) then
     write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(ps), iret=',iret
     call stop2(23)
  endif
  psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.

  if (allocated(nems_vcoord))     deallocate(nems_vcoord)
  allocate(nems_vcoord(nlevs+1,3,2))
  call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
  if ( iret /= 0 ) then
     write(6,*)' gridio:  ***ERROR*** problem reading header ', &
        'vcoord, Status = ',iret
     call stop2(99)
  endif

  allocate(ak(nlevs+1),bk(nlevs+1))

  if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
     ak = zero
     bk = nems_vcoord(1:nlevs+1,1,1)
  elseif ( idvc == 1 ) then                     ! sigma coordinate
     ak = zero
     bk = nems_vcoord(1:nlevs+1,2,1)
  elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
     ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1) ! convert to mb
     bk = nems_vcoord(1:nlevs+1,2,1)
  else
     write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
     call stop2(85)
  endif
  if (nanal .eq. 1) then
     print *,'time level ',nb
     print *,'---------------'
  endif
  ! pressure at interfaces
  do k=1,nlevs+1
     pressi(:,k)=ak(k)+bk(k)*psg
     if (nanal .eq. 1) print *,'nemsio, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
  enddo
  deallocate(ak,bk)
  
  !==> get U,V,temp,q,ps on gaussian grid.
  ! u is first nlevs, v is second, t is third, then tracers.
  clip=tiny_r_kind
  do k=1,nlevs
     call nemsio_readrecv(gfile,'ugrd','mid layer',k,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
        call stop2(23)
     endif
     ug = nems_wrk
     call nemsio_readrecv(gfile,'vgrd','mid layer',k,nems_wrk,iret=iret)
     if (iret/=0) then
         write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
         call stop2(23)
     endif
     vg = nems_wrk
     if (u_ind > 0)       call copytogrdin(ug,grdin(:,levels(u_ind-1) + k,nb))
     if (v_ind > 0)       call copytogrdin(vg,grdin(:,levels(v_ind-1) + k,nb))

     ! Transformation to EFSOI relevant quantities
     ! Assign weighted kinetic energy components
     if(.not. present(qsat)) then
        grdin(:,levels(u_ind-1) + k,nb) = weight(:,levels(u_ind-1) + k) * grdin(:,levels(u_ind-1) + k,nb)
        grdin(:,levels(v_ind-1) + k,nb) = weight(:,levels(v_ind-1) + k) * grdin(:,levels(v_ind-1) + k,nb)
     end if
  
     ! calculate vertical integral of mass flux div (ps tendency)
     ! this variable is analyzed in order to enforce mass balance in the analysis
     if (pst_ind > 0) then
        ug = ug*(pressi(:,k)-pressi(:,k+1))
        vg = vg*(pressi(:,k)-pressi(:,k+1))
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     endif
     call nemsio_readrecv(gfile,'tmp','mid layer',k,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
        call stop2(23)
     endif
     call nemsio_readrecv(gfile,'spfh','mid layer',k,nems_wrk2,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
        call stop2(23)
     endif
     if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
     ug = nems_wrk
     if (tsen_ind > 0)    call copytogrdin(ug,grdin(:,levels(tsen_ind-1)+k,nb))
     nems_wrk = nems_wrk * ( 1.0 + fv*nems_wrk2 ) ! convert T to Tv
     ug = nems_wrk
     vg = nems_wrk2
     call copytogrdin(ug,tv(:,k))
     call copytogrdin(vg, q(:,k))
    
     ! Transformation to EFSOI relevant quantities
     ! Mass component
     tv(:,k) = cptr * weight(:,k) * tv(:,k)
     ! Moisture component
     q(:,k) = qweight * weight(:,k) * q(:,k)

     if (tv_ind > 0)               grdin(:,levels(tv_ind-1)+k,nb) = tv(:,k)
     if (q_ind > 0)                grdin(:,levels( q_ind-1)+k,nb) =  q(:,k)
     if (oz_ind > 0) then
        call nemsio_readrecv(gfile,'o3mr','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
           call stop2(23)
        endif
        if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
        ug = nems_wrk2
        call copytogrdin(ug,grdin(:,levels(oz_ind-1)+k,nb))
        endif
        if (cw_ind > 0 .or. ql_ind > 0 .or. qi_ind > 0) then
           call nemsio_readrecv(gfile,'clwmr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
              call stop2(23)
           endif
        if (imp_physics == 11) then
           call nemsio_readrecv(gfile,'icmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata_efsoi: gfs model: problem with nemsio_readrecv(icmr), iret=',iret
              call stop2(23)
           else
              nems_wrk2 = nems_wrk2 + nems_wrk
           endif
        endif
        if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           call copytogrdin(ug,cw(:,k))
           if (cw_ind > 0)            grdin(:,levels(cw_ind-1)+k,nb) = cw(:,k)
     endif
  enddo

  ! surface pressure
  if (ps_ind > 0) then
    call copytogrdin(psg,grdin(:,levels(n3d) + ps_ind,nb))
  endif

  ! Transformation to EFSOI relevant quantities
  ! Surface pressure contribution
  grdin(:,levels(n3d) + ps_ind,nb) = rdtrpr * grweight(:) * 100._r_kind * grdin(:,levels(n3d) + ps_ind,nb)

  ! surface pressure tendency
  if (pst_ind > 0) then
     pstend = sum(vmassdiv,2)
     if (nanal .eq. 1) &
     print *,nanal,'min/max first-guess ps tend',minval(pstend),maxval(pstend)
     call copytogrdin(pstend,grdin(:,levels(n3d) + pst_ind,nb))
  endif

  ! compute saturation q.
  do k=1,nlevs
    ! layer pressure from phillips vertical interolation
    ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
            (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr

    call copytogrdin(ug,pslg(:,k))
    ! Jacobian for gps in pressure is saved in different units in GSI; need to
    ! multiply pressure by 0.1
    if (prse_ind > 0)     grdin(:,levels(prse_ind-1)+k,nb) = 0.1*pslg(:,k)

  end do
  if (pseudo_rh) then
     call genqsat1(q,qsat(:,:,nb),pslg,tv,ice,npts,nlevs)
  else
     qsat(:,:,nb) = 1._r_double
  end if

  ! cloud derivatives
  if (ql_ind > 0 .or. qi_ind > 0) then
     do k = 1, nlevs
        do i = 1, npts
           qi_coef        = -r0_05*(tv(i,k)/(one+fv*q(i,k))-t0c)
           qi_coef        = max(zero,qi_coef)
           qi_coef        = min(one,qi_coef)    ! 0<=qi_coef<=1
           if (ql_ind > 0) then 
             grdin(i,levels(ql_ind-1)+k,nb) = cw(i,k)*(one-qi_coef)
           endif
           if (qi_ind > 0) then
             grdin(i,levels(qi_ind-1)+k,nb) = cw(i,k)*qi_coef
           endif
        enddo
     enddo
  endif

  if (sst_ind > 0) then
    grdin(:,levels(n3d)+sst_ind, nb) = zero
  endif

  deallocate(pressi,pslg)
  deallocate(psg)
  if (pst_ind > 0) deallocate(vmassdiv,pstend)
  if (use_gfs_nemsio) call nemsio_close(gfile,iret=iret)

  return
 
  contains
 ! copying to grdin (calling regtoreduced if reduced grid)
  subroutine copytogrdin(field, grdin)
  implicit none

  real(r_kind), dimension(:), intent(in)      :: field
  real(r_single), dimension(:), intent(inout) :: grdin

!  if (reducedgrid) then
    call regtoreduced(field, grdin)
!  else
!    grdin = field
!  endif

  end subroutine copytogrdin

 end subroutine readgriddata_efsoi

end module gridio_efsoi
