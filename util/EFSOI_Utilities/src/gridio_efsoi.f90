 module gridio_efsoi
!$$$  module documentation block
!
! module: gridio                     subroutines for reading and writing
!                                    ensemble members files using
!                                    EnKF internal format.  A separate
!                                    program must be run before and
!                                    after the EnKF analysis to convert
!                                    to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: I/O for ensemble member files.
!
! Public Functions:
!  readgriddata, writegriddata
!
! this version reads and writes NCEP GFS sigma files.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Initial version.
!   2015-06-29  Add ability to read/write multiple time levels
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP, DPDT removed)
!               For GFS and NMMB
!   2017-06-14  Adding functionality to optionally write non-inflated ensembles,  
!               a required input for EFSO calculations 
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,fv,rd,grav,tiny_r_kind
 use params, only: nlons,nlats,ndim,reducedgrid,nvars,nlevs,use_gfs_nemsio,pseudo_rh, &
                   cliptracers,nlons,nlats,datestring,datapath,massbal_adjust,&
                   nbackgrounds,fgfileprefixes,anlfileprefixes,imp_physics,&
                   tref,pref
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: ntrunc,npts,ptop  ! gridinfo must be called first!
 use specmod, only: sptezv_s, sptez_s, init_spec_vars, ndimspec => nc, &
                    isinitialized
 use reducedgrid_mod, only: regtoreduced, reducedtoreg
 use mpisetup, only: nproc
 implicit none
 private
 public :: readgriddata, writegriddata
 contains

 subroutine get_weight 
  use nemsio_module
  implicit none 
 
  character(len=500) :: filename 
 
  real(r_kind), dimension(npts,nlevs+1) :: pressi 
  real(r_kind), dimension(nlons*nlats) :: ug 
  real(r_kind), dimension(npts) :: tmpgrd 
  type(sigio_data) sigdata 
 
  real(r_kind), allocatable, dimension(:) :: ak,bk 
  real(r_kind) :: sumcoslat 
 
  integer(i_kind) nlevsin,ntrunc 
  integer(i_kind) i,j,k,iunitsig,iret 
 
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
  if(reducedgrid) then 
     k=0 
     do i=1,nlats 
        do j=1,lonsperlat(i) 
           k=k+1 
           grweight(k) = cos(gaulats(i)) * real(nlonsfull,r_kind) & 
                / real(lonsperlat(i),r_kind) 
           sumcoslat = sumcoslat + grweight(k) 
        end do 
     end do 
  else 
     do i=1,nlons*nlats 
        grweight(i) = cos(latsgrd(i)) 
        sumcoslat = sumcoslat + grweight(i) 
     end do 
  end if 
  sumcoslat = 1.0_r_kind / sumcoslat 
  grweight(:) = sqrt(grweight(:)*sumcoslat) 
  if (reducedgrid) then 
     call regtoreduced(ug,tmpgrd) 
  else 
     tmpgrd(:) = ug(:) 
  end if

  !==> input psg is ln(ps) in centibars - convert to ps in Pa. 
  tmpgrd = 1000._r_kind*exp(tmpgrd) 
  ! calculate half level pressures 
  if (idvc == 0) then ! sigma coordinate, old file format. 
     ak = zero 
     bk = nems_vcoord(1:nlevs+1,1,1)
  else if (idvc == 1) then ! sigma coordinate 
     ak = zero 
     bk = nems_vcoord(1:nlevs+1,2,1) 
  else if (idvc == 2 .or. idvc == 3) then ! hybrid coordinate 
     ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1)
     bk = nems_vcoord(1:nlevs+1,2,1)
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
  real(r_kind), dimension(npts_max,ndim), intent(inout) :: grdin 
  real(r_kind) cptr,qweight,rdtrpr 
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
 
 subroutine readgriddata(nanal,ft,mode,grdin,qsat,infilename)
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getfilehead,nemsio_getheadvar,nemsio_realkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  implicit none

  character(len=500) :: filename
  character(len=3) charnanal
  integer, intent(in) :: nanal
  integer, intent(in) :: ft
  integer, intent(in) :: mode
  real(r_double), dimension(npts,nlevs), intent(out) :: qsat
  real(r_single), dimension(npts,ndim), intent(out) :: grdin
  character(*), intent(in), optional :: infilename

  real(r_kind) kap,kapr,kap1,cptr,qweight,rdtrpr,clip

  real(r_kind), allocatable, dimension(:,:) :: vmassdiv
  real(r_single), allocatable, dimension(:,:) :: pressi,pslg
  real(r_kind), dimension(nlons*nlats) :: ug,vg
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec
  real(r_kind), allocatable, dimension(:) :: psg,pstend,ak,bk
  real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  type(sigio_head) sighead
  type(sigio_data) sigdata
  character(len=2) :: charft
  type(nemsio_gfile) :: gfile


  integer(i_kind) k,nt,iunitsig,iret,nb,idvc,nlonsin,nlatsin,nlevsin
  logical ice

  ! File name 
  write(charft,'(i2.2)') ft 
  if(nanal == 0) then 
     charnanal = 'ensmean' 
  else 
     write(charnanal,'(a,i3.3)') 'mem',nanal 
  end if 
  if(present(infilename)) then 
     filename=trim(adjustl(datapath))//infilename 
  else if(mode == 0) then 
     filename = trim(adjustl(datapath))//"sfg_"//gdatestring//"_fhr"//charft & 
          & //"_"//trim(adjustl(charnanal)) 
  else if(mode == 1) then 
     if(ft == 0) then 
        filename = trim(adjustl(datapath))//"sanl_"//datestring & 
             & //"_"//trim(adjustl(charnanal)) 
     else 
        filename = trim(adjustl(datapath))//"sfg_"//datestring//"_fhr"//charft & 
             & //"_"//trim(adjustl(charnanal)) 
     end if 
  else 
     print *,'This mode is not supported: mode=',mode 
     call stop2(23) 
  end if 

  call nemsio_init(iret=iret)
  if(iret/=0) then
     write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_init, iret=',iret
     call stop2(23)
  end if

  call nemsio_open(gfile,filename,'READ',iret=iret)
  if (iret/=0) then
     write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_open, iret=',iret
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

  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pslg(npts,nlevs))
  allocate(psg(nlons*nlats),pstend(nlons*nlats))

  if (use_gfs_nemsio) then
     call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
         write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ps), iret=',iret
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
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
           call stop2(23)
        endif
        ug = nems_wrk
        call nemsio_readrecv(gfile,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
            call stop2(23)
        endif
        vg = nems_wrk
        if (reducedgrid) then
           call regtoreduced(ug,grdin(:,k))
           call regtoreduced(vg,grdin(:,nlevs+k))
        else
           grdin(:,k) = ug
           grdin(:,nlevs+k) = vg
        endif

        if(.not. present(qsat)) then 
           grdin(:,k) = weight(:,k) * grdin(:,k) 
           grdin(:,nlevs+k) = weight(:,k) * grdin(:,nlevs+k) 
        end if

        call nemsio_readrecv(gfile,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
        call nemsio_readrecv(gfile,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif

        if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
        nems_wrk = nems_wrk * ( 1.0 + fv*nems_wrk2 ) ! convert T to Tv
        ug = nems_wrk
        vg = nems_wrk2
        if (reducedgrid) then
           call regtoreduced(ug,grdin(:,2*nlevs+k,nb))
           call regtoreduced(vg,grdin(:,3*nlevs+k,nb))
        else
           grdin(:,2*nlevs+k,nb) = ug
           grdin(:,3*nlevs+k,nb) = vg
        endif
        if (nvars .ge. 5) then
           call nemsio_readrecv(gfile,'o3mr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
              call stop2(23)
           endif
           if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           if (reducedgrid) then
              call regtoreduced(ug,grdin(:,4*nlevs+k,nb))
           else
              grdin(:,4*nlevs+k,nb) = ug
           endif
           if(nt == 1 .and. .not. present(qsat)) then 
              grdin(:,(3+nt-1)*nlevs+k) = qweight * weight(:,k) * grdin(:,(3+nt-1)*nlevs+k) 
           else if(.not. present(qsat)) then 
              grdin(:,(3+nt-1)*nlevs+k) = 0._r_kind 
           end if 
        endif
        if (nvars .ge. 6) then
           call nemsio_readrecv(gfile,'clwmr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
              call stop2(23)
           endif
           if (imp_physics == 11) then
              call nemsio_readrecv(gfile,'icmr','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(icmr), iret=',iret
                 call stop2(23)
              else
                 nems_wrk2 = nems_wrk2 + nems_wrk
              endif
           endif
           if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           if (reducedgrid) then
              call regtoreduced(ug,grdin(:,5*nlevs+k,nb))
           else
              grdin(:,5*nlevs+k,nb) = ug
           endif
        endif
     enddo

     ! surface pressure is last grid.
     if (reducedgrid) then
        call regtoreduced(psg,grdin(:,ndim,nb))
     else
        grdin(:,ndim,nb) = psg
     endif

     if(.not. present(qsat)) then 
        grdin(:,ndim) = rdtrpr * grweight(:) * 100._r_kind * grdin(:,ndim) 
        deallocate(pressi,pslg) 
        deallocate(psg,pstend,ak,bk) 
        if (massbal_adjust) deallocate(vmassdiv) 
        return 
     end if 
  
     ! surface pressure tendency is next to last grid.
     if (massbal_adjust) then
        pstend = sum(vmassdiv,2)
        if (nanal .eq. 1) &
        print *,nanal,'min/max first-guess ps tend',minval(pstend),maxval(pstend)
     if (reducedgrid) then
        call regtoreduced(pstend,grdin(:,ndim-1,nb))
     else
        grdin(:,ndim-1,nb) = pstend
     endif
  endif

  ! compute saturation q.
  do k=1,nlevs
     ! layer pressure from phillips vertical interolation
     ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
             (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
     if (reducedgrid) then
        call regtoreduced(ug,pslg(:,k))
     else
        pslg(:,k) = ug
     endif
  end do

  if (pseudo_rh) then
     call genqsat1(grdin(:,3*nlevs+1:4*nlevs,nb),qsat(:,:,nb),pslg,grdin(:,2*nlevs+1:3*nlevs,nb),ice,npts,nlevs)
  else
     qsat(:,:,nb) = 1._r_double
  end if
  
  deallocate(pressi,pslg)
  deallocate(psg,pstend)

  call nemsio_close(gfile,iret=iret) 

 end subroutine readgriddata

 end module gridio
