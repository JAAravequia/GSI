module gridio

  !========================================================================

  !$$$ Module documentation block
  ! 
  ! This module contains various routines to ingest and update
  ! variables from Weather Research and Forecasting (WRF) model Advanced
  ! Research WRF (ARW) and Non-hydrostatic Mesoscale Model (NMM) dynamical
  ! cores which are required by the Ensemble Kalman Filter (ENKF) currently
  ! designed for operations within the National Centers for Environmental
  ! Prediction (NCEP) Global Forecasting System (GFS)
  !
  ! prgmmr: Winterbottom        org: ESRL/PSD1       date: 2011-11-30
  !
  ! program history log:
  !   
  !   2011-11-30 Winterbottom - Initial version.
  !
  !   2019-01-?? Ting  -- 
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================
  ! Define associated modules
  use gridinfo, only:  npts
  use kinds,    only: r_double, r_kind, r_single, i_kind
  use mpisetup, only: nproc
  use netcdf_io
  use params,   only: nlevs, cliptracers, datapath, arw, nmm
  use params,   only: nx_res,ny_res,nlevs,ntiles
  use params,   only:  pseudo_rh
  USE params,   ONLY: fgfileprefixes,anlfileprefixes,datestring
  use mpeu_util, only: getindex
  USE read_fv3_restarts ,ONLY: &
       &read_fv3_restart_data2d,&
       &read_fv3_restart_data3d,read_fv3_restart_data4d
  use netcdf_mod,only: nc_check

  USE gridinfo, ONLY: ntracers_gocart,vars3d_supported_aero

  implicit none

  !-------------------------------------------------------------------------
  ! Define all public subroutines within this module
  private
  public :: readgriddata
  public :: writegriddata

  !-------------------------------------------------------------------------

contains
  ! Generic WRF read routine, calls ARW-WRF or NMM-WRF
  SUBROUTINE readgriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,reducedgrid,vargrid,qsat)
   USE constants, ONLY:zero,one,half,fv, max_varname_length,cp,rd
   use gridinfo,only: ak,bk,ptop
   use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
   use netcdf, only: nf90_inq_dimid,nf90_inq_varid
   use netcdf, only: nf90_nowrite,nf90_write,nf90_inquire,nf90_inquire_dimension
   implicit none
   INTEGER, INTENT(in) :: nanal1,nanal2
   INTEGER, intent(in) :: n2d, n3d, ndim, ntimes
   character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
   character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
   integer, dimension(0:n3d), intent(in)        :: levels
   character(len=120), dimension(7), intent(in) :: fileprefixes
   logical, intent(in) :: reducedgrid

   CHARACTER(len=120) :: fileprefix

   REAL(r_kind), PARAMETER :: clip = TINY(1.)
   REAL(r_kind) ::  kap,kapr,kap1

   REAL(r_single), DIMENSION(npts,ndim,ntimes,nanal2-nanal1+1),  INTENT(out) :: vargrid
   REAL(r_double), DIMENSION(npts,nlevs,ntimes,nanal2-nanal1+1), INTENT(out) :: qsat

    ! Define local variables 
    CHARACTER(len=:),ALLOCATABLE  :: filename_core,filename_tracer,&
         &filename_in
    character(len=7)   :: charnanal
    INTEGER(i_kind) file_id,itracer

    real(r_single), dimension(:,:,:), allocatable ::workvar3d,uworkvar3d,&
                        vworkvar3d,tvworkvar3d,&
                        workprsi,qworkvar3d
    real(r_double),dimension(:,:,:),allocatable:: qsatworkvar3d
    real(r_single), dimension(:,:),   allocatable ::pswork

    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname
     
    character(len=1) char_tile
    character(len=24),parameter :: myname_ = 'fv3: getgridio'

    ! Define counting variables
    integer :: nlevsp1
    INTEGER :: i,j, k,nn,ntile,nn_tile0, nb, ne, nanal
    integer :: u_ind, v_ind, tv_ind,tsen_ind, q_ind, oz_ind
    integer :: ps_ind, sst_ind
    INTEGER(i_kind), DIMENSION(ntracers_gocart) :: aero_ind

    integer :: ttind 
    logical :: ice

    !======================================================================

    IF (reducedgrid) THEN
       PRINT *,'reducedgrid for fv3_native not implemented'
       CALL stop2(716)
    ENDIF

    fileprefix=fileprefixes(1)

    kap=rd/cp
    kapr=cp/rd
    kap1=kap + one

    nlevsp1=nlevs+1

    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 't')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
!    prse_ind = getindex(vars3d, 'prse') ! pressure

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst') ! SST (2D)

    DO i=1,ntracers_gocart
       aero_ind(i)=getindex(vars3d,vars3d_supported_aero(i))
    ENDDO

    ! Initialize all constants required by routine
    allocate(qworkvar3d(nx_res,ny_res,nlevs))
    allocate(tvworkvar3d(nx_res,ny_res,nlevs))
    allocate(workvar3d(nx_res,ny_res,nlevs))
    allocate(qsatworkvar3d(nx_res,ny_res,nlevs))
    ALLOCATE(workprsi(nx_res,ny_res,nlevsp1))
    ALLOCATE(pswork(nx_res,ny_res))


    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif

    ne = 0
    ensmemloop: DO nanal=nanal1,nanal2
    ne = ne + 1

    IF (nanal > 0) THEN
       WRITE(charnanal,'(a3, i3.3)') 'mem', nanal
    ELSE
       charnanal = 'ensmean'
    ENDIF

    backgroundloop: DO nb=1,ntimes

     DO ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      write(char_tile, '(i1)') ntile

      filename_core = TRIM(datapath)//TRIM(datestring)//"/"//TRIM(charnanal)//"/"//&
           &TRIM(fileprefix)//".fv_core.res.tile"//TRIM(char_tile)//".nc"
      filename_tracer = TRIM(datapath)//TRIM(datestring)//"/"//TRIM(charnanal)//"/"//&
           &TRIM(fileprefix)//".fv_tracer.res.tile"//TRIM(char_tile)//".nc"

    !----------------------------------------------------------------------
      filename_in=filename_core

      call nc_check( nf90_open(trim(adjustl(filename_in)),nf90_nowrite,file_id),&
        myname_,'open: '//trim(adjustl(filename_in)) )

    !----------------------------------------------------------------------
    ! Update u and v variables (same for NMM and ARW)
    ! read u-component  
    if (u_ind > 0) then
    allocate(uworkvar3d(nx_res,ny_res+1,nlevs))
       varstrname = 'u'
       filename_in=TRIM(filename_core)
         call read_fv3_restart_data3d(varstrname,filename_in,file_id,uworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
        do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(u_ind-1)+k,nb,ne)=uworkvar3d(i,j,k) 
         enddo
        enddo
      enddo
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFV3 : u ',                           &
                 & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       enddo

    deallocate(uworkvar3d)
    endif
    IF (v_ind > 0) THEN
    allocate(vworkvar3d(nx_res+1,ny_res,nlevs))
       varstrname = 'v'
       filename_in=TRIM(filename_core)
         call read_fv3_restart_data3d(varstrname,filename_in,file_id,vworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
        do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(v_ind-1)+k,nb,ne)=vworkvar3d(i,j,k) 
         enddo
       enddo
      enddo
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFV3 : v ',                           &
                 & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       enddo
    deallocate(vworkvar3d)

    ENDIF

    varstrname = 'T'
    CALL read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)

    IF (tsen_ind > 0 ) THEN
       DO k=1,nlevs
          nn = nn_tile0
          DO j=1,ny_res
             DO i=1,nx_res
                nn=nn+1
                vargrid(nn,levels(tsen_ind-1)+k,nb,ne)=workvar3d(i,j,k) 
             ENDDO
          ENDDO
       ENDDO
       DO k = levels(tsen_ind-1)+1, levels(tsen_ind)
          IF (nproc .EQ. 0)                                               &
               WRITE(6,*) 'READFV3 : tsen ',                           &
               & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       ENDDO

    ENDIF

    CALL nc_check( nf90_close(file_id),&
         myname_,'close '//TRIM(filename_in) )
    
    varstrname = 'sphum'
    filename_in=TRIM(filename_tracer)
    CALL nc_check( nf90_open(TRIM(ADJUSTL(filename_in)),nf90_nowrite,file_id),&
         myname_,'open: '//TRIM(ADJUSTL(filename_in)) )
    CALL read_fv3_restart_data3d(varstrname,filename_in,file_id,qworkvar3d)
    IF (q_ind > 0) THEN
       DO k=1,nlevs
          nn = nn_tile0
          DO j=1,ny_res
             DO i=1,nx_res
                nn=nn+1
                vargrid(nn,levels(q_ind-1)+k,nb,ne)=qworkvar3d(i,j,k) 
             ENDDO
          ENDDO
       ENDDO
       DO k = levels(q_ind-1)+1, levels(q_ind)
          IF (nproc .EQ. 0)   &
               &WRITE(6,*) 'READFV3 : q ',        &
               & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       ENDDO

       
    ENDIF

    CALL nc_check( nf90_close(file_id),&
         myname_,'close '//TRIM(filename_in) )

    DO k=1,nlevs
       DO j=1,ny_res
          DO i=1,nx_res
             tvworkvar3d(i,j,k)=workvar3d(i,j,k)*(one+fv*qworkvar3d(i,j,k))
          ENDDO
       ENDDO
    ENDDO
    
    IF (tv_ind >  0) THEN
       
       DO k=1,nlevs
          nn = nn_tile0
          DO j=1,ny_res
             DO i=1,nx_res
                nn=nn+1
                vargrid(nn,levels(tv_ind-1)+k,nb,ne)=tvworkvar3d(i,j,k) 
             ENDDO
          ENDDO
       ENDDO
       DO k = levels(ttind-1)+1, levels(ttind)
          IF (nproc .EQ. 0)                                    &
               WRITE(6,*) 'READFV3 : t ',                           &
               & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       ENDDO
       
    ENDIF
    
    if (oz_ind > 0) then
       varstrname = 'o3mr'
       filename_in=TRIM(filename_tracer)
       CALL nc_check( nf90_open(TRIM(ADJUSTL(filename_in)),nf90_nowrite,file_id),&
            myname_,'open: '//TRIM(ADJUSTL(filename_in)) )

         call read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
         do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(oz_ind-1)+k,nb,ne)=workvar3d(i,j,k) 
         enddo
         enddo
      enddo
       do k = levels(oz_ind-1)+1, levels(oz_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFV3 : oz ',                           &
                 & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
       enddo

       CALL nc_check( nf90_close(file_id),&
            myname_,'close '//TRIM(filename_in) )
       
    endif

    filename_in=TRIM(filename_tracer)
    CALL nc_check( nf90_open(TRIM(ADJUSTL(filename_in)),nf90_nowrite,file_id),&
         myname_,'open: '//TRIM(ADJUSTL(filename_in)) )
    
    DO itracer=1,ntracers_gocart

       IF (aero_ind(itracer) > 0) THEN
          varstrname = vars3d_supported_aero(itracer)
          CALL read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
          IF (cliptracers)  WHERE (workvar3d < clip) workvar3d = clip
          DO k=1,nlevs
             nn = nn_tile0
             DO j=1,ny_res
                DO i=1,nx_res
                   nn=nn+1
                   vargrid(nn,levels(aero_ind(itracer)-1)+k,nb,ne)=workvar3d(i,j,k) 
                ENDDO
             ENDDO
          ENDDO

!          DO k = levels(aero_ind(itracer)-1)+1, levels(aero_ind(itracer))
!             IF (nproc .EQ. 0)                                               &
!                  WRITE(6,*) 'READFV3 : '//trim(varstrname),                           &
!                  & k, MINVAL(vargrid(:,k,nb,ne)), MAXVAL(vargrid(:,k,nb,ne))
!          ENDDO

       ENDIF

    ENDDO

    CALL nc_check( nf90_close(file_id),&
         myname_,'close '//TRIM(filename_in) )

! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb,ne) = zero
    endif


    !----------------------------------------------------------------------
    ! Allocate memory for variables computed within routine
 
    varstrname = 'delp'
    filename_in=TRIM(filename_core)
    CALL nc_check( nf90_open(TRIM(ADJUSTL(filename_in)),nf90_nowrite,file_id),&
         myname_,'open: '//TRIM(ADJUSTL(filename_in)) )
    CALL read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)  !cltto think different files be used
!print *,'min/max delp',ntile,minval(delp),maxval(delp)
    CALL nc_check( nf90_close(file_id),&
         myname_,'close '//TRIM(filename_in) )
    
    pswork=SUM(workvar3d,3) + ptop
    
    IF (ps_ind > 0) THEN
       nn = nn_tile0
       DO j=1,ny_res
          DO i=1,nx_res
             nn=nn+1
             vargrid(nn,levels(n3d)+ps_ind, nb,ne) =pswork(i,j) 
          ENDDO
       ENDDO
       
       CALL nc_check( nf90_close(file_id),&
            myname_,'close '//TRIM(filename_in) )
       
    ENDIF
     
    DO k=1,nlevsp1
       DO j=1,ny_res
          DO i=1,nx_res
             workprsi(i,j,k) = ak(nlevsp1-k+1,1)+&
                  &bk(nlevsp1-k+1,1)*pswork(i,j)
          ENDDO
       ENDDO
    ENDDO

    DO k=1,nlevs
       DO j=1,ny_res  
          DO i=1,nx_res
             workvar3d(i,j,k)=&
                  &((workprsi(i,j,k)**kap1- workprsi(i,j,k+1)**kap1)/&
                  &(kap1*(workprsi(i,j,k)-workprsi(i,j,k+1))))**kapr
          ENDDO
       ENDDO
    ENDDO

    ice=.TRUE.  !tothink
    IF (pseudo_rh) THEN
       CALL genqsat1(qworkvar3d,qsatworkvar3d,workvar3d,tvworkvar3d,ice,  &
            nx_res*ny_res,nlevs)
    ELSE
       qsatworkvar3d(:,:,:) = 1._r_double
    ENDIF
    

    DO k=1,nlevs
       nn = nn_tile0
       DO j=1,ny_res
          DO i=1,nx_res
             nn=nn+1
             qsat(nn,k,nb,ne)=qsatworkvar3d(i,j,k) 
          ENDDO
       ENDDO
    ENDDO


    
    !======================================================================
    ! Deallocate memory 
     end do ! ntile loop

    end do backgroundloop ! loop over backgrounds to read in
    end do ensmemloop ! loop over ens members to read in


   if(allocated(workprsi))     deallocate(workprsi)
   if(allocated(pswork))     deallocate(pswork)
   if(allocated(tvworkvar3d)) deallocate(tvworkvar3d)
   if(allocated(qworkvar3d)) deallocate(qworkvar3d)
   if(allocated(qsatworkvar3d)) deallocate(qsatworkvar3d)
   IF(ALLOCATED(workvar3d))             DEALLOCATE(workvar3d)

    return

  end subroutine readgriddata

  !========================================================================
  ! readgriddata_nmm.f90: read WRF-NMM state or control vector
  !-------------------------------------------------------------------------


  !========================================================================
  ! writegriddata.f90: write WRF-ARW or WRF-NMM analysis
  !-------------------------------------------------------------------------

  SUBROUTINE writegriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,vargrid,no_inflate_flag)
    use constants, only: zero, one,fv,half
    use params, only: nbackgrounds
    use params,   only: nx_res,ny_res,nlevs,ntiles
    use gridinfo,only: ak,bk,ptop
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_inq_dimid,nf90_inq_varid
    use netcdf, only: nf90_write,nf90_write,nf90_inquire,nf90_inquire_dimension
    use write_fv3_restarts,only:write_fv3_restart_data1d,write_fv3_restart_data2d
    use write_fv3_restarts,only:write_fv3_restart_data3d,write_fv3_restart_data4d
    include 'netcdf.inc'      

    !----------------------------------------------------------------------
    ! Define variables passed to subroutine
    INTEGER, INTENT(in)  :: nanal1,nanal2, n2d, n3d, ndim
    character(len=*), dimension(n2d), intent(in) :: vars2d
    character(len=*), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    REAL(r_single), DIMENSION(npts,ndim,nbackgrounds,nanal2-nanal1+1), INTENT(in) :: vargrid
    logical, intent(in) :: no_inflate_flag

    !----------------------------------------------------------------------
    ! Define variables computed within subroutine

    CHARACTER(len=:),ALLOCATABLE  :: filename_core,filename_tracer,&
         &filename_in
    character(len=7)    :: charnanal

    !----------------------------------------------------------------------
    integer(i_kind) :: u_ind, v_ind, tv_ind, tsen_ind,q_ind, ps_ind,oz_ind
    integer(i_kind) :: w_ind, cw_ind, ph_ind

    INTEGER(i_kind), DIMENSION(ntracers_gocart) :: aero_ind

    integer(i_kind) file_id
    REAL(r_single), DIMENSION(:,:), ALLOCATABLE ::pswork,psworkinc
    real(r_single), dimension(:,:,:), allocatable ::workvar3d,workinc3d,workinc3d2,uworkvar3d,&
                        vworkvar3d,tvworkvar3d,tsenworkvar3d,&
                        workprsi,qworkvar3d
    !----------------------------------------------------------------------
    ! Define variables required by for extracting netcdf variable
    ! fields
    integer :: nlevsp1
    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname
    character(len=1) char_tile
    character(len=:),allocatable:: varname
    character(len=24),parameter :: myname_ = 'fv3: writegriddata'

    !----------------------------------------------------------------------
    ! Define counting variables
    INTEGER :: i,j,k,nn,ntile,nn_tile0, nb, ne, nanal, itracer

    CHARACTER(len=120) :: fileprefix


    !----------------------------------------------------------------------


    IF (no_inflate_flag) THEN
       PRINT *, 'That would be to not inflate'
       CALL stop2(717)
    ENDIF
    
    fileprefix=anlfileprefixes(1)

    nlevsp1=nlevs+1

    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 't')  ! Tv (3D)
    tsen_ind  = getindex(vars3d, 'tsen')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    cw_ind  = getindex(vars3d, 'cw')  ! CWM for WRF-NMM

    DO i=1,ntracers_gocart
       aero_ind(i)=getindex(vars3d,vars3d_supported_aero(i))
    ENDDO
    
    w_ind   = getindex(vars3d, 'w')   ! W for WRF-ARW
    ph_ind  = getindex(vars3d, 'ph')  ! PH for WRF-ARW

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)

    ALLOCATE(workinc3d(nx_res,ny_res,nlevs),&
         &workinc3d2(nx_res,ny_res,nlevsp1))
    allocate(workvar3d(nx_res,ny_res,nlevs))
    allocate(qworkvar3d(nx_res,ny_res,nlevs))
    allocate(tvworkvar3d(nx_res,ny_res,nlevs))
    ALLOCATE(workprsi(nx_res,ny_res,nlevsp1))
    ALLOCATE(pswork(nx_res,ny_res),psworkinc(nx_res,ny_res))
    
    !----------------------------------------------------------------------
    if (nbackgrounds > 1) then
       write(6,*)'gridio/writegriddata: writing multiple backgrounds not yet supported'
       call stop2(23)
    endif


    ne = 0
    ensmemloop: DO nanal=nanal1,nanal2
    ne = ne + 1

    WRITE(charnanal,'(a3, i3.3)') 'mem', nanal

    backgroundloop: do nb=1,nbackgrounds

    !----------------------------------------------------------------------
    ! First guess file should be copied to analysis file at scripting
    ! level; only variables updated by EnKF are changed
    !----------------------------------------------------------------------
    ! Update u and v variables (same for NMM and ARW)
    do ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      write(char_tile, '(i1)') ntile

      filename_core = TRIM(datapath)//TRIM(datestring)//"/"//TRIM(charnanal)//"/"//&
           &TRIM(fileprefix)//".fv_core.res.tile"//TRIM(char_tile)//".nc"
      filename_tracer = TRIM(datapath)//TRIM(datestring)//"/"//TRIM(charnanal)//"/"//&
           &TRIM(fileprefix)//".fv_tracer.res.tile"//TRIM(char_tile)//".nc"

      goto 1000

!for core variables just WRITE values from previous restart - no time to bother now

    !----------------------------------------------------------------------
    ! read u-component
      filename_in=TRIM(ADJUSTL(filename_core))
      call nc_check( nf90_open(trim(adjustl(filename_in)),nf90_write,file_id),&
        myname_,'open: '//trim(adjustl(filename_in)) )


    ! update CWM for WRF-NMM
    if (u_ind > 0) then
    allocate(uworkvar3d(nx_res,ny_res+1,nlevs))
       varstrname = 'u'
         
       call read_fv3_restart_data3d(varstrname,filename_in,file_id,uworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(u_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo
      uworkvar3d(:,1:ny_res,:)=workvar3d(:,1:ny_res,:)+workinc3d
      uworkvar3d(:,ny_res+1,:)=uworkvar3d(:,ny_res,:)
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,uworkvar3d)
    deallocate(uworkvar3d)

    endif

    if (v_ind > 0) then
    allocate(vworkvar3d(nx_res+1,ny_res,nlevs))
       varstrname = 'v'
         
       call read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(v_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo
      vworkvar3d(1:nx_res,:,:)=workvar3d(1:nx_res,:,:)+workinc3d
      vworkvar3d(nx_res+1,:,:)=vworkvar3d(nx_res,:,:)
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,vworkvar3d)
    deallocate(vworkvar3d)

    endif
    if (tv_ind > 0.or.tsen_ind>0 ) then
       varstrname = 'T'
         
      if(tsen_ind>0) then
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(tsen_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo
       call read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
          workvar3d=workvar3d+workinc3d
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
     else  ! tv_ind >0  
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(tv_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo

        call read_fv3_restart_data3d(varstrname,filename_in,file_id,tsenworkvar3d)
        call read_fv3_restart_data3d(varstrname,filename_in,file_id,qworkvar3d)
        tvworkvar3d=tsenworkvar3d*qworkvar3d
        tvworkvar3d=tvworkvar3d+workinc3d
       if(q_ind > 0) then
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(q_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo
       qworkvar3d=qworkvar3d+workinc3d   
       endif
       tsenworkvar3d=tvworkvar3d/(one+fv*qworkvar3d(i,j,k))
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,tsenworkvar3d)
       if(q_ind>0) then
       varname='sphum'
     
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,qworkvar3d)
       endif
       
      
       
     endif

    endif
    if (oz_ind > 0) then
       varstrname = 'o3mr'
         
       call read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(oz_ind-1)+k,nb,ne) 
         enddo
      enddo
      enddo
      workvar3d=workvar3d+workinc3d
       call write_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)

    endif


1000 CONTINUE

    filename_in=TRIM(filename_tracer)

    CALL nc_check( nf90_open(TRIM(ADJUSTL(filename_in)),nf90_write,file_id),&
         myname_,'open: '//TRIM(ADJUSTL(filename_in)) )

    DO itracer=1,ntracers_gocart

       IF (aero_ind(itracer) > 0) THEN

          varstrname = vars3d_supported_aero(itracer)
         
          CALL read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
   
          DO k=1,nlevs
             nn = nn_tile0
             DO j=1,ny_res
                DO i=1,nx_res
                   nn=nn+1
                   workinc3d(i,j,k)=vargrid(nn,levels(aero_ind(itracer)-1)+k,nb,ne) 
                ENDDO
             ENDDO
          ENDDO
          workvar3d=workvar3d+workinc3d
          CALL write_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
          
       ENDIF

    ENDDO

    CALL nc_check( nf90_close(file_id),&
         myname_,'close '//TRIM(filename_in) )
    
    GOTO 1001

    if (ps_ind > 0) then
       varstrname = 'delp'

      call read_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)  !cltto think different files be used
      !print *,'min/max delp',ntile,minval(delp),maxval(delp)

      pswork=SUM(workvar3d,3) + ptop

          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
!ps increment
            psworkinc(i,j)=vargrid(nn,levels(n3d)+ps_ind,nb,ne)
         enddo
      enddo

      DO k=1,nlevs
         DO j=1,ny_res
            DO i=1,nx_res
               workvar3d(i,j,k)=ak(k,1)-ak(k+1,1)+&
                    &(bk(k,1)-bk(k+1,1))*(pswork(i,j)+psworkinc(i,j))
            ENDDO
         ENDDO
      ENDDO

      varstrname='delp'

       call write_fv3_restart_data3d(varstrname,filename_in,file_id,workvar3d)
     endif

      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename_in) )
    
1001  CONTINUE

  
    !----------------------------------------------------------------------
    ! update time stamp is to be considered NSTART_HOUR in NMM (HWRF) restart file.
    !======================================================================
     end do ! tiles
  
    end do backgroundloop ! loop over backgrounds to read in
    end do ensmemloop ! loop over ens members to write out    

    IF(ALLOCATED(workinc3d))     DEALLOCATE(workinc3d)
    IF(ALLOCATED(workinc3d2))     DEALLOCATE(workinc3d2)
    IF(ALLOCATED(workprsi))     DEALLOCATE(workprsi)
    IF(ALLOCATED(pswork))     DEALLOCATE(pswork)
    IF(ALLOCATED(psworkinc))     DEALLOCATE(psworkinc)
    IF(ALLOCATED(tvworkvar3d)) DEALLOCATE(tvworkvar3d)
    IF(ALLOCATED(qworkvar3d)) DEALLOCATE(qworkvar3d)


    ! Return calculated values
    return

    !======================================================================

  end subroutine writegriddata


end module gridio
