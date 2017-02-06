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
  !   2011-11-30  Initial version.
  !
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================

  ! Define associated modules
  use gridinfo, only: dimensions, npts, cross2dot, dot2cross
  use kinds,    only: r_double, r_kind, r_single, i_kind
  use mpisetup, only: nproc
  use netcdf_io
  use params,   only: nlevs, nlons, nlats, cliptracers, datapath,            &
                      arw, nmm, datestring, pseudo_rh
  use constants, only: zero,one,cp,fv,rd,grav,zero,max_varname_length
  use mpeu_util, only: getindex

  implicit none

  !-------------------------------------------------------------------------

  ! Define all public subroutines within this module
  private
  public :: readgriddata
  public :: writegriddata

  !-------------------------------------------------------------------------

contains

  subroutine readgriddata(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,reducedgrid,vargrid,qsat)
   integer, intent(in)  :: nanal, n2d, n3d,ndim, ntimes
   character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
   character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
   integer, dimension(0:n3d), intent(in) :: levels
   character(len=120), dimension(7), intent(in)  :: fileprefixes
   logical, intent(in) :: reducedgrid

   real(r_single), dimension(npts,ndim,ntimes),  intent(out) :: vargrid
   real(r_double), dimension(npts,nlevs,ntimes), intent(out) :: qsat

   if (arw) then
     call readgriddata_arw(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,vargrid,qsat)
   else
     call readgriddata_nmm(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,vargrid,qsat)
   endif
  end subroutine readgriddata

  !========================================================================

  ! readgriddata_arw.f90: This subroutine will receive a WRF-ARW
  ! netcdf file name and variable string and will subsequently return
  ! the respective variable interpolated to an unstaggered grid; all
  ! checks for grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine readgriddata_arw(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,vargrid,qsat)
    use constants

    !======================================================================
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal, n2d, n3d,ndim, ntimes
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    character(len=120), dimension(7), intent(in)  :: fileprefixes

    ! Define variables returned by subroutine
    real(r_single), dimension(npts,ndim,ntimes),  intent(out) :: vargrid
    real(r_double), dimension(npts,nlevs,ntimes), intent(out) :: qsat

    ! Define variables computed within subroutine
    character(len=500) :: filename
    character(len=7)   :: charnanal

    logical :: ice
    real, dimension(:,:,:), allocatable  :: wrfarw_znu
    real, dimension(:,:,:), allocatable  :: wrfarw_znw
    real, dimension(:,:,:), allocatable  :: wrfarw_mu
    real, dimension(:,:,:), allocatable  :: wrfarw_mub
    real, dimension(:,:,:), allocatable  :: wrfarw_ptop
    real(r_single), dimension(:,:), allocatable :: enkf_temp, enkf_virttemp
    real(r_single), dimension(:,:), allocatable :: enkf_pressure
    real(r_single), dimension(:),   allocatable :: enkf_psfc
    real(r_single), dimension(:,:), allocatable :: enkf_mixratio, enkf_spechumd
    real(r_single), dimension(:),   allocatable :: qintegral
    real(r_single) :: ptop

    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname

    ! Define counting variables
    integer :: xdim, ydim, zdim
    integer :: i, j, k, nb
    integer :: counth
    integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind
    integer :: tsen_ind, prse_ind
    integer :: ps_ind, sst_ind

    !======================================================================

    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
    prse_ind = getindex(vars3d, 'prse')

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst')

    ! Initialize all constants required by routine
    call init_constants(.true.)

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

       
    !======================================================================

    ! Begin: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for ARW dynamical core
    !----------------------------------------------------------------------
    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif
    backgroundloop: do nb=1,ntimes

    ! Define character string for ensemble member file
    if (nanal > 0) then
      write(charnanal,'(a3, i3.3)') 'mem', nanal
    else
      charnanal = 'ensmean'
    endif
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
    if (u_ind > 0) then
       varstrname = 'U'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb),nlevs)
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: u ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
    ! read v-component
    if (v_ind > 0) then
       varstrname = 'V'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb),nlevs)
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: v ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
    ! set ozone to zero for now (like in GSI?)
    if (oz_ind > 0) then
       vargrid(:,levels(oz_ind-1)+1:levels(oz_ind),nb) = zero
    endif
    ! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb) = zero
    endif

    !======================================================================
    ! Begin: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-ARW grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid
    !----------------------------------------------------------------------
    ! Define all constants required by routine
    ice = .false.

    !----------------------------------------------------------------------

    ! Allocate memory for all variables ingested by routine
    if(.not. allocated(wrfarw_mu))     allocate(wrfarw_mu(xdim,ydim,1))
    if(.not. allocated(wrfarw_mub))    allocate(wrfarw_mub(xdim,ydim,1))
    if(.not. allocated(wrfarw_znu))    allocate(wrfarw_znu(1,1,zdim))
    if(.not. allocated(wrfarw_znw))    allocate(wrfarw_znw(1,1,zdim+1))
    if(.not. allocated(wrfarw_ptop))   allocate(wrfarw_ptop(1,1,1))

    ! Allocate memory for variables computed within routine
    if(.not. allocated(enkf_temp))     allocate(enkf_temp(npts,nlevs))
    if(.not. allocated(enkf_virttemp)) allocate(enkf_virttemp(npts,nlevs))
    if(.not. allocated(enkf_psfc))     allocate(enkf_psfc(npts))
    if(.not. allocated(qintegral))     allocate(qintegral(npts))
    if(.not. allocated(enkf_pressure)) allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_mixratio)) allocate(enkf_mixratio(npts,nlevs))
    if(.not. allocated(enkf_spechumd)) allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------
    ! Ingest the perturbation potential temperature from the external
    ! file
    varstrname= 'T'
    call readwrfvar(filename, varstrname, enkf_temp, nlevs)

    ! Ingest the water vapor mixing ratio from the external file
    varstrname = 'QVAPOR'
    call readwrfvar(filename, varstrname, enkf_mixratio, nlevs)

    ! Ingest the model vertical (eta) levels from the external file
    varstrname = 'ZNU'
    call readnetcdfdata(filename,wrfarw_znu,varstrname,1,1,zdim)

    ! Ingest the model vertical (aeta) levels from the external file
    varstrname = 'ZNW'
    call readnetcdfdata(filename,wrfarw_znw,varstrname,1,1,zdim+1)

    ! Ingest the model perturbation dry air mass from the external
    ! file
    varstrname = 'MU'
    call readnetcdfdata(filename,wrfarw_mu,varstrname,xdim,ydim,1)

    ! Ingest the model base state dry air mass from the external file
    varstrname = 'MUB'
    call readnetcdfdata(filename,wrfarw_mub,varstrname,xdim,ydim,1)

    ! Ingest the model top pressure level from the external file
    varstrname = 'P_TOP'
    call readnetcdfdata(filename,wrfarw_ptop,varstrname,1,1,1)
    ptop = wrfarw_ptop(1,1,1)

    qintegral = one
    do i = 1, npts
       do k = 1, zdim
          qintegral(i) = qintegral(i) +                              &
              (wrfarw_znw(1,1,k) - wrfarw_znw(1,1,k+1))*enkf_mixratio(i,k)
       enddo
    enddo 

    ! Loop through vertical coordinate; compute the hydrostatic
    ! pressure level
    do k = 1, zdim
       ! Initialize counting variable
       counth = 1
       ! Loop through meridional horizontal coordinate
       do j = 1, ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim
             ! Compute the dry hydrostatic pressure at the respective
             ! grid coordinate; This is dry pressure not full
             ! pressure, ignore this difference, since we are only
             ! using this to compute qsat, which in turn is only used
             ! to compute normalized humidity analysis variable
             enkf_pressure(counth,k) = r0_01 * (wrfarw_znu(1,1,k)*(wrfarw_mu(i,j,1) +  &
                                                wrfarw_mub(i,j,1)) + ptop)

             counth = counth + 1
          end do ! do i = 1, xdim
       end do ! do j = 1, ydim
    end do ! do k = 1, zdim

    ! Initialize counting variable
    counth = 1
    ! Loop through meridional horizontal coordinate
    do j = 1, ydim
       ! Loop through zonal horizontal coordinate
       do i = 1, xdim
          ! Convert from Pa to hPa and update the global surface
          ! pressure array

          ! Dry surface pressure
          enkf_psfc(counth) = r0_01*(wrfarw_mu(i,j,1) + wrfarw_mub(i,j,1)+ptop)
      
          ! Moist surface pressure
          enkf_psfc(counth) = (enkf_psfc(counth) - ptop) * qintegral(counth) + ptop
          counth = counth + 1
       end do ! do i = 1, dimensions%xdim
    end do ! do j = 1, dimensions%ydim


    do k = 1, nlevs
       do i = 1, npts
          enkf_pressure(i,k) = r0_01 * (wrfarw_znu(1,1,k)*(100*enkf_psfc(i)-ptop)+ptop)

          ! Compute mixing ratio from specific humidity.
          enkf_spechumd(i,k) = (enkf_mixratio(i,k))/(1.0 + enkf_mixratio(i,k))

          ! Compute sensible temperature
          enkf_temp(i,k) = (enkf_temp(i,k) + 300.0) *  &
                           (0.001 * enkf_pressure(i,k))**rd_over_cp_mass
!                (0.001 * (wrfarw_znu(1,1,k)*(enkf_psfc(i)-ptop) + ptop))**rd_over_cp_mass

          ! Compute virtual temperature
          enkf_virttemp(i,k) = enkf_temp(i,k)* (1. + fv*enkf_spechumd(i,k))
       end do
    end do

    if (tsen_ind > 0) then
       vargrid(:,levels(tsen_ind-1)+1:levels(tsen_ind),nb) = enkf_temp
       do k = levels(tsen_ind-1)+1, levels(tsen_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: tsen ',                        &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    if (q_ind > 0) then
       vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb) = enkf_spechumd
       do k = levels(q_ind-1)+1, levels(q_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: q ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    if (tv_ind > 0) then
       vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb) = enkf_virttemp
       do k = levels(tv_ind-1)+1, levels(tv_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: tv ',                          &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
 
    if (ps_ind > 0) then
       vargrid(:,levels(n3d)+ps_ind,nb) = enkf_psfc
       k = levels(n3d) + ps_ind
       if (nproc .eq. 0)                                               &
          write(6,*) 'READGRIDDATA_ARW: ps ',                           &
              & minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
    endif

    if (prse_ind > 0) then
       vargrid(:,levels(prse_ind-1)+1:levels(prse_ind)-1, nb) = enkf_pressure
       do k = levels(prse_ind-1)+1, levels(prse_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: prse ',                        &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    !----------------------------------------------------------------------
    ! Compute the saturation specific humidity

    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb),enkf_pressure/100.0,enkf_virttemp,ice,  &
                     npts,nlevs)
    else
       qsat(:,:,nb) = 1._r_double
    endif
          

    !---------------------------------------------------------------------
    ! End: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-ARW grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !======================================================================
    ! Deallocate memory for variables ingested by routine

    if(allocated(wrfarw_mu))           deallocate(wrfarw_mu)
    if(allocated(wrfarw_mub))          deallocate(wrfarw_mub)
    if(allocated(wrfarw_znu))          deallocate(wrfarw_znu)
    if(allocated(wrfarw_znw))          deallocate(wrfarw_znw)
    if(allocated(wrfarw_ptop))         deallocate(wrfarw_ptop)

    ! Deallocate memory for variables computed within routine
    if(allocated(enkf_temp))           deallocate(enkf_temp)
    if(allocated(enkf_virttemp))       deallocate(enkf_virttemp)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_mixratio))       deallocate(enkf_mixratio)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    end do backgroundloop ! loop over backgrounds to read in

    return

  end subroutine readgriddata_arw

  !========================================================================

  ! readgriddata_nmm.f90: This subroutine will receive a WRF-NMM
  ! netcdf file name and variable string and will subsequently return
  ! the respective variable interpolated to an unstaggered grid; all
  ! checks for grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine readgriddata_nmm(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,vargrid,qsat)
    use constants
    !======================================================================
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal, n2d, n3d, ndim, ntimes
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    character(len=120), dimension(7), intent(in)  :: fileprefixes

    ! Define variables returned by subroutine
    real(r_single),  dimension(npts,ndim,ntimes),  intent(out) :: vargrid
    real(r_double),  dimension(npts,nlevs,ntimes), intent(out) :: qsat

    ! Define variables computed within subroutine
    logical :: ice
    real, dimension(:,:,:), allocatable          :: wrfnmm_pd
    real, dimension(:,:,:), allocatable          :: wrfnmm_eta1
    real, dimension(:,:,:), allocatable          :: wrfnmm_eta2
    real, dimension(:,:,:), allocatable          :: wrfnmm_pdtop
    real, dimension(:,:,:), allocatable          :: wrfnmm_pt
    real(r_single), dimension(:),   allocatable  :: enkf_psfc
    real(r_single), dimension(:,:), allocatable  :: enkf_temp
    real(r_single), dimension(:,:), allocatable  :: enkf_pressure
    real(r_single), dimension(:,:), allocatable  :: enkf_spechumd

    character(len=12)  :: varstrname
    character(len=500) :: filename
    character(len=7)   :: charnanal

    ! Define counting variables
    integer(i_kind) :: xdim, ydim, zdim
    integer(i_kind) :: nb, i, j, k, counth
    integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind
    integer(i_kind) :: cw_ind, tsen_ind, prse_ind
    integer(i_kind) :: ps_ind, sst_ind

    !======================================================================
    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
    prse_ind = getindex(vars3d, 'prse')

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst')

    ! Initialize all constants required by routine
    call init_constants(.true.)

    ! Define all local variables
    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim
       
    !======================================================================

    ! Begin: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for NMM dynamical core

    !----------------------------------------------------------------------
    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif

    backgroundloop: do nb=1,ntimes

    ! Define character string for ensemble member file
    if (nanal > 0) then
      write(charnanal,'(a3, i3.3)') 'mem', nanal
    else
      charnanal = 'ensmean'
    endif
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
    if (u_ind > 0) then
       varstrname = 'U' 
       call readwrfvar(filename, varstrname,                               &
                       vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb), nlevs)
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: u ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
    ! read v-component
    if (v_ind > 0) then
       varstrname = 'V'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb), nlevs)
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: v ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
    ! read cwm
    if (cw_ind > 0) then
       varstrname = 'CWM'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(cw_ind-1)+1:levels(cw_ind),nb), nlevs)
       do k = levels(cw_ind-1)+1, levels(cw_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: cw',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif
    ! set ozone to zero for now (like in GSI?)
    if (oz_ind > 0) then
       vargrid(:,levels(oz_ind-1)+1:levels(oz_ind),nb) = zero
    endif
    ! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb) = zero
    endif

    !======================================================================

    ! Begin: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-NMM grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid
    !----------------------------------------------------------------------

    ! Define all constants required by routine
    ice = .false.

    !----------------------------------------------------------------------

    ! Allocate memory for all variables ingested by routine
    if(.not. allocated(wrfnmm_pd))     allocate(wrfnmm_pd(xdim,ydim,1)) 
    if(.not. allocated(wrfnmm_eta1))   allocate(wrfnmm_eta1(1,1,zdim))
    if(.not. allocated(wrfnmm_eta2))   allocate(wrfnmm_eta2(1,1,zdim))
    if(.not. allocated(wrfnmm_pdtop))  allocate(wrfnmm_pdtop(1,1,1))
    if(.not. allocated(wrfnmm_pt))     allocate(wrfnmm_pt(1,1,1))

    ! Allocate memory for variables computed within routine
    if(.not. allocated(enkf_psfc))     allocate(enkf_psfc(npts))
    if(.not. allocated(enkf_temp))     allocate(enkf_temp(npts,nlevs))
    if(.not. allocated(enkf_pressure)) allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_spechumd)) allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------

    ! Ingest the (sensible) temperature from the external file
    varstrname= 'T'
    call readwrfvar(filename, varstrname, enkf_temp, nlevs)
    if (tsen_ind > 0) then
       vargrid(:,levels(tsen_ind-1)+1:levels(tsen_ind),nb) = enkf_temp
       do k = levels(tsen_ind-1)+1, levels(tsen_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: tsen ',                        &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    ! Ingest the specific humidity from the external file
    varstrname = 'Q'
    call readwrfvar(filename, varstrname, enkf_spechumd, nlevs)
    if (q_ind > 0) then
       vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb) = enkf_spechumd
       do k = levels(q_ind-1)+1, levels(q_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: q ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    ! calculate virtual temperature
    do k = 1, nlevs
       do i = 1, npts
          enkf_temp(i,k) = enkf_temp(i,k)* (1. + fv*enkf_spechumd(i,k))
       end do ! do i = 1, xdim
    end do ! do j = 1, ydim
    if (tv_ind > 0) then
       vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb) = enkf_temp
       do k = levels(tv_ind-1)+1, levels(tv_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: tv ',                          &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif

    ! Ingest surface pressure from the external file
    varstrname = 'PD'
    call readnetcdfdata(filename,wrfnmm_pd,varstrname,xdim,ydim,1)

    ! Ingest hybrid vertical coordinate from the external file
    varstrname = 'AETA1'
    call readnetcdfdata(filename,wrfnmm_eta1,varstrname,1,1,zdim)

    ! Ingest hybrid vertical coordinate from the external file
    varstrname = 'AETA2'
    call readnetcdfdata(filename,wrfnmm_eta2,varstrname,1,1,zdim)

    ! Ingest pressure at top of domain from the external file
    varstrname = 'PT'
    call readnetcdfdata(filename,wrfnmm_pt,varstrname,1,1,1)

    ! Ingest mass within pressure domain from the external file
    varstrname = 'PDTOP'
    call readnetcdfdata(filename,wrfnmm_pdtop,varstrname,1,1,1)

    !----------------------------------------------------------------------

    counth = 1
    ! Loop through meridional horizontal coordinate
    do j = 1, ydim
       ! Loop through zonal horizontal coordinate
       do i = 1, xdim
          ! Compute the surface pressure profile
          enkf_psfc(counth) = r0_01 * (wrfnmm_pd(i,j,1) + wrfnmm_pdtop(1,1,1) +    &
               & wrfnmm_pt(1,1,1))

          counth = counth + 1
       end do ! do i = 1, xdim
    end do ! do j = 1, ydim

    if (ps_ind > 0) then
       vargrid(:,levels(n3d)+ps_ind,nb) = enkf_psfc
       k = levels(n3d) + ps_ind
       if (nproc .eq. 0)                                               &
          write(6,*) 'READGRIDDATA_NMM: ps ',                           &
              & minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
    endif

    ! Loop through vertical horizontal coordinate
    do k = 1, zdim
       counth = 1
       ! Loop through meridional horizontal coordinate
       do j = 1, ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim
             ! Compute the pressure profile; the following formulation
             ! (should be) is identical to that in the Gridpoint
             ! Statistical Interpolation (GSI) routines for the
             ! WRF-NMM dynamical core
             enkf_pressure(counth,k) = r0_01 * (wrfnmm_eta1(1,1,k)*wrfnmm_pdtop(1,1,1) +  &
                  & wrfnmm_eta2(1,1,k)*(enkf_psfc(counth)*100 - wrfnmm_pdtop(1,1,1) -     &
                  &                     wrfnmm_pt(1,1,1)) + wrfnmm_pt(1,1,1))
             counth = counth + 1
          end do ! do i = 1, xdim
       end do ! do j = 1, ydim
    end do ! do k = 1, zdim

    if (prse_ind > 0) then
       vargrid(:,levels(prse_ind-1)+1:levels(prse_ind)-1, nb) = enkf_pressure
       do k = levels(prse_ind-1)+1, levels(prse_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: prse ',                        &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    endif


    !----------------------------------------------------------------------

    ! Compute the saturation specific humidity
    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb),enkf_pressure,enkf_temp,ice,  &
                    npts,nlevs)
    else
       qsat(:,:,nb) = 1._r_double
    endif

    !----------------------------------------------------------------------

    ! End: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-NMM grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !======================================================================

    ! Deallocate memory for variables ingested by routine
    if(allocated(wrfnmm_pd))           deallocate(wrfnmm_pd)
    if(allocated(wrfnmm_eta1))         deallocate(wrfnmm_eta1)
    if(allocated(wrfnmm_eta2))         deallocate(wrfnmm_eta2)
    if(allocated(wrfnmm_pdtop))        deallocate(wrfnmm_pdtop)
    if(allocated(wrfnmm_pt))           deallocate(wrfnmm_pt)

    ! Deallocate memory for variables computed within routine
    if(allocated(enkf_temp))           deallocate(enkf_temp)
    if(allocated(enkf_psfc))           deallocate(enkf_psfc)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in

    ! Return calculated values

    return

  end subroutine readgriddata_nmm


    !======================================================================

  subroutine readwrfvar(filename, varname, grid, nlevs)
    implicit none
    integer(i_kind), intent(in) :: nlevs
    real(r_single),  dimension(npts,nlevs),  intent(out) :: grid
    character(len=12),  intent(in) :: varname
    character(len=500), intent(in) :: filename

    ! Define variables computed within subroutine
    real, dimension(:,:,:), allocatable :: workgrid
    real, dimension(:,:,:), allocatable :: vargrid_native
    integer :: xdim, ydim, zdim
    integer :: xdim_native, ydim_native, zdim_native
    integer :: xdim_local,  ydim_local,  zdim_local

    ! Define variables requiredfor netcdf variable I/O
    character(len=50) :: attstr
    character(len=12) :: varstagger
    character(len=12) :: varmemoryorder

    ! Define counting variables
    integer :: i, j, k
    integer :: counth

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    ! Define staggering attributes for variable grid
    attstr = 'stagger'
    call variableattribute_char(filename,varname,attstr,        &
            & varstagger)

    xdim_native = xdim
    ydim_native = ydim
    zdim_native = zdim
    ! If variable grid is staggered assign array dimensions appropriately
    if(varstagger(1:1) .eq. 'X') then
       xdim_native = xdim + 1
    else if(varstagger(1:1) .eq. 'Y') then
       ydim_native = ydim + 1
    else if(varstagger(1:1) .eq. 'Z') then
       zdim_native = zdim + 1
    end if ! if(varstagger(1:1) .eq. 'X')

    !----------------------------------------------------------------------
    ! Define memory attributes for variable grid
    attstr = 'MemoryOrder'
    call variableattribute_char(filename,varname,attstr,       &
            & varmemoryorder)

    ! If variable is a 2-dimensional field, rescale variables appropriately
    if(varmemoryorder(1:3) .eq. 'XY ') then
       zdim_local = 1
       zdim_native = 1
    else
       zdim_local = zdim
    end if ! if(varmemoryorder(1:3) .eq. 'XY ')

    ! Define local variable dimensions
    xdim_local = xdim
    ydim_local = ydim

    ! Allocate memory for local variable arrays
    if(.not. allocated(workgrid))                                     &
         & allocate(workgrid(xdim_local,ydim_local,zdim_local))
    if(.not. allocated(vargrid_native))                               &
         & allocate(vargrid_native(xdim_native,ydim_native,zdim_native))

    ! Ingest variable from external netcdf formatted file
    call readnetcdfdata(filename,vargrid_native,varname,     &
            & xdim_native,ydim_native,zdim_native)

    ! Interpolate variable from staggered (i.e., E-) grid to
    ! unstaggered (i.e., A-) grid. If variable is staggered in
    ! vertical, intepolate from model layer interfaces
    ! (including surface and top) to model layer midpoints.
    call cross2dot(vargrid_native,xdim_native,ydim_native,            &
         & zdim_native,xdim_local,ydim_local,zdim_local,workgrid)

    !----------------------------------------------------------------------
    ! Loop through vertical coordinate
    do k = 1, zdim_local
       ! Initialize counting variable
       counth = 1
       ! Loop through meridional horizontal coordinate
       do j = 1, ydim_local
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim_local
             ! Assign values to output variable array
             grid(counth,k) = workgrid(i,j,k)

             counth = counth + 1
          end do ! do i = 1, xdim_local
       end do ! do j = 1, ydim_local
    end do ! do k = 1, zdim_local

    !----------------------------------------------------------------------
    ! Deallocate memory for local variables
    if(allocated(vargrid_native)) deallocate(vargrid_native)
    if(allocated(workgrid))       deallocate(workgrid)

  end subroutine readwrfvar

  !========================================================================

  ! writegriddata.f90: This subroutine will receive a netcdf file name
  ! and variable string and will subsequently return the respective
  ! variable interpolated to the native variable grid; all checks for
  ! grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine writegriddata(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,vargrid)

    use constants
    use params, only: nbackgrounds, anlfileprefixes, fgfileprefixes
    include 'netcdf.inc'      

    !----------------------------------------------------------------------
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal, n2d, n3d, ndim
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single),    dimension(npts,ndim,nbackgrounds), intent(in)    :: vargrid

    !----------------------------------------------------------------------
    ! Define variables computed within subroutine
    integer  :: nvars
    character(len=max_varname_length), dimension(:), allocatable :: vars
    character(len=500)                                                                         :: filename
    character(len=3)                                                                           :: charnanal
    real,    dimension(:,:,:),                allocatable                            :: vargrid_native
    real,    dimension(:,:,:),                allocatable                            :: vargridin_native
    real,    dimension(:,:,:),                allocatable                            :: workgrid
    real                                                                             :: clip
    integer iyear,imonth,iday,ihour,dh1,ierr,iw3jdn
    integer                                                                                    :: xdim_native
    integer                                                                                    :: ydim_native
    integer                                                                                    :: zdim_native
    integer                                                                                    :: xdim_local
    integer                                                                                    :: ydim_local
    integer                                                                                    :: zdim_local

    !----------------------------------------------------------------------
    ! Define array dimension variables
    integer                                                                                    :: xdim
    integer                                                                                    :: ydim
    integer                                                                                    :: zdim

    !----------------------------------------------------------------------
    ! Define variables required by for extracting netcdf variable
    ! fields
    character(len=50)                                                                          :: attstr
    character(len=12)                                                                          :: varstagger,varstrname
    character(len=12)                                                                          :: varmemoryorder
    character(len=19)  :: DateStr

    !----------------------------------------------------------------------
    ! Define counting variables
    integer                                                                                    :: i, j, k, l, nb
    integer                                                                                    :: counth, countv

    !----------------------------------------------------------------------

    ! Save all (3D and 2D) variables in vars array
    nvars = n2d + n3d
    allocate(vars(nvars))
    do i = 1, n3d
       select case (vars3d(i))
         case ("u")
           vars(i) = "U"
         case ("v")
           vars(i) = "V"
         case ("tv")
           vars(i) = "T"
         case ("q")
           if (arw) then 
             vars(i) = "QVAPOR"
           elseif (nmm) then
             vars(i) = "Q"
           endif
         case ("w")
           vars(i) = "W"
         case ("ph")
           vars(i) = "PH"
         case ("cw")
           vars(i) = "CWM"
         case default
           if (nproc .eq. 0) then
             print *,'Error: 3D variable ', vars3d(i), ' is not supported in current version.'
           endif
           call stop2(502)
       end select
    enddo
    do i = 1, n2d
       select case (vars2d(i))
         case ("mu")
           vars(n3d+i) = "MU"
         case ("pd")
           vars(n3d+i) = "PD"
         case ("ps")
           if (nmm) vars(n3d+i) = "PD"
         case default
           if (nproc .eq. 0) then
             print *,'Error: 2D variable ', vars2d(i), ' is not supported in current version.'
           endif
           call stop2(502)
       end select
    enddo

    ! Initialize constants required by routine
    call init_constants(.true.)

    !----------------------------------------------------------------------
    ! Define all array dimensions
    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    if (nbackgrounds > 1) then
       write(6,*)'gridio/writegriddata: writing multiple backgrounds not yet supported'
       call stop2(23)
    endif

    backgroundloop: do nb=1,nbackgrounds

    ! Allocate memory for local variable
    allocate(workgrid(xdim,ydim,zdim))

    !----------------------------------------------------------------------
    ! End: Define all local variables required by routine

    !======================================================================
    ! Begin: Loop through each prognostic variable and determine the
    ! spatial array dimensions for each variable contained within
    ! file, define appropriate array dimensions, and allocate memory;
    ! update respective analysis (e.g., prognostic model) variables

    !----------------------------------------------------------------------
    ! Initialize counting variable

    countv = 1

    !----------------------------------------------------------------------
    ! First guess file should be copied to analysis file at scripting
    ! level; only variables updated by EnKF are changed

    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal

    !----------------------------------------------------------------------
    ! Loop through all analysis variables to be updated

    do l = 1, nvars

    !----------------------------------------------------------------------
       ! For WRF-ARW; analysis variables are defined on C-grid; the
       ! check for interpolation between mass and velocity points is
       ! done here
       if(arw) then

    !----------------------------------------------------------------------
          ! Define staggering attributes for variable grid
          attstr = 'stagger'
          call variableattribute_char(filename,vars(l),attstr,     &
               & varstagger)

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim
    !----------------------------------------------------------------------
          ! If variable grid is staggered in X-direction, assign array
          ! dimensions appropriately
          if(varstagger(1:1) .eq. 'X') then
             xdim_native = xdim + 1
    !----------------------------------------------------------------------
             ! If variable grid is staggered in Y-direction, assign
             ! array dimensions appropriately
          else if(varstagger(1:1) .eq. 'Y') then
             ydim_native = ydim + 1
    !----------------------------------------------------------------------
             ! If variable grid is staggered in Z-direction, assign
             ! array dimensions appropriately
          else if(varstagger(1:1) .eq. 'Z') then
             zdim_native = zdim + 1
          end if ! if(varstagger(1:1) .eq. 'X')
    !----------------------------------------------------------------------
       endif ! if(arw)

    !----------------------------------------------------------------------

       ! For WRF-NMM; analysis variables are defined on E-grid;
       ! although th grid may still be staggered, the array dimensions
       ! (along the horizontal planes) remain the same dimension,
       ! however just offset
       if(nmm) then
          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim
       end if ! if(nmm)

    !----------------------------------------------------------------------
       ! Define memory attributes for variable grid; this is done for
       ! ARW only
       if(arw) then
          attstr = 'MemoryOrder'
          call variableattribute_char(filename,vars(l),attstr,     &
               & varmemoryorder)
       end if ! if(arw)

    !----------------------------------------------------------------------

       ! If variable is a 2-dimensional field, rescale variables
       ! appropriately
       if(l > n3d) then !if (vars(l) .eq. 'MU' .or. vars(l) .eq. 'PD') then
          zdim_local = 1
          zdim_native = 1
       else
          ! Define local array dimension
          zdim_local = zdim
       end if ! if(gridvarstring(l) .eq. 'MU' .or. gridvarstring(l) .eq. 
              ! 'PD')

    !----------------------------------------------------------------------

       ! Define local variable dimensions
       xdim_local = xdim
       ydim_local = ydim

    !----------------------------------------------------------------------

       ! Allocate memory local arrays (first check whether they are
       ! already allocated)
       if (allocated(vargrid_native)) deallocate(vargrid_native)
       allocate(vargrid_native(xdim_native,ydim_native,zdim_native))
       if (allocated(vargridin_native)) deallocate(vargridin_native)
       allocate(vargridin_native(xdim_native,ydim_native,zdim_native))

    !----------------------------------------------------------------------
       
       ! Read in first-guess (i.e., analysis without current
       ! increments) and store in local array
       call readnetcdfdata(filename,vargridin_native,vars(l),      &
            & xdim_native,ydim_native,zdim_native)

    !----------------------------------------------------------------------

       ! Loop through vertical coordinate
       do k = 1, zdim_local
          ! Initialize counting variable
          counth = 1

          ! Loop through meridional horizontal coordinate
          do j = 1, ydim
             ! Loop through zonal horizontal coordinate
             do i = 1, xdim
                ! Assign values to local array
                workgrid(i,j,k) = vargrid(counth,countv,nb)

                counth = counth + 1
             end do ! do i = 1, xdim
          end do ! do j = 1, ydim

          countv = countv + 1

       end do ! k = 1, zdim_local

    !----------------------------------------------------------------------

       ! Interpolate increments to native grid (i.e., from A-grid to
       ! C-grid; if necessary); on input, workgrid is increments on
       ! unstaggered grid; on output vargrid_native is increments on
       ! model-native (i.e., staggered grid); vargridin_native is
       ! unmodified first guess on native staggered grid
       call dot2cross(xdim_local,ydim_local,zdim_local,xdim_native,          &
            ydim_native,zdim_native,workgrid,vargrid_native)

       if (vars(l) == "PD") then
          if (vars2d(l-n3d) == "ps")    vargrid_native = 100. * vargrid_native
       endif

       ! Add first guess to increment to get analysis on native grid;
       ! this currently done only for ARW grids
       if(arw) then
          if (varstagger(1:1) .eq. 'Z') then ! if 'W' or 'PH' don't update surface
             vargridin_native(:,:,2:zdim_native) =                           &
                  & vargrid_native(:,:,2:zdim_native) +                      &
                  & vargridin_native(:,:,2:zdim_native)
          else
             vargridin_native = vargrid_native + vargridin_native
          endif ! if (varstagger(1:1) .eq. 'Z')
       endif ! if(arw)

       ! Clip all tracers (assume names start with 'Q')
       if (cliptracers .and. vars(l)(1:1) .eq. 'Q') then   !AS: bad, bad, bad. should check the tracer flag (that I'm not saving right now!)
          clip = tiny(vargridin_native(1,1,1))
          where (vargridin_native < clip) vargridin_native = clip
       end if ! if (cliptracers .and. gridvarstring(l)(1:1) .eq. 'Q')

    !----------------------------------------------------------------------

       if(nmm) then
          vargridin_native = vargrid_native + vargridin_native
       end if

       ! Write analysis variable.
       call writenetcdfdata(filename,vargridin_native,vars(l),       &
             xdim_native,ydim_native,zdim_native)

    end do ! do l = 1, nvars+1

    !----------------------------------------------------------------------

    ! Deallocate memory for local variable

    deallocate(workgrid)

    ! update NSTART_HOUR in NMM (HWRF) restart file.
    read(datestring(1:4),'(i4)') iyear
    read(datestring(5:6),'(i2)') imonth
    read(datestring(7:8),'(i2)') iday
    read(datestring(9:10),'(i2)') ihour
    if (nmm) then
       varstrname = 'NSTART_HOUR'
       vargrid_native(1,1,1) = ihour
       call writenetcdfdata(filename,vargrid_native,varstrname,1,1,1)
    end if
    !
    !  update START_DATE, SIMULATION_START_DATE, GMT, JULYR, JULDAY 
    !  global attributes.
    !
    write(DateStr,'(i4,"-",i2.2,"-",i2.2,"-",i2.2,"_",i2.2,":",i2.2)') iyear,imonth,iday,ihour,0,0
    ierr = NF_OPEN(trim(filename), NF_WRITE, dh1)
    IF (ierr .NE. NF_NOERR) print *, 'OPEN ',NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'START_DATE',len(trim(DateStr)),DateStr)
    IF (ierr .NE. NF_NOERR) print *,'PUT START_DATE', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'SIMULATION_START_DATE',len(trim(DateStr)),DateStr)
    IF (ierr .NE. NF_NOERR) print *,'PUT SIMULATION_START_DATE', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_REAL(dh1,NF_GLOBAL,'GMT',NF_FLOAT,1,float(ihour))
    IF (ierr .NE. NF_NOERR) print *,'PUT GMT', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULYR',NF_INT,1,iyear)
    IF (ierr .NE. NF_NOERR) print *,'PUT JULYR', NF_STRERROR(ierr)
    ierr=NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULDAY',NF_INT,1,iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1)
    IF (ierr .NE. NF_NOERR) print *,'PUT JULDAY', NF_STRERROR(ierr)
    ierr = NF_CLOSE(dh1)
    IF (ierr .NE. NF_NOERR) print *, 'CLOSE ',NF_STRERROR(ierr)

    !----------------------------------------------------------------------

    ! End: Loop through each prognostic variable and determine the
    ! spatial array dimensions for each variable contained within
    ! file, define appropriate array dimensions, and allocate memory;
    ! update respective analysis (e.g., prognostic model) variables

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in

    ! Deallocate vars
    if(allocated(vars))                     deallocate(vars)

    ! Return calculated values

    return

    !======================================================================

  end subroutine writegriddata

  subroutine writewrfvar(filename, varname, grid, nlevs)
    implicit none
    integer(i_kind), intent(in) :: nlevs
    real(r_single),  dimension(npts,nlevs),  intent(out) :: grid
    character(len=12),  intent(in) :: varname
    character(len=500), intent(in) :: filename



  end subroutine writewrfvar

  !========================================================================

end module gridio
