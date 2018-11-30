module aodmod
!$$$ module documentation block
!
! module:       aodmod
!   prgrmmr: Martin     org:            date: 2018-11-09
!
! abstract: this module calculates aerosol optical depth from 3D aerosol fields
!           for use in a 2D analysis
!
! program history log:
!    2018-11-09 Martin  Initial verson.
!
! Subroutines included:
!   sub calc_2d_aod     - calculate 2D aerosol from chemguess bundle aerosols
!                          and puts the result in chemguess bundle for aod
!$$$ end documentation block

  implicit none

  private
  public calc_2d_aod

  character(len=*),parameter::myname='aodmod'

contains

  subroutine calc_2d_aod(it)
!$$$ subprogram documentation block
!
! subprogram:   calc_2d_aod
!
!   prgrmmr: C. Martin
!
! abstract: Calculates 2D AOD from chemguess bundle aerosols using CRTM
!           and puts the result in chemguess bundle for aod for 2D analysis
!
! program history log:
!   2018-11-09  Martin  - Initial version
!
!   input argument list:
!                       it - integer of which analysis time to use/process
!
!   output argument list:
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gridmod, only: lat1,lon1,nsig,nlat,nlon,rlats,rlons,istart,jstart
    use gridmod, only: ijn, displs_g,strip,iglobal,itotsub
    use mpimod, only: npe,mype,mpi_comm_world,ierror,mpi_real4,mpi_rtype
    use radiance_mod, only: rad_obs_type,radiance_obstype_search
    use obsmod, only: mype_diaghdr,rmiss_single
    use crtm_interface, only: init_crtm,call_crtm,destroy_crtm
    use constants, only: rad2deg,deg2rad,zero
    use radinfo, only: nsigradjac
    use general_commvars_mod, only: load_grid

    implicit none

    ! argument list
    integer(i_kind), intent(in   ) :: it

    ! local variables
    real(r_kind),pointer,dimension(:,:)::ptr2daod=>NULL()
    type(rad_obs_type) :: radmod
    character(20) :: isis
    character(10) :: obstype
    integer(i_kind) :: nchanl,nreal
    integer(i_kind) :: i,j,k
    integer(i_kind) :: is,ie,js,je
    integer(i_kind) :: iret
    real(r_kind) :: tdiff
    real(r_kind),dimension(11+20)::data_s ! nreal hard code 11 nchanl 20
    real(r_kind),allocatable,dimension(:,:) :: aod2d
    real(r_kind),allocatable,dimension(:,:,:) :: aod2dch
    real(r_kind),dimension(nsig,20):: layer_od ! nchanl hard coded at 20
    integer(i_kind),dimension(20):: ich ! again hard coded at 20 for now
    real(r_kind) :: gridlat,gridlon
    ! things for CRTM
    real(r_kind),dimension(nsig):: qvp,tvp
    real(r_kind),dimension(nsig+1):: prsitmp
    real(r_kind),dimension(nsig):: prsltmp
    real(r_kind) :: clw_guess, tzbgr, sfc_speed
    real(r_kind),dimension(nsig,20):: wmix,temp,ptau5 ! hard coded
    real(r_kind),dimension(20):: emissivity,ts,emissivity_k ! hard coded
    real(r_kind),dimension(nsigradjac,20):: jacobian ! hard coded
    real(r_kind) :: trop5
    real(r_kind) dtsavg
    real(r_kind),dimension(20):: tsim ! hard coded, sad!
    integer(i_kind) error_status
    ! output array
    real(r_kind),allocatable,dimension(:,:) :: aodtmp
    real(4),allocatable,dimension(:,:) :: aodout
    real(r_kind),dimension(lat1*lon1) :: aodsm
    real(r_kind),dimension(max(iglobal,itotsub))     :: work1
    

    if (mype==0) write(6,*) 'Computing 2D AOD guess from chemguess bundle'

    ! hard coded things here for now
    nchanl = 20
    nreal = 11
    ! assuming the background is a grid of MODIS AOD observations
    isis='v.modis_aqua'
    obstype='modis_aod'
    tdiff=0. ! assumes 'observations' are at analysis time

    ! init CRTM - probably fix this to be more generic later, for testing
    call radiance_obstype_search('modis_aod',radmod)

    call init_crtm(.true.,mype_diaghdr(1),mype,nchanl,isis,obstype,radmod)

    ! get domain information, going to call CRTM for each gridpoint

    ! set up ich
    do i=1,nchanl
      ich(i)=i
    end do
    
    ! get the indices for loop and allocation
    is=istart(mype+1)-1
    ie=lat1+istart(mype+1)
    js=jstart(mype+1)-1
    je=lon1+jstart(mype+1)

    ! allocate arrays
    allocate(aod2d(is:ie,js:je),aod2dch(is:ie,js:je,nchanl)) 
    allocate(aodtmp(nlon,nlat-2),aodout(nlon,nlat-2))

    ! call CRTM for all gridpoints
    aodout = zero
    do i=is,ie
      do j=js,je-1
        if (i>nlat) cycle
        layer_od = zero
        ! fill out data_s
        data_s(:) = zero 
        data_s(1) = 784. ! satellite ID
        data_s(2) = tdiff ! difference from analysis time
        ! have to convert these using grdcrd1
        gridlon = rlons(j)
        gridlat = rlats(i)
        call grdcrd1(gridlon,rlons,nlon,1)
        call grdcrd1(gridlat,rlats,nlat,1)
        data_s(3) = gridlon ! grid relative lon
        data_s(4) = gridlat ! grid relative lat
        data_s(5) = rlons(j)*rad2deg ! lon in degrees
        data_s(6) = rlats(i)*rad2deg ! lat in degrees
        ! the following 4 are not necessary for modis or viirs aod...
        data_s(7) = rmiss_single
        data_s(8) = 90. ! solar zenith angle
        data_s(9) = 0. ! solar azimuth angle
        data_s(10) = 1. ! sfc type
        data_s(11) = 3. ! deep blue confidence flag
        ! going to try it without anything in the channel rows
        ! and see if CRTM works for AOD generation anyways
        !print *, i,j
        call call_crtm(obstype,tdiff,data_s,nchanl,nreal,ich, &
            tvp,qvp,clw_guess,prsltmp,prsitmp, &
            trop5,tzbgr,dtsavg,sfc_speed, &
            tsim,emissivity,ptau5,ts,emissivity_k, &
            temp,wmix,jacobian,error_status,layer_od=layer_od)
        if (error_status /=0) print *, 'CRTM error',error_status,i,j
        do k=1,nchanl
          aod2dch(i,j,k) = sum(layer_od(:,k))
          !print *,i,j,k,aod2dch(i,j,k)
        end do
        !aod2d(i,j) = sum(aod2dch(i,j,:))/nchanl ! is this right?
        aod2d(i,j) = aod2dch(i,j,4) ! channel 4 hard coded for now?
      end do
    end do
    ! below temporary to figure out numbers for vertical redistribution
    !do i=is,ie
    ! do j=js,je-1
    !   if (aod2d(i,j) < 0.05 .and. aod2d(i,j) > 0) then
    !      write(27000+mype,'(2i6,1f8.3)') i,j,aod2d(i,j)
    !   end if
    ! enddo
    !enddo 
    ! save AOD to chemguess bundle
    call destroy_crtm
    call gsi_bundlegetpointer(gsi_chemguess_bundle(it),'aod',ptr2daod,iret)
    if (iret == 0) ptr2daod=aod2d
    ! write out binary file
    call strip(aod2d   ,aodsm   )
    !aodsm = reshape(aod2d,(/size(aodsm)/))
    call mpi_gatherv(aodsm,ijn(mype+1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            0,mpi_comm_world,ierror)
    call load_grid(work1,aodtmp)
    aodout = real(aodtmp)
    
    if (mype==0) then
      open(302,file='aodguess',status='new',form='unformatted',access='sequential')
      write(302) aodout
      close(302)
    end if
    deallocate(aodtmp,aodout,aod2d,aod2dch)

  end subroutine calc_2d_aod
end module aodmod
