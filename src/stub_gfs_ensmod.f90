module get_gfs_ensmod_mod

    use mpeu_util, only: die
    use mpimod, only: mype,npe
    use abstract_get_gfs_ensmod_mod

    implicit none

    type, extends(abstract_get_gfs_ensmod_class) :: get_gfs_ensmod_class
    contains
        procedure, pass(this) :: non_gaussian_ens_grid_ => non_gaussian_ens_grid_dummy
        procedure, pass(this) :: get_user_ens_ => get_user_ens_dummy
        procedure, pass(this) :: put_gsi_ens_ => put_gsi_ens_dummy
    end type get_gfs_ensmod_class

contains

subroutine get_user_ens_dummy(this,grd,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_user_ens_    pretend atmos bkg is the ensemble
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Read in GFS ensemble members in to GSI ensemble.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2016-07-20  mpotts   - refactored into class/module
!
!   input argument list:
!     grd      - grd info for ensemble
!     member   - index for ensemble member
!     ntindex  - time index for ensemble
!
!   output argument list:
!     atm_bundle - atm bundle w/ fields for ensemble member
!     iret       - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: i_kind,r_kind,r_single
    use gridmod, only: use_gfs_nemsio
    use general_sub2grid_mod, only: sub2grid_info
    use hybrid_ensemble_parameters, only: n_ens,ens_fast_read
    use hybrid_ensemble_parameters, only: grd_ens
    use gsi_bundlemod, only: gsi_bundle
    use control_vectors, only: nc2d,nc3d

    implicit none

    ! Declare passed variables
    class(get_gfs_ensmod_class), intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle(:)
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='get_user_ens_gfs'
    real(r_single),allocatable,dimension(:,:,:,:) :: en_loc3
    integer(i_kind) :: m_cvars2d(nc2d),m_cvars3d(nc3d)

    integer(i_kind) :: n
    real(r_kind),allocatable,dimension(:) :: clons,slons

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    if ( use_gfs_nemsio .and. ens_fast_read ) then
       allocate(en_loc3(grd_ens%lat2,grd_ens%lon2,nc2d+nc3d*grd_ens%nsig,n_ens))
       allocate(clons(grd_ens%nlon),slons(grd_ens%nlon))
       call get_user_ens_gfs_fastread_(ntindex,en_loc3,m_cvars2d,m_cvars3d, &
                         grd_ens%lat2,grd_ens%lon2,grd_ens%nsig, &
                         nc2d,nc3d,n_ens,iret,clons,slons)
       do n=1,n_ens
          call move2bundle_(grd,en_loc3(:,:,:,n),atm_bundle(n), &
                            m_cvars2d,m_cvars3d,iret,clons,slons)
       end do
       deallocate(en_loc3,clons,slons)
    else
       do n = 1,n_ens
          call get_user_ens_gfs_member_(grd,n,ntindex,atm_bundle(n),iret)
       end do
    endif

    return

end subroutine get_user_ens_dummy

subroutine put_gsi_ens_dummy(this,grd,member,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_gsi_ens_    write out an internally gen ens to file
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Write out GSI ensemble to file.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2016-07-20  mpotts   - refactored into class/module
!
!   input argument list:
!     grd      - grd info for ensemble
!     member   - index for ensemble member
!     ntindex  - time index for ensemble
!   atm_bundle - bundle of ensemble perturbations
!
!   output argument list:
!     iret      - return code, 0 for successful write
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: i_kind
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_bundlemod, only: gsi_bundle
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: ensemble_path
    use hybrid_ensemble_parameters, only: sp_ens
    use gridmod, only: use_gfs_nemsio

    implicit none

    ! Declare passed variables
    class(get_gfs_ensmod_class), intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='put_gsi_ens_gfs'
    character(len=70) :: filename
    integer(i_kind) :: mype_atm
    logical,save :: inithead = .true.

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    mype_atm = member

    write(filename,13) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),member
13  format(a,'sigf',i2.2,'_ens_pert',i3.3)

    if ( use_gfs_nemsio ) then
       if ( mype == 0 ) then
          write(6,*) 'write_nemsatm is not adapted to write out perturbations yet'
          iret = 999
       endif
       !call write_nemsatm(grd,...)
    else
       call general_write_gfsatm(grd,sp_ens,sp_ens,filename,mype_atm, &
            atm_bundle,ntindex,inithead,iret)
    endif

    inithead = .false.

    if ( iret /= 0 ) then
       if ( mype == mype_atm ) then
          write(6,'(A)') 'put_gsi_ens_: WARNING!'
          write(6,'(3A,I5)') 'Trouble writing ensemble perturbation to file : ', trim(filename), ', IRET = ', iret
       endif
    endif

    return

end subroutine put_gsi_ens_dummy

subroutine non_gaussian_ens_grid_dummy(this,elats,elons)

    use kinds, only: r_kind
    use hybrid_ensemble_parameters, only: sp_ens

    implicit none

    ! Declare passed variables
    class(get_gfs_ensmod_class), intent(inout) :: this
    real(r_kind), intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    elats=sp_ens%rlats
    elons=sp_ens%rlons

    return

end subroutine non_gaussian_ens_grid_dummy

end module get_gfs_ensmod_mod
