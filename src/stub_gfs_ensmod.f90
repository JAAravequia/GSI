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

    return

end subroutine non_gaussian_ens_grid_dummy

end module get_gfs_ensmod_mod
