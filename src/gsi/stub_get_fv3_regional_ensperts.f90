module get_fv3_regional_ensperts_mod
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module stub_get_fv3_regional_ensperts
!   prgmmr:	 j guo <jing.guo-1@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2020-02-12
!
! abstract: a stub of a type(get_fv3_regional_ensperts_class) object
!
! program history log:
!   2020-02-12  j guo   - tailored from cplr_get_fv3_regional_ensperts.f90, to
!                         support builds without a need for an actual object
!                         of type(get_fv3_regional_ensperts_class).
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use abstract_get_fv3_regional_ensperts_mod,only: abstract_get_fv3_regional_ensperts_class
  use kinds, only : i_kind
  implicit none
  private	! except
  public :: get_fv3_regional_ensperts_class

  type, extends(abstract_get_fv3_regional_ensperts_class) :: get_fv3_regional_ensperts_class
  contains
    procedure, pass(this) :: get_fv3_regional_ensperts => get_fv3_regional_ensperts_run
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_fv3_regional
    procedure, pass(this) :: general_read_fv3_regional
  end type get_fv3_regional_ensperts_class

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
contains
subroutine get_fv3_regional_ensperts_run(this,en_perts,nelen,ps_bar)
    use kinds, only: i_kind,r_single
    use gsi_bundlemod, only: gsi_bundle
    implicit none

  ! Declare passed variables
    class(get_fv3_regional_ensperts_class)     , intent(inout):: this
    type(gsi_bundle),allocatable               , intent(inout):: en_perts(:,:)
    integer(i_kind)                            , intent(in   ):: nelen
    real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  return
  end subroutine get_fv3_regional_ensperts_run
  
subroutine general_read_fv3_regional(this,fv3_filenameginput,g_ps,g_u,g_v,g_tv,g_rh,g_oz)
    use kinds, only: r_kind
    use gsi_rfv3io_mod,only: type_fv3regfilenameg
    implicit none
  
  ! Declare passed variables
    class(get_fv3_regional_ensperts_class), intent(inout):: this
    type (type_fv3regfilenameg)           , intent(in   ):: fv3_filenameginput
    real(r_kind),dimension(:,:,:)         , intent(  out):: g_u,g_v,g_tv,g_rh,g_oz
    real(r_kind),dimension(:,:  )         , intent(  out):: g_ps

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  return
  end subroutine general_read_fv3_regional

subroutine ens_spread_dualres_regional_fv3_regional(this,mype,en_perts,nelen,en_bar)
    use kinds, only: i_kind
    use gsi_bundlemod, only: gsi_bundle
    implicit none

  ! Declare passed variables
    class(get_fv3_regional_ensperts_class), intent(inout) :: this
    type(gsi_bundle),OPTIONAL,    intent(in):: en_bar
    integer(i_kind ),             intent(in):: mype
    type(gsi_bundle),allocatable, intent(in):: en_perts(:,:)
    integer(i_kind ),             intent(in):: nelen
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  return
  end subroutine ens_spread_dualres_regional_fv3_regional
  
end module get_fv3_regional_ensperts_mod
