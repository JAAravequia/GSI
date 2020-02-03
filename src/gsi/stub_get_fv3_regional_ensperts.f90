module get_fv3_regional_ensperts_mod
use abstract_get_fv3_regional_ensperts_mod,only: abstract_get_fv3_regional_ensperts_class
  use kinds, only : i_kind
  type, extends(abstract_get_fv3_regional_ensperts_class) :: get_fv3_regional_ensperts_class
  contains
    procedure, pass(this) :: get_fv3_regional_ensperts => get_fv3_regional_ensperts_run
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_fv3_regional
    procedure, pass(this) :: general_read_fv3_regional
  end type get_fv3_regional_ensperts_class

contains
  subroutine get_fv3_regional_ensperts_run(this,en_perts,nelen,ps_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_fv3_regional_ensperts  read arw model ensemble members
  !   prgmmr: Ting            org: EMC/NCEP            date: 2018-12-13
  !
  ! abstract: read ensemble members from the fv3 regional (fv3_SAR)
  ! model,following Wanshu's programs to read those background files 
  !
  !
  ! program history log:
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine: 
  !
  !$$$ end documentation block
  
      use kinds, only: i_kind,r_single
      use gsi_bundlemod, only: gsi_bundle
      implicit none
      class(get_fv3_regional_ensperts_class)     , intent(inout):: this
      type(gsi_bundle),allocatable               , intent(inout):: en_perts(:,:)
      integer(i_kind)                            , intent(in   ):: nelen
      real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  return
  end subroutine get_fv3_regional_ensperts_run
  
  subroutine general_read_fv3_regional(this,fv3_filenameginput,g_ps,g_u,g_v,g_tv,g_rh,g_oz)
  !$$$  subprogram documentation block
  !     first compied from general_read_arw_regional           .      .    .                                       .
  ! subprogram:    general_read_fv3_regional  read fv3sar model ensemble members
  !   prgmmr: Ting             org: emc/ncep            date: 2018
  !
  ! abstract: read ensemble members from the fv3sar model in "restart" or "cold start"  netcdf format
  !           for use with hybrid ensemble option. 
  !
  ! program history log:
  !   2018-  Ting      - intial versions  
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use kinds, only: r_kind
      use gsi_rfv3io_mod,only: type_fv3regfilenameg
      implicit none
  !
  ! Declare passed variables
      class(get_fv3_regional_ensperts_class), intent(inout) :: this
      type (type_fv3regfilenameg)           , intent(in)    :: fv3_filenameginput
      real(r_kind),dimension(:,:,:),intent(out)::g_u,g_v,g_tv,g_rh,g_oz
      real(r_kind),dimension(:,:  ),intent(out)::g_ps

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  return
  end subroutine general_read_fv3_regional
  subroutine ens_spread_dualres_regional_fv3_regional(this,mype,en_perts,nelen,en_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    ens_spread_dualres_regional
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract:
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2011-04-05  parrish - add pseudo-bundle capability
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !
  !   input argument list:
  !     en_bar - ensemble mean
  !      mype  - current processor number
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  !
    use kinds, only: i_kind
    use gsi_bundlemod, only: gsi_bundle
    implicit none

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
