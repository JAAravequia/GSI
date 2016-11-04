module abstract_setup_mod
  type,abstract :: abstract_setup_class
  use kinds, only: r_kind
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  contains
    procedure(setup), deferred, pass(this) :: setup
    procedure(init_vars_), deferred, pass(this) :: init_vars_
    procedure, pass(this) :: check_vars_
    procedure, pass(this) :: final_vars_
  end type abstract_setup_class
  abstract interface 
    subroutine setup(this,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
      use kinds, only: r_kind,r_single,r_double,i_kind       
      import abstract_setup_class
      class(abstract_setup_class)                      ,intent(inout) :: this
      integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
      real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
      real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
      integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
      logical                                          ,intent(in   ) :: conv_diagsave
    end subroutine setup
  end interface
  abstract interface 
    subroutine init_vars_(this)
      import abstract_setup_class
      class(abstract_setup_class)                      ,intent(inout) :: this
    end subroutine init_vars_
  end interface
    
  subroutine final_vars_(this)
      import abstract_setup_class
      class(abstract_setup_class)                      ,intent(inout) :: this
      if(allocated(this%ges_v )) deallocate(this%ges_v )
      if(allocated(this%ges_u )) deallocate(this%ges_u )
      if(allocated(this%ges_z )) deallocate(this%ges_z )
      if(allocated(this%ges_ps)) deallocate(this%ges_ps)
  end subroutine final_vars_
  subroutine check_vars_ (this,proceed)
      import abstract_setup_class
      class(abstract_setup_class)                      ,intent(inout) :: this
      logical                                          ,intent(inout) :: proceed
      integer(i_kind) ivar, istatus
    ! Check to see if required guess fields are available
      call gsi_metguess_get ('var::ps', ivar, istatus )
      proceed=ivar>0
      call gsi_metguess_get ('var::z' , ivar, istatus )
      proceed=proceed.and.ivar>0
      call gsi_metguess_get ('var::u' , ivar, istatus )
      proceed=proceed.and.ivar>0
      call gsi_metguess_get ('var::v' , ivar, istatus )
      proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

end module abstract_setup_mod
