module abstract_setup_mod
  use kinds, only: r_kind, i_kind
  type,abstract :: abstract_setup_class
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  character(len=10) :: myname
  integer(i_kind) ivar, istatus
  contains
    procedure(init_vars_derived), deferred, pass(this) :: init_vars_derived
    procedure, pass(this) :: setup
    procedure, pass(this) :: final_vars_
    procedure, pass(this) :: init_vars_base
    procedure, pass(this) :: check_vars_
  end type abstract_setup_class

  abstract interface 
    subroutine init_vars_derived(this)
      import abstract_setup_class
      class(abstract_setup_class)                      ,intent(inout) :: this
    end subroutine init_vars_derived
  end interface
contains    
  subroutine setup(this,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
      use kinds, only: r_kind,r_single,r_double,i_kind       
      use gridmod, only: nsig
      use qcmod, only: npres_print
      use convinfo, only: nconvtype
      class(abstract_setup_class)                      ,intent(inout) :: this
      integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
      real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
      real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
      integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
      logical                                          ,intent(in   ) :: conv_diagsave
      write(6,*) 'this is a dummy setup'
  end subroutine setup
  subroutine final_vars_(this)
      class(abstract_setup_class)                      ,intent(inout) :: this
      if(allocated(this%ges_v )) deallocate(this%ges_v )
      if(allocated(this%ges_u )) deallocate(this%ges_u )
      if(allocated(this%ges_z )) deallocate(this%ges_z )
      if(allocated(this%ges_ps)) deallocate(this%ges_ps)
  end subroutine final_vars_
  subroutine check_vars_ (this,proceed)
      use kinds, only: i_kind       
      use gsi_metguess_mod, only : gsi_metguess_get
      class(abstract_setup_class)                      ,intent(inout) :: this
      logical                                          ,intent(inout) :: proceed
    ! Check to see if required guess fields are available
      call gsi_metguess_get ('var::ps', this%ivar, this%istatus )
      proceed=this%ivar>0
      call gsi_metguess_get ('var::z' , this%ivar, this%istatus )
      proceed=proceed.and.this%ivar>0
      call gsi_metguess_get ('var::u' , this%ivar, this%istatus )
      proceed=proceed.and.this%ivar>0
      call gsi_metguess_get ('var::v' , this%ivar, this%istatus )
      proceed=proceed.and.this%ivar>0
  end subroutine check_vars_ 
  subroutine init_vars_base(this)
 
  use kinds, only: r_kind,i_kind
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use guess_grids, only: hrdifsig,geop_hgtl,ges_lnprsl,&
         nfldsig,sfcmod_gfs,sfcmod_mm5,comp_fact10
  implicit none 
  class(abstract_setup_class)                              , intent(inout) :: this 

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(this%ges_ps))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(this%ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         this%ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            this%ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(this%ges_z))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(this%ges_z(size(rank2,1),size(rank2,2),nfldsig))
         this%ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            this%ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(this%ges_u))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(this%ges_u(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         this%ges_u(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            this%ges_u(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(this%ges_v))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(this%ges_v(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         this%ges_v(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            this%ges_v(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(this%myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_base

end module abstract_setup_mod
