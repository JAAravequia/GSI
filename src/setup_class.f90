module abstract_setup_mod
  use kinds, only: r_kind, i_kind
  type,abstract :: abstract_setup_class
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_gust
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_wspd10m
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_cldch
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_lcbas
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_mitm
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_pblh
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_th2 
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_pm10
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_pm2_5
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_pmsl
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_q2m
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_q2
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_tcamt
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_td2m
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_vis
  character(len=10) :: myname
  character(len=10),allocatable,dimension(:) :: varnames
  integer(i_kind) numvars
  contains
    procedure(init_vars_derived), deferred, pass(this) :: init_vars_derived
    procedure, pass(this) :: setup
    procedure, pass(this) :: final_vars_
    procedure, pass(this) :: init_vars_base
    procedure, pass(this) :: check_vars_
    procedure, pass(this) ::  init_ges
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
      if(allocated(this%ges_tv)) deallocate(this%ges_tv)
      if(allocated(this%ges_gust)) deallocate(this%ges_gust)
      if(allocated(this%ges_wspd10m)) deallocate(this%ges_wspd10m)
      if(allocated(this%ges_cldch)) deallocate(this%ges_cldch)
      if(allocated(this%ges_lcbas)) deallocate(this%ges_lcbas)
      if(allocated(this%ges_pm10)) deallocate(this%ges_pm10)
      if(allocated(this%ges_pm2_5)) deallocate(this%ges_pm2_5)
      if(allocated(this%ges_pmsl)) deallocate(this%ges_pmsl)
      if(allocated(this%ges_q2m)) deallocate(this%ges_q2m)
      if(allocated(this%ges_q)) deallocate(this%ges_q)
      if(allocated(this%ges_q)) deallocate(this%ges_q2)
      if(allocated(this%ges_tcamt)) deallocate(this%ges_tcamt)
      if(allocated(this%ges_td2m)) deallocate(this%ges_td2m)
      if(allocated(this%ges_pblh)) deallocate(this%ges_pblh)
      if(allocated(this%ges_th2)) deallocate(this%ges_th2)
      if(allocated(this%ges_mitm)) deallocate(this%ges_mitm)
      if(allocated(this%ges_vis)) deallocate(this%ges_vis)
  end subroutine final_vars_

  subroutine check_vars_(this,proceed)
      use kinds, only: i_kind       
      use gsi_bundlemod, only : gsi_bundlegetpointer
      use gsi_metguess_mod, only : gsi_metguess_bundle
      use gsi_metguess_mod, only : gsi_metguess_get
      implicit none
      class(abstract_setup_class)                      ,intent(inout) :: this
      logical                                          ,intent(inout) :: proceed
      integer(i_kind) ivar, istatus, i

      write(6,*) 'in checkvars for ',this%myname,' with proceed = ',proceed
      do i = 1,size(this%varnames)
         call gsi_metguess_get (this%varnames(i), ivar, istatus )
         write(6,*) 'checked ',this%varnames(i),' and ivar = ',ivar 
         if( i == 1 ) then
           proceed=ivar>0
         else
           proceed=proceed.and.ivar>0
         endif
      enddo
      write(6,*) 'after checkvars proceed = ',proceed
  end subroutine check_vars_ 
  subroutine init_ges(this, varname, rank)

    use kinds, only: r_kind,i_kind
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
    use guess_grids, only: hrdifsig,geop_hgtl,ges_lnprsl,&
         nfldsig,sfcmod_gfs,sfcmod_mm5,comp_fact10
    implicit none 
    class(abstract_setup_class)                              , intent(inout) :: this 
    character(len=5)                                         , intent(in) :: varname
    integer(i_kind)                                          , intent(in) :: rank
    real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
    real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
    real(r_kind),dimension(:,:,:  ),pointer:: ges=>NULL()
    real(r_kind),dimension(:,:,:,: ),pointer:: ges3=>NULL()
    integer(i_kind) ifld, istatus
    select case (varname)
      case ('ps')
        ges = this%ges_ps
      case ('z')
        ges = this%ges_z
      case ('gust')
        ges = this%ges_gust
      case ('wspd10m')
        ges = this%ges_wspd10m
      case ('cldch')
        ges = this%ges_cldch
      case ('lcbas')
        ges = this%ges_lcbas
      case ('mitm')
        ges = this%ges_mitm
      case ('pblh')
        ges = this%ges_pblh
      case ('th2')
        ges = this%ges_th2
      case ('pmsl')
        ges = this%ges_pmsl
      case ('q2m')
        ges = this%ges_q2m
      case ('q2')
        ges = this%ges_q2
      case ('tcamt')
        ges = this%ges_tcamt
      case ('td2m')
        ges = this%ges_td2m
      case ('vis')
        ges = this%ges_vis
    end select

    if(rank == 3) then
    call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
    if (istatus==0) then
         if(allocated(ges))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges(size(rank2,1),size(rank2,2),nfldsig))
         ges(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges(:,:,ifld)=rank2
         enddo
    else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
    endif
    else
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges3))then
            write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges3(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges3(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges3(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
    endif

  end subroutine init_ges
 
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
     varname='ps'
     call this%init_ges(varname,2)
     varname='z'
     call this%init_ges(varname,3)
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
