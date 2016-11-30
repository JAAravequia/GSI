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
    procedure, pass(this) :: check_vars_
    procedure, pass(this) ::  init_ges
    procedure, pass(this) ::  allocate_ges3
    procedure, pass(this) ::  allocate_ges4
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
  subroutine allocate_ges3(this,ges,varname)
    use gsi_metguess_mod, only : gsi_metguess_bundle
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use guess_grids, only: nfldsig
    implicit none
    class(abstract_setup_class)                              , intent(inout) :: this
    real(r_kind),allocatable,dimension(:,:,:  ), intent(inout) :: ges
    character(len=5),                            intent(in   ) :: varname
    real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
    integer(i_kind) ifld, istatus
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

  end subroutine allocate_ges3

  subroutine allocate_ges4(this,ges,varname)
    use gsi_metguess_mod, only : gsi_metguess_bundle
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use guess_grids, only: nfldsig
    implicit none
    class(abstract_setup_class)                              , intent(inout) :: this
    real(r_kind),allocatable,dimension(:,:,:,:), intent(inout) :: ges
    character(len=5),                            intent(in   ) :: varname
    real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
    integer(i_kind) ifld, istatus

    call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
    if (istatus==0) then
          if(allocated(ges))then
             write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
             call stop2(999)
          endif
          allocate(ges(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
          ges(:,:,:,1)=rank3
          do ifld=2,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
             ges(:,:,:,ifld)=rank3
          enddo
    else
          write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
          call stop2(999)
    endif

  end subroutine allocate_ges4
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
      do i = 1,this%numvars
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
  subroutine init_ges(this)

    use kinds, only: r_kind,i_kind
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
    use guess_grids, only: hrdifsig,geop_hgtl,ges_lnprsl,&
         nfldsig,sfcmod_gfs,sfcmod_mm5,comp_fact10
    implicit none 
    class(abstract_setup_class)                              , intent(inout) :: this 
    character(len=10) :: fullname
    character(len=5) :: varname
    real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
    real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
    real(r_kind),dimension(:,:,:  ),pointer :: ges
    real(r_kind),dimension(:,:,:,: ),pointer :: ges4
    integer(i_kind) ifld, istatus, i, idx, rank
    do i = 1,this%numvars
      fullname = this%varnames(i)
      varname = fullname(6:10)
      select case (varname)
        case ('ps')
          call this%allocate_ges3(this%ges_ps,varname)
        case ('z')
          call this%allocate_ges3(this%ges_z,varname)
        case ('gust')
          call this%allocate_ges3(this%ges_gust,varname)
        case ('wspd10m')
          call this%allocate_ges3(this%ges_wspd10m,varname)
        case ('cldch')
          call this%allocate_ges3(this%ges_cldch,varname)
        case ('lcbas')
          call this%allocate_ges3(this%ges_lcbas,varname)
        case ('mitm')
          call this%allocate_ges3(this%ges_mitm,varname)
        case ('pblh')
          call this%allocate_ges3(this%ges_pblh,varname)
        case ('th2')
          call this%allocate_ges3(this%ges_th2,varname)
        case ('pmsl')
          call this%allocate_ges3(this%ges_pmsl,varname)
        case ('q2m')
          call this%allocate_ges3(this%ges_q2m,varname)
        case ('q2')
          call this%allocate_ges3(this%ges_q2,varname)
        case ('tcamt')
          call this%allocate_ges3(this%ges_tcamt,varname)
        case ('td2m')
          call this%allocate_ges3(this%ges_td2m,varname)
        case ('vis')
          call this%allocate_ges3(this%ges_vis,varname)
        case ('u')
          call this%allocate_ges4(this%ges_u,varname)
        case ('v')
          call this%allocate_ges4(this%ges_v,varname)
        case ('tv')
          call this%allocate_ges4(this%ges_tv,varname)
        case ('pm10')
          call this%allocate_ges4(this%ges_pm10,varname)
        case ('q')
          call this%allocate_ges4(this%ges_q,varname)
        case ('pm2_5')
          call this%allocate_ges4(this%ges_pm2_5,varname)
      end select

    enddo
  end subroutine init_ges
 
end module abstract_setup_mod
