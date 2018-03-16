module abstract_setup_mod
  use kinds, only: r_kind, i_kind
  type,abstract :: abstract_setup_class
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_howv
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_mxtm
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_co
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_oz
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_div
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_w
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


  character(len=16) :: myname
  character(len=14),allocatable,dimension(:) :: varnames
  integer(i_kind) numvars
  contains
    procedure, pass(this) :: setup
    procedure, pass(this) :: setupp
    procedure, pass(this) :: setupDerived
    procedure, pass(this) :: allocate_and_check_vars
    procedure, pass(this) :: final_vars_
    procedure, pass(this) :: check_vars_
    procedure, pass(this) ::  init_ges
    procedure, pass(this) ::  allocate_ges3
    procedure, pass(this) ::  allocate_ges4
!   procedure, pass(this) :: setup_ctor2
!   procedure, pass(this) :: setup_ctor3
    procedure, pass(this) :: initialize 
!   procedure, pass(this) :: setup_ctor5
!   procedure, pass(this) :: setup_ctor6
!   procedure, pass(this) :: setup_ctor7
!   procedure, pass(this) :: setup_ctor8
  end type abstract_setup_class
  interface abstract_setup_class
       module procedure setup_ctor2
       module procedure setup_ctor3
  end interface 

contains    
! subroutine setup_cctor()
!    this%myname = "UNDEFINED"
!    this%numvars = 0
! end subroutine setup_cctor
  type(abstract_setup_class) function setup_ctor2(obsname,varname1)
     character(len=16),                        intent(in) :: obsname
     character(len=14),                        intent(in) :: varname1
     setup_ctor2.myname = obsname
     setup_ctor2.numvars = 1
     allocate(setup_ctor2.varnames(setup_ctor2.numvars))
     setup_ctor2.varnames(1) = varname1
  end function setup_ctor2
  type(abstract_setup_class) function setup_ctor3(obsname,varname1,varname2)
     character(len=16),                        intent(in) :: obsname
     character(len=14),                        intent(in) :: varname1
     character(len=14),                        intent(in) :: varname2
     setup_ctor3.myname = obsname
     setup_ctor3.numvars = 2
     allocate(setup_ctor3.varnames(setup_ctor3.numvars))
     setup_ctor3.varnames(1) = varname1
     setup_ctor3.varnames(2) = varname2
  end function setup_ctor3
  subroutine initialize(this,obsname,varname1,varname2,varname3)
      class(abstract_setup_class)              ,intent(inout) :: this
      character(*),                        intent(in) :: obsname
      character(*),                        intent(in) :: varname1
      character(*),                        intent(in) :: varname2
      character(*),                        intent(in) :: varname3
      this%myname = obsname
      this%numvars = 3
      allocate(this%varnames(this%numvars))
      this%varnames(1) = varname1
      this%varnames(2) = varname2
      this%varnames(3) = varname3
   end subroutine initialize
!  subroutine setup_ctor5(this,obsname,varname1,varname2,varname3,varname4)
!     class(abstract_setup_class)              ,intent(inout) :: this
!     character(len=16),                        intent(in) :: obsname
!     character(len=14),                        intent(in) :: varname1
!     character(len=14),                        intent(in) :: varname2
!     character(len=14),                        intent(in) :: varname3
!     character(len=14),                        intent(in) :: varname4
!     this%myname = obsname
!     this%numvars = 4
!     allocate(this%varnames(this%numvars))
!     this%varnames(1) = varname1
!     this%varnames(2) = varname2
!     this%varnames(3) = varname3
!     this%varnames(4) = varname4
!  end subroutine setup_ctor5
!  subroutine setup_ctor6(this,obsname,varname1,varname2,varname3,varname4,varname5)
!     class(abstract_setup_class)              ,intent(inout) :: this
!     character(len=16),                        intent(in) :: obsname
!     character(len=14),                        intent(in) :: varname1
!     character(len=14),                        intent(in) :: varname2
!     character(len=14),                        intent(in) :: varname3
!     character(len=14),                        intent(in) :: varname4
!     character(len=14),                        intent(in) :: varname5
!     this%myname = obsname
!     this%numvars = 5
!     allocate(this%varnames(this%numvars))
!     this%varnames(1) = varname1
!     this%varnames(2) = varname2
!     this%varnames(3) = varname3
!     this%varnames(4) = varname4
!     this%varnames(5) = varname5
!  end subroutine setup_ctor6
!  subroutine setup_ctor7(this,obsname,varname1,varname2,varname3,varname4,varname5,varname6)
!     class(abstract_setup_class)              ,intent(inout) :: this
!     character(len=16),                        intent(in) :: obsname
!     character(len=14),                        intent(in) :: varname1
!     character(len=14),                        intent(in) :: varname2
!     character(len=14),                        intent(in) :: varname3
!     character(len=14),                        intent(in) :: varname4
!     character(len=14),                        intent(in) :: varname5
!     character(len=14),                        intent(in) :: varname6
!     this%myname = obsname
!     this%numvars = 6
!     allocate(this%varnames(this%numvars))
!     this%varnames(1) = varname1
!     this%varnames(2) = varname2
!     this%varnames(3) = varname3
!     this%varnames(4) = varname4
!     this%varnames(5) = varname5
!     this%varnames(6) = varname6
!  end subroutine setup_ctor7
!  subroutine setup_ctor8(this,obsname,varname1,varname2,varname3,varname4,varname5,varname6,varname7)
!     class(abstract_setup_class)              ,intent(inout) :: this
!     character(len=16),                        intent(in) :: obsname
!     character(len=14),                        intent(in) :: varname1
!     character(len=14),                        intent(in) :: varname2
!     character(len=14),                        intent(in) :: varname3
!     character(len=14),                        intent(in) :: varname4
!     character(len=14),                        intent(in) :: varname5
!     character(len=14),                        intent(in) :: varname6
!     character(len=14),                        intent(in) :: varname7
!     this%myname = obsname
!     this%numvars = 7
!     allocate(this%varnames(this%numvars))
!     this%varnames(1) = varname1
!     this%varnames(2) = varname2
!     this%varnames(3) = varname3
!     this%varnames(4) = varname4
!     this%varnames(5) = varname5
!     this%varnames(6) = varname6
!     this%varnames(7) = varname7
!  end subroutine setup_ctor8


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
      real(r_kind),dimension(nele,nobs)                               :: data
      logical,dimension(nobs)                                         :: luse 

      write(6,*) ' in setup for ',this.myname,' with varnames ',this.varnames
      call this%allocate_and_check_vars(this.myname,lunin,luse,nele,nobs,data,this.varnames)
      call this%setupDerived(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave,luse,data)
      call this%final_vars_
  end subroutine setup
  subroutine setupDerived(this,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave,luse,data)
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
      logical,dimension(nobs)                          ,intent(inout) :: luse 
      real(r_kind),dimension(nele,nobs)                ,intent(inout) :: data
      write(6,*) 'this is a dummy setupDerived'
  end subroutine setupDerived

  subroutine setupp(this,obsname,varnames,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
      use kinds, only: r_kind,r_single,r_double,i_kind       
      use gridmod, only: nsig
      use qcmod, only: npres_print
      use convinfo, only: nconvtype
      class(abstract_setup_class)                      ,intent(inout) :: this
      character(len=16)                                ,intent(in   ) :: obsname
      character(len=14)                                ,intent(in   ) :: varnames(:)
      integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
      real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
      real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
      integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
      logical                                          ,intent(in   ) :: conv_diagsave

      logical,dimension(nobs)          :: luse
      real(r_kind),dimension(nele,nobs):: data
      write(6,*) ' in setupp for ',obsname,' with varnames ',varnames
      call this%allocate_and_check_vars(obsname,lunin,luse,nele,nobs,data,varnames)
      call this%setupDerived(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave,luse,data)
      call this%final_vars_

  end subroutine setupp
  subroutine allocate_and_check_vars(this,obsname,lunin,luse,nele,nobs,data,varnames)
      use kinds, only: i_kind       
      implicit none
      class(abstract_setup_class)                      ,intent(inout) :: this
      character(len=16)                                ,intent(in   ) :: obsname
      integer(i_kind)                                  ,intent(in   ) :: lunin,nobs,nele
      logical,dimension(nobs)                          ,intent(inout) :: luse
      character(len=14)                                ,intent(in   ) :: varnames(:)
      real(r_kind),dimension(nele,nobs)                ,intent(inout) :: data
      integer(i_kind) :: i

      logical :: proceed = .true.

      this%myname=obsname
      this%numvars = size(varnames)
      write(6,*) ' in allocate for ',obsname,' with size of varnames ',this%numvars
      allocate(this%varnames(this%numvars))
      do i=1,this%numvars  
        this%varnames(i) = varnames(i)
      enddo
! Check to see if required guess fields are available
      call this%check_vars_(proceed)
      if(.not.proceed) then
         read(lunin)data,luse   !advance through input file
         return  ! not all vars available, simply return
      endif

! If require guess vars available, extract from bundle ...
      call this%init_ges
      return 
  end subroutine allocate_and_check_vars
  subroutine allocate_ges3(this,ges,varname)
    use gsi_metguess_mod, only : gsi_metguess_bundle
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use guess_grids, only: nfldsig
    implicit none
    class(abstract_setup_class)                              , intent(inout) :: this
    real(r_kind),allocatable,dimension(:,:,:  ), intent(inout) :: ges
    character(len=*),                            intent(in   ) :: varname
    real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
    integer(i_kind) ifld, istatus

    call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
    if (istatus==0) then
          if(allocated(ges))then
             write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
             call stop2(999)
          endif
          write(6,*) 'in ges3, ',this%myname,' allocating ',varname
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
    character(len=9),                            intent(in   ) :: varname
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
      if(allocated(this%ges_w )) deallocate(this%ges_w )
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
      if(allocated(this%ges_howv)) deallocate(this%ges_howv)
      if(allocated(this%ges_mxtm)) deallocate(this%ges_mxtm)
      if(allocated(this%ges_co)) deallocate(this%ges_co)
      if(allocated(this%ges_div)) deallocate(this%ges_div)
      if(allocated(this%ges_oz)) deallocate(this%ges_oz)
      deallocate(this%varnames)

  end subroutine final_vars_

  subroutine check_vars_(this,proceed,include_w)
      use kinds, only: i_kind       
      use gsi_bundlemod, only : gsi_bundlegetpointer
      use gsi_metguess_mod, only : gsi_metguess_bundle
      use gsi_metguess_mod, only : gsi_metguess_get
      implicit none
      class(abstract_setup_class)                      ,intent(inout) :: this
      logical                                          ,intent(inout) :: proceed
      logical                                 ,optional,intent(inout) :: include_w
      integer(i_kind) ivar, istatus, i, loop_end

      proceed = .true.
      write(6,*) 'in checkvars for ',this%myname,' with proceed = ',proceed
      if( present(include_w) ) then
         loop_end = this%numvars - 1
      else 
         loop_end = this%numvars 
      endif
      do i = 1,loop_end
         call gsi_metguess_get (this%varnames(i), ivar, istatus )
         write(6,*) 'checked ',this%varnames(i),' and ivar = ',ivar 
         proceed=proceed.and.ivar>0
      enddo
      if( present(include_w) ) then
         call gsi_metguess_get (this%varnames(this%numvars), ivar, istatus )
         include_w = (ivar > 0)
      endif
      write(6,*) 'after checkvars proceed,ivar = ',proceed,ivar
      if( present(include_w) ) write(6,*) 'include_w is ',include_w
  end subroutine check_vars_ 
  subroutine init_ges(this)

    use kinds, only: r_kind,i_kind
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
    use guess_grids, only: hrdifsig,geop_hgtl,ges_lnprsl,&
         nfldsig,sfcmod_gfs,sfcmod_mm5,comp_fact10
    implicit none 
    class(abstract_setup_class)                              , intent(inout) :: this 
    character(len=14) :: fullname
    character(len=9) :: varname
    real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
    real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
    real(r_kind),dimension(:,:,:  ),pointer :: ges
    real(r_kind),dimension(:,:,:,: ),pointer :: ges4
    integer(i_kind) ifld, istatus, i, idx, rank
    write(6,*) 'HEY, setting up ges in ',this%myname
    do i = 1,this%numvars
      fullname = this%varnames(i)
      varname = fullname(6:14)
      write(6,*) 'HEY working on ',varname,' for ',this%myname
      select case (varname)
        case ('ps')
          write(6,*) 'allocating ',varname
          call this%allocate_ges3(this%ges_ps,varname)
        case ('z')
          write(6,*) 'allocating ',varname
          call this%allocate_ges3(this%ges_z,varname)
        case ('gust')
          call this%allocate_ges3(this%ges_gust,varname)
        case ('wspd10m')
          write(6,*) 'allocating ',varname
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
          write(6,*) 'allocating ',varname
          call this%allocate_ges4(this%ges_u,varname)
        case ('v')
          write(6,*) 'allocating ',varname
          call this%allocate_ges4(this%ges_v,varname)
        case ('w')
          write(6,*) 'allocating ',varname
          call this%allocate_ges4(this%ges_w,varname)
        case ('tv')
          write(6,*) 'allocating ',varname
          call this%allocate_ges4(this%ges_tv,varname)
        case ('pm10')
          call this%allocate_ges4(this%ges_pm10,varname)
        case ('q')
          call this%allocate_ges4(this%ges_q,varname)
        case ('pm2_5')
          call this%allocate_ges4(this%ges_pm2_5,varname)
        case ('mxtm')
          call this%allocate_ges3(this%ges_mxtm,varname)
        case ('howv')
          call this%allocate_ges3(this%ges_howv,varname)
        case ('co')
          call this%allocate_ges4(this%ges_co,varname)
        case ('oz')
          call this%allocate_ges4(this%ges_oz,varname)
        case ('div')
          call this%allocate_ges4(this%ges_div,varname)
      end select

    enddo
  end subroutine init_ges
 
end module abstract_setup_mod
