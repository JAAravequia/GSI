module windht

!$$$ module documentation block
!
! module:     windht
! programmer: levine
!
! abstract: contains subroutine for addition of wind sensor
!           height for various providers and subproviders.
!           Generates lists of provider/subproviders and applies
!           these heights to wind observations if present.
!
! program history log:
!   2018-12-13  levine (first shot)
!
! subroutines included:
!   sub readin_wndht_list
!   sub init_wndht_lists
!   sub destroy_windht_lists
!
!  variable definitions:  ??
!
!  attributes:
!    language: f90
!    machine: IBM-WCOSS
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_single,r_double
  use obsmod, only: bmiss

  implicit none

  private

  !variable declarations across routines go here
  logical listexist
  logical mwlistexist
  logical htlistexist
  integer(i_kind),parameter::pmax=500
  integer(i_kind),parameter::smax=100000
  character(len=8),parameter::misprv="XXXXXXXX"

  character(len=8),allocatable,dimension(:,:)::cprovs !provider and subprovider names (2D)
  real(r_kind),allocatable,dimension(:,:)::rwndht !provider/subprovider heights.  Syncs with cprovs.

  public readin_wndht_list
  public init_windht_lists
  public destroy_windht_lists

contains

  subroutine readin_windht_list (filename,fexist,provlist,heightlist,ndim,ncount)

    !call twice: once for provider list, and once for MesoWest subprovider list
    !use both lists

    implicit none

    !Passed vars:
    character(len=80),intent(in)::filename
    integer(i_kind),intent(in)::ndim
    logical,intent(out)::fexist

    integer(i_kind), intent(out)::ncount
    character(*), intent(out)::provlist(ndim)
    real(r_kind), intent(out)::heightlist(ndim)

!Local vars:
    integer(i_kind)::file_unit,n,m
    real(r_kind)::htreal
    character(len=80)::cstring

    ncount=0
    inquire(file=trim(filename),exist=fexist)
    if (fexist) then
       allocate(provlist(ndim))
       allocate(heightlist(ndim))
       open (file_unit),file=trim(filename),form='formatted')
       do n=1,ndim
          read(file_unit,fmt=753,end=131) cstring,htreal
          provlist(n)=cstring
          heightlist(n)=htreal
       enddo
753    format !to read input file
       print*, "WARNING: Reached maximum number of providers when reading in list:", ndim
131    continue
       ncount=n-1
       close(file_unit)
    endif

  end subroutine readin_windht_list


  subroutine init_wndht_lists
    !doc block

    use mpimod, only: mype

    implicit none

    !declare passed variables (none)

    !declare local variabes:

    integer(i_kind) prv_unit,mwest_unit
    
    !!==>Allocate and initialize arrays based on provmax variable
    allocate(cprovht(pmax))
    allocate(rprovht(pmax))
    allocate(cmwht(pmax))
    allocate(rmwht(pmax))
    cprovht(:)=misprv
    cmwht(:)=misprv
    rprovht(:)=bmiss
    rmwht(:)=bmiss

    !==> Read proivder list and wind heights
    fname='provider_windheight'
    call readin_wndht_list(fname,listexist,cprovht,iprovht,pmax,nprov)
    if (verbose) then
       print*, 'Provider wind height list: listexist,nprov=',listexist,nprov
    endif
    htlistexist=listexist !identify whether list exists, deallocate if not
    if (htlistexist.eq..false.) then
       deallocate(cprovht)
       deallocate(rprovht)
    endif

    !==> Read mesowest subprovider list and wind heights
    fname='mesowest_windheight'
    call readin_wndht_list(fname,listexist,cmwht,imwht,pmax,nprov)
    if (verbose) then
       print*, 'MesoWest wind height list: listexist,nprov=',listexist,nprov
    endif
    mwlistexist=listexist !identify whether list exists, deallocate if not
    if (mwlistexist.eq..false.) then
       deallocate(cmwht)
       deallocate(rmtht)
    endif

  end subroutine init_wndht_lists

  
  subroutine destroy_windht_lists

    !abstract: Destroy wind height arrays previously allocated

    implicit none

    if (htlistexist) then
       deallocate (cprovht)
       deallocate (rprovht)
    endif
    if (mwlistexist) then
       deallocate (cmwht)
       deallocate (rmwht)
    endif

  end subroutine destroy_windht_lists

end module windht
