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
  use constants, only: ten,zero,r10
  !use obsmod, only: bmiss

  implicit none

  private

  !variable declarations across routines go here
  logical listexist
  logical fexist
  !logical mwlistexist
  !logical htlistexist
  !integer(i_kind),parameter::pmax=500
  !integer(i_kind),parameter::smax=100000
  integer(i_kind),parameter::nmax=60000_i_kind
  character(len=8),parameter::misprv="XXXXXXXX"
  character(len=8),parameter::allprov="allsprvs"
  real(r_kind),parameter:: bmiss = 1.0e9_r_kind

  ! Original Variables
  !character(len=8),allocatable,dimension(:)::cprovs
  !character(len=8),allocatable,dimension(:,:)::csubprovs !provider and subprovider names (2D)
  !real(r_kind),allocatable,dimension(:,:)::rwndht !provider/subprovider heights.  Syncs with cprovs.

  !New/Better Variables
  character(len=16),allocatable,dimension(:)::provlist
  real(r_kind),allocatable,dimension(:)::heightlist
  integer(i_kind)::numprovs

  public readin_windht_list
  !public better_readin_windht_list
  public init_windht_lists
  !public better_init_windht_lists
  public destroy_windht_lists
  public find_wind_height

  logical::verbose=.true.

contains

  subroutine readin_windht_list(filename,fexist,ncount) !,provlist,heightlist,ncount)

    implicit none

    !passed vars:
    character(len=80),intent(in)::filename
    !integer(i_kind),intent(in)::ndim
    logical,intent(out)::fexist
    !character(*),intent(out)::provlist(nmax)
    !real(r_kind),intent(out)::heightlist(nmax)
    integer(i_kind),intent(out)::ncount

    !local variables
    integer(i_kind)::meso_unit,n,reason
    character(len=16)::cstring
    character(len=8)::cprov,csubprov
    real(r_kind)::height

    !start subroutine
    ncount=0
    !initialize arrays
    allocate(provlist(nmax))
    allocate(heightlist(nmax))
    provlist(:)=misprv
    heightlist(:)=bmiss
    inquire(file=trim(filename),exist=fexist)
    if(fexist) then
       open(meso_unit,file=trim(filename),form='formatted')
       !initialize counter and reader
       n=0
       reason=0
       !read provider/subprovider/height file
       do while (reason.eq.0)
          read(meso_unit,423,iostat=reason) cprov,csubprov,height
          print*, "Reading in provider,subprovider,height:",cprov,csubprov,height
          n=n+1
          if (n.gt.nmax) then
             print*, "WARNING: Exceeding maximum number of provider/subprovder combinations (current,max)=",n,nmax
             exit
          endif
          cstring=cprov//csubprov
          provlist(n)=cstring
          heightlist(n)=height
       end do
423    format(A8,2X,A8,2X,F5.2)
       ncount=n-1
       print*, "Number of provider/subprovider combinations:",ncount
       close(meso_unit)
    endif

  end subroutine readin_windht_list
    

!  subroutine readin_windht_list (filename,provlist,sprovlist,heightlist,ndim)
!
!    !call once: use a 2d array: 1d for provider, 1d for subprovider
!
!    implicit none
!
!    !Passed vars:
!    character(len=80),intent(in)::filename
!
!    integer(i_kind),intent(in)::ndim
!    character(len=8),intent(out)::provlist(ndim)
!    character(len=8),intent(out)::sprovlist(ndim,ndim)
!    real(r_kind),intent(out)::heightlist(ndim,ndim)
!
!!Local vars:
!    integer(i_kind)::file_unit,n,m,reason,pnum,snum
!    real(r_kind)::htreal
!    character(len=80)::cstring
!    character(len=8)::cprov,csubprov,cprovlast
!    logical::fexist
!
!    !initialize output arrays and temporary variables
!    provlist(:)=misprv
!    sprovlist(:,:)=misprv
!    cprovlast=misprv
!    heightlist(:,:)=bmiss
!
!    inquire(file=trim(filename),exist=fexist)
!    if (fexist) then
!       open (file_unit,file=trim(filename),form='formatted')
!       do n=1,ndim
!          if (verbose) then
!             print*, "Reading provider file line number:",n
!          endif
!          read (file_unit,753,iostat=reason) cprov,csubprov,htreal
!          if (reason.lt.0) then !early EOF
!             print*, "WARNING: Prov/subprov end of file at line,expected,reason=:",n,reason
!             exit
!          elseif (reason.gt.0) then !error - stop reading
!             print*, "WARNING: Prov/subprov error at line:",n
!             print*, "Stopping prov/subprov read at this many entries"
!             exit
!          else !normal
!             if (n.eq.1) then
!                pnum=1
!                snum=1
!                provlist(pnum)=cprov
!                sprovlist(pnum,snum)=csubprov
!             else
!                if (cprov.eq.cprovlast) then !same provider, new subprovider
!                   snum=snum+1
!                   sprovlist(pnum,snum)=csubprov
!                else   !new provider, and therefore new subprovider also
!                   pnum=pnum+1
!                   snum=1
!                   provlist(pnum)=cprov
!                   sprovlist(pnum,snum)=csubprov
!                endif
!             endif
!             heightlist(pnum,snum)=htreal
!             cprovlast = cprov
!          endif
!       enddo
!753    format (A8,2X,A8,2X,F7.2) !to read input file
!       close(file_unit)
!    endif
!    
!  end subroutine readin_windht_list
  
  subroutine init_windht_lists

    implicit none

    !local variables
    integer(i_kind)::use_unit
    character(150)::cstring
    character(80)::filename

    !allocate(cprovs(nmax))
    !allocate(rwndht(nmax))

    filename='provider_windheight'
    inquire(file=trim(filename),exist=listexist)
    if(listexist) then
       !allocate()
       !allocate()
       call readin_windht_list(filename,fexist,numprovs)
       print*, "Second chance!  Number of provider/subprovider combinations=",numprovs
    else
       print*, "WARNING: Wind sensor height list file does not exist!"
       print*, "WARNING: Wind sensors will be assumed of height of 10 m AGL!"
       !rwndht=ten
    endif

  end subroutine init_windht_lists
  
!  subroutine init_windht_lists
!    !doc block
!    
!    !use mpimod, only: mype
!    
!    implicit none
!    
!    !declare passed variables (none)
!    
!    !declare local variabes:
!    
!    integer(i_kind)::file_unit,n,reason,nprov
!    character(len=80)::cstring,filename
!    !logical::fexist
!
!    !==> Read proivder list and wind heights
!    filename='provider_windheight'
!    inquire(file=trim(filename),exist=listexist)
!    if (listexist) then
!       open (file_unit,file=trim(filename),form='formatted',iostat=reason)
!       !do n=1!,nmax
!       n=1
!       do while (reason==0)
!          read(file_unit,'(A80)',iostat=reason) cstring
!          if (reason.lt.0) then
!             nprov=n-1
!             exit
!          elseif (reason.gt.0) then
!             print*, "WARNING: Error reading provider/subprovider wind height at line:",n
!             print*, "Stopping read at this number of providers!"
!             nprov=n-1
!             exit
!          endif
!       enddo
!       close(file_unit)
!       if (verbose) then
!          print*, 'Provider wind height list: listexist,nprov=',listexist,nprov
!       endif
!       allocate(cprovs(nprov))
!       allocate(csubprovs(nprov,nprov))
!       allocate(rwndht(nprov,nprov))
!       call readin_windht_list(filename,cprovs,csubprovs,rwndht,nprov)
!       !call better_readin_windht_list(filename,
!       !if (verbose) then
!       !   print*, 'Provider wind height list: listexist,nprov=',listexist,nprov
!       !endif
!    else
!       print*, "WARNING: Provider/subprovider wind height file not found!"
!    endif
!    print*, cprovs
!    print*, csubprovs
!    print*, rwndht 
!  end subroutine init_windht_lists
  
  subroutine destroy_windht_lists
    
    !abstract: Destroy wind height arrays previously allocated
    
    implicit none

    if (listexist) then
       deallocate(provlist)
       !deallocate(csubprovs)
       deallocate(heightlist)
    endif
    
  end subroutine destroy_windht_lists

  subroutine find_wind_height(cprov,csubprov,finalheight)

    implicit none

    character(len=8),intent(in)::cprov,csubprov
    !character(len=16),dimension(:),intent(in)::provs(nmax)
    !integer(i_kind),intent(in)::numprovs
    !real(r_kind),dimension(:),intent(in)::heights(nmax)
    real(r_kind),intent(out)::finalheight

    !local vars
    integer(i_kind)::i
    character(len=8)::tmpprov,tmpsubprov

    !sanity check
    if (.not.fexist) then
       print*, "WARNING: File containing sensor heights does not exist.  Defaulting to 10 m..."
       finalheight=r10
       return
    elseif(.not.listexist) then
       print*, "WARNING: List of providers not properly in memory.  Defaulting to 10 m..."
       finalheight=r10
       return
    elseif (numprovs.gt.nmax) then
       print*, "WARNING: Invalid number of provider/subprovider combinations (number,max)=",numprovs,nmax
       print*, "WARNING: Defaulting to 10 m wind sensor height!"
       finalheight=r10
       return
    endif

    do i=1,nmax
       if (i.gt.numprovs) then
          !print*, "Provider/subprovider not found:",cprov,csubprov
          finalheight=r10
          return
       else
          tmpprov=provlist(i)(1:8)
          tmpsubprov=provlist(i)(9:16)
          if (cprov.eq.tmpprov.and.((tmpsubprov.eq.csubprov).or.(tmpsubprov.eq.allprov))) then
             finalheight=heightlist(i)
             return
          endif
       endif
    enddo

    !abstract: Find provider and subprovider in pre-determined arrays
    !Then return wind sensor height
    !If provider/subprovider is not found, return default height of 10 m.

  end subroutine find_wind_height
  
end module windht
