!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_diag                       read ozone diag file
!   prgmmr: hliu           org: np20                date: 2009-04-15
!
! abstract:  This module contains code to process ozone
!            diagnostic files.  The module defines structures
!            to contain information from the ozone
!            diagnostic files and then provides two routines
!            to access contents of the file.
!
! program history log:
!
! contains
!   read_ozndiag_header - read ozone diagnostic file header
!   read_ozndiag_data   - read ozone diagnostic file data
!   set_netcdf_read     - call set_netcdf_read(.true.) to use nc4 hooks,
!                       otherwise read file as binary format
!   open_ozndiag        - open a diag file for reading
!   close_ozndiag       - close an open diag file
!------------------------------------------------------------
!

module read_diag

  !--- use

  use kinds, only: r_single,r_double,i_kind
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close, & 
                           nc_diag_read_get_dim, nc_diag_read_get_global_attr, &
                           nc_diag_read_get_var_names, &
                           nc_diag_read_get_global_attr_names, &
                           nc_diag_read_get_var
 
  use ncdr_vars, only:     nc_diag_read_check_var


  !--- implicit

  implicit none


  !--- public & private

  private

  public :: diag_header_fix_list
  public :: diag_header_nlev_list
  public :: diag_data_fix_list
  public :: diag_data_nlev_list
  public :: diag_data_extra_list

  public :: open_ozndiag
  public :: close_ozndiag
  public :: read_ozndiag_header
  public :: read_ozndiag_data

  public :: set_netcdf_read


  !--- diagnostic file format - header
  
  type diag_header_fix_list
    sequence
    character(len=20) :: isis           ! sat and sensor type
    character(len=10) :: id             ! sat type
    character(len=10) :: obstype	! observation type
    integer(i_kind)   :: jiter          ! outer loop counter (1 = ges, 3 = anl)
    integer(i_kind)   :: nlevs		! number of levels (layer amounts + total column) per obs
    integer(i_kind)   :: ianldate	! analysis date in YYYYMMDDHH 
    integer(i_kind)   :: iint		! mpi task number
    integer(i_kind)   :: ireal		! # of real elements in the fix part of a data record
    integer(i_kind)   :: iextra		! # of extra elements for each level
  end type diag_header_fix_list

  type diag_header_nlev_list
    sequence
    real(r_single) :: pob		! SBUV/2,omi and gome-2 obs pressure level
    real(r_single) :: grs		! gross error
    real(r_single) :: err		! observation error
    integer(i_kind):: iouse		! use flag
  end type diag_header_nlev_list

  !--- diagnostic file format - data

  integer,parameter :: IREAL_RESERVE  = 3
  
  type diag_data_fix_list
    sequence
    real(r_single) :: lat            ! latitude (deg)
    real(r_single) :: lon            ! longitude (deg)
    real(r_single) :: obstime        ! observation time relative to analysis
  end type diag_data_fix_list

  type diag_data_nlev_list
    sequence
    real(r_single) :: ozobs              ! ozone (obs)
    real(r_single) :: ozone_inv          ! obs-ges
    real(r_single) :: varinv             ! inverse obs error **2
    real(r_single) :: sza                ! solar zenith angle
    real(r_single) :: fovn               ! scan position (field of view)
    real(r_single) :: toqf               ! omi row anomaly index or MLS o3mr precision
  end type diag_data_nlev_list

  type diag_data_extra_list
  sequence
    real(r_single) :: extra              ! extra information
  end type diag_data_extra_list

  logical,save     ::  netcdf = .false.

  type ncdiag_status
     logical :: nc_read
     integer(i_kind) :: cur_ob_idx
     integer(i_kind) :: num_records
     type(diag_data_fix_list),  allocatable  :: all_data_fix(:)
     type(diag_data_nlev_list), allocatable  :: all_data_nlev(:,:)  ! rad nchan= ozn nlev
     type(diag_data_extra_list), allocatable :: all_data_extra(:,:,:)
  end type ncdiag_status

  integer(i_kind), parameter                            :: MAX_OPEN_NCDIAG = 2
  integer(i_kind), save                                 :: nopen_ncdiag = 0
  integer(i_kind), dimension(MAX_OPEN_NCDIAG), save     :: ncdiag_open_id = (/-1, -1/)
  type(ncdiag_status), dimension(MAX_OPEN_NCDIAG), save :: ncdiag_open_status


 contains

  !------------------------------------------------------------
  ! subroutine open_ozndiag
  !------------------------------------------------------------
  subroutine open_ozndiag(filename, ftin, istatus)
     character*500,   intent(in) :: filename

     !---------------------------------------------------------------------------- 
     !  Note:  This use of ftin here as inout is pretty sloppy.  Internally this
     !  module should translate ftin (file id from time.f90) into the proper index
     !  for use w/in this module.  Encapsulation is cleaner than modification of 
     !  external variables.  I got this directly from the src/gsi read_diag.f90
     !  but that doesn't make it right.  I'll clean this up when all is working.
     !
     integer(i_kind), intent(inout) :: ftin             
     integer(i_kind), intent(out):: istatus
     
     integer(i_kind) :: i,ncd_nobs

     write(6,*)'--> open_ozndiag'
     write(6,*)'      filename, ftin = ', TRIM(filename), ftin
     istatus = -999

     if (netcdf) then

        write(6,*) '  nopen_ncdiag, MAX_OPEN_NCDIAG = ', nopen_ncdiag, MAX_OPEN_NCDIAG
        write(6,*) '  ncdiag_open_id = ', ncdiag_open_id

        if (nopen_ncdiag >= MAX_OPEN_NCDIAG) then
           write(6,*) 'OPEN_RADIAG:  ***ERROR*** Cannot open more than ', &
                    MAX_OPEN_NCDIAG, ' netcdf diag files.'
           istatus = -1
        endif

        if ( istatus /= 0 ) then
           call nc_diag_read_init(filename,ftin)
           write(6,*) 'after nc_diag_read_init, ftin = ', ftin
           istatus=0

           do i = 1, MAX_OPEN_NCDIAG
              write(6,*) 'top of 1..MAX_OPEN_NCDIAG loop, i = ', i
              write(6,*) '  and ncdiag_open_id(i) = ', ncdiag_open_id(i)

              if (ncdiag_open_id(i) < 0) then
                 
                 ncdiag_open_id(i) = ftin
                 write(6,*) 'now i, ncdiag_open_id(i), ftin = ', i, ncdiag_open_id(i), ftin
                 ncdiag_open_status(i)%nc_read = .false.
                 ncdiag_open_status(i)%cur_ob_idx = 1 

                 if (allocated(ncdiag_open_status(i)%all_data_fix)) then
                    deallocate(ncdiag_open_status(i)%all_data_fix)
                 endif
                 if (allocated(ncdiag_open_status(i)%all_data_nlev)) then
                    deallocate(ncdiag_open_status(i)%all_data_nlev)
                 endif
                 if (allocated(ncdiag_open_status(i)%all_data_extra)) then
                    deallocate(ncdiag_open_status(i)%all_data_extra)
                 endif

                 ncdiag_open_status(i)%num_records = nc_diag_read_get_dim(ftin,'nobs')
                 nopen_ncdiag = nopen_ncdiag + 1

                 write(6,*) 'ncdiag_open_status(i) dump, i     = ', i
                 write(6,*) '                      %nc_read    = ', ncdiag_open_status(i)%nc_read
                 write(6,*) '                      %cur_ob_idx = ', ncdiag_open_status(i)%cur_ob_idx
                 write(6,*) '                      %num_records= ', ncdiag_open_status(i)%num_records
                 write(6,*) 'nopen_ncdiag = ', nopen_ncdiag
                 write(6,*) 'ncdiag_open_id(i) = ', ncdiag_open_id(i)
                 exit

              endif

           enddo
        endif

     else
       open(ftin,form="unformatted",file=filename,iostat=istatus)
       rewind(ftin)
     endif

     write(6,*)'<-- open_ozndiag'
     write(6,*)'                '

  end subroutine open_ozndiag


  !------------------------------------------------------------
  ! subroutine close_ozndiag
  !------------------------------------------------------------
  subroutine close_ozndiag(filename, ftin)
     character*500,   intent(in) :: filename
     integer(i_kind), intent(inout) :: ftin

     integer(i_kind) :: id

     write(6,*)'--> close_ozndiag'
     if (netcdf) then
        id = find_ncdiag_id(ftin)
        if (id < 0) then
           write(6,*) 'CLOSE_RADIAG:  ***ERROR*** ncdiag file ', filename,   &
                      ' was not opened'
!           call stop2(456)
        endif
        call nc_diag_read_close(filename)
        ncdiag_open_id(id) = -1
        ncdiag_open_status(id)%nc_read = .false.
        ncdiag_open_status(id)%cur_ob_idx = -9999
        ncdiag_open_status(id)%num_records = -9999
        if (allocated(ncdiag_open_status(id)%all_data_fix)) then
           deallocate(ncdiag_open_status(id)%all_data_fix)
        endif
        if (allocated(ncdiag_open_status(id)%all_data_nlev)) then
           deallocate(ncdiag_open_status(id)%all_data_nlev)
        endif
        if (allocated(ncdiag_open_status(id)%all_data_extra)) then
           deallocate(ncdiag_open_status(id)%all_data_extra)
        endif
        nopen_ncdiag = nopen_ncdiag - 1
     else
        close(ftin)
     endif

     write(6,*)'<-- close_ozndiag'
  end subroutine close_ozndiag


  !------------------------------------------------------------
  ! subroutine set_netcdf_read
  !
  ! set the use_netcdf flag to read either binary (default) or
  !    netcdf formatted diagnostic files.
  !------------------------------------------------------------
  subroutine set_netcdf_read( use_netcdf )

     logical,intent(in)                     :: use_netcdf

     write(6,*)'--> set_netcdf, use_netcdf = ', use_netcdf
     netcdf = use_netcdf
     write(6,*)'netcdf = ', netcdf
     write(6,*)'<-- set_netcdf'

  end subroutine set_netcdf_read


  !------------------------------------------------------------
  !  read_ozndiag_header
  !
  !  Read a header record of a diagnostic file in either 
  !  NetCDF for binary format.
  !------------------------------------------------------------
  subroutine read_ozndiag_header( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus

  
    write(6,*) '--> read_ozndiag_header'
    istatus = 0
 
    if ( netcdf ) then
       call read_ozndiag_header_nc( ftin, header_fix, header_nlev, new_hdr, istatus )
    else
       call read_ozndiag_header_bin( ftin, header_fix, header_nlev, new_hdr, istatus )
    endif

    print*, 'ftin                = ',        ftin
    print*, 'header_fix%isis     = ',        header_fix%isis
    print*, 'header_fix%id       = ',        header_fix%id
    print*, 'header_fix%obstype  = ',        header_fix%obstype
    print*, 'header_fix%jiter    = ',        header_fix%jiter  
    print*, 'header_fix%nlevs    = ',        header_fix%nlevs  
    print*, 'header_fix%ianldate = ',        header_fix%ianldate
    print*, 'header_fix%iint     = ',        header_fix%iint    
    print*, 'header_fix%ireal    = ',        header_fix%ireal    
    print*, 'header_fix%iextra   = ',        header_fix%iextra   

    print*, 'istatus             = ',        istatus
    write(6,*) '<-- read_ozndiag_header'
 
  end subroutine read_ozndiag_header



  !------------------------------------------------------------
  !  subroutine read_ozndiag_header_nc
  !------------------------------------------------------------
  subroutine read_ozndiag_header_nc( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus

    !--- variables
    
    integer,save :: nlevs_last = -1

    character(len=10):: sat,obstype
    character(len=20):: isis
    integer(i_kind):: jiter,nlevs
    integer(i_kind),dimension(:),allocatable :: iouse
    real(r_double),dimension(:),allocatable  :: pobs,gross,tnoise
   
    integer(i_kind)                          :: nsdim,k,idate
    integer(i_kind),dimension(:),allocatable :: iuse_flag
    integer(i_kind)                          :: analysis_use_flag,idx
    integer(i_kind)                          :: num_vars, var_name_mlen
    integer(i_kind)                          :: num_global_attrs, attr_name_mlen
    character(len=:),dimension(:), allocatable :: var_names,attr_names

 
    istatus = 0
    write(6,*) ''; write(6,*) ''
    write(6,*) '--> read_ozndiag_header_nc, ftin = ', ftin


    call nc_diag_read_get_global_attr_names(ftin, num_global_attrs, &
                attr_name_mlen, attr_names)
    do k=1,num_global_attrs
       write(6,*) 'k, attr_names = ', k, attr_names(k)
    end do 

    call nc_diag_read_get_var_names(ftin, num_vars, var_name_mlen, var_names)
    write(6,*) ' num_vars = ', num_vars

    do k=1,num_vars
       write(6,*) 'k, var_names = ', k, var_names(k)
    end do 

    call nc_diag_read_get_global_attr(ftin, "date_time", idate)            ;
    call nc_diag_read_get_global_attr(ftin, "Satellite_Sensor", isis)      ;
    call nc_diag_read_get_global_attr(ftin, "Satellite", sat) 
    call nc_diag_read_get_global_attr(ftin, "Observation_type", obstype)   ;
    call nc_diag_read_get_global_attr(ftin, "Number_of_state_vars", nsdim )

    call nc_diag_read_get_global_attr(ftin, "pobs", pobs )
    call nc_diag_read_get_global_attr(ftin, "gross", gross )
    call nc_diag_read_get_global_attr(ftin, "tnoise", tnoise )

    !-------------------------------------------------------------------
    !  The Anaysis_Use_Flag in the netcdf file resides in the 
    !  obs data rather than global (equivalent of binary file header 
    !  location.  So we need read that in a different way.  Also, iuse 
    !  assignment by level is not possible, so the first value is good 
    !  for all (or so I've been told).

    idx = find_ncdiag_id(ftin)

    if( ncdiag_open_status(idx)%num_records > 0 ) then 
       allocate( iuse_flag( ncdiag_open_status(idx)%num_records ))
       call nc_diag_read_get_var( ftin, 'Analysis_Use_Flag', iuse_flag )
       analysis_use_flag = iuse_flag(1)
       deallocate( iuse_flag )
    else
       analysis_use_flag = -1
    end if 

    nlevs = SIZE( pobs )
    write(6,*) 'nlevs = ', nlevs

    header_fix%isis      = isis
    header_fix%id        = sat
    header_fix%obstype   = obstype
!    header_fix%jiter     = jiter
    header_fix%nlevs     = nlevs
    header_fix%ianldate  = idate
!    write(6,*) 'header_fix asignments complete'

    !--- allocate if necessary

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        deallocate( header_nlev )
      endif
      allocate( header_nlev( header_fix%nlevs ) )
      nlevs_last = header_fix%nlevs
    endif

    !--- read header (level part)
    
    do k=1,header_fix%nlevs
       header_nlev(k)%pob = pobs(k)
       header_nlev(k)%grs = gross(k)
       header_nlev(k)%err = tnoise(k)
       header_nlev(k)%iouse = analysis_use_flag

    end do
    deallocate( pobs,gross,tnoise )


    write(6,*) '<-- read_ozndiag_header_nc'
    write(6,*) ''; write(6,*) ''

  end subroutine read_ozndiag_header_nc



  !------------------------------------------------------------
  !  subroutine read_ozndiag_header_bin
  !------------------------------------------------------------
  subroutine read_ozndiag_header_bin( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus


    !--- variables
    
    integer,save :: nlevs_last = -1
    integer :: ilev,k,ioff0
    character(len=10):: id,obstype
    character(len=20):: isis
    integer(i_kind):: jiter,nlevs,ianldate,iint,ireal,iextra
    integer(i_kind),dimension(:),allocatable:: iouse
    real(r_single),dimension(:),allocatable:: pob,grs,err
    
    istatus = 0
    write(6,*) '--> read_ozndiag_header_bin'

    !--- read header (fix part)
    !--- the new header format contains one additional integer value 
    !
    if ( new_hdr ) then
       read(ftin) isis,id,obstype,jiter,nlevs,ianldate,iint,ireal,iextra,ioff0
    else
       read(ftin) isis,id,obstype,jiter,nlevs,ianldate,iint,ireal,iextra
    endif
    write(6,*) 'isis, nlevs = ', isis, nlevs

    header_fix%isis      = isis
    header_fix%id        = id
    header_fix%obstype   = obstype
    header_fix%jiter     = jiter
    header_fix%nlevs     = nlevs
    header_fix%ianldate  = ianldate
    header_fix%iint      = iint
    header_fix%ireal     = ireal
    header_fix%iextra    = iextra
    print *, 'header_fix has been assigned'   

    !--- check header
    
    if( header_fix%ireal  /= IREAL_RESERVE  ) then

      print *, '### ERROR: UNEXPECTED DATA RECORD FORMAT'
      print *, 'ireal  =', header_fix%ireal  
      stop 99

    endif

    if (header_fix%iextra /= 0) then
       write(6,*)'READ_DIAG_HEADER:  extra diagnostic information available, ',&
            'iextra=',header_fix%iextra
    endif

    !--- allocate if necessary

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        deallocate( header_nlev )
      endif
      allocate( header_nlev( header_fix%nlevs ) )
      nlevs_last = header_fix%nlevs
    endif

    !--- read header (level part)
    
    if(isis /= "ompslp_npp")then
      allocate (pob(header_fix%nlevs))
      allocate (grs(header_fix%nlevs))
      allocate (err(header_fix%nlevs))
      allocate (iouse(header_fix%nlevs))
      read(ftin)  pob,grs,err,iouse
      do k=1,header_fix%nlevs
         header_nlev(k)%pob = pob(k)
         header_nlev(k)%grs = grs(k)
         header_nlev(k)%err = err(k)
         header_nlev(k)%iouse = iouse(k)
      end do
      deallocate (pob,grs,err,iouse)
      print*,'header_nlev%pob=', header_nlev%pob
      print*,'header_nlev%grs=', header_nlev%grs
      print*,'header_nlev%err=', header_nlev%err
      print*,'header_nlev%iouse=', header_nlev%iouse
    else
      print*,'OMPSLP_NPP detected, but not handled'
    endif

    !--- read header (level part)

!    write(6,*) ' reading pob,grs,err,iouse' 
!    read(ftin)  pob,grs,err,iouse
!    write(6,*) ' pob,grs,err,iouse have been read' 
!    do k=1,header_fix%nlevs
!       header_nlev(k)%pob = pob(k)
!       header_nlev(k)%grs = grs(k)
!       header_nlev(k)%err = err(k)
!       header_nlev(k)%iouse = iouse(k)
!       write(6,*) 'header_nlev(k)%pob   = ', k, header_nlev(k)%pob
!       write(6,*) 'header_nlev(k)%grs   = ', k, header_nlev(k)%grs
!       write(6,*) 'header_nlev(k)%err   = ', k, header_nlev(k)%err
!       write(6,*) 'header_nlev(k)%iouse = ', k, header_nlev(k)%iouse
!    end do
!    deallocate (pob,grs,err,iouse)


    write(6,*) '<-- read_ozndiag_header_bin'
  end subroutine read_ozndiag_header_bin



  !------------------------------------------------------------
  !  subroutine read_ozndiag_data
  !
  !  Read a data record of the diagnostic file in either
  !  NetCDF or binary format.
  !------------------------------------------------------------

  subroutine read_ozndiag_data( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           iobs.  So they should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  The time.f90 could deallocate them at the
    !           very end of the program, I think.
    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)
    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
   
 
    write(6,*) '===> read_ozndiag_data'
    write(6,*) '  netcdf = ', netcdf

    if ( netcdf ) then
       call read_ozndiag_data_nc( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
    else
       call read_ozndiag_data_bin( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
    end if 


    print*, '<=== read_ozndiag_data'
  end subroutine read_ozndiag_data



  !------------------------------------------------  
  !  subroutine read_ozndiag_data_nc
  !------------------------------------------------  
  subroutine read_ozndiag_data_nc( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           iobs.  So they should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  The time.f90 could deallocate them at the
    !           very end of the program, I think.
    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)
    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
    integer(i_kind)                         :: id,ii,jj,cur_idx
    integer(i_kind),allocatable             :: Use_Flag(:)

    real(r_single),allocatable              :: lat(:)            ! latitude (deg)
    real(r_single),allocatable              :: lon(:)            ! longitude (deg)
    real(r_single),allocatable              :: obstime(:)        ! observation time relative to analysis

    real(r_single),allocatable              :: ozobs(:)          ! observation
    real(r_single),allocatable              :: ozone_inv(:)      ! obs-forecast adjusted
    real(r_single),allocatable              :: varinv(:)         ! inverse obs error
    real(r_single),allocatable              :: sza(:)            ! solar zenith angle   
    real(r_single),allocatable              :: fovn(:)           ! scan position (fielf of view)
    real(r_single),allocatable              :: toqf(:)           ! row anomaly index

    write(6,*) ''
    write(6,*) ''
    write(6,*) '============================================='
    write(6,*) '============================================='

    print*, '===> read_ozndiag_data_nc'

    cur_idx = ncdiag_open_id( nopen_ncdiag )

    !----------------------------------------------------------
    !  The binary file read (the original version of the file
    !  read) is designed to be called in a loop, as it reads
    !  each obs from the file.  
    !
    !  The newer netcdf read processes each field for all obs 
    !  so it can grab all the obs data in a single call.  The
    !  calling routine in time.f90 uses the iflag value to 
    !  process the results, and non-zero value to indicate 
    !  everything has been read.  In order to use this same
    !  iflag mechanism we'll use the ncdiag_open_status%nc_read
    !  field, setting it to true and iflag to 0 after reading,
    !  and if nc_read is already true then set iflag to -1.
    !
    !  It's not as clear or clean as it should be, so I'll 
    !  leave this comment in as a note-to-self to redesign this
    !  when able.
    
    if( ncdiag_open_status(cur_idx)%nc_read == .true. ) then 
       iflag = -1  
    else  
       iflag = 0
       ntobs = 0

       id = find_ncdiag_id(ftin)
       ntobs = ncdiag_open_status(id)%num_records

       !------------------------------------
       !  allocate the returned structures
       !
       allocate( data_fix( ntobs ) )
       allocate( data_nlev( header_fix%nlevs,ntobs ) )

      
       !---------------------------------
       ! load data_fix structure
       !
       allocate( lat(ntobs) ) 
       allocate( lon(ntobs) )
       allocate( obstime(ntobs) ) 
       call nc_diag_read_get_var( ftin, 'Latitude', lat )
       call nc_diag_read_get_var( ftin, 'Longitude', lon )
       call nc_diag_read_get_var( ftin, 'Time', obstime )

       do ii=1,ntobs
          data_fix(ii)%lat     = lat(ii)
          print*, 'data_fix(ii)%lat = ', ii, data_fix(ii)%lat
          data_fix(ii)%lon     = lon(ii)
          print*, 'data_fix(ii)%lon = ', ii, data_fix(ii)%lon
          data_fix(ii)%obstime = obstime(ii)
          print*, 'data_fix(ii)%obstime = ', ii, data_fix(ii)%obstime
       end do
 
       deallocate( lat, lon, obstime )


       !---------------------------------
       ! load data_nlev structure
       !
       allocate( data_nlev( header_fix%nlevs,ntobs ) )

       allocate( ozobs(ntobs) ) 
       call nc_diag_read_get_var( ftin, 'Observation', ozobs )

       allocate( ozone_inv(ntobs) ) 
       call nc_diag_read_get_var( ftin, 'Obs_Minus_Forecast_adjusted', ozone_inv )

       allocate( varinv(ntobs) ) 
       call nc_diag_read_get_var( ftin, 'Inverse_Observation_Error', varinv )

       allocate( sza(ntobs) ) 
       call nc_diag_read_get_var( ftin, 'Solar_Zenith_Angle', sza )

       allocate( fovn(ntobs) )
       call nc_diag_read_get_var( ftin, 'Scan_Position', fovn )

       allocate( toqf(ntobs) )
       call nc_diag_read_get_var( ftin, 'Row_Anomaly_Index', toqf )

       do jj=1,ntobs
          do ii=1,header_fix%nlevs
             data_nlev(ii,jj)%ozobs     =     ozobs( jj )
             write(6,*) 'data_nlev(ii,jj)%ozobs     = ',ii,jj,data_nlev(ii,jj)%ozobs
             data_nlev(ii,jj)%ozone_inv = ozone_inv( jj )
             write(6,*) 'data_nlev(ii,jj)%ozone_inv     = ',ii,jj,data_nlev(ii,jj)%ozone_inv
             data_nlev(ii,jj)%varinv    =    varinv( jj )
             write(6,*) 'data_nlev(ii,jj)%varinv     = ',ii,jj,data_nlev(ii,jj)%varinv
             data_nlev(ii,jj)%sza       =       sza( jj )
             write(6,*) 'data_nlev(ii,jj)%sza     = ',ii,jj,data_nlev(ii,jj)%sza
             data_nlev(ii,jj)%fovn      =      fovn( jj )
             write(6,*) 'data_nlev(ii,jj)%fovn     = ',ii,jj,data_nlev(ii,jj)%fovn
             data_nlev(ii,jj)%toqf      =      toqf( jj )
             write(6,*) 'data_nlev(ii,jj)%toqf     = ',ii,jj,data_nlev(ii,jj)%toqf
          end do
       end do

       deallocate( ozobs )
       deallocate( ozone_inv )
       deallocate( varinv )
       deallocate( sza )
       deallocate( fovn )
       deallocate( toqf )

       ncdiag_open_status(cur_idx)%nc_read = .true.

       !!------------------------------------------------
       !!   Contents of NetCDF file:
       !!
       !!        float Reference_Pressure(nobs) ;
       !!        int Analysis_Use_Flag(nobs) ;
       !!        float Observation(nobs) ;
       !!        float Inverse_Observation_Error(nobs) ;
       !!        float Obs_Minus_Forecast_adjusted(nobs) ;
       !!        float Obs_Minus_Forecast_unadjusted(nobs) ;
       !!        float Solar_Zenith_Angle(nobs) ;
       !!        float Scan_Position(nobs) ;
       !!        float Row_Anomaly_Index(nobs) ;


    end if

    write(6,*) '' ; 
    print*, '<=== read_ozndiag_data_nc'
    write(6,*) ''; write(6,*) ''

  end subroutine read_ozndiag_data_nc



  !------------------------------------------------  
  !  subroutine read_ozndiag_data_bin
  !------------------------------------------------  
  subroutine read_ozndiag_data_bin( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           ntobs.  So they should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  The time.f90 could deallocate them at the
    !           very end of the program, I think.

    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)

    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
    integer(i_kind)            ,pointer     :: data_mpi(:)
    
    !--- variables
    integer,save :: nlevs_last = -1
    integer,save :: iextra_last = -1
    integer :: iev,iobs,i,j
    real(r_single),allocatable,dimension(:,:)  :: tmp_fix
    real(r_single),allocatable,dimension(:,:,:):: tmp_nlev
    real(r_single),allocatable,dimension(:,:)  :: tmp_extra

    !--- allocate if necessary
    print*, '===> read_ozndiag_data_bin'
    print*, 'nlevs_last, header_fix%nlevs=',nlevs_last,header_fix%nlevs

    read(ftin,IOSTAT=iflag) ntobs
    print*,'ntobs, iflag =',ntobs, iflag

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        print*, 'deallocate array data_nlev, data_mpi and data_fix'
        deallocate( data_nlev )
        deallocate( data_fix )
        deallocate( data_mpi )
      endif

      allocate( data_fix( ntobs ) )
      allocate( data_mpi( ntobs ) )
      allocate( data_nlev( header_fix%nlevs,ntobs ) )
      nlevs_last = header_fix%nlevs
    endif

    if (iextra_last > 0) then
       deallocate (data_extra)
       print*, 'deallocated data_extra'
    endif

    allocate( data_extra(header_fix%iextra,ntobs) )
    print*, 'allocated data_extra, iextra, ntobs', header_fix%iextra, ntobs
    iextra_last = header_fix%iextra

    !--- read a record

    allocate( tmp_fix(3,ntobs))
    allocate( tmp_nlev(10,header_fix%nlevs,ntobs))

    if (header_fix%iextra == 0) then
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev
    else
       allocate(  tmp_extra(header_fix%iextra,ntobs) )
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev, tmp_extra

       do j=1,ntobs
          do i=1,header_fix%iextra
             data_extra(i,j)%extra=tmp_extra(i,j)
          end do
       end do

       deallocate(tmp_extra)
    endif

    do j=1,ntobs
       data_fix(j)%lat     = tmp_fix(1,j)
       data_fix(j)%lon     = tmp_fix(2,j)
       data_fix(j)%obstime = tmp_fix(3,j)
    end do
    deallocate(tmp_fix)

    do j=1,ntobs
       do i=1,header_fix%nlevs
          data_nlev(i,j)%ozobs     = tmp_nlev(1,i,j)
          data_nlev(i,j)%ozone_inv = tmp_nlev(2,i,j)
          data_nlev(i,j)%varinv    = tmp_nlev(3,i,j)
          data_nlev(i,j)%sza       = tmp_nlev(4,i,j)
          data_nlev(i,j)%fovn      = tmp_nlev(5,i,j)
          data_nlev(i,j)%toqf      = tmp_nlev(6,i,j)
       end do
    end do
    deallocate(tmp_nlev)

    nlevs_last = -1

    print*, '<=== read_ozndiag_data_bin'
  end subroutine read_ozndiag_data_bin


  !------------------------------------------------  
  !  function find_ncdiag_id
  !------------------------------------------------  
  integer( i_kind ) function find_ncdiag_id( ftin )

     integer(i_kind), intent(in) :: ftin

     integer(i_kind) :: i

     find_ncdiag_id = -1
     do i = 1, MAX_OPEN_NCDIAG
        if ( ncdiag_open_id(i) == ftin ) then
           find_ncdiag_id = i
           return
        endif
     enddo

     return
  end function find_ncdiag_id


end module read_diag

