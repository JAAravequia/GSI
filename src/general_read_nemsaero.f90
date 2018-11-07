subroutine general_read_nemsaero(grd,sp_a,filename,mype,gfschem_bundle, &
       naero,aeroname,init_head,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_nemsaero   adaptation of general_read_gfsatm
!                                        for reading in aerosols from NEMSI/O
!
! abstract: copied from general_read_gfsatm, primarily for reading in aerosol
!           tracer variables from NEMS GFS I/O files 
!
! program history log:
!   2018-10-31  Wei/Martin - modifications to read in aerosol arrays
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info,
!                    located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for
!     analysis
!                    (initialized by general_init_spec_vars, located in
!                    general_specmod.f90)
!     filename - input sigma file name
!     mype     - mpi task id
!     naero    - number of aerosol tracers to read
!     aeroname - len(naero) character strings of aerosol tracers to read
!     init_head- flag to read header record.  Usually .true. unless
!     repeatedly
!                reading similar files (ensembles)
!
!   input/output list:
!     gfschem_bundle - GSI bundle containing chem/aerosol arrays
!
!   output argument list:
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,r_single,i_kind
    use gridmod, only: ntracer,ncloud,itotsub,jcap_b
    use general_commvars_mod, only: fill_ns,fill2_ns
    use general_sub2grid_mod, only: sub2grid_info
    use general_specmod, only: spec_vars
    use mpimod, only: npe
    use constants, only: zero,one,fv,r0_01
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
    use ncepnems_io, only: error_msg
    use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    use egrid2agrid_mod, only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad,r60,r3600
    use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer

    implicit none
    
!   Declare local parameters
    real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    type(spec_vars)                       ,intent(in   ) :: sp_a
    character(*)                          ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    integer(i_kind)                       ,intent(in   ) :: naero
    character(*),dimension(naero)         ,intent(in   ) :: aeroname
    logical                               ,intent(in   ) :: init_head
    integer(i_kind)                       ,intent(  out) :: iret_read
    type(gsi_bundle)                      ,intent(inout) :: gfschem_bundle
    
!   Declare local variables
    character(len=120) :: my_name = 'general_read_nemsaero'
    character(len=1)   :: null = ' '
    character(len=20),dimension(npe) :: ch_aero
    integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
    integer(i_kind):: i,j,k,l,icount,kk,istatus,ier
    integer(i_kind) :: latb, lonb, levs, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 101
    integer(i_kind),dimension(npe)::ilev,iflag,mype_use
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate
    real(r_kind) :: fhour
    real(r_kind),dimension(:,:,:),pointer :: &
               ae_d1,ae_d2,ae_d3,ae_d4,ae_d5,&
               ae_s1,ae_s2,ae_s3,ae_s4,ae_so4,&
               ae_ocpho,ae_ocphi,ae_bcpho,ae_bcphi

    real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
    real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
         grid_vor, grid_div, grid_b, grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
    real(r_kind),allocatable,dimension(:)   :: work, work_v
    real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1, rwork1d2
    real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
    real(4),allocatable,dimension(:) :: r4lats,r4lons

    logical :: procuse,diff_res,eqspace
    type(nemsio_gfile) :: gfile
    type(egrid2agrid_parm) :: p_high
    logical,dimension(1) :: vector

!******************************************************************************  
    if(mype==0) write(6,*) trim(my_name)," start and filename is ",trim(filename)

!   Initialize variables used below
    iret_read=0
    iret=0
    nlatm2=grd%nlat-2
    iflag = 0
    ilev = 0

    nlevs=grd%nsig
    mype_use=-1
    icount=0
    procuse=.false.
    if(mype == 0)procuse = .true.
    do i=1,npe
       if(grd%recvcounts_s(i-1) > 0)then
         icount = icount+1
         mype_use(icount)=i-1
         if(i-1 == mype) procuse=.true.
       end if
    end do
    icm=icount
    allocate( work(grd%itotsub) )
    work=zero
    if(procuse)then

      if (mype==0) write(6,*) "before nemsio init"
       
      if (init_head) call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'getfilehead',istop+1,iret)

      if( nframe /= 0 ) then
         if ( mype == 0 ) &
         write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      end if

      fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

!      write(6,*) "iret= ",iret
      if ( iret == 0 .and. mype == 0 ) then
         write(6,'(''NGAC file time='',i4.4,i2.2,i2.2,i2.2)') odate(4),odate(2),odate(3),odate(1)
      end if
!
!  g_* array already pre-allocate as (lat2,lon2,<nsig>) => 2D and <3D>
!  array
!
      diff_res=.false.
      if(latb /= nlatm2) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
  !      call stop2(101)
      end if
      if(lonb /= grd%nlon) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
  !      call stop2(101)
      end if
      if(levs /= grd%nsig)then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      end if

      if (mype==0) write(6,*) "after check the resolution"
!
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if(diff_res)then
         allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
      end if
      allocate( rwork1d0(latb*lonb) )
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb),rwork1d2(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
        rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*r4lons(j)
      end do
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      end do

      nord_int=4
      eqspace=.false.

    if (mype==0) write(6,*) "before create egrid2agrid"

      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons,&
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

    end if

    if (mype==0) write(6,*) "after create egrid2agrid"

    istatus=0
    do l=1,naero
       select case(trim(aeroname(l)))
       case ('sulf')
           call gsi_bundlegetpointer(gfschem_bundle,'sulf' ,ae_so4  ,ier); istatus=istatus+ier
       case ('oc1')
           call gsi_bundlegetpointer(gfschem_bundle,'oc1'  ,ae_ocpho,ier); istatus=istatus+ier
       case ('oc2')
           call gsi_bundlegetpointer(gfschem_bundle,'oc2'  ,ae_ocphi,ier); istatus=istatus+ier
       case ('bc1')
           call gsi_bundlegetpointer(gfschem_bundle,'bc1'  ,ae_bcpho,ier); istatus=istatus+ier
       case ('bc2')
           call gsi_bundlegetpointer(gfschem_bundle,'bc2'  ,ae_bcphi,ier); istatus=istatus+ier
       case ('dust1')
           call gsi_bundlegetpointer(gfschem_bundle,'dust1',ae_d1   ,ier); istatus=istatus+ier
       case ('dust2')
           call gsi_bundlegetpointer(gfschem_bundle,'dust2',ae_d2   ,ier); istatus=istatus+ier
       case ('dust3')
           call gsi_bundlegetpointer(gfschem_bundle,'dust3',ae_d3   ,ier); istatus=istatus+ier
       case ('dust4')
           call gsi_bundlegetpointer(gfschem_bundle,'dust4',ae_d4   ,ier); istatus=istatus+ier
       case ('dust5')
           call gsi_bundlegetpointer(gfschem_bundle,'dust5',ae_d5   ,ier); istatus=istatus+ier
       case ('seas1')
           call gsi_bundlegetpointer(gfschem_bundle,'seas1',ae_s1   ,ier); istatus=istatus+ier
       case ('seas2')
           call gsi_bundlegetpointer(gfschem_bundle,'seas2',ae_s2   ,ier); istatus=istatus+ier
       case ('seas3')
           call gsi_bundlegetpointer(gfschem_bundle,'seas3',ae_s3   ,ier); istatus=istatus+ier
       case ('seas4')
           call gsi_bundlegetpointer(gfschem_bundle,'seas4',ae_s4   ,ier); istatus=istatus+ier
       end select
    end do
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'general_read_nemsaero: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif

    icount=0
!   Process guess fields according to type of input file.   NCEP_SIGIO
!   files
!   are spectral coefficient files and need to be transformed to the
!   grid.
!   Once on the grid, fields need to be scattered from the full domain
!   to 
!   sub-domains.
    do l=1,naero
    do k=1,nlevs
       icount=icount+1
!       iflag(icount)=l
       ilev(icount)=k
       ch_aero(icount)=trim(aeroname(l))
       vector(1)=.false.
       if (mype==mype_use(icount)) then
         
          select case ( trim(aeroname(l)) )
           case ('sulf')
          call nemsio_readrecv(gfile,'so4','mid layer',k,rwork1d0,iret=iret)
           case ('bc1')
          call nemsio_readrecv(gfile,'bcphobic','mid layer',k,rwork1d0,iret=iret)
           case ('bc2')
          call nemsio_readrecv(gfile,'bcphilic','mid layer',k,rwork1d0,iret=iret)
           case ('oc1')
          call nemsio_readrecv(gfile,'ocphobic','mid layer',k,rwork1d0,iret=iret)
           case ('oc2')
          call nemsio_readrecv(gfile,'ocphilic','mid layer',k,rwork1d0,iret=iret)
           case ('dust1')
          call nemsio_readrecv(gfile,'du001','mid layer',k,rwork1d0,iret=iret)
!               rwork1d0=rwork1d0*half
           case ('dust2')
          call nemsio_readrecv(gfile,'du002','mid layer',k,rwork1d0,iret=iret)
!               rwork1d0=rwork1d0*half
           case ('dust3')
          call nemsio_readrecv(gfile,'du003','mid layer',k,rwork1d0,iret=iret)
!               rwork1d0=rwork1d0*half
           case ('dust4')
          call nemsio_readrecv(gfile,'du004','mid layer',k,rwork1d0,iret=iret)
!               rwork1d0=rwork1d0*half
           case ('dust5')
          call nemsio_readrecv(gfile,'du005','mid layer',k,rwork1d0,iret=iret)
!               rwork1d0=rwork1d0*half
           case ('seas1')
          call nemsio_readrecv(gfile,'ss001','mid layer',k,rwork1d1,iret=iret)
          call nemsio_readrecv(gfile,'ss002','mid layer',k,rwork1d2,iret=iret)
           rwork1d0=rwork1d1+rwork1d2
           case ('seas2')
          call nemsio_readrecv(gfile,'ss003','mid layer',k,rwork1d0,iret=iret)
           case ('seas3')
          call nemsio_readrecv(gfile,'ss004','mid layer',k,rwork1d0,iret=iret)
           case ('seas4')
          call nemsio_readrecv(gfile,'ss005','mid layer',k,rwork1d0,iret=iret)
          end select

! Convert NGAC mixing ratio unit from kg/kg( 10^3 g/kg ) to ug/kg( 10^-6 g/kg )
! CRM are we sure we want to do this here?
          rwork1d0=rwork1d0*1.0e+9_r_kind

!          rwork1d0=zero

          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop+7,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call general_fill_ns(grd,grid,work)
          end if

       end if
       if(icount == icm)then
!          if(mype==0) write(6,*) "Before reload aerosol data, icount=",icount
          call aerosol_reload(grd,ae_d1,ae_d2,ae_d3,ae_d4,ae_d5, &
                 ae_s1,ae_s2,ae_s3,ae_s4,ae_so4,&
                 ae_ocpho,ae_ocphi,ae_bcpho,ae_bcphi, &
                 icount,ilev,ch_aero,work)
       end if
    end do
    end do

    if(procuse)then
!       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
       if(diff_res) deallocate(grid_b,grid_c,grid2)
       call destroy_egrid2agrid(p_high)
       deallocate(rwork1d1,clons,slons)
       deallocate(rwork1d0)
       deallocate(grid)!,grid_v)
       call nemsio_close(gfile,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop+9,iret)
    end if
    deallocate(work)


!   Print date/time stamp 
    if(mype==0) then
       write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate
700    format('READ_NGAC_AEROSOL:  ges read/scatter, lonb,latb,levs=',&
            3i6,', nlon,nlat=',2i6,', hour=',f10.1,', idate=',4i5)
    end if

    return


!   ERROR detected while reading file
1000 continue
     write(6,*)'GENERAL_READ_GFSATM:  ***ERROR*** reading ',&
         trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
     return

!   End of routine.  Return

    return
end subroutine general_read_nemsaero
!
subroutine aerosol_reload(grd,ae_d1,ae_d2,ae_d3,ae_d4,ae_d5, &
           ae_s1,ae_s2,ae_s3,ae_s4,ae_so4, &
           ae_ocpho,ae_ocphi,ae_bcpho,ae_bcphi, &
           icount,ilev,chaero,work)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  integer(i_kind),intent(inout) ::icount
  integer(i_kind),dimension(npe),intent(inout):: ilev!,iflag
  real(r_kind),dimension(grd%itotsub),intent(in) :: work
  character(*),dimension(npe) , intent(in) :: chaero

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: &
           ae_d1,ae_d2,ae_d3,ae_d4,ae_d5, &
           ae_s1,ae_s2,ae_s3,ae_s4,ae_so4, &
           ae_ocpho,ae_ocphi,ae_bcpho,ae_bcphi


! !DESCRIPTION: Transfer contents of 2-d array to 3-d array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) i,j,k,ij,klev
  real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

  call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
       sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
       mpi_comm_world,ierror)
!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
  do k=1,icount
     klev=ilev(k)
     ij=0
     select case ( chaero(k) )
      case ('sulf')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_so4(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('bc1')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_bcpho(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('bc2')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_bcphi(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('oc1')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_ocpho(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('oc2')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_ocphi(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('dust1')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_d1(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('dust2')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_d2(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('dust3')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_d3(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('dust4')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_d4(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('dust5')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_d5(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('seas1')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_s1(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('seas2')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_s2(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('seas3')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_s3(i,j,klev)=sub(ij,k)
           end do
        end do
      case ('seas4')
        do j=1,grd%lon2
           do i=1,grd%lat2
              ij=ij+1
              ae_s4(i,j,klev)=sub(ij,k)
           end do
        end do
     end select
  end do
  icount=0
  ilev=0
  return
end subroutine aerosol_reload

