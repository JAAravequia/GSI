module gridinfo
!$$$  module documentation block
!
! module: gridinfo                     read horizontal (lons, lats) and
!                                      vertical (pressure) information from
!                                      ensemble mean first guess file.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module reads the ensemble mean background file and
! extracts information about the analysis grid, including the
! longitudes and latitudes of the analysis grid points and
! the pressure on each grid point/vertical level.
!
! Public Subroutines:
!   getgridinfo: read latitudes, longitudes, pressures and orography for analysis grid,
!    broadcast to each task. Compute spherical cartesian coordinate values
!    for each analysis horizontal grid point.
!   gridinfo_cleanup: deallocate allocated module variables.
!
! Public Variables:
!   npts: number of analysis grid points in the horizontal (from module params).
!   nlevs: number of analysis vertical levels (from module params).
!    specific humidity, ozone and cloud condensate).
!   ptop: (real scalar) pressure (hPa) at top model layer interface.
!   lonsgrd(npts): real array of analysis grid longitudes (radians).
!   latsgrd(npts): real array of analysis grid latitudes (radians).
!   logp(npts,ndim):  -log(press) for all 2d analysis grids. Assumed invariant
!   in assimilation window, computed fro ensemble mean at middle of window.
!   gridloc(3,npts): spherical cartesian coordinates (x,y,z) for analysis grid.
!
! Modules Used: mpisetup, params, kinds
!
! program history log:
!   2009-02-23  Initial version.
!   2016-05-02: shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP & DPDT removed)
!
! attributes:
!   language: f95
!
!$$$

use mpisetup, only: nproc, mpi_integer, mpi_real4, mpi_comm_world
USE params, ONLY: datapath,datestring,nlevs,use_gfs_nemsio,fgfileprefixes, &
                  fv3fixpath, nx_res, ny_res, ntiles
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,max_varname_length
use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
use netcdf, only: nf90_inq_dimid,nf90_inq_varid
use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
use netcdf_mod, only: nc_check

implicit none
private
public :: getgridinfo, gridinfo_cleanup
integer(i_kind),public :: nlevs_pres
REAL(r_single),ALLOCATABLE, DIMENSION(:,:), SAVE, PUBLIC :: ak,bk
REAL(r_single), SAVE, PUBLIC :: ptop
REAL(r_single),PUBLIC, ALLOCATABLE, DIMENSION(:) :: lonsgrd, latsgrd
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer,public :: npts
integer,public :: ntrunc
! supported variable names in anavinfo
CHARACTER(len=max_varname_length),PUBLIC, DIMENSION(10), PARAMETER :: vars3d_supported_met = (/'u   ', 'v   ', 'tv  ', 'q   ', 'oz  ', 'cw  ', 'tsen', 'prse', 'ql  ', 'qi  '/)
CHARACTER(len=max_varname_length),PUBLIC, DIMENSION(4), PARAMETER  :: vars2d_supported = (/'ps ', 'pst', 'sst', 'aod' /)

!can include gases such as e.g. so2 at some point
INTEGER, PARAMETER, PUBLIC :: ntracers_gocart=14
CHARACTER(len=max_varname_length), DIMENSION(ntracers_gocart), PARAMETER, PUBLIC :: &
     &vars3d_supported_aero = (/&
     &'sulf','bc1','bc2','oc1','oc2',&
     &'dust1','dust2','dust3','dust4','dust5',&
     &'seas1','seas2','seas3','seas4'/)

CHARACTER(len=max_varname_length),DIMENSION(10+ntracers_gocart), PARAMETER, PUBLIC :: &
     &vars3d_supported=(/vars3d_supported_met,vars3d_supported_aero/)


! supported variable names in anavinfo
contains

SUBROUTINE getgridinfo(fileprefix, reducedgrid)
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
USE read_fv3_restarts, ONLY: read_fv3_restart_data2d,read_fv3_restart_data3d,read_fv3_restart_data4d
implicit none

character(len=120), intent(in) :: fileprefix
LOGICAL, intent(in)            :: reducedgrid


INTEGER(i_kind) ierr, k, nn,  ntile
character(len=500) filename
integer(i_kind) i,j
real(r_kind), allocatable, dimension(:) :: spressmn
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn
real(r_kind) kap,kapr,kap1

integer(i_kind) file_id,dim_id,nlevsp1
real(r_single), allocatable, dimension(:,:) :: lat_tile,lon_tile,ps
REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:,:) :: delp
character(len=4) char_res
character(len=1) char_tile
character(len=24) :: myname_='gridinfo_fv3 ' 

character(len=12) :: varstrname

IF (reducedgrid) THEN
   PRINT *,'reducedgrid for fv3_native not implemented'
   call stop2(717)
ENDIF

nlevsp1 = nlevs + 1
nlevs_pres = nlevs
npts = ntiles*nx_res*ny_res
kap = rd/cp
kapr = cp/rd
kap1 = kap + one

ALLOCATE(ak(nlevsp1,1),bk(nlevsp1,1))
ALLOCATE(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
ALLOCATE(gridloc(3,npts))
ALLOCATE(latsgrd(npts),lonsgrd(npts))

! read data on root task
if (nproc .eq. 0) then

   !  read ak,bk from ensmean fv_core.res.nc
   filename = TRIM(datapath)//TRIM(datestring)//"/ensmean/"//&
        &TRIM(fileprefix)//".fv_core.res.nc"
   call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
   myname_,'open: '//trim(adjustl(filename)) )
   call nc_check( nf90_inq_dimid(file_id,'xaxis_1',dim_id),&
       myname_,'inq_dimid xaxis_1 '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nlevsp1),&
       myname_,'inquire_dimension xaxis_1 '//trim(filename) )

   call read_fv3_restart_data2d('ak',filename,file_id,ak)
   call read_fv3_restart_data2d('bk',filename,file_id,bk)

   ptop = ak(1,1)

   !  read lats/lons from C###_oro_data.tile#.nc 
   ! (this requires path to FV3 fix dir)
   write(char_res, '(i4)') nx_res
   allocate(lat_tile(nx_res,ny_res),lon_tile(nx_res,ny_res))
   nn = 0
   DO ntile=1,ntiles
      write(char_tile, '(i1)') ntile
      filename=trim(adjustl(fv3fixpath))//'/C'//trim(adjustl(char_res))//'/C'//trim(adjustl(char_res))//'_oro_data.tile'//char_tile//'.nc'
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )
      call read_fv3_restart_data2d('geolon',filename,file_id,lon_tile)
      call read_fv3_restart_data2d('geolat',filename,file_id,lat_tile)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      do j=1,ny_res
         do i=1,nx_res
            nn = nn + 1
            latsgrd(nn) = lat_tile(i,j)
            lonsgrd(nn) = lon_tile(i,j)
         enddo
      enddo
   enddo

   latsgrd = pi*latsgrd/180._r_single
   lonsgrd = pi*lonsgrd/180._r_single

!cltthink the unit of the lat/lon
   ALLOCATE(delp(nx_res,ny_res,nlevs,1),ps(nx_res,ny_res))
   allocate(pressimn(npts,nlevsp1),presslmn(npts,nlevs))
   allocate(spressmn(npts))
   nn = 0

   varstrname='delp'
   DO ntile=1,ntiles
      write(char_tile, '(i1)') ntile
      filename = TRIM(datapath)//TRIM(datestring)//"/ensmean/"//&
           &TRIM(fileprefix)//".fv_core.res.tile"//char_tile//".nc"
!      print *,trim(adjustl(filename))
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )


      call read_fv3_restart_data4d(varstrname,filename,file_id,delp)
!      PRINT *,'min/max delp',ntile,MINVAL(delp),MAXVAL(delp)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      ps = SUM(delp(:,:,:,1),3) + ptop
      do j=1,ny_res
         do i=1,nx_res
            nn = nn + 1
            spressmn(nn) = ps(i,j)
         enddo
      enddo
   ENDDO

   ! pressure at interfaces
   do k=1,nlevsp1
      pressimn(:,k) = ak(nlevsp1-k+1,1)+bk(nlevsp1-k+1,1)*spressmn(:)
   enddo

   DEALLOCATE(delp,ps)
   do k=1,nlevs
     ! layer pressure from Phillips vertical interpolation.
     presslmn(:,k) = ((pressimn(:,k)**kap1-pressimn(:,k+1)**kap1)/&
                      (kap1*(pressimn(:,k)-pressimn(:,k+1))))**kapr
   end do
   print *,'ensemble mean first guess surface pressure:'
   print *,minval(spressmn),maxval(spressmn)
   ! logp holds log(pressure) or pseudo-height on grid, for each level/variable.

   do k=1,nlevs
      ! all variables to be updated are on mid-layers, not layer interfaces.
      logp(:,k) = -log(presslmn(:,k))
!      PRINT *,'min/max presslmn',k,MINVAL(presslmn(:,k)),MAXVAL(presslmn(:,k)),MINVAL(logp(:,k)),MAXVAL(logp(:,k))
   end do
   deallocate(spressmn,presslmn,pressimn)

endif ! root task

!call mpi_bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)

do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ak,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(bk,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
  
!==> precompute cartesian coords of analysis grid points.
do nn=1,npts
   gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
   gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
   gridloc(3,nn) = SIN(latsgrd(nn))
end do

end subroutine getgridinfo

subroutine gridinfo_cleanup()
if (allocated(lonsgrd)) deallocate(lonsgrd)
if (allocated(latsgrd)) deallocate(latsgrd)
if (allocated(logp)) deallocate(logp)
if (allocated(gridloc)) deallocate(gridloc)
end subroutine gridinfo_cleanup

end module gridinfo
