
subroutine read_viirsLST(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
     prsl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_viirsLST                    read NETCDF satellite retrieved Land
! Surface Temperature
!   prgmmr: zhang, xiaoyan      copied from read_satwnd                date: !   2018-09-16
!
! abstract:  This routine reads satellite retrieved Land Surface
! Temperature(LST) from VIIRS.
!            it also has options to thin the data by using conventional
!            thinning programs

! program history log:
!   2018-09-16 zhang, x.
!
!   input argument list:
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of VIIRS LST read
!     ndata    - number of VIIRS LST retained for further processing
!     nodata   - number of VIIRS LST retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind,r_single
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional,wrf_nmm_regional
  use qcmod, only: errormod,njqc
  use convthin, only: make3grids,map3grids,map3grids_m,del3grids,use_all
  use convthin_time, only: make3grids_tm,map3grids_tm,map3grids_m_tm,del3grids_tm,use_all_tm
  use constants, only: deg2rad,zero,rad2deg,one_tenth,&
        tiny_r_kind,huge_r_kind,r60inv,one_tenth,&
        one,two,three,four,five,half,quarter,r60inv,r100,r2000
  use converr,only: etabl
  use converr_uv,only: etabl_uv,isuble_uv,maxsub_uv
  use convb_uv,only: btabl_uv
  use obsmod, only: perturb_obs,perturb_fact,ran01dom,bmiss
  use convinfo, only: nconvtype, &
       icuse,ictype,icsubtype,ioctype, &
       ithin_conv,rmesh_conv,pmesh_conv,pmot_conv,ptime_conv, &
       use_prepb_satwnd

  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use mpimod, only: npe
!x LST
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use netcdf
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs


  real(r_kind)                          ,intent(in   ) :: twind
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full

! Declare local parameters

  real(r_kind),parameter:: r1_2= 1.2_r_kind
  real(r_kind),parameter:: r3_33= 3.33_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r50= 50.0_r_kind
  real(r_kind),parameter:: r70= 70.0_r_kind
  real(r_kind),parameter:: r90= 90.0_r_kind
  real(r_kind),parameter:: r105= 105.0_r_kind
  real(r_kind),parameter:: r110= 110.0_r_kind
  real(r_kind),parameter:: r125=125.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r250=250.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r600=600.0_r_kind
  real(r_kind),parameter:: r700=700.0_r_kind
  real(r_kind),parameter:: r850=850.0_r_kind
  real(r_kind),parameter:: r199=199.0_r_kind
  real(r_kind),parameter:: r299=299.0_r_kind
  real(r_kind),parameter:: r799=799.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r10000= 10000.0_r_kind
  logical outside,inflate_error
  logical luse,ithinp

!x LST
  ! Declare local variables
  logical iuse
  real(r_kind) pred
  integer(i_kind) n

  integer(i_kind) i,maxobs,idomsfc,nsattype,ncount
  integer(i_kind) nc,isflg,itx,j,nchanl
  integer(i_kind) ntb,ntmatch,ncx,ncsave,ntread
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nmind,lunin,idate,ilat,ilon,iret,k
  integer(i_kind) nreal,ithin,iout,ntmp,icount,iiout,ii
  integer(i_kind) nobs_lst
  integer(i_kind) itype,iosub,ixsub,isubsub,iobsub,itypey,ierr
  integer(i_kind) qm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) pflag
  integer(i_kind) ntest,nvtest
  integer(i_kind) kl,k1,k2
  integer(i_kind) nmsg                ! message index

  integer(i_kind),dimension(nconvtype) :: ntxall
  integer(i_kind),dimension(nconvtype+1) :: ntx

  integer(i_kind),dimension(5):: idate5
  integer(i_kind),allocatable,dimension(:):: nrep,isort,iloc
  integer(i_kind),allocatable,dimension(:,:):: tab


  integer(i_kind) ntime,itime
  integer(i_kind) itt


  real(r_kind) toff,t4dv
  real(r_kind) rmesh,ediff,usage,tdiff
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb,ppb2,qifn,qify,ee,ree
  real(r_kind) toe,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00,uob1,vob1
  real(r_kind) del,werrmin,obserr,ppb1,var_jb,wjbmin,wjbmax
  real(r_kind) tsavg,ff10,sfcr,sstime,gstime,zz
  real(r_kind) crit1,dist1,timedif,xmesh,pmesh,pmot,ptime
  real(r_kind),dimension(nsig):: presl

  real(r_double),dimension(5):: hdrdat
  real(r_double),dimension(6):: obsdat
  real(r_double),dimension(3,5) :: heightdat
  real(r_double),dimension(6,4) :: derdwdat
  real(r_double),dimension(3,12) :: qcdat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:):: rusage
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out
  real(r_kind),allocatable,dimension(:):: lst_rec

  data ithin / -9 /
  data lunin / 11 /
  data rmesh / -99.999_r_kind /

  !integer, parameter :: NX = 3200, NY = 768 !dimension of 1 granule VIIRS LST
  integer  :: NX, NY, NT 
  integer, allocatable, dimension(:,:,:) :: data_in,LST,QI_Flag,LON,LAT

  ! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid,rcode,ndims,nvars,natts,recdim,dimid
  integer :: numDims, numAtts,titleLen
  character (len = 80) :: time,year,month,day,hour,minute
  character (len = 80) :: satid,satname,intname
  real, parameter :: scale_factor_1=0.0025455155
  real, parameter :: scale_factor_2=183.2
  integer,parameter :: lst_missing_value=65528


  ! Loop indexes, and error handling.
  !integer :: x, y

!**************************************************************************
  
   write(6,*)'start to reading VIIRS LST in NETCDF format'
   nread=0
   ndata=0
   nodata=0

   nreal=26  !satellite zenith angle added
   nc=7 !x viirs LST is set to the 7th in convinfo

   ithin=ithin_conv(nc)
   if (ithin > 0 )then
     rmesh=rmesh_conv(nc)
   end if
! Make thinning grids
   call makegrids(rmesh,ithin)

! READ LST observation


  !#1 Open the file. NF90_NOWRITE tells netCDF we want read-only access to
  ! the file.
  print *, 'VIIRS LST NETCDF infile is ', infile
  call check( nf90_open(infile, NF90_NOWRITE, ncid) )
  print *, 'ncid =', ncid

  !#2 Get the number of dimensions, varaibles, attributes
  call check ( nf90_inquire(ncid,ndims,nvars,natts,recdim,rcode))
  print *, 'ndims = ', ndims
  print *, 'nvars = ', nvars

  !#3 Check the dimension of variables
  call check (nf90_inquire_dimension(ncid, 1, len=NX))
  call check (nf90_inquire_dimension(ncid, 2, len=NY))
  if (ndims >= 3) then
     call check (nf90_inquire_dimension(ncid, 3, len=NT))
  else
     NT=1
  end if
  print*, 'the dimension of VLST is ',NX,NY,NT 
  nobs_lst=NX*NY*NT

  allocate (data_in(NX,NY,NT))
  allocate (LST(NX,NY,NT))
  allocate (QI_Flag(NX,NY,NT))
  allocate (LON(NX,NY,NT))
  allocate (LAT(NX,NY,NT))

  allocate (cdata_all(nreal,nobs_lst))
  allocate (lst_rec(nobs_lst))
  cdata_all=zero
  lst_rec=0
  ilon=2
  ilat=3


  !#4 Check the satellite scan time
  call check( nf90_get_att(ncid,nf90_global,"time_coverage_start" , time))
  print *, 'time varid =', time
  year=time(1:4)
  month=time(6:7)
  day=time(9:10)
  hour=time(12:13)
  minute=time(15:16)

  print*,'year month day ',year, month, day, hour,minute

! Compare relative obs time with window.  If obs
! falls outside of window, don't use this obs
  read(year,'(i4)')idate5(1)       !year
  read(month,'(i4)')idate5(2)      !month
  read(day,'(i4)')idate5(3)        !day
  read(hour,'(i4)')idate5(4)       !hours
  read(minute,'(i4)')idate5(5)        !minutes
  print*, 'idate = ', idate5       
  call w3fs21(idate5,nmind)
  t4dv = real((nmind-iwinbgn),r_kind)*r60inv
  sstime = real(nmind,r_kind)
  tdiff=(sstime-gstime)*r60inv

  if (l4dvar.or.l4densvar) then
     if (t4dv<zero .OR. t4dv>winlen) go to 900  
  else
     !if (abs(tdiff)>twind) cycle read_loop
     if (abs(tdiff)>twind) then 
       write(6,*) ' the time of LST is out of the time window STOP HERE!' 
       go to 900
     end if
  endif



  !#5 Check the satellite infomation
  call check( nf90_get_att(ncid,nf90_global,"satellite_name" , satname))
  call check( nf90_get_att(ncid,nf90_global,"instrument_name" ,intname))
  call check( nf90_get_att(ncid,nf90_global,"id" , satid))
  print*, 'satellitenName is ', satname
  print*, 'instrument name is ', intname
  print*, 'satellite ID is ', satid

  iosub=0
  qm=2
  

  !#5 Get the varid of the VLST, based on its name.
  call check( nf90_inq_varid(ncid, "VLST", varid) )
  print *, 'varid =', varid

  !#5.1 Read the VLST data.
  print*, 'reading VLST'
  call check( nf90_get_var(ncid, varid, data_in) )
  print*, 'data_in (1,1,1) =',  data_in (1,1,1:NT)
  
  LST=data_in*scale_factor_1+scale_factor_2
  print*, 'LST (1,1) =', LST(1,1,:)

  !#6 Get the varid of the VLST_Quality_Flag, based on its name.
  call check( nf90_inq_varid(ncid, "VLST_Quality_Flag", varid) )

  !#6.1 Read the QI_Flag.
  print*, 'reading VLST_Quality_Flag'
  call check( nf90_get_var(ncid, varid, QI_Flag) )
  print*, 'QI_Flag (1,1) =',  QI_Flag (1,1,:)

  !#7 Get the varid of the Latitude, based on its name.
  call check( nf90_inq_varid(ncid, "Latitude", varid) )

  !#7.1 Read the Latitude 
  print*, 'reading Latitude'
  call check( nf90_get_var(ncid, varid, LAT) )
  print*, 'LAT (1,1,1) =',  LAT (1,1,1)
  print*, 'LAT (1,1,2) =',  LAT (1,1,2)

  !#8 Get the varid of the Longitude, based on its name.
  call check( nf90_inq_varid(ncid, "Longitude", varid) )

  !#8.1 Read the  Longitude 
  print*, 'reading Longtitude'
  call check( nf90_get_var(ncid, varid, LON) )
  print*, 'LON (1,1) =',  LON (1,1,:)

! Close the file, freeing all resources.
  call check( nf90_close(ncid) )

  print *,"*** SUCCESS reading example file ", infile, "! "

  !#9 Prosess data for writing out
  do n=1,NT
  do i=1,NX
  do j=1,NY
     if (data_in(i,j,n) >= lst_missing_value) cycle
     if (abs (LAT(i,j,n)) > r90 )cycle
     if (LON(i,j,n) <zero) LON(i,j,n)=LON(i,j,n)+r360  
     if (LON(i,j,n) ==360) LON(i,j,n)=LON(i,j,n)-r360  
     if (LON(i,j,n) >360) cycle
 
     dlon_earth_deg=LON(i,j,n) 
     dlat_earth_deg=LAT(i,j,n)
     dlon_earth=dlon_earth_deg*deg2rad
     dlat_earth=dlat_earth_deg*deg2rad

!    If regional, map obs lat,lon to rotated grid.
     if(regional)then
       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
       if(diagnostic_reg) then
          call txy2ll(dlon,dlat,rlon00,rlat00)
          ntest=ntest+1
          cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
          cdist=max(-one,min(cdist,one))
          disterr=acos(cdist)*rad2deg
          disterrmax=max(disterrmax,disterr)
       end if
       if(outside) cycle
      else
       dlon=dlon_earth
       dlat=dlat_earth
       call grdcrd1(dlat,rlats,nlat,1)
       call grdcrd1(dlon,rlons,nlon,1)
    endif

!processing thinning
    if (thin4d) then
       crit1=0.01_r_kind
    else
       timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
       crit1=0.01_r_kind+timedif
    endif

    call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
    if(.not. iuse) cycle 

!       Set common predictor parameters
!test
    pred=zero
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is
!       "best"

     crit1 = crit1+pred
     call finalcheck(dist1,crit1,itx,iuse)

     if(.not. iuse)cycle 

     nread=nread+1
     lst_rec(nread)=itx

     !x
     toe=r1_2
     cdata_all(1,itx)=toe                  ! obs error
     cdata_all(2,itx)=dlon                 ! grid relative longitude
     cdata_all(3,itx)=dlat                 ! grid relative latitude
     cdata_all(4,itx)=950.0                ! ln(pressure in cb)
     cdata_all(5,itx)=LST(i,j,n)             ! VIIRS Land Surface Temperature
     cdata_all(6,itx)=666                  ! station id
     cdata_all(7,itx)=t4dv                 ! time
     cdata_all(8,itx)=nc                   ! itype in convinfo
     cdata_all(10,itx)=QI_Flag(i,j,n)        ! set the quality mark
     cdata_all(11,itx)=three               ! set the original obs. error
     cdata_all(12,itx)=zero                ! set the iuse
     cdata_all(13,itx)=one                 ! dominate surface type (1=land)
     cdata_all(14,iout)=LST(i,j,n)           ! skin temperature
     cdata_all(15,iout)=-99.999_r_kind     ! 10 meter wind factor
     cdata_all(16,iout)=-99.999_r_kind     ! surface roughness
     cdata_all(17,itx)=dlon_earth_deg      ! earth relative longitude (degrees)
     cdata_all(18,itx)=dlat_earth_deg      ! earth relative latitude (degrees)
     cdata_all(19,iout)=2.0                ! station elevation (m)
     cdata_all(20,iout)=2.0                ! observation height (m)
     cdata_all(21,iout)=zero               ! terrain height at ob location
     cdata_all(22,iout)=zero               ! provider name
     cdata_all(23,iout)=zero               ! subprovider name
     cdata_all(26,itx)=-999                ! SolarZenithAngle
  end do
  end do
  end do

900 continue
  write(6,*)'Final VIIRS LST nobs, nread , itx =', nobs_lst,nread,itx
  
! call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
!     nele,itxmax,nread,ndata,cdata_all,score_crit,nrec)

  ! Write header record and data to output file for further processing

  ndata=nread
  !allocate(cdata_out(nreal,ndata))
  allocate(cdata_out(nreal,nread))
  do i=1,nread
    cdata_out(:,i)=cdata_all(:,lst_rec(i))
  end do
  ! call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) cdata_out
  write(888,*) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(888,*) cdata_out


  write(6,*) 'READ_VIIRSLST,nread,ndata,nreal,nodata=',nread,ndata,nreal,nodata

  deallocate(cdata_all)
  deallocate(cdata_out)
  deallocate(lst_rec)
  deallocate(data_in)
  deallocate(LST)
  deallocate(QI_Flag)
  deallocate(LAT)
  deallocate(LON)


contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    print *,nf90_noerr
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end subroutine read_viirsLST

