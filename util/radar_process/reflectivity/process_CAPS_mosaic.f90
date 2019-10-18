program process_CAPS_mosaic
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!        (Originally copied from process_NSSL_mosaic.f90 and modified.)
!
! ABSTRACT: 
!     This routine mainly adopts the basic structure of process_NSSL_mosaic 
!     (originally developed by Ming Hu, and modified by Gang Zhao) to process
!     MRMS tilt-merged reflectivity data and have them interpolated onto 
!     model original grids (in horizontal) based on the model grid config.
!     info provided.
!
!     tversion=8  : NSSL 8 tiles netcdf
!     tversion=81 : NCEP 8 tiles binary
!     tversion=4  : NSSL 4 tiles binary
!     tversion=1  : NSSL 1 tiles grib2
!
! PROGRAM HISTORY LOG:
!    Youngsun Jung (10/26/2018) Added data thinning
!    Chongchi Tong (02/22/2019) Added model option to enable FV3 processing   
!    Chongchi Tong (03/09/2019) Modified the output of dbzbufr file to keep
!                                 both lon/lat and grid relative i/j info,
!                                 istead of optional controlled by 'l_latlon'.
!                                 Therefore, the 'l_latlon' DOES NOT take any
!                                 effect since this modification.
!    Youngsun Jung (04/10/2019) Added vertical data thinning option
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES: dbzbufr
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!
!$$$
!
!_____________________________________________________________________

  use mpi
  use kinds, only: r_kind,i_kind

  implicit none
!
  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
  real(r_kind),parameter:: r90  =  90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: zero =   0.0_r_kind
!
  character*256 output_file
!
!  grid
  integer(i_kind) :: nlon,nlat       ! model grid number in x and y directions
  real,allocatable:: xlon(:,:)       ! earth absolute longitude of model grids
  real,allocatable:: ylat(:,:)       ! earth absolute latitude of model grids
  REAL, allocatable :: ref3d(:,:,:)  ! 3D reflectivity
  REAL, allocatable :: ref0(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: maxref(:,:)   ! composite reflectivity (column max)
  REAL(r_kind), allocatable :: ref3d_column(:,:)  ! column formatted 3D reflectivity
  CHARACTER*180   geofile
!
!  For reflectivity mosaic
!
  CHARACTER*256   mosaicfile
  CHARACTER*256   filenameall(200)

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: msclev(:)        ! level of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity
  REAL, allocatable :: mscValue3d(:,:,:)    ! reflectivity

  REAL   :: lonMin,latMin,lonMax,latMax
  REAL*8 :: dlon,dlat
  integer :: height
  integer :: levelheight(33)
  data levelheight /500, 750, 1000, 1250, 1500, 1750, 2000, 2250,  2500, 2750, &
                   3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, &
                   8000, 8500, 9000,10000,11000,12000,13000,14000,15000,16000, &
                  17000,18000,19000/

! 4-byte variables (used for NCL plot and save space)
  REAL, allocatable     :: rlvlhgt_r4(:)
  integer   :: maxlvl4, nlat4, nlon4, numref4
                   
!
!  4 Tile binary format
!
  integer           :: ntot, ntot2d, mt
  integer*4         :: nx,ny,nz
  integer*4         :: yr, mo, da, hr, mn, sc
  real*8            :: rdx,rdy
  real*4            :: rdx4,rdy4
  real              :: rlatmax,rlonmin
  integer*4         :: var_scale

  integer*2, dimension(:),   allocatable  :: var
!
!
!  namelist files
!
  INTEGER(i_kind)  ::  tversion
  character*10 :: analysis_time
  CHARACTER*180   dataPath
  logical   :: l_latlon
  logical   :: l_psot      ! single obs of dbz
  logical   :: l_latlon_psot
  integer(i_kind)  :: modlopt ! denote which model conf. is ued: WRF(1) or FV3(2)
  real(r_kind)      :: olat,olon,olvl
  real(r_kind)      :: odbz
  integer(i_kind) :: iskip, jskip   !YJ
  integer(r_kind)   :: lev_keep     !YJ
! namelist/setup/ tversion,analysis_time,dataPath
  namelist/setup/tversion,analysis_time,dataPath,l_latlon,l_psot,iskip,jskip,lev_keep,modlopt
  namelist/oneob/ l_latlon_psot,olat,olon,olvl,odbz
  integer(i_kind)  ::  idate
!
!
!  ** misc
      
  real        ::rix  ! x-grid coordinate (grid units)
  real        ::riy  ! y-grid coordinate (grid units)
  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain
  logical     :: fileexist

  integer i,j,k,itype,iymdh,ier,jret,ifn,kk
  integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

  integer :: NCID

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp,ipp1,jpp1
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4,refl_ltng

  INTEGER(i_kind)  ::  maxlvl, nlevel
  INTEGER(i_kind)  ::  numlvl,numref,nnn
  integer :: status
  REAL ::  rthresh_ref,rthresh_miss

  integer   :: i_max, j_max, k_max
  real      :: dbz_max, dbz_min
  real      :: xlon_max, ylat_max

  real(r_kind)  :: pi, deg2rad
  real(r_kind)  :: r_earth
  real(r_kind)  :: olon_e, olat_e, rlon_e, rlat_e, dlon_e, dlat_e
  real(r_kind)  :: a, a2, b, b2, c, dist, dist_min
  real(r_kind)  :: rlat_min, rlon_min
  integer(i_kind)  :: i_min, j_min

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if(mype==0) write(*,*) mype, 'deal with mosaic'

  l_latlon = .TRUE.

  l_psot = .FALSE.
  l_latlon_psot = .FALSE.
  olat = 0.0
  olon = 0.0
  olvl = 0.0
  odbz = -64.0

  iskip=1; jskip=1             !YJ
  lev_keep = 250               !YJ

  modlopt = 1

  pi      = acos(-1.0_r_kind)
  deg2rad = pi/180.0_r_kind
  rad2deg = 1.0_r_kind/deg2rad
  r_earth = 6.3712e+6_r_kind

  open(15, file='mosaic.namelist')
    read(15,setup)
    if ( l_psot ) then
        read(15,oneob)
    end if
  close(15)

  if (mype==0) then
      write(6,*) ' Check up namelist ...  '
      write(6,*) ' Vertical thinning parameter used: ',lev_keep,' m'
      write(6,setup)
      if ( l_psot ) then
          write(6,oneob)
      end if
  end if

  if (l_psot) then
      write(6,*)' generate single obs of dbz at (lat lon lvl):',olat,olon,olvl
      write(6,*)' obs dbz :',odbz
  end if

  read(analysis_time,'(I10)') idate
  if(mype==0) write(6,*) 'cycle time is :', idate

  if( tversion == 8 .or. tversion == 14) then
     maxlvl = 31
     rthresh_ref=-500.0
     rthresh_miss=-5000.0
  elseif( tversion == 81 ) then
     maxlvl = 31
     rthresh_ref=-90.0
     rthresh_miss=-900.0
  elseif( tversion == 4 ) then
     maxlvl = 33
     rthresh_ref=-500.0
     rthresh_miss=-5000.0
     allocate(rlvlhgt_r4(maxlvl))
     rlvlhgt_r4(:) = levelheight(:)
  elseif( tversion == 1 ) then
     maxlvl = 33
     rthresh_ref=-90.0
     rthresh_miss=-900.0
     if(npe < maxlvl) then
        write(*,*) 'npe must larger or equal to maxlvl'
        stop 1234
     endif
     allocate(rlvlhgt_r4(maxlvl))
     rlvlhgt_r4(:) = levelheight(:)
  else
     write(*,*) 'unknow tversion !'
     stop 1234
  endif
!
! set geogrid file name
!
  write(geofile,'(a,a)') './', 'geo_em.d01.nc'

  if(mype==0) write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT,modlopt)
  if(mype==0) write(*,*) 'NLON,NLAT',NLON,NLAT
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon,modlopt)
  call CLOSE_geo(NCID)
!
  mypeLocal=mype+1
  if ( tversion == 1 ) then
     open(10,file='filelist_mrms',form='formatted',err=300)
     do n=1,200
        read(10,'(a)',err=200,end=400) filenameall(n)
     enddo
300  write(6,*) 'read_grib2 open filelist_mrms failed ',mype
     stop(555)
200  write(6,*) 'read_grib2 read msmr file failed ',n,mype
     stop(555)
400  nlevel=n-1
     close(10)
     if(nlevel .ne. maxlvl) then
        write(*,*) 'vertical level is wrong:',nlevel,maxlvl
        stop 666
     endif

     if(mypeLocal <= npe) then
        mosaicfile=trim(filenameall(mypeLocal))
        write(*,*) 'process level:',mypeLocal,trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe < nlevel ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  elseif(tversion==81) then
     if(mypeLocal <= 8) then
        write(mosaicfile,'(a,a,I1)') trim(dataPath), 'mosaic_t',mypeLocal
        write(*,*) 'process tile:',trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe <8 ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  else
     if(mypeLocal <= tversion) then
        write(mosaicfile,'(a,a,I1)') trim(dataPath), 'mosaic_t',mypeLocal
        write(*,*) 'process tile:',trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe < tversion ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  endif
!
!   deal with certain tile
!
  fileexist=.false.
   
  if( tversion == 8 .or. tversion == 14) then
     call ifexist_file(mosaicfile,STATUS)
     fileexist=STATUS .EQ. NF_NOERR
  elseif( tversion == 81) then
     inquire(file=trim(mosaicfile),exist=fileexist)
     if(mypeLocal > 8) fileexist=.false.
  elseif(tversion == 4) then
     open(99,file=trim(mosaicfile),form='unformatted',access='direct',&
             recl=6*4,status='old',err=225)
        rewind(99)
        read(99,rec=1,err=225) yr, mo, da, hr, mn, sc
        fileexist=.true.
225     continue
     close(99)
  elseif(tversion == 1) then
     inquire(file=trim(mosaicfile),exist=fileexist)
     if(mypeLocal > nlevel) fileexist=.false.
  endif

  if(fileexist) then
      IF( tversion == 14 ) then
         call GET_DIM_ATT_Mosaic(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
         var_scale=10
      ELSEIF( tversion == 8 ) then
         call GET_DIM_ATT_Mosaic8(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
         var_scale=10
      ELSEIF( tversion == 81 ) then
         call read_ncep_binary_head(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,rdx4,rdy4)
         nx=mscNlon
         ny=mscNlat
         nz=mscNlev
         dlon=rdx4
         dlat=rdy4
         var_scale=1
      ELSEIF( tversion == 4 ) then
         call read_head_Mosaic4(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy,var_scale)
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)
      ELSEIF( tversion == 1 ) then
         call read_grib2_head(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy)
         var_scale=1
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)
      ELSE
         write(*,*) ' unknown tile version !!!'
         stop 123
      ENDIF
      if(mype==0) then
            write(*,*) 'mscNlon,mscNlat,mscNlev'
            write(*,*) mscNlon,mscNlat,mscNlev
            write(*,*) 'dlon,dlat,lonMin,lonMax,latMax,latMin'
            write(*,*) dlon,dlat,lonMin,lonMax,latMax,latMin
      endif

      if( maxlvl == mscNlev .or. maxlvl == nlevel ) then
         allocate(ref3d(nlon,nlat,maxlvl))
      else
         write(*,*) 'Wrong vertical layers:', maxlvl, mscNlev
         stop 1234
      endif
      ref3d=-999.0

      allocate(msclon(mscNlon))
      allocate(msclat(mscNlat))
      allocate(msclev(mscNlev))
      allocate(mscValue(mscNlon,mscNlat))

      DO i=1,mscNlon
         msclon(i)=lonMin+(i-1)*dlon
      ENDDO
      DO i=1,mscNlat
         msclat(i)=latMin+(i-1)*dlat
      ENDDO
!
!  ingest mosaic file and interpolation
! 
      if( tversion == 8 .or. tversion == 14) then
         call OPEN_Mosaic(mosaicfile, NCID)

         if(tversion == 14 ) then
            call Check_DIM_ATT_Mosaic(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
         elseif(tversion == 8 ) then
            call Check_DIM_ATT_Mosaic8(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
         endif
         write(*,*) mscNlon,mscNlat,mscNlev
         write(*,*) 'Area of tile=',lonMin,latMin,lonMax,latMax,dlon,dlat

      elseif(tversion == 81) then
          allocate(mscValue3d(mscNlon,mscNlat,mscNlev))
          call read_ncep_binary_value(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   mscValue3d)
      elseif(tversion == 4) then
         ntot = nx*ny*nz
         allocate(var(ntot))
         call read_data_Mosaic4(mosaicfile,ntot,var)
      elseif(tversion == 1) then
         ntot = nx*ny*nz
         call read_grib2_sngle(mosaicfile,ntot,height,mscValue)
!         write(*,*) 'height,max,min',height,maxval(mscValue),minval(mscValue)
         if(levelheight(mypeLocal) .ne. height) then
            write(6,*) 'Error, the order of each level height is wrong', &
                      mypeLocal,levelheight(mypeLocal), height
            stop 12345
         endif
      else
         write(*,*) 'unknown type'
         stop 1234
      endif
  endif
  call mpi_barrier(MPI_COMM_WORLD,ierror)
!  stop 999
  if(fileexist) then
!
      DO k=1, mscNlev
!          if(tversion > 1) write(*,*) mype, 'deal with level:', k,mscNlon,mscNlat
          if( tversion == 8 .or. tversion == 14) then
             call  GET_Mosaic_sngl_Mosaic(NCID,mscNlon,mscNlat,k,mscValue)
          elseif(tversion == 81) then
             do j=1,ny
             do i=1,nx
                mscValue(i,j) = mscValue3d(i,j,k)
             enddo
             enddo
          elseif(tversion == 4) then
             ntot2d=nx*ny*(k-1)
             do j=1,ny
             do i=1,nx
                mscValue(i,j) = var(ntot2d+(j-1)*nx+i)
             enddo
             enddo
          elseif(tversion == 1) then
             write(*,*) 'level max min height',mypeLocal,maxval(mscValue),minval(mscValue),height
          endif
          DO j=1,nlat
          DO i=1,nlon
             rlat=ylat(i,j)
             rlon=xlon(i,j)

             if(tversion == 14 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(rlat-latMin)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             elseif(tversion == 8 .or. tversion == 81) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(latMax-rlat)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp 
             elseif(tversion == 4 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(rlat-latMin)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             elseif(tversion == 1 ) then
               if(rlon<0.0) rlon=360.0+rlon
               rip=(rlon-lonMin)/dlon+1
               rjp=(latMax-rlat)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             else
               write(*,*) ' Unknown Mosaic format !!'
               stop 123
             endif
             if( ip >= 1 .and. ip <= mscNlon ) then
             if( jp >= 1 .and. jp <= mscNlat ) then
! inside mosaic domain
               ipp1=min(ip+1,mscNlon)
               jpp1=min(jp+1,mscNlat)
               w1=(1.0-dip)*(1.0-djp)
               w2=dip*(1.0-djp)
               w3=dip*djp
               w4=(1.0-dip)*djp
               ref1=mscValue(ip,jp)
               ref2=mscValue(ipp1,jp)
               ref3=mscValue(ipp1,jpp1)
               ref4=mscValue(ip,jpp1)
               kk=k
               if(tversion == 1) kk=mypeLocal
               if(ref1 > rthresh_ref .and. ref2 > rthresh_ref .and.  &
                  ref3 > rthresh_ref .and. ref4 > rthresh_ref ) then
                  if(mod(levelheight(kk),lev_keep) == 0) then
                     ref3d(i,j,kk)=(ref1*w1+ref2*w2+ref3*w3+ref4*w4)/float(var_scale)
                  else
                     ref3d(i,j,kk)=-999.0  ! YJ: Set to no observation to reduce the number of obs
                  endif
               elseif(ref1 > rthresh_miss .and. ref2 > rthresh_miss .and.  &
                  ref3 > rthresh_miss .and. ref4 > rthresh_miss ) then
                  if(mod(levelheight(kk),lev_keep*2) == 0) then
                     ref3d(i,j,kk)=-99.0   ! clear
                  else
                     ref3d(i,j,kk)=-999.0  ! YJ: Set to no observation to reduce the number of obs
                  endif
               else
                  ref3d(i,j,kk)=-999.0  ! no observation
               endif
             endif
             endif
          ENDDO
          ENDDO
      ENDDO  ! mscNlev

      if( tversion == 8 .or. tversion == 14) then
         call CLOSE_Mosaic(NCID)
      endif
      if(tversion == 4)   deallocate(var)
      if(tversion == 81)   deallocate(mscValue3d)

      deallocate(msclon)
      deallocate(msclat)
      deallocate(msclev)
      deallocate(mscValue)
   else
      allocate(ref3d(nlon,nlat,maxlvl))
      ref3d=-999.0
      write(*,*) trim(mosaicfile), '   does not exist!!!'
   ENDIF

   call mpi_barrier(MPI_COMM_WORLD,ierror)
!
!  collect data from all processes to root (0)
!
   if(mype==0) then
     allocate( ref0(nlon,nlat,maxlvl) )
     allocate( maxref(nlon,nlat) )
   endif
   call MPI_REDUCE(ref3d, ref0, nlon*nlat*maxlvl, MPI_REAL, MPI_MAX, 0, &
                     MPI_COMM_WORLD, ierror)
   deallocate(ref3d)
!
  if(mype==0) then
    OPEN(10,file='./'//'RefInGSI3D.dat',form='unformatted')
     write(10) maxlvl,nlon,nlat
     write(10) ref0
    close(10)
        DO k=1,maxlvl
           write(*,*) k,maxval(ref0(:,:,k)),minval(ref0(:,:,k))
        ENDDO

    DO i=1, nlon
      DO j=1, nlat
        maxref(i,j)=maxval(ref0(i,j,:))
      END DO
    END DO
    OPEN(11,file='MRMScompZ.txt',STATUS='unknown')
    WRITE(11,'(576(f9.3,1x))') ((maxref(i,j),j=1,nlat),i=1,nlon)
    CLOSE(11)
  endif

  if(mype==0 .and. 1==1) then
!
    allocate(ref3d_column(maxlvl+2,nlon*nlat))

    ref3d_column=-999.0

    if ( .NOT. l_psot ) then

        dbz_max=-999.0
        dbz_min= 999.0
        i_max=-1; j_max=-1; k_max=-1

        numref=0
!       DO j=1,nlat
!       DO i=1,nlon
        nnn=0
        print*,'nlat=',nlat,'nlon=',nlon,'maxlvl=',maxlvl
        DO j=2,nlat-1,jskip              !YJ
            DO i=2,nlon-1,iskip          !YJ
                numlvl=0
                DO k=1,maxlvl
                   if (ref0(i,j,k) == -999.0 ) then !Rong Kong modified here for missing value
                     ref0(i,j,k) = -64.0           !-64.0 respresents missing in bufr
                   elseif (ref0(i,j,k) < 0.0 .and. ref0(i,j,k) > -999.0 ) then
                     nnn=nnn+1
                     ref0(i,j,k) = 0.0 
                   endif

!                   change the thresh_hold to -64.0 dbz.  (G. Zhao 11092017)
!                   if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
!                    if( ref0(i,j,k) >= -64.0 .and. ref0(i,j,k) < 100.0 ) numlvl=numlvl+1
                    if( ref0(i,j,k) >= 0.0 .and. ref0(i,j,k) < 100.0 ) numlvl=numlvl+1
                ENDDO

                if(numlvl > 0 ) then
                    numref=numref+1
 
                    rlat=ylat(i,j)
                    rlon=xlon(i,j)

                    if(abs(rlat)>r90 .or. abs(rlon)>r360) then
                        write(6,*) 'lat and/or lon is not in proper value range'
                        stop 
                    end if
                    if(rlon == r360)rlon=rlon-r360
                    if(rlon < zero) rlon=rlon+r360

                    ref3d_column(1,numref)=rlon
                    ref3d_column(2,numref)=rlat
                   
                    ref3d_column(3,numref)=float(i)/10.0_r_kind
                    ref3d_column(4,numref)=float(j)/10.0_r_kind
                   
                    DO k=1,maxlvl
                        ref3d_column(2+k,numref)=ref0(i,j,k)

                        if ( ref0(i,j,k) .gt. dbz_max ) then
                            dbz_max = ref0(i,j,k)
                            i_max=i
                            j_max=j
                            xlon_max=xlon(i_max,j_max)
                            ylat_max=ylat(i_max,j_max)
                            k_max=k
                        end if

                        if ( ref0(i,j,k) .lt. dbz_min ) then
                            dbz_min = ref0(i,j,k)
                        end if

                    ENDDO
                endif
            ENDDO
        ENDDO

    else   !l_psot

        write(6,*) 'generating single obs of dbz:'
        numref = 1
        dist_min = 999999999999.0_r_kind
        if ( l_latlon_psot ) then
            ! if given lat-lon for single obs, then search for the closest grid point
            rlat_min = olat
            rlon_min = olon
            i_min = -1
            j_min = -1
            DO j=2,nlat-1
                DO i=2,nlon-1
                    rlat=ylat(i,j)
                    rlon=xlon(i,j)
                    if(abs(rlat)>r90 .or. abs(rlon)>r360) then
                        write(6,*) 'lat and/or lon is not in proper value range'
                        stop 
                    end if
                    if(rlon == r360)rlon=rlon-r360
                    if(rlon < zero) rlon=rlon+r360
                    if(olon < zero) olon=olon+r360

                    olat_e = olat * deg2rad
                    olon_e = olon * deg2rad
                    rlat_e = rlat * deg2rad
                    rlon_e = rlon * deg2rad

                    dlat_e = (olat - rlat) * deg2rad
                    dlon_e = (olon - rlon) * deg2rad

                    a    = sin(dlat_e*0.5_r_kind) * sin(dlat_e*0.5_r_kind) + &
                        cos(olat_e) * cos(rlat_e) * sin(dlon_e*0.5_r_kind) * sin(dlon_e*0.5_r_kind)
                    a2   = sqrt(a)
                    b    = 1.0_r_kind - a
                    b2   = sqrt(b)
                    c    = 2.0_r_kind * atan2(a2,b2)
                    dist = r_earth * c

                    if (dist < dist_min) then
                        dist_min = dist
                        rlat_min = rlat
                        rlon_min = rlon
                        i_min = i
                        j_min = j
                    end if

                END DO
            END DO
        else
            ! if given x-y indices, then found the grid point for single obs ! directly
            i_min = NINT(olon)
            j_min = NINT(olat)
            rlon_min = xlon(i_min, j_min)
            rlat_min = ylat(i_min, j_min)
            if(abs(rlat_min)>r90 .or. abs(rlon_min)>r360) then
                write(6,*) 'lat and/or lon of model(geo.nc) is not in proper value range'
                stop 
            end if
            if(rlon_min == r360)rlon_min=rlon_min-r360
            if(rlon_min < zero) rlon_min=rlon_min+r360
        end if
        ref3d_column(1,numref)=rlon_min
        ref3d_column(2,numref)=rlat_min
        k = NINT(olvl)
        ref3d_column(2+k,numref)=odbz

        if (l_latlon_psot) then
            write(6,*) 'for single obs  @ given olat(deg) olon(deg) olvl:',olat,olon,olvl
            write(6,*) 'single obs is placed on the closest grid @(lat lon-->i j):',rlat_min,rlon_min,'-->',i_min,j_min
            write(6,*) '        it is palace at height(meter):', levelheight(k)
            write(6,*) ' the distance from given latlon to closest grid point :',dist_min
        else
            write(6,*) 'for single obs: it is placed on the grid @ given x y indices: i,j,lvl:',olon, olat, olvl
            write(6,*) '                its lat/lon in deg (i,j-->lat lon):',rlat_min,rlon_min
            write(6,*) '                it is palace at height(meter):', levelheight(k)
        end if

    end if   !l_psot

    if ( .NOT. l_psot) then
        write(*,*)'max reflectivity =',dbz_max,' at (i,j,k)=',i_max,j_max,k_max
        write(*,*)'max reflectivity =',dbz_max,' at (x,y,k)=',xlon_max,ylat_max,k_max
        write(*,*)'min reflectivity =',dbz_min
    end if

    write(*,*) 'Start write_bufr_nsslref'
    call write_bufr_capsref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
    print*,'numref=',numref,'nnn=',nnn
    deallocate(ref3d_column)
    deallocate(xlon,ylat)

  endif

  call MPI_FINALIZE(ierror)

!
end program process_CAPS_mosaic
