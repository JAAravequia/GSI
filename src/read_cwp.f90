subroutine read_cwp(nread,ndata,nodata,infile,lunout,obstype,twind,sis,hgtl_full,nobs)
!$$$   subprogram documentation block
!                .      .    .                                       .
!   subprogram: read_cwp        read CWP observations
!   
! abstract: Read and process CWP retrievals
!           observations in DART-like netcdf format.  
!
! program history log:
!   2016-09-25    Thomas Jones:  Read CWP retrievals from netcdf file
!   2018-06-28    Thomas Jones:  Modify for new CWP netcdf format &
!                                Add night time retrieval capability                            
!
! program history log:
!           
!   input argument list:
!     infile   - file from which to read data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!
!   output argument list:
!     nread    - number of cwp observations read
!     ndata    - number of cwp observations retained for further processing
!     nodata   - number of cwp observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!
! Variable Definitions:
!
!  cdata_all - real - dim(maxdat,maxobs) - array holding all data for assimilation
!  cstaid - char - radar station ide
!  dlat - real - grid relative latitude of observation (grid units)
!  dlon - real - grid relative longitude of observation (grid units)
!  maxobs - int - max number of obs converted to no precip observations
!  num_m2nopcp -int - number of missing obs 
!  num_missing - int - number of missing observations
!  num_noise - int - number of rejected noise observations
!  num_nopcp - int - number of noise obs converted to no precip observations
!  numbadtime - int - number of elevations outside time window
!  outside - logical - if observations are outside the domain -> true
!  radartwindow - real - time window for observations (minutes)
!  rmins_an - real - analysis time from reference date (minutes)
!  rmins_ob - real -  observation time from reference date (minutes)
!  thiserr - real - observation error
!  thislat - real - latitude of observation, point
!  thislon - real - longitude of observation, point
!  thishgt - real - observation height, point
!  timeb - real - obs time (analyis relative minutes)
!  cwpQC - real - CWP observation (kg m2)
!  cwp_err - real - observation error of CWP
!  cbp - real - Cloud base presure (Pa)
!  ctp - real - Cloud top pressure (Pa)
!  obkind - int - kind of CWP observation (IWP, LWP, or ZERO)
!  height - real - height of observation
!  lon    - real - longitude of observation
!  lat    - real - latitude of observation
!  utime  - real - time for each observation point
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg, &
                       one_tenth,r1000,r60,r60inv,r100,r400,grav_equator, &
                       eccentricity,somigliana,grav_ratio,grav,semi_major_axis,flattening 
  use gridmod, only: regional,tll2xy,nsig,nlat,nlon
  use obsmod, only: iadatemn,debugmode, time_window
  use convinfo, only: nconvtype,ctwind,cgross,icuse,ioctype
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use jfunc, only: miter
  use mpimod, only: npe
  use netcdf

  implicit none

  include 'netcdf.inc'

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full
  integer(i_kind) ,dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxdat=18_i_kind, ione = 1_i_kind         ! Used in generating cdata array
  character(len=4), parameter :: satid = 'GOES-R'
  
! === CWP variable declaration
  real(r_kind), allocatable, dimension(:)       :: cwpQC, data_r_1d, cwp_err, height,&
                                                   lon, lat, utime, ctp, cbp ! obkind
  integer(i_kind), allocatable, dimension(:)    :: obkind 
  integer(i_kind)                               :: idate5(5), sec70
  character(4)                                  :: idate5s(5)
  logical                                       :: if_input_exist

!------------------
!  NETCDF-RELATED
!------------------
   INTEGER(i_kind)   :: ncdfID
   INTEGER(i_kind)   :: status
   INTEGER(i_kind)   :: datestlen, varID, DimID
   INTEGER(i_kind)   :: vardim, natts
   INTEGER(i_kind)   :: vartype, id_time
   INTEGER(i_kind)   :: numdim, numvars, numatt

!--Counters for diagnostics
  integer(i_kind) :: num_missing=0,num_clear=0, &      !counts 
                    numbadtime=0   
  integer(i_kind) ::nlevz,icntpnt,klon1,klat1,kk,klatp1,klonp1
  integer(i_kind) ntmp,iout,iiout
  integer(i_kind) maxout,maxdata
  integer(i_kind),allocatable,dimension(:):: isort

  real(r_kind) sin2,termg,termr,termrg,zobs,hgt
  real(r_kind) crit1,timedif
  real(r_kind),parameter:: r16000 = 16000.0_r_kind
  logical :: luse
       
  !--General declarations
  integer(i_kind) :: ierror,i,j,k,v,na,nb,nelv,nvol, &
                     ikx,mins_an,mins_ob,nn, length
  integer(i_kind) :: maxobs,nchanl,ilat,ilon,scount
  
  real(r_kind) :: selev0,celev0,this_stahgt,thishgt                           
  real(r_kind) :: celev,selev,t4dv, dumvar, &
                  dlat,dlon,thiserr,thislon,thislat, &
                  timeb
  real(r_kind) :: radartwindow
  real(r_kind) :: rmins_an,rmins_ob                                                     
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  
  !character(8) cstaid
  !character(4) this_staid
  !equivalence (this_staid,cstaid)
  !equivalence (cstaid,rstation_id)

  logical      :: outside
    
  real(r_kind) :: minobrange,maxobrange

  minobrange=0.0  !kg/m2
  maxobrange=10.0

!--------------------------------------------------------------------------------------!
!                            END OF ALL DECLARATIONS                                   !
!--------------------------------------------------------------------------------------!

  scount=0
  ikx=0
  do i=ione,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== ione) then
        ikx=i 
        radartwindow=ctwind(ikx)*r60         !Time window units converted to minutes 
                                             !  (default setting for cwp within convinfo is 0.05 hours)
        exit                                 !Exit loop when finished with initial convinfo fields     
     else if ( i==nconvtype ) then
        write(6,*) 'READ_CWP: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USE FLAG IS ZERO'
        write(6,*) 'READ_CWP: ABORTTING read_cwp.f90 - NO CWP OBS READ!'
        return
     endif
  end do     

  if (minobrange >= maxobrange) then
  write(6,*) 'MININMUM OB RANGE >= MAXIMUM OB RANGE FOR READING CWP - PROGRAM STOPPING FROM READ_CWP.F90'
  call stop2(400)
  end if
        
  !-next three values are dummy values for now
  nchanl=0
  ilon=2_i_kind
  ilat=3_i_kind
  
  maxobs=5000000_i_kind    !value taken from read_radar.f90 

  !--Allocate cdata_all array
   allocate(cdata_all(maxdat,maxobs),isort(maxobs))

   maxout=0
   maxdata=0
   isort=0
   icntpnt=0

  ! CHECK IF DATA FILE EXISTS
  length     = len_trim(infile)
  inquire(file=infile(1:length), exist=if_input_exist)
  fileopen: if (if_input_exist) then

    ! OPEN NETCDF FILE
    status = nf90_open(TRIM(infile), NF90_NOWRITE, ncdfID)
    print*, '*** OPENING GOES-16/17 CWP OBS  NETCDF FILE: ', infile, status

    !------------------------
    ! Get date information
    !-------------------------
    status = nf90_get_att( ncdfID, nf90_global, 'year', idate5s(1) )
    status = nf90_get_att( ncdfID, nf90_global, 'month', idate5s(2) )
    status = nf90_get_att( ncdfID, nf90_global, 'day', idate5s(3) )
    status = nf90_get_att( ncdfID, nf90_global, 'hour', idate5s(4) )
    status = nf90_get_att( ncdfID, nf90_global, 'minute', idate5s(5) )
    read(idate5s(:) , *) idate5(:)
    print*, idate5

    !------------------------
    ! Get Dimension Info (1-D)
    !-------------------------
    status = nf90_inq_varid( ncdfID, 'numobs', varID )
    status = nf90_get_var( ncdfID, varID, nn )

   !------------------------
   ! Allocate data arrays
   !-------------------------
   ALLOCATE( lat( nn ) )
   ALLOCATE( lon( nn ) )
   ALLOCATE( height( nn ) )      !PRESSUE
   ALLOCATE( cwpQC( nn ) )
   ALLOCATE( cwp_err( nn ) )
   ALLOCATE( cbp( nn ) )
   ALLOCATE( ctp( nn ) )
   ALLOCATE( obkind( nn ) )
   ALLOCATE( utime( 1 ) )

   !------------------------
   ! Get useful data arrays
   !-------------------------
   ! LAT
   status = nf90_inq_varid( ncdfID, 'lat', varID )
   status = nf90_get_var( ncdfID, varID, lat )

   ! LON
   status = nf90_inq_varid( ncdfID, 'lon', varID )
   status = nf90_get_var( ncdfID, varID, lon )

   ! PRESSURE HEIGHT (mb) (CEP with mods)
   status = nf90_inq_varid( ncdfID, 'pressure', varID )
   status = nf90_get_var( ncdfID, varID, height )

   ! CWP VALUE (kg / m2)
   status = nf90_inq_varid( ncdfID, 'cwp', varID )
   status = nf90_get_var( ncdfID, varID, cwpQC )

   ! CWP OBSERVATION ERROR
   status = nf90_inq_varid( ncdfID, 'cwp_err', varID )
   status = nf90_get_var( ncdfID, varID, cwp_err )

   ! Cloud Base Pressure (mb)
   status = nf90_inq_varid( ncdfID, 'cbp', varID )
   status = nf90_get_var( ncdfID, varID, cbp )

   ! Cloud Top Pressure (mb)
   status = nf90_inq_varid( ncdfID, 'ctp', varID )
   status = nf90_get_var( ncdfID, varID, ctp )

   ! Cloud Phase / Day - Night Flag
   ! 0 = CWP=zero DAY; 1 = LWP DAY; 2 = IWP DAY; 3 = CWP=zero NIGHT; 4 = LWP NIGHT; 5 = IWP NIGHT
   status = nf90_inq_varid( ncdfID, 'phase', varID )
   status = nf90_get_var( ncdfID, varID, obkind )

   ! TIME
   status = nf90_inq_varid( ncdfID, 'time', varID )
   status = nf90_get_var( ncdfID, varID, utime )

  ! CLOSE NETCDF FILE
  status = nf90_close( ncdfID )

  !-Obtain analysis time in minutes since reference date
  sec70 = 252460800  ! seconds since from 01/01/1970


  call w3fs21(iadatemn,mins_an)  !mins_an -integer number of mins snce 01/01/1978
  rmins_an=mins_an             !convert to real number


  ! LOOP THROUGH ALL OBSERVATIONS IN FILE  
  do i = 1, nn

       rmins_ob = ( utime(1) - sec70 )/60

       timeb = rmins_ob-rmins_an

       !if(abs(timeb/60.0) > abs(time_window)) then
       !  numbadtime=numbadtime+ione
       !  cycle
       !end if
       
       if( cwpQC(i) >= minobrange .and. cwpQC(i) < maxobrange ) then
     
          if ( cwpQC(i) == 0.0) then
            num_clear    = num_clear + ione
            !cycle   !Temp to not read in CWP=0 obs
          endif
       else
         num_missing    = num_missing + ione
         cycle
       end if

       thishgt = height(i) ! unit : Pa
       hgt     = thishgt

       thislon = lon(i)
       thislat = lat(i)
  
       !-Check format of longitude and correct if necessary
       if(thislon>=r360) thislon=thislon-r360
       if(thislon<zero ) thislon=thislon+r360
                 
       !-Convert back to radians                 
       thislat = thislat*deg2rad
       thislon = thislon*deg2rad
                 
       !find grid relative lat lon locations of earth lat lon
                 
       call tll2xy(thislon,thislat,dlon,dlat,outside)
       if (outside) cycle
          
                                           !If observation is outside the domain
                                           ! then cycle, but don't increase range right away.
                                           ! Domain could be rectangular, so ob may be out of
                                           ! range at one end, but not the other.		     					                   		   		   
       !thiserr = sqrt(cwp_err(i))    !CWP INPUT IN ERROR FORM ALREADY
       thiserr = cwp_err(i)               
 
       ! for 0 cwp values, set vertical coordinate level to 500 hPa
       !if ( cwpQC(i) < 0.001 ) then
       !   thishgt = 500_r_kind
       !endif

       nread = nread + ione
       icntpnt=icntpnt+1
       ndata =ndata+1
       nodata=nodata+1
       iout=ndata
       isort(icntpnt)=iout


!####################  OUTPUT TO BINARY FILE    ##################
            dumvar = -99.9
            
            cdata_all(1,iout) = thiserr                       ! observation error
            cdata_all(2,iout) = dlon                          ! grid relative longitude
            cdata_all(3,iout) = dlat                          ! grid relative latitude
            cdata_all(4,iout) = thishgt                       ! obs height (hPa) middle of cloud
            cdata_all(5,iout) = cwpQC(i)                      ! CWP QC 
            cdata_all(6,iout) = dumvar                        ! dummy
            cdata_all(7,iout) = timeb*r60inv                  ! obs time (analyis relative hour)
            cdata_all(8,iout) = ikx                           ! type		   
            cdata_all(9,iout) = cbp(i)                        ! cloud base pressure (Pa)
            cdata_all(10,iout)= ctp(i)                        ! cloud top pressure (Pa)
            cdata_all(11,iout)= obkind(i)                     ! kind of CWP obs (LWP, IWP, OR ZERO)
            cdata_all(12,iout)= icuse(ikx)                    ! usage parameter
            cdata_all(13,iout)= thislon*rad2deg               ! earth relative longitude (degrees)
            cdata_all(14,iout)= thislat*rad2deg               ! earth relative latitude (degrees)
            cdata_all(15,iout)= dumvar                        ! dummy 
            cdata_all(16,iout)= thiserr                       ! orginal error from data file
            cdata_all(17,iout)= dumvar                        ! dummy
            cdata_all(18,iout)= dumvar                        ! dummy 

            !print*, cdata_all(2,iout), cdata_all(3,iout), cdata_all(4,iout), cdata_all(5,iout), cdata_all(9,iout),  cdata_all(10,iout), cdata_all(11,iout)

     end do    ! k

  987 continue      
!  if (.not. use_all) then 
!     deallocate(zl_thin) 
!     call del3grids
!  endif

!---all looping done now print diagnostic output
  write(6,*)'READ_CWP: Reached eof on CWP file'
  write(6,*)'READ_CWP: # read in obs. number               =',nread
  write(6,*)'READ_CWP: # obs outside time window           =',numbadtime
  write(6,*)'READ_CWP: # of cloud free obs                 =',num_clear
  write(6,*)'READ_CWP: # of bad data                       =',num_missing

!---Write observation to scratch file---!
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs) 
  write(lunout) obstype,sis,maxdat,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=ione,maxdat),i=ione,ndata)
  
  !---------------DEALLOCATE ARRAYS-------------!
  deallocate(cdata_all)

 else  !fileopen
  write(6,*) 'READ_CWP: ERROR OPENING CWP FILE: ',trim(infile)
 end if fileopen

314 continue

print* ,'FINISHED WITH READ_CWP'

end subroutine read_cwp
