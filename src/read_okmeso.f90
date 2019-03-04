subroutine read_okmeso(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,prsl_full, nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_okmeso                read Oklahoma mesonet obs from *mdf files
!   prgmmr: jones          org: cimms                date: 2016-12-28
!
! abstract:  This routine reads conventional data found in the OK mesonet
!            *mdf files.  Specific observation types read by this routine 
!            include surface pressure, temperature, winds (components
!            and speeds)
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! Mesonet observations are station lists at a common time with observations in
! the following order:
! stid (A4), stnm (I3), time (I4), relh (I4), tair (F6.1), wspd (F6.1), wvec
! (F6.1), wdir (I3), wdsd (F6.1), wssd (F6.1), wmax (F6.1), rain (F7.2), pres
! (F7.2), srad (I4), ta9m (F6.1), ws2m (F6.1), ts10 (F6.1), tb10 (F6.1), ts05
! (F6.1), tb05 (F6.1), ts30 (F6.1), tr05 (F6.1), tr25 (F6.1), tr60 (F6.1)
! first 3 lines are header, date info drawn from middle line

! program history log:
!           
!   input argument list:
!     infile   - file from which to read data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!
!   output argument list:
!     nread    - number of observations read
!     ndata    - number of observations retained for further processing
!     nodata   - number of observations retained for further processing
!     sis      - satellite/instrument/sensor indicator


  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
      three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r10,r100,r2000
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
      tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
      rlats,rlons,twodvar_regional
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use convinfo, only: nconvtype,ctwind,cgross,icuse,ioctype
  use mpimod, only: npe  

 implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full
  integer(i_kind) ,dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_75 = 0.75_r_kind
  real(r_kind),parameter:: r0_7 = 0.7_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r3_33= three + one/three
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r20  = 20.0_r_kind
  real(r_kind),parameter:: r50  = 50.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r999 = 999.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-6_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind
  
  character(80),parameter:: cspval= '88888888'
  integer(i_kind),parameter:: maxobs=300
  integer(i_kind),parameter:: nmsgmax=100000 ! max message count
  
  
  ! Declare local variables
  logical outside,convobs,inflate_error
  logical luse
  integer(i_kind)  maxmesoobs, nbad, usage, iunit, y4, m2, d2, h2, n2, s2, time, stnm, mx, iout
  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,next, tobs, ilon, ilat, relh, wdir
  integer(i_kind) nreal, k, n, i, ikx, nr, idomsfc
  real(r_kind) toe, qoe, woe, poe, tdoe
  real(r_kind) qtflg, tair, wspd, wvec,  wdsd, wssd, wmax, rain, pres, t4dv, dlnpob, dewpt
  real(r_kind) tmpk, tdry, qmaxerr, qminerr, alti, qvap, uwind, vwind, relh2, melev, wdir2
  real(r_kind) dlon,dlat,timedif,crit1,dist1, mlon, mlat
  real(r_kind) dlon_earth,dlat_earth, thislon, thislat
  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat0, rlon00, rlat00
  real(r_kind) tsavg,ff10,sfcr,zz
  character(4) stid
  character(80) header
  
  ! Data arrays
  real(r_kind), dimension(maxobs) :: oktemp, okdewpt, okqvap, okuwind, okvwind, okpres, oklon, oklat
  real(r_kind), dimension(maxobs) :: utime, okelev, okaltm
  real(r_kind),allocatable,dimension(:,:) :: cdata_all
  !integer(i_kind),allocatable,dimension(:):: nrec
  !character(10),  dimension(maxobs) :: okstn
  integer,  dimension(maxobs) :: okstn
    
     
  ikx=0
  write(6,*) 'READ_OK MESONET: START'
  do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
        ikx=i
        exit                                 !Exit loop when finished with initial convinfo fields     
     else if ( i==nconvtype ) then
        write(6,*) 'READ_OK MESONET: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USE FLAG IS ZERO'
        write(6,*) 'READ_OK MESONET: ABORTTING read_okmeso.f90 - NO OBS READ!'
        return
     endif
  end do  
    
  ! Initialize variables
  nreal=0
  if(obstype == 'okt')then
     nreal=25
  else if(obstype == 'okuv') then
     nreal=25
  else if(obstype == 'okspd') then
     nreal=24
  else if(obstype == 'okps') then
     nreal=20
  else if(obstype == 'okq') then
     nreal=23
  else if(obstype == 'oktd') then
     nreal=26 
 endif

!Define observation errors
    !toe = 1.0   !K
    !tdoe = 1.0   !K
    !qoe = 0.0125 ! Error in RH/100.0
    !qmaxerr = 0.20
    !qminerr = 0.0125
    !woe = 1.0  !m/s
    !poe = 0.75 !mb

! TAJ ERRORS
    toe = 1.75   !K
    tdoe = 1.5   !K
    qoe = 0.0125 ! Error in RH/100.0
    qmaxerr = 0.20
    !qminerr = 0.0125
    woe = 1.75  !m/s
    poe = 1.0 !mb

! Set usage variable              
    usage = zero
    
! Set unit variable               
    iunit = 60    

! ***** READ IN OK MESONET DATA
 open(unit=iunit, file = infile, status = 'old')
 read(iunit,*) ! header
! get the date info from the file
 read(iunit,'(A50)',END=200) header
 read(header,12) y4, m2, d2
12 format(5X,I4,2(X,I2))
 write(*,*) 'Date ', y4, m2, d2
! the time is gathered below - minutes since 00 UTC
 read(iunit,*) ! header  need date and time from here

! loop through all of the obs until reaching the end of the file
nread=0
nr=0
obsloop: do
!  do while (1 .eq. 1)
    read(iunit,22,END=200) stid, stnm, time, relh, tair, wspd, wvec, wdir, &
                           wdsd, wssd, wmax, rain, pres

22 format(1X,A4,3X,I3,2x,I4,3X,I4,3(1X,F6.1),2X,I4,3(1X,F6.1),1X,F7.2,2X,F7.2)

! Given the station id, get the lat, lon, and elevation
    call get_geo(stid,mlat,mlon,melev)   
! Convert temp to Kelvin    
   tmpk = tair + 273.15_r_kind
! Convert the wind speed and direction to u and v wind components, tair to K
    if (wspd .lt. 100.0_r_kind) then
      wdir2 = 1.0_r_kind*wdir
      call wind_dirspd_to_uv(wdir2, wspd, uwind, vwind)
      !print*, 'WIND CHECK: ',wdir2, wspd, uwind, vwind
    else
      uwind = -999.9
      vwind = -999.9
    endif
! Convert RH to Qvap  
    if (relh .gt. 0 .and. relh .le. 100 .and. tmpk .gt. 100_r_kind ) then
      relh2 = relh/(100.0_r_kind)  
      call rh_and_temp_to_mixing(relh2, tmpk, pres, qvap)
    else
      qvap = -999.9
    endif
! Convert RH to Dewpoint
    if (relh .gt. 0 .and. relh .le. 100 .and. tmpk .gt. 100_r_kind ) then
      relh2 = relh/(100.0_r_kind)  
      call rh_and_temp_to_dewpoint(relh2, tmpk, dewpt)
    else
      dewpt = -999.9
    endif     
    
! compute surface altimeter
    call compute_altimeter(pres, melev, alti)   ! pres hPa and elev in m
 
   
! convert time to UTC
    h2 = time/60
    n2 = time - h2*60
    tobs = h2*100 + n2
    s2 = 0

    
!write(*,*) stid, mlat, mlon, melev, tobs, relh, tair, wdir, uwind, vwind, pres, alti, qvap
!    nlon = -nlon  
    if ( mlon < 0.0 )  mlon = mlon + 360.0


!Save everything into arrays
    oktemp(nr) = tmpk
    okdewpt(nr) = dewpt
    okuwind(nr) = uwind
    okvwind(nr) = vwind
    okqvap(nr) = qvap
    okpres(nr) = pres/10.0  !convert hpa to cb
    utime(nr) = tobs
    okelev(nr) = melev
    !okstn(nr) = stid
    okstn(nr) = stnm
    okaltm(nr) = alti
    oklat(nr) = mlat
    oklon(nr) = mlon
    
    nr = nr+1
    nread = nread+1
  end do obsloop
200 continue
close(iunit) 

  
! ***** BIG LOOP THROUGH OBS
! loop over convinfo file entries; operate on matches

  allocate( cdata_all(nreal,nread) )
  cdata_all=zero

  disterrmax=-9999.0_r_kind
  nbad=0
  ndata=0
  nchanl=0
  ilon=2
  ilat=3

  loop_convinfo: do mx=0, nr
  
    !    TIME CHECK
         ! LATER
          t4dv = 0.0  !No time diff between model analysis and obs-time
  
    !     DOMAIN CHECK
    	    dlon_earth=oklon(mx)*deg2rad
            dlat_earth=oklat(mx)*deg2rad
    
            if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
                 if(diagnostic_reg) then
                    call txy2ll(dlon,dlat,rlon00,rlat00)
                    cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                         (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                    cdist=max(-one,min(cdist,one))
                    disterr=acos(cdist)*rad2deg
                    disterrmax=max(disterrmax,disterr)
                 end if
                 if(outside) cycle loop_convinfo   ! check to see if outside regional domain
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd1(dlat,rlats,nlat,1)
                 call grdcrd1(dlon,rlons,nlon,1)
              endif

  !         SIMPLE IF/THEN QC SANITY CHECK
            if ( oktemp(mx) .lt. 100.0 .or. okqvap(mx) .lt. 0.0 .or. ABS(okuwind(mx)) .gt. 100 ) then
                nbad=nbad+1
                write(6,*)'READ_OKMESO: BAD OB ', nbad
                cycle loop_convinfo
            endif
  
  ! Get information from surface file necessary for conventional data here
            call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

  !         Extract pressure level and quality marks
            dlnpob=log(okpres(mx))  ! ln(pressure in cb)   
  
               ndata = ndata+1
               iout=ndata
              
  !             Temperature
              if(obstype == 'okt') then
		 qtflg = 1 			!VIRTUAL TEMP FLAG: 1 = Sensible Temp; 0= Virt temp
                 cdata_all(1,iout)=toe                     ! temperature error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=oktemp(mx)               ! temperature ob.
                 cdata_all(6,iout)=okstn(mx)                ! station id
                 cdata_all(7,iout)=t4dv                ! time
                 cdata_all(8,iout)=ikx                       ! type
                 cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
                 cdata_all(10,iout)=-99.9                  ! quality mark
                 cdata_all(11,iout)=toe                    ! original obs error            
                 cdata_all(12,iout)=usage                  ! usage parameter
                 cdata_all(13,iout)=idomsfc                  ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                 ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                    ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=okelev(mx)              ! station elevation (m)
                 cdata_all(20,iout)=okelev(mx)              ! observation height (m)
                 cdata_all(21,iout)=zz                    ! terrain height at ob location
                 cdata_all(22,iout)=0         			   ! provider name
                 cdata_all(23,iout)=0                      ! subprovider name
                 cdata_all(24,iout)=0                      ! cat
                 cdata_all(25,iout)=0                      ! non linear qc JJH 
  
               else if(obstype == 'okuv') then
                 cdata_all(1,iout)=woe                     ! wind error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=okelev(mx)+10.0         ! height of observation (10 m wind)
                 cdata_all(6,iout)=okuwind(mx)             ! u obs
                 cdata_all(7,iout)=okvwind(mx)             ! v obs
                 cdata_all(8,iout)=okstn(mx)            ! station id
                 cdata_all(9,iout)=t4dv                    ! time
                 cdata_all(10,iout)=ikx                    ! type
                 cdata_all(11,iout)=okelev(mx)                 ! station elevation
                 cdata_all(12,iout)=0                 ! quality mark
                 cdata_all(13,iout)=woe            ! original obs error
                 cdata_all(14,iout)=usage                  ! usage parameter
                 cdata_all(15,iout)=idomsfc               ! dominate surface type
                 cdata_all(16,iout)=tsavg                  ! skin temperature
                 cdata_all(17,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(18,iout)=sfcr                    ! surface roughness
                 cdata_all(19,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(20,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=0          ! provider name
                 cdata_all(23,iout)=0         ! subprovider name
                 cdata_all(24,iout)=0            ! cat
                 cdata_all(25,iout)=0                      ! non linear qc JJH
                 
                else if(obstype == 'okps') then
                 !poe=obserr(1,k)*one_tenth                  ! convert from mb to cb
                 cdata_all(1,iout)=poe                     ! surface pressure error (cb)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude

                 cdata_all(4,iout)=okpres(mx)             ! pressure (in cb)

                 cdata_all(5,iout)=okelev(mx)             ! surface height
                 cdata_all(6,iout)=oktemp(mx)         ! surface temperature
                 cdata_all(7,iout)=okstn(mx)             ! station id
                 cdata_all(8,iout)=t4dv                    ! time
                 cdata_all(9,iout)=ikx                     ! type
                 cdata_all(10,iout)=0                 ! quality mark
                 cdata_all(11,iout)=poe*one_tenth  ! original obs error (cb)
                 cdata_all(12,iout)=usage                  ! usage parameter
                 cdata_all(13,iout)=sfcr                ! dominate surface type
                 cdata_all(14,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(15,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(16,iout)=okelev(mx)                ! station elevation (m)
                 cdata_all(17,iout)=zz                     ! terrain height at ob location
                 cdata_all(18,iout)=0        ! provider name
                 cdata_all(19,iout)=0         ! subprovider name
                 cdata_all(20,iout)=0                      ! non linear qc JJH  

                else if(obstype == 'okq') then
                 tdry=r999
                 !if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
                 cdata_all(1,iout)=qoe                     ! q error   
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=okqvap(mx)                  ! q ob
                 cdata_all(6,iout)=okstn(mx)             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=ikx                     ! type
                 cdata_all(9,iout)=qmaxerr                  ! q max error
                 cdata_all(10,iout)=tdry                   ! dry temperature (obs is tv)
                 cdata_all(11,iout)=0.0                 ! quality mark
                 cdata_all(12,iout)=qoe          ! original obs error
                 cdata_all(13,iout)=usage                  ! usage parameter
                 cdata_all(14,iout)=idomsfc              ! dominate surface type
                 cdata_all(15,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(16,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(17,iout)=okelev(mx)               ! station elevation (m)
                 cdata_all(18,iout)=okelev(mx)            ! observation height (m)
                 cdata_all(19,iout)=zz                    ! terrain height at ob location
                 cdata_all(20,iout)=0          ! provider name
                 cdata_all(21,iout)=0         ! subprovider name
                 cdata_all(22,iout)=0            ! cat
                 cdata_all(23,iout)=0                      ! non linear qc JJH               
 
                else if(obstype == 'oktd') then 
                 tdry=r999
                 cdata_all(1,iout)=tdoe                     ! temperature error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=okdewpt(mx)               ! temperature ob.
                 cdata_all(6,iout)=okstn(mx)                ! station id
                 cdata_all(7,iout)=t4dv                ! time
                 cdata_all(8,iout)=ikx                       ! type
                 cdata_all(9,iout)= qtflg                  ! temperature flag, 1 sensible, 0 virtual temp
                 cdata_all(10,iout)=0.0                 ! quality mark
                 cdata_all(11,iout)=tdoe          ! original obs error
                 cdata_all(12,iout)=usage                  ! usage parameter
                 cdata_all(13,iout)=idomsfc               ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                  ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                    ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=okelev(mx)              ! station elevation (m)
                 cdata_all(20,iout)=okelev(mx)              ! observation height (m)
                 cdata_all(21,iout)=zz                      ! terrain height at ob location
                 cdata_all(22,iout)=0                       ! provider name
                 cdata_all(23,iout)=0                       ! subprovider name              
                 cdata_all(24,iout)=0                        ! cat
                 cdata_all(25,iout)=oktemp(mx)               ! sensible air temperature
                 cdata_all(26,iout)=0                      ! non linear qc for
                endif
                 
                !print*, obstype, ndata,   cdata_all(1,iout), cdata_all(4,iout), oktemp(mx), okqvap(mx), okuwind(mx), okvwind(mx) 
                 
! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches                 
                 
                              
!---Write observation to scratch file---!
!  call count_obs(numobs,maxdat,ilat,ilon,cdata_all,nobs)
!print*, 'CALL COUNT OKMESO', ndata,nreal,ilat,ilon,nobs
  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,n),k=1,nreal),n=1,ndata)

900 continue

   write(6,*)'FINISH READ_OKMESO: ', obstype, ndata, nbad, nobs

! End of routine
  return

end subroutine read_okmeso                
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine get_geo(search_stid,omlat,omlon,elevf)

use 	kinds, only: r_single,r_kind,r_double,i_kind
  
implicit none

character(len=4), intent(in)  :: search_stid
real(r_kind), intent(out) :: omlat, omlon, elevf

! Given a station id, find the lat, lon and elevation from the geo file
! provided by the Oklahoma Mesonent website
!
! the geoinfo.csv file has data in the following order:
!
! stnm - station number
! stid - CHAR len=4
! name - CHAR of variable size
! city - CHAR of variable size
! rang - distance of station from city, float
! cdir - CHAR of variable size
! cnty - CHAR of variable size
! nlat - latitude, float
! nlon - longitude, float
! elev - elevation, float
! + other stuff we don't need
!

! temp vars
integer :: stnm, iunit2, elev
character(len=4) :: stid
character(len=15) :: name, city, cdir, cnty
real :: rang
character(len=330) :: line
integer :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, nend
logical :: stn_found

 stn_found = .false.
 iunit2 = 61
 open(unit=iunit2, file='geoinfo.csv', status='old')

 read(iunit2,*) ! header

  do while (1 .eq. 1)
! read through each line and look for a match 
 read(iunit2,'(A330)',END=200) line

! Ugh... a csv file with mixed var types is a 
! pain. Find the locations of delimiters and 
! assign vars as whatever is found between them. 
! Getting out to the 10th item gives us all we
! need. 
! Determine locations of the first 10 delimiters
n1  = index(line, ',')
nend = len_trim(line)
n2  = n1 + index(line(n1+1:nend), ',')
n3  = n2 + index(line(n2+1:nend), ',')
n4  = n3 + index(line(n3+1:nend), ',')
n5  = n4 + index(line(n4+1:nend), ',')
n6  = n5 + index(line(n5+1:nend), ',')
n7  = n6 + index(line(n6+1:nend), ',')
n8  = n7 + index(line(n7+1:nend), ',')
n9  = n8 + index(line(n8+1:nend), ',')
n10 = n9 + index(line(n9+1:nend), ',')

!write (*,*) 'index values ',n1, n2, n3
!
 read (line(1:n1-1),'(I3)') stnm
 read (line(n1+1:n2-1),'(A4)') stid
 read (line(n2+1:n3-1),'(A15)') name
 read (line(n3+1:n4-1),'(A15)') city
 read (line(n4+1:n5-1),'(f6.3)') rang
 read (line(n5+1:n6-1),'(A3)') cdir
 read (line(n6+1:n7-1),'(A15)') cnty
 read (line(n7+1:n8-1),'(f10.7)') omlat
 read (line(n8+1:n9-1),'(f10.7)') omlon
 read (line(n9+1:n10-1),'(I4)') elev
  elevf = 1.0_r_kind * elev
  if (search_stid .eq. stid) then
    !write(*,*) 'station found ',stid, omlat, omlon, elevf
    stn_found = .true.
    goto 200
  end if

  end do
200  continue
!  if (stn_found) then
!    write(*,*) ' done' 
!  else
!    write(*,*) ' station ',stid,' not found'
!  end if

close(iunit2) 
 
end subroutine get_geo
  
  
  
subroutine compute_altimeter(psfc, hsfc, altimeter)

use 	kinds, only: r_kind

real(r_kind), parameter :: k1 = 0.190284
real(r_kind), parameter :: k2 = 8.4228807E-5

real(r_kind), intent(in) :: psfc  !  (hPa)
real(r_kind), intent(in) :: hsfc  !  (m above MSL)

real(r_kind), intent(out) :: altimeter !  (hPa)

altimeter = ((psfc - 0.3) ** k1 + k2 * hsfc) ** (1.0 / k1)

end subroutine compute_altimeter  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   wind_dirspd_to_uv - subroutine that converts a wind direction and 
!                       wind speed to a zonal and meridional wind 
!                       component.
!
!    wdir - wind direction
!    wspd - wind speed (m/s)
!    uwnd - u component of the wind (m/s)
!    vwnd - v component of the wind (m/s)
!
!     created Oct. 2007 Ryan Torn, NCAR/MMM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine wind_dirspd_to_uv(wdir, wspd, uwnd, vwnd)

use 	kinds, only: r_kind

real(r_kind), intent(in)  :: wdir, wspd
real(r_kind), intent(out) :: uwnd, vwnd

real(r_kind), parameter :: PI = 3.14159265358979323846_r_kind
real(r_kind), parameter :: deg2rad = PI / 180.0_r_kind

uwnd =  wspd * cos(deg2rad * (90.0 + wdir))
vwnd = -wspd * sin(deg2rad * (90.0 + wdir))

!uwnd =  wspd * cos(deg2rad * (wdir))
!vwnd = -wspd * sin(deg2rad * (wdir))

end subroutine wind_dirspd_to_uv


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   sat_vapor_press_bolton - function that uses Bolton's approximation to
!                            compute saturation vapor pressure given
!                            temperature.
!
!   reference:  Bolton 1980, MWR, 1046-1053
!
!    sat_vapor_press_bolton - saturation vapor pressure (Pa)
!    tmpk                   - temperature (K)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sat_vapor_press_bolton(tmpk, sat_vapor_press)

use 	kinds, only: r_kind

real(r_kind), intent(out) :: sat_vapor_press
real(r_kind), intent(in) :: tmpk
real(r_kind)             :: tmpc               ! temperature (Celsius)
real(r_kind), parameter  :: es0C=611.0_r_kind  ! vapor pressure at 0 C (Pa) 
real(r_kind), parameter  :: Tfrez = 273.15_r_kind ! water freezing point (K)

tmpc = tmpk - Tfrez
if ( tmpc <= -200.0 ) then
  print*,'sat_vapor_press_bolton:  tmpc too low ',tmpc
  stop
end if
sat_vapor_press = es0C * exp( 17.67 * tmpc / (tmpc + 243.5) )


end subroutine sat_vapor_press_bolton

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   rh_and_temp_to_mixing - function that computes the dewpoint
!                             given relative humidity and temperature
!
!   qvapor                  - mixing ratio
!   rh                      - relative humidity (0.00 - 1.00)
!   tmpk                    - temperature (Kelvin)
!   inpres					- pressure (hPA)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine rh_and_temp_to_mixing(rh, tmpk, inpres, qvapor)

use 	kinds, only: r_kind

real(r_kind), intent(out) :: qvapor
real(r_kind), intent(in) :: rh
real(r_kind), intent(in) :: tmpk
real(r_kind), intent(in) :: inpres

real(r_kind)             :: e                  ! vapor pressure (Pa)
real(r_kind)             :: es                 ! saturation vapor pressure (Pa)

if ( ( rh <= 0.00 ) .or. ( rh > 1.00 ) ) then
  print*,'rh_and_temp_to_qvap:  bad rh ',rh
  stop
end if
if ( rh <= 0.01 ) then
  print*,'rh_and_temp_to_qvap: low rh ',rh
end if

call sat_vapor_press_bolton(tmpk, es)
e = rh * es

!Convert vapor pressure to mixing ratio (g/g)
qvapor =  0.622*(e / (inpres*100.0_r_kind)) 

end subroutine rh_and_temp_to_mixing
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   rh_and_temp_to_dewpoint - function that computes the dewpoint
!                             given relative humidity and temperature
!
!    rh_and_temp_to_dewpoint - dewpoint (Kelvin)
!    rh                      - relative humidity (0.00 - 1.00)
!    tmpk                    - temperature (Kelvin)
!
!     created Dec. 2008 David Dowell, NCAR/MMM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine rh_and_temp_to_dewpoint(rh, tmpk, dewpoint)

use 	kinds, only: r_kind

real(r_kind), intent(out) :: dewpoint
real(r_kind), intent(in) :: rh
real(r_kind), intent(in) :: tmpk

real(r_kind)             :: e                  ! vapor pressure (Pa)
real(r_kind)             :: es                 ! saturation vapor pressure (Pa)
real(r_kind)             :: dptc               ! dptc (Celsius)

real(r_kind), parameter  :: es0C=611.0_r_kind  ! vapor pressure at 0 C (Pa) 
real(r_kind), parameter  :: Tfrez = 273.15_r_kind ! water freezing point (K)

if ( ( rh <= 0.00_r_kind ) .or. ( rh > 1.00_r_kind ) ) then
  print*,'rh_and_temp_to_dewpoint:  bad rh ',rh
  stop
end if
if ( rh <= 0.01_r_kind ) then
  print*,'rh_and_temp_to_dewpoint: low rh ',rh
end if

call sat_vapor_press_bolton(tmpk, es)
e = rh * es

dptc = 243.5_r_kind / (17.67_r_kind / log(e/es0C) - 1.0_r_kind)

dewpoint = dptc + Tfrez

end subroutine  rh_and_temp_to_dewpoint   
