subroutine read_amv(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
     prsl_full,nobs)
!
! subprogram: read_amv  read Atmospheric Motion Vector (AMV) observations from GOES-16
!
!
! program history log:
!
!   2018-05-14    Swapan Mallick :  Read GOES-16 AMV retrievals from netcdf file
!
! abstract:  Read and process GOES-16 AMV retrievals
!            observations in netcdf format.
!            it also has options to thin the data by using conventional
!            thinning programs
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!            For the satellite ID type  itype==240   c_prvstg='GOESR' ; c_sprvstg='IRSW'
!            For the satellite ID type  itype==245   c_prvstg='GOESR' ; c_sprvstg='IR'
!            For the satellite ID type  itype==246   c_prvstg='GOESR' ; c_sprvstg='WVCT'
!            For the satellite ID type  itype==247   c_prvstg='GOESR' ; c_sprvstg='WVCS'
!            For the satellite ID type  itype==251   c_prvstg='GOESR' ; c_sprvstg='VIS'
!            respectively
!            The quality mark:  QM, the values range from 0 to 15, 0-7 used, 8-15
!                              monitored, 0 is best, when the value greater than
!                              3, the observation error needed to be enflated.
!
! program history log:
!
!   input argument list:
!     infile   - file from which to read data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     gstime   - analysis time in minutes from reference date
!
!   output argument list:
!     nread    - number of AMV observations read
!     ndata    - number of AMV observations retained for further processing
!     nodata   - number of AMV observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!
! Variable Definitions:
!
!  cdata_all - real - dim(maxdat,maxobs) - array holding all data for assimilation
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
!  lon    - real - longitude of observation
!  lat    - real - latitude of observation
!  utime  - real - time for each observation point
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
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
  use obsmod, only: perturb_obs,perturb_fact,ran01dom,bmiss
  use convinfo, only: nconvtype, &
       icuse,ictype,icsubtype,ioctype, &
       ithin_conv,rmesh_conv,pmesh_conv,pmot_conv,ptime_conv, &
       use_prepb_satwnd

  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use mpimod, only: npe
  implicit none

  include 'netcdf.inc'
!
! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs
  integer(i_kind), parameter                           :: max_num_vars = 50, max_num_dims = 20
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

! Declare local variables
  logical outside,inflate_error
  logical luse,ithinp
  logical,allocatable,dimension(:,:):: lmsg     ! set true when convinfo entry id found in a message
  logical                           :: if_input_exist

  character str_date
  character(50) qcstr
  character(8) subset
  character(8) c_prvstg,c_sprvstg
  character(8) c_station_id,stationid
  character(12) str_date1
  character(20),dimension(max_num_vars) ::  var_list
  
  integer(i_kind) mxtb,nmsgmax
  integer(i_kind) ireadmg,ireadsb,iuse
  integer(i_kind) i,maxobs,idomsfc,nsattype,ncount
  integer(i_kind) nc,nx,isflg,itx,j,nchanl
  integer(i_kind) ntb,ntmatch,ncx,ncsave,ntread
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nmind,lunin,idate,ilat,ilon,iret,k
  integer(i_kind) nreal,ithin,iout,ntmp,icount,iiout,ii
  integer(i_kind) itype,iosub,iidx,iobsub,itypey,ierr
  integer(i_kind) qm
  integer(i_kind) nlevp               ! vertical level for thinning
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
!---SM
  integer(i_kind) maxout,maxdata,nlevz,icntpnt,length,rcode,cdfid
  integer(i_kind) yy1, mm1, dd1, hr1, mn1
  integer(i_kind) ivar, var_num, sec70
  integer(i_kind) natts, ivtype
  integer(i_kind), dimension(max_num_dims)              ::  dimids, one_read
  integer(i_kind), dimension(max_num_vars)              ::  id_var, ndims, istart
  integer(i_kind), dimension(max_num_vars, max_num_dims):: dims
  integer(i_kind), parameter                            :: maxdat=18_i_kind, ione = 1_i_kind

  real(r_kind) toff,t4dv
  real(r_kind) rmesh,ediff,usage,tdiff
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb,ppb2,qifn,qify,ee,ree
  real(r_kind) woe,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00,uob1,vob1
  real(r_kind) del,werrmin,obserr,ppb1,var_jb,wjbmin,wjbmax
  real(r_kind) tsavg,ff10,sfcr,sstime,gstime,zz
  real(r_kind) crit1,timedif,xmesh,pmesh,pmot,ptime
  real(r_kind),dimension(nsig):: presl
  
  real(r_double),dimension(13):: hdrdat
  real(r_double),dimension(4):: obsdat
  real(r_double),dimension(3,5) :: heightdat
  real(r_double),dimension(6,4) :: derdwdat
  real(r_double),dimension(3,12) :: qcdat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:):: rusage 
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out
  real(r_double) rstation_id
!----SM
  real(r_kind)                            :: celev,selev, dumvar, &
                                             thiserr,thislon,thislat,timeb
  real(r_kind), allocatable, dimension(:) :: cwpQC, data_r_1d, height,&
                                             lon, lat, utime, obkind
  real(r_kind), allocatable, dimension(:) :: UU, VV, P, T, LAT1, LON1, QC, &
                                             LZN, SZN, BID, TIME1

! equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg)
  equivalence(r_sprvstg(1,1),c_sprvstg)
  equivalence(rstation_id,c_station_id)

  data ithin / -9 /
  data lunin / 11 /
  data rmesh / -99.999_r_kind /

!----------- READ NC FILE-------------------------------------------
  WRITE(6,*)"GOES-16 AMVs Reading",infile

   nreal=25
   maxout=0
   maxdata=0
   isort=0
   icntpnt=0
!
  use_all=.true.
  var_list(1:11) = (/ "U        ", "V        ", "P        ", "T        ", "LON      ", &
                     "LAT      ", "QC       ", "LZN      ", "SZN      " , &
                     "BID      ", "TIME      " /)
  var_num       = 11
  print *, "AMV file open ",trim(infile)
  length     = len_trim(infile)
  inquire(file=infile(1:length), exist=if_input_exist)
!
  fileopen: if (if_input_exist) then
  rcode = nf_open( infile(1:length), NF_NOWRITE, cdfid )
  DO ivar = 1, var_num
       ! Check variable is in file, and get variable id:
       rcode = nf_inq_varid ( cdfid, var_list(ivar), id_var(ivar) )
       if ( rcode /= 0 ) then
          write(6,FMT='(A,A)') &
             var_list(ivar), ' variable is not in input file'
       end if
    !   Get number of dimensions, and check of real type:
        dimids = 0
        rcode = nf_inq_var( cdfid, id_var(ivar), var_list(ivar), ivtype, ndims(ivar), dimids, natts )
        if ( ivtype /= 6 ) then
           write(6,FMT='(A,A)') var_list(ivar), ' variable is not real type'
        end if
    !   Get dimensions of field:
        dims(ivar,:) = 0
        do i = 1, ndims(ivar)
           rcode = nf_inq_dimlen( cdfid, dimids(i), dims(ivar,i) )
        end do

   end do  ! ivar
!
  allocate( UU(dims(1,1)), VV(dims(2,1)), P(dims(3,1)), T(dims(4,1)), &
             LON1(dims(5,1)), LAT1(dims(6,1)),  QC(dims(7,1)), &
             LZN(dims(8,1)), SZN(dims(9,1)), BID(dims(10,1)),TIME1(dims(11,1)) )
    one_read = 1
!
    do ivar = 1, var_num
       allocate( data_r_1d(dims(ivar,1)) )
       call ncvgt( cdfid, id_var(ivar), one_read, dims(ivar,:), data_r_1d, rcode )
       if( ivar == 1 )then
         UU   = data_r_1d
       else if( ivar == 2 )then
         VV = data_r_1d
       else if( ivar == 3 )then
         P = data_r_1d
       else if( ivar == 4 )then
         T  = data_r_1d
       else if( ivar == 5 )then
         LON1    = data_r_1d
       else if( ivar == 6 )then
         LAT1  = data_r_1d
       else if( ivar == 7 )then
         QC  = data_r_1d
       else if( ivar == 8 )then
         LZN  = data_r_1d
       else if( ivar == 9 )then
         SZN  = data_r_1d
       else if( ivar == 10 )then
         BID  = data_r_1d
       else if( ivar == 11 )then
         TIME1  = data_r_1d
       endif
       deallocate( data_r_1d )
!
    end do
!
  ivar = 2
  maxobs=dims(2,1)
  nreal=25
  ntread=1
  rcode = nf_close(cdfid)
!
 else  !fileopen
  write(6,*) 'READ_AMV: ERROR OPENING AMV FILE: ',trim(infile)
 end if fileopen
!
! do i =1 , maxobs
!    write(845,'(11f9.3,2x,f15.0)'),i*1.0,BID(i)*1.0,LAT1(i),LON1(i),    &
!               P(i),T(i),UU(i),VV(i),QC(i),LZN(i)/100000.0,SZN(i),TIME1(i)*1.0
!  enddo
!
! read observation error table

  disterrmax=zero
  vdisterrmax=zero
  wjbmin=zero
  wjbmax=5.0_r_kind
  pflag=0
  var_jb=zero

! Set lower limits for observation errors
  werrmin=one
  nsattype=0
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
!-------------------
  do nc=1,nconvtype
     if( trim(ioctype(nc)) == 'uv' ) then
        ntmatch=ntmatch+1
        ntxall(ntmatch)=nc
        ithin=ithin_conv(nc)
        if(ithin > 0)then
           ntread=ntread+1
           ntx(ntread)=nc
        end if
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' READ_SATWND: no matching obstype found in obsinfo ',obstype
     return
  end if
      
!!
  mxtb=980383
  nmsgmax=17634
  toff = 7.0
  allocate(lmsg(nmsgmax,ntread),tab(mxtb,3),nrep(nmsgmax))
 
  lmsg = .false.
  tab=0
  nmsg=0
  nrep=0
  ntb =0
!
  allocate(cdata_all(nreal,maxobs),isort(maxobs),rusage(maxobs))
  isort = 0
  cdata_all=zero
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  rusage=101.0_r_kind

!!  read satellite winds one type a time
!!  ntread = 1 as only one type of satellite date GOES-16 

  loop_convinfo: do nx=1,ntread 
     use_all = .true.
     use_all_tm = .true.
     ithin=0
     if(nx >1) then
        nc=ntx(nx)
        ithin=ithin_conv(nc)
        if (ithin > 0 ) then
           rmesh=rmesh_conv(nc)
           pmesh=pmesh_conv(nc)
           pmot=pmot_conv(nc)
           ptime=ptime_conv(nc)
           if(pmesh > zero) then
              pflag=1
              nlevp=r1200/pmesh
           else
              pflag=0
              nlevp=nsig
           endif
           xmesh=rmesh
           if( ptime >zero) then
              use_all_tm = .false.
              ntime=6.0_r_kind/ptime                   !!  6 hour winddow
              call make3grids_tm(xmesh,nlevp,ntime)
           else
              use_all = .false.
              call make3grids(xmesh,nlevp)
           endif
           if (.not.use_all .or. .not.use_all_tm) then
              allocate(presl_thin(nlevp))
              if (pflag==1) then
                 do k=1,nlevp
                    presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
                 enddo
              endif
           endif
           write(6,*)'READ_SATWND: ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                   ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc,pmot,ptime
        endif
     endif

     ntb = 0
     nmsg = 0
     ncount=0
        loop_readsb: do i = 1, maxobs
           nmsg = nmsg+1
           ntb = ntb+1
           nc=tab(ntb,1)
           !----Check this line----if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb
           hdrdat=bmiss
           obsdat=bmiss
           heightdat=bmiss
           derdwdat=bmiss
           qcdat=bmiss
           iobsub=0
           itype=-1
           uob=bmiss
           vob=bmiss
           ppb=bmiss
           ppb1=bmiss
           ppb2=bmiss
           uob1=bmiss
           vob1=bmiss
           ee=r110
           qifn=r110
           qify=r110
           !----
           obsdat(1)  = 1.0                 ! HAMD (HEIGHT ASSIGNMENT METHOD) 
           obsdat(2)  = P(i)*100.0          ! Pressure in Pa
           obsdat(3)  = UU(i)               ! u-wind in m/s
           obsdat(4)  = VV(i)               ! v-wind in m/s
           ppb=obsdat(2)
           ! 
           hdrdat(1) = 260                  ! GOES-16 SAID (SATELLITE IDENTIFIER) 
           hdrdat(2) = LAT1(i)              ! LATITUDE(COARSE ACCURACY) 
           hdrdat(3) = LON1(i)              ! LONGITUDE (COARSE ACCURACY)
           hdrdat(9) = 1                    ! SWCM  = 1 or 2 SATELLITE DERIVED WIND CALCULATION METHOD (SWCM) 
           hdrdat(10) = LZN(i)              ! SATELLITE ZENITH ANGLE  
           hdrdat(11) = 160.00              ! OGCE (ORIGINATING/GENERATING CENTER)  This is from BUFR file from other satellite
           hdrdat(12) = 28037400000000.00   ! SCCF (SATELLITE CHANNEL CENTER FREQUECY)
           hdrdat(13) = 2.0                 ! SWQM (SDMEDIT SATELLITE WIND QUALITY MARK)
           !
!   Compare relative obs time with window.  If obs 
!   falls outside of window, don't use this obs
           if (ppb > 100000000.0_r_kind .or. hdrdat(3) >100000000.0_r_kind &
            .or. obsdat(4) > 100000000.0_r_kind) cycle loop_readsb
           if(ppb >r10000) ppb=ppb/r100
           if (ppb <r125) cycle loop_readsb    !  reject data above 125mb
           !call w3fs21(idate5,nmind)
           nmind = TIME1(i)
           t4dv = real((nmind-iwinbgn),r_kind)*r60inv
           sstime = real(nmind,r_kind) 
           tdiff=(sstime-gstime)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle loop_readsb 
           else
              if (abs(tdiff)>twind) cycle loop_readsb 
           endif
           iosub=0
           if(abs(hdrdat(2)) >r90 ) cycle loop_readsb 
           if(hdrdat(3) <zero) hdrdat(3)=hdrdat(3)+r360
           if(hdrdat(3) == r360) hdrdat(3)=hdrdat(3)-r360
           if(hdrdat(3) >r360) cycle loop_readsb 
           qm=2
           iobsub=int(hdrdat(1))
           write(stationid,'(i3)') iobsub

           ! assign types and get quality info : start
              if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! the range of NESDIS satellite IDS 
                 c_prvstg='GOES16'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
!
                 if(BID(i) == 2.0)  then                          ! visible winds -- 0.64mm
                   itype=251
                   iidx=72
                   c_station_id='VI'//stationid
                   c_sprvstg='GOESR'
                 else if(BID(i) == 7.0) then                      ! IR Shortwave winds Wavelenght -- 3.9mm
                   itype=240
                   iidx=63                                        ! Original iidx=63 is for itype=241
                   c_station_id='IRSW'//stationid
                   c_sprvstg='GOESR'
                 else if(BID(i) == 8.0) then                      ! Upper level Tropospheric WV -- 6.2mm
                   itype=246
                   iidx=67
                   c_station_id='WV1'//stationid
                   c_sprvstg='GOESR'
                 else if(BID(i) == 9.0) then                      ! Mid-Level Tropospheric WV -- 6.9mm
                   itype=247
                   iidx=68
                   c_station_id='WV2'//stationid
                   c_sprvstg='GOESR'
                 else if(BID(i) == 10.0) then                     ! Lower-Level Tropospheric WV -- 7.3mm
                   itype=248
                   iidx=69
                   c_station_id='WV3'//stationid
                   c_sprvstg='GOESR'
                 else if(BID(i) == 14.0) then                     ! IR Longwave Window Band -- 11.2mm
                   itype=245
                   iidx=66
                   c_station_id='IRLW'//stationid
                   c_sprvstg='GOESR'
                 endif
                 qcdat(1,:)=1.0
                 qcdat(2,:)=1.0
                 qcdat(3,:)=1.0
! get quality information
                 do j=1,8
                    if( qify <=r105 .and. qifn <r105 .and. ee < r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if( qcdat(2,j) == one .and. qifn >r105 ) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j) 
                       endif  
                    endif
                 enddo
!QI not applied to CAWV for now - may in the future
                 if(qifn <85.0_r_kind .and. itype /= 247)  then
                    qm=15
                 endif
                 if(wrf_nmm_regional) then
! Minimum speed requirement for CAWV of 8m/s for HWRF. 
! Tighten QC for 247 winds by removing winds below 450hPa
                    if(itype == 247 .and. obsdat(4) < 8.0_r_kind .and. ppb > 450.0_r_kind) then
                       qm=15
! Tighten QC for 240 winds by remove winds above 700hPa
                    elseif(itype == 240 .and. ppb < 700.0_r_kind) then
                       qm=15
! Tighten QC for 251 winds by remove winds above 750hPa
                    elseif(itype == 251 .and. ppb < 750.0_r_kind) then
                       qm=15
                    endif
                 else
! Minimum speed requirement for CAWV of 10m/s
                    if(itype == 247 .and. obsdat(4) < 10.0_r_kind)  then
                       qm=15
                    endif
                 endif
              endif
           ! assign types and get quality info : end

           if ( qify == zero) qify=r110
           if ( qifn == zero) qifn=r110
           if ( ee == zero)   ee=r110

           nread=nread+2
           dlon_earth_deg=hdrdat(3)
           dlat_earth_deg=hdrdat(2)
           dlon_earth=hdrdat(3)*deg2rad
           dlat_earth=hdrdat(2)*deg2rad
                              
!       If regional, map obs lat,lon to rotated grid.
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
           if(outside) cycle loop_readsb 
!
           uob=obsdat(3)
           vob=obsdat(4)
!  first to get observation error from PREPBUFR observation error table
           ppb=max(zero,min(ppb,r2000))
           if (njqc) then
              itypey=itype
              ierr=0
           else                         ! else use the ONE error table
              if(ppb>=etabl(itype,1,1)) k1=1
              do kl=1,32
                 if(ppb>=etabl(itype,kl+1,1).and.ppb<=etabl(itype,kl,1)) k1=kl
              end do
              if(ppb<=etabl(itype,33,1)) k1=33
              k2=k1+1
              ediff = etabl(itype,k2,1)-etabl(itype,k1,1)
              if (abs(ediff) > tiny_r_kind) then
                 del = (ppb-etabl(itype,k1,1))/ediff
              else
                 del = huge_r_kind
              endif
              del=max(zero,min(del,one))
              obserr=(one-del)*etabl(itype,k1,4)+del*etabl(itype,k2,4)
              obserr=max(obserr,werrmin)
           endif                    ! end of njqc

!  using Santek quality control method,calculate the original ee value:
!  NOTE: Up until GOES-R winds algorithm, EE (expected error, ee) is reported as percent 0-100% (the higher the ee, the better the wind quality)
              if(ee <r105) then
                 ree=(ee-r100)/(-10.0_r_kind)
                 if(obsdat(4) >zero) then
                    ree=ree/obsdat(4)
                 else
                    ree=two
                 endif
              else
                 ree=0.2_r_kind
              endif
              if( ppb >= 800.0_r_kind .and. ree >0.55_r_kind) then
                  qm=15
              else if (ree >0.8_r_kind) then
                  qm=15
              endif
!
!         Set usage variable
           usage = 0 
           iuse=icuse(nc)
           if(iuse <= 0)usage=r100
           if(qm == 15 .or. qm == 12 .or. qm == 9)usage=r100
! Get information from surface file necessary for conventional data here
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)
 
!!    process the thining procedure
                
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
           if(ithinp)then
!          Interpolate guess pressure profile to observation location
              klon1= int(dlon);  klat1= int(dlat)
              dx   = dlon-klon1; dy   = dlat-klat1
              dx1  = one-dx;     dy1  = one-dy
              w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
              klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
              if (klon1==0) klon1=nlon
              klatp1=min(nlat,klat1+1); klonp1=klon1+1
              if (klonp1==nlon+1) klonp1=1
              do kk=1,nsig
                 presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                           w10*prsl_full(klatp1,klon1 ,kk) + &
                           w01*prsl_full(klat1 ,klonp1,kk) + &
                           w11*prsl_full(klatp1,klonp1,kk)
              end do
 
 !         Compute depth of guess pressure layersat observation location
           end if
           dlnpob=log(one_tenth*ppb)  ! ln(pressure in cb)
           ppb=one_tenth*ppb         ! from mb to cb
 !         Special block for data thinning - if requested
           if (ithin > 0 .and. iuse >=0 .and. qm <4) then
              ntmp=ndata  ! counting moved to map3gridS
 !         Set data quality index for thinning
              if (thin4d) then
                 timedif = zero
              else
                 timedif=abs(t4dv-toff)
              endif
              if(itype == 243 .or. itype == 253 .or. itype == 254) then
                 if(qifn <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else if(itype == 245 .or. itype == 246) then
                 if(qifn <r105 .and. ee <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33+(one-ee/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else
                 crit1 = timedif/r6+half
              endif
              if (pflag==0) then
                 do kk=1,nsig
                    presl_thin(kk)=presl(kk)
                 end do
              endif
              if (ptime >zero ) then
                 itime=int((tdiff+three)/ptime)+1
                 if(pmot <one) then
                    call map3grids_tm(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                                        ppb,itime,crit1,ndata,iout,ntb,iiout,luse,.false.,.false.)
                    if (.not. luse) cycle loop_readsb
                    if(iiout > 0) isort(iiout)=0
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    rusage(iout)=usage
                    isort(ntb)=iout
                 else
                    call map3grids_m_tm(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                              ppb,itime,crit1,ndata,iout,ntb,iiout,luse,maxobs,usage,rusage,.false.,.false.)
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                 endif
              else 
                 if(pmot <one) then
                    call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                                 ppb,crit1,ndata,iout,ntb,iiout,luse,.false.,.false.)
                    if (.not. luse) cycle loop_readsb
                    if(iiout > 0) isort(iiout)=0
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                    rusage(iout)=usage
                 else
                    call map3grids_m(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                              ppb,crit1,ndata,iout,ntb,iiout,luse,maxobs,usage,rusage,.false.,.false.)
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                 endif
              endif
           else
              ndata=ndata+1
              nodata=nodata+2
              iout=ndata
              isort(ntb)=iout
              rusage(iout)=usage
           endif
           inflate_error=.false.
           if (qm==3 .or. qm==7) inflate_error=.true.
           woe=obserr
           if (inflate_error) woe=woe*r1_2
           if(regional)then
              u0=uob
              v0=vob
              call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlon,dlat)
              if(diagnostic_reg) then
                 call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlon,dlat)
                 nvtest=nvtest+1
                 disterr=sqrt((u0-u00)**2+(v0-v00)**2)
                 vdisterrmax=max(vdisterrmax,disterr)
              end if
           endif
!------SWAPAN OBSERR Change here---
           !obserr = 2.0
           if(itype .eq. 251)then    ! For Visibility
            obserr = 1.5
           else
            obserr = 2.0
           endif
           woe=obserr
           usage = 0.0 
           cdata_all(1,iout)=woe                  ! wind error
           cdata_all(2,iout)=dlon                 ! grid relative longitude
           cdata_all(3,iout)=dlat                 ! grid relative latitude
           cdata_all(4,iout)=dlnpob               ! ln(pressure in cb)
           cdata_all(5,iout)=ee                   !  quality information 
           cdata_all(6,iout)=uob                  ! u obs
           cdata_all(7,iout)=vob                  ! v obs 
           cdata_all(8,iout)=hdrdat(1)            ! GOES-16 SAID (SATELLITE IDENTIFIER) hdrdat(1) = 270
           cdata_all(9,iout)=t4dv                 ! time
           cdata_all(10,iout)=iidx               ! index of type in convinfo file
           cdata_all(11,iout)=qifn +1000.0_r_kind*qify   ! quality mark infor  
           cdata_all(12,iout)=qm                  ! quality mark
           cdata_all(13,iout)=obserr              ! original obs error
           cdata_all(14,iout)=usage               ! usage parameter
           cdata_all(15,iout)=idomsfc             ! dominate surface type
           cdata_all(16,iout)=tsavg               ! skin temperature
           cdata_all(17,iout)=ff10                ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                ! surface roughness
           cdata_all(19,iout)=dlon_earth_deg      ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth_deg      ! earth relative latitude (degrees)
           cdata_all(21,iout)=zz                  ! terrain height at ob location
           cdata_all(22,iout)=r_prvstg(1,1)       ! provider name
           cdata_all(23,iout)=r_sprvstg(1,1)      ! subprovider name
           cdata_all(25,iout)=var_jb              ! non linear qc parameter
        enddo  loop_readsb
!
  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg,tab,nrep)

  ! Write header record and data to output file for further processing
  allocate(iloc(ndata))
  icount=0
  do i=1,maxobs
     if(isort(i) > 0)then
        icount=icount+1
        iloc(icount)=isort(i)
     end if
  end do
  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     itx=iloc(i)
     do k=1,13
        cdata_out(k,i)=cdata_all(k,itx)
     end do
     cdata_out(14,i)=cdata_all(14,i)
     !cdata_out(14,i)=rusage(itx)  ! Check the line
     do k=15,nreal
        cdata_out(k,i)=cdata_all(k,itx)
     end do
  end do
  deallocate(iloc,isort,cdata_all,rusage)
  
  call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) cdata_out

  deallocate(cdata_out)
  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_SATWND:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_SATWND:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax

! End of routine
  return

end subroutine read_amv
