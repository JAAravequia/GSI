subroutine read_aeribufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full, nobs)
    !$$$  subprogram documentation block

!   subprogram: read_aeribufr  read aeribufr thermodynamic profile (prepbufr format)
!    prgmmr: jjh               date: 2017-01-23 
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     prsl_full- 3d pressure on full domain grid
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of "obstype" observations that are in bad quality, no further processing
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator

    use kinds, only: r_single,r_kind,r_double,i_kind
    use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
      three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r10,r100,r2000
    use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
      tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
      rlats,rlons,twodvar_regional
    use convinfo, only: nconvtype,icuse,ictype,ioctype,ctwind
    use gsi_4dvar, only: l4dvar,time_4dvar,winlen
    use obsmod, only: iadatemn
    use obsmod, only: offtime_data,bmiss
    use obsmod, only: LH_err
    use deter_sfc_mod, only: deter_sfc2
    use dewpoint_obs_err_mod, only:dewpt_error_from_rh_and_temp,rh_error_from_dewpt_and_temp
    use met_mod, only: temp_and_dewpoint_to_rh
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
    real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
    real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
    character(80),parameter:: cspval= '88888888'

    integer(i_kind),parameter:: mxlv=255 ! max pressure levels

    ! Declare local variables
    logical outside

    character(40) hdstr,qcstr,oestr,obstr,levstr
    character(8) subset
    character(10) date

    integer(i_kind) ireadmg,ireadsb
    integer(i_kind) lunin,maxobs,levs
    integer(i_kind) nc,ihh,idd,idate,iret,k,n,im,iy
    integer(i_kind) kx,nchanl,nreal,ilat,ilon
    integer(i_kind) cat
    integer(i_kind) nmsg                ! number of message
    integer(i_kind) iyyyymm
    integer(i_kind),dimension(5):: idate5
    integer(i_kind) tqm,qqm
    integer(i_kind) minobs,minan
    integer(i_kind) idomsfc

    real(r_kind) toff,t4dv,timeobs,time,zeps
    real(r_kind) usage
    real(r_kind) poe,dlnpob,tdry
    real(r_kind) toe,qoe,tdoe,dlat,dlon,dlat_earth,dlon_earth
    real(r_kind) elev
    real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
    real(r_kind) qmaxerr
    real(r_kind),allocatable,dimension(:,:):: cdata_all
    real(r_kind) :: zob,tob,qob,pob,tdob
    real(r_kind) :: rh,rh_err,rep_t,rep_rh
    real(r_kind) time_correction
    real(r_kind) tsavg,ff10,sfcr,zz

    real(r_double) rstation_id
    real(r_double),dimension(6):: hdr
    real(r_double),dimension(4,mxlv):: qcmark,obserr
    real(r_double),dimension(6,mxlv):: obsdat 
    real(r_double),dimension(1,mxlv):: levdat

    data hdstr /'SID XOB YOB DHR TYP ELV'/ 
    data levstr /'ZOB'/
    data obstr /'CAT TOB QOB ZOB POB TDO'/
    data qcstr /'TQM QQM ZQM PQM'/
    data oestr /'TOE QOE ZOE POE'/

    data lunin / 61 /

    write(6,*) 'READ_AERIBUFR: INSIDE READ_AERIBUFR'


    ! check the obs in convinfo only
    do nc=1,nconvtype
!        write(6,*) 'READ_AERIBUFR: nc=',nc,'obstype=',obstype,'ioctype(nc)=',ioctype(nc),'ictype(nc)=',ictype(nc)
        ! obstype aerit OR aeriq in convinfo file, and use flag should be 1
        if( trim(obstype) == trim(ioctype(nc)) .and. icuse(nc)==1) then
            kx = ictype(nc) 
            ! nc is the index of this obs(obstype,and ictype(nc)) in convinfo file
            write(6,*) 'READ_AERIBUFR: kx =',kx,', nc= ',nc
            exit
        else if (nc == nconvtype) then
            write(6,*) 'READ_AERIBUFR: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USERFLAG IS NOT EQUAL TO 1'
            write(6,*) 'READ_AERIBUFR: ABORT READING AERIBUFR, NO OBS IS READ IN!'
            return
        end if
    end do

    write(6,*) 'READ_AERIBUFR: FINISHING READING CONVINFO CHECK' 
 
    ! Initialize variables
    if (obstype == 'aerit') then 
        nreal = 25 
    else if (obstype == 'aeriq') then 
        nreal = 26 
    else if (obstype == 'aeritd') then
        nreal = 26
    endif

    ! set usage parameter
    usage = zero

    !start reading the AERIbufr file, some basic check and get observation amount
    call closbf(lunin)
    open(lunin,file=trim(infile),form='unformatted')
    call openbf(lunin,'IN',lunin)
    call datelen(10)
    
    nchanl = 0
    ilon=2
    ilat=3
    nread = 0 ! total number of obs data
    ndata = 0 ! number of good obs data (processed further)
    nodata = 0 !number of data that will not be processed further
    nmsg = 0 ! total number of messages in dwlbufr file 
    disterrmax = -9999.0_r_kind
 
    msg_report:  do while(ireadmg(lunin,subset,idate) == 0)
                !write(6,*) subset
                if(subset == 'AERITR') then
                     nmsg = nmsg+1
                else
                     cycle msg_report
                endif
                !    Time offset
                if(nmsg == 1) call time_4dvar(idate,toff)

                !write(*,'(3a,i10)') 'subset=',subset,' cycle time=',idate 

                levs = 0 
                loop_report: do while(ireadsb(lunin) == 0)
                                call ufbint(lunin,hdr,6,1,iret,hdstr) 
                                call ufbint(lunin,levdat,1,mxlv,levs,levstr) !obs 
                                if(levs>0) then
                                      nread = nread+levs
                                endif
                end do loop_report
    end do msg_report
    call closbf(lunin)
    !close(lunin)

    if (nmsg==0 .OR. nread==0) then
        write(6,*) 'READ_AERIBUFR: ERROR - NO DATA HAS BEEN READ! EXIT'
    endif

    write(6,*)'READ_AERIBUFR: messages = ',nmsg,' maxobs = ',nread

    ! organize the data into an array for further processing
    allocate(cdata_all(nreal,nread)) 
    cdata_all = 0
 
    open(lunin,file=trim(infile),form='unformatted')
    call openbf(lunin,'IN',lunin)
    call datelen(10)
 
    msg_loop:  do while(ireadmg(lunin,subset,idate) == 0)
                if(subset/='AERITR') then 
                    cycle msg_loop
                end if
                levs = 0
                subset_loop: do while(ireadsb(lunin) == 0)
                                call ufbint(lunin,hdr,6,1,iret,hdstr)
                                call ufbint(lunin,obsdat,6,mxlv,levs,obstr) !obs
                                call ufbint(lunin,qcmark,4,mxlv,levs,qcstr) !qm
                                call ufbint(lunin,obserr,4,mxlv,levs,oestr) !oerr 

                                if(levs<=0) then
                                     cycle subset_loop
                                endif

                                if(kx/=hdr(5)) then
                                    write(6,*) 'READ_AERIBUFR: ERROR - TYPE IN BUFR FILE is ', hdr(5),', BUT INFO FILE IS', kx
                                    cycle subset_loop 
                                endif

                                if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) then
                                    write(6,*) 'READ_AERIBUFR: ERROR - LAT AND LONG INVALID, LONG = ', hdr(2),', LAT = ', hdr(3)
                                    cycle subset_loop 
                                endif 
                                ! check the lat and long values, the long should
                                ! be in range [0,360)
                                if(hdr(2) == r360) hdr(2)=hdr(2)-r360
                                if(hdr(2) < zero) hdr(2)=hdr(2)+r360 ! -180->0 converted to 180->360 by adding 360
                                dlon_earth=hdr(2)*deg2rad
                                dlat_earth=hdr(3)*deg2rad

                                ! domain check
                                if(regional) then
                                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
                                    if(diagnostic_reg) then
                                        call txy2ll(dlon,dlat,rlon00,rlat00)
                                        cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                                              (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                                        cdist=max(-one,min(cdist,one))
                                        disterr=acos(cdist)*rad2deg
                                        disterrmax=max(disterrmax,disterr)
                                    end if
                                    if(outside) cycle subset_loop ! check to see if outside regional domain
                                else
                                    dlat = dlat_earth
                                    dlon = dlon_earth
                                    call grdcrd1(dlat,rlats,nlat,1)
                                    call grdcrd1(dlon,rlons,nlon,1)
                                endif

                                if(offtime_data) then
!             in time correction for observations to account for analysis
!                      time being different from obs file time.
                                    write(date,'( i10)') idate
                                    read (date,'(i4,3i2)') iy,im,idd,ihh
                                    idate5(1)=iy
                                    idate5(2)=im
                                    idate5(3)=idd
                                    idate5(4)=ihh
                                    idate5(5)=0
                                    call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
                                    idate5(1)=iadatemn(1)
                                    idate5(2)=iadatemn(2)
                                    idate5(3)=iadatemn(3)
                                    idate5(4)=iadatemn(4)
                                    idate5(5)=iadatemn(5) ! JJH changed from iadate to iadatemn 2018/05/03 
                                    call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date

!             Add obs reference time, then subtract analysis time to get obs time relative to analysis
 
                                    time_correction=float(minobs-minan)*r60inv

                                else
                                    time_correction=zero
                                end if

                                timeobs=real(real(hdr(4),r_single),r_double)
                                t4dv=timeobs + toff

                                zeps=1.0e-8_r_kind
                                if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
                                if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
                                t4dv=t4dv + time_correction
                                time=timeobs + time_correction
                                !print*,'READ_AERIBUFR:hdr(1:4)=',hdr(1),'-',hdr(2),'-',hdr(3),'-',hdr(4),'toff=',toff,'t4dv=',t4dv,'time_correction=',time_correction,'timeobs=',timeobs,'time=',time,'iadatemn=',iadatemn,'levs=',levs,'ctwind(nc)=',ctwind(nc),'twindin=',twindin

                                if (l4dvar) then
                                   if ((t4dv<zero.OR.t4dv>winlen)) cycle subset_loop ! outside time window
                                else
                                   if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin))) then 
                                      !print *,'READ_AERIBUFR--------outside window'
                                      cycle subset_loop ! outside time window
                                   end if
                                endif
                         
                                loop_obs:do k=1,levs
                                            tqm = qcmark(1,k) 
                                            qqm = qcmark(2,k)
                                            cat = obsdat(1,k)
                                            tob = obsdat(2,k) ! C
                                            qob = obsdat(3,k) ! mg/kg
                                            zob = obsdat(4,k) ! m MSL
                                            pob = obsdat(5,k) ! mb  
                                            tdob = obsdat(6,k) ! C

                                            toe = obserr(1,k) ! C (or K, error does not matter)

                                            qoe = obserr(2,k)! RH error 0.0 - 1.0

                                            elev = hdr(6) ! m
                                            rstation_id = hdr(1)
                                            !if(obstype == 'aerit') then
                                               !write(6,*) 'READ_AERIBUFR: lon,lat,dhr,pob,tob,qob,tdob,toe,qoe=',hdr(2),hdr(3),hdr(4),pob,tob,qob,tdob,toe,qoe
                                            !endif
                                            ! missing value and simple check
                                            if(tob>1.0e+3_r_kind .or. qob>1.0e+6_r_kind .or. tdob>1.0e+3_r_kind .or. toe>100.0_r_kind .or. qoe>1.0_r_kind .or. toe<0.0_r_kind .or. qoe<0.0_r_kind) then
                                                cycle loop_obs
                                            endif
                                            ! before we start computing things based on the dewpoint,
                                            ! make sure it isn't larger than the air temp.  if it is
                                            ! more than a degree larger, skip it completely.  if it is
                                            ! less, set them equal and continue.
                                            if(tdob > tob) then
                                                if(tdob > tob+1.0_r_kind) then
                                                        cycle loop_obs
                                                else
                                                        tdob = tob
                                                endif
                                            endif

                                            ! convert temperature from C to K
                                            tob = tob + t0c  ! convert C to K

                                            ! convert dewpoint temperature from C to K
                                            tdob = tdob + t0c  ! convert C to K

                                            ! add the representation error
                                            call rep_err_t(pob,rep_t)
                                            call rep_err_rh(pob,rep_rh)
                                            toe = toe+rep_t
                                            rh_err = qoe+rep_rh/10.0
                                            ! obs error for dew point
                                            if(LH_err) then ! LH_err
                                                 call temp_and_dewpoint_to_rh(tob,tdob, rh)
                                                 if(rh<= 0.00_r_kind .or. rh > 1.00) then
                                                      cycle loop_obs
                                                 else
                                                      tdoe = dewpt_error_from_rh_and_temp(tob,rh)
                                                 endif
                                            else
                                                 ! set to be toe+1.5
                                                 tdoe = toe+1.5
                                            endif

                                            ! Convert pressure from MB to CB
                                            pob = pob*one_tenth ! convert mb to cb  
                                            dlnpob = log(pob) 

                                            ! convert water vapor from mg/kg to kg.kg
                                            qob = qob*1.e-06_r_kind ! convert mg/kg to kg/kg

                                            ! Get information from surface file necessary for conventional data here
                                            call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

                                            ndata = ndata+1

                                            if(obstype == 'aerit') then
                                                cdata_all(1,ndata)=toe                     ! temperature error, K
                                                cdata_all(2,ndata)=dlon                    ! grid relative longitude
                                                cdata_all(3,ndata)=dlat                    ! grid relative latitude
                                                cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
                                                cdata_all(5,ndata)=tob                     ! temperature ob, K
                                                cdata_all(6,ndata)=rstation_id             ! station id
                                                cdata_all(7,ndata)=t4dv                    ! time
                                                cdata_all(8,ndata)=nc                      ! type (index in convinfo file)
                                                cdata_all(9,ndata)=one                     ! 0:virtual temperature, 1: sensible temp 
                                                cdata_all(10,ndata)=tqm                    ! quality mark
                                                cdata_all(11,ndata)=toe                    ! original obs error, K
                                                cdata_all(12,ndata)=usage                  ! usage parameter
                                                cdata_all(13,ndata)=idomsfc                ! dominate surface type
                                                cdata_all(14,ndata)=tsavg                  ! skin temperature
                                                cdata_all(15,ndata)=ff10                   ! 10 meter wind factor
                                                cdata_all(16,ndata)=sfcr                   ! surface roughness
                                                cdata_all(17,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                                                cdata_all(18,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                                                cdata_all(19,ndata)=elev                   ! station elevation, m 
                                                cdata_all(20,ndata)=zob                    ! observation height, m 
                                                cdata_all(21,ndata)=zz                     ! terrain height at ob location, m
                                                cdata_all(22,ndata)=0                      ! provider name
                                                cdata_all(23,ndata)=0                      ! subprovider name
                                                cdata_all(24,ndata)=cat                    ! cat
                                                cdata_all(25,ndata)=0                      ! non linear qc for T
                                            else if (obstype == 'aeriq') then
                                                qmaxerr = 0.2
                                                !if(qoe>qmaxerr) then
                                                    !qoe = qmaxerr
                                                !end if
                                                tdry=(tob)/(one+fv*qob)
                                                cdata_all(1,ndata)=rh_err                  ! relative humdith error
                                                cdata_all(2,ndata)=dlon                    ! grid relative longitude
                                                cdata_all(3,ndata)=dlat                    ! grid relative latitude
                                                cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
                                                cdata_all(5,ndata)=qob                     ! q ob, kg/kg
                                                cdata_all(6,ndata)=rstation_id             ! station id
                                                cdata_all(7,ndata)=t4dv                    ! time
                                                cdata_all(8,ndata)=nc                      ! type (index in convinfo file) 
                                                cdata_all(9,ndata)=qmaxerr                 ! q max error (in relative humidity)
                                                cdata_all(10,ndata)=tdry                    ! dry temperature, K (obs is tv) 
                                                cdata_all(11,ndata)=qqm                    ! quality mark
                                                cdata_all(12,ndata)=rh_err                    ! original obs error (relative humidity error)
                                                cdata_all(13,ndata)=usage                  ! usage parameter
                                                cdata_all(14,ndata)=idomsfc                ! dominate surface type
                                                cdata_all(15,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                                                cdata_all(16,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                                                cdata_all(17,ndata)=elev                   ! station elevation, m 
                                                cdata_all(18,ndata)=zob                    ! observation height, m 
                                                cdata_all(19,ndata)=zz                     ! terrain height at ob location, m
                                                cdata_all(20,ndata)=0                      ! provider name
                                                cdata_all(21,ndata)=0                      ! subprovider name
                                                cdata_all(22,ndata)=cat                    ! cat
                                                cdata_all(23,ndata)=0                      ! non linear qc for q
                                            else if(obstype == 'aeritd') then !
                                                ! virtual temp flag: 1=Sensible temp; 0=virtual temp
                                                cdata_all(1,ndata)=tdoe! dew point temperature error, K
                                                cdata_all(2,ndata)=dlon! grid relative longitude
                                                cdata_all(3,ndata)=dlat! grid relative latitude
                                                cdata_all(4,ndata)=dlnpob! ln(pressure in cb)
                                                cdata_all(5,ndata)=tdob! dew point temperature ob, K
                                                cdata_all(6,ndata)=rstation_id! station id
                                                cdata_all(7,ndata)=t4dv! time
                                                cdata_all(8,ndata)=nc! type (index in convinfo file)
                                                cdata_all(9,ndata)=one! sensible temp 
                                                cdata_all(10,ndata)=tqm! quality mark
                                                cdata_all(11,ndata)=tdoe! original obs error, K
                                                cdata_all(12,ndata)=usage! usage parameter
                                                cdata_all(13,ndata)=idomsfc ! dominate surface type
                                                cdata_all(14,ndata)=tsavg ! skin temperature
                                                cdata_all(15,ndata)=ff10 ! 10 meter wind factor
                                                cdata_all(16,ndata)=sfcr ! surface roughness
 
                                                cdata_all(17,ndata)=dlon_earth*rad2deg! earth relative longitude (degrees)
                                                cdata_all(18,ndata)=dlat_earth*rad2deg! earth relative latitude (degrees)
                                                cdata_all(19,ndata)=elev! station elevation, m 
                                                cdata_all(20,ndata)=zob! observation height, m 
                                                cdata_all(21,ndata)=zz! terrain height at ob location, m
                                                cdata_all(22,ndata)=0! provider name
                                                cdata_all(23,ndata)=0! subprovider name
                                                cdata_all(24,ndata)=cat! cat
                                                cdata_all(25,ndata)= tob ! air temperature
                                                cdata_all(26,ndata)= 0 !non linear qc for td 
                                                !print *,'READ_AERIBUFR:pres,tob,tdob,toe,tdoe=',pob*10.0,tob,tdob,toe,tdoe
                                           endif
                                end do loop_obs

                end do subset_loop
    end do msg_loop
    !call closbf(lunin)
    !close(lunin)

    if (ndata == 0) then
        write(6,*)'READ_AERIBUFR: NO DATA VALID'
        !call closbf(lunin)
    endif

    nodata = nread-ndata

    ! write observation to scratch file
    print*, 'CALL COUNT AERIBUFR:', ndata,nreal,ilat,ilon,nobs
    call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((cdata_all(k,n),k=1,nreal),n=1,ndata)

    deallocate(cdata_all)
    call closbf(lunin)
    close(lunin)
    close(55)

    write(6,*) 'FINISHED READ_AERIBUFR,nobstype,ndata,nodata,nobs=',obstype,ndata,nodata,nobs
    ! End of routine
    return

end subroutine read_aeribufr

! define the representation error for t, according to pressure levels
subroutine rep_err_t(presw,rep_err)
           use kinds, only: r_kind

           real(r_kind), intent(in) :: presw  ! mb
           real(r_kind), intent(out) :: rep_err ! C/K

           if(presw>=1075.0)  then ! 1100 mb select the closest level 
              rep_err = 1.27 - 0.2
           elseif(presw>=1025.0 .and. presw<1075.0) then ! 1050mb
              rep_err = 1.33 - 0.2
           elseif(presw>=975.0 .and. presw<1025.0) then ! 1000mb
              rep_err = 1.39 - 0.2
           elseif(presw>=925.0 .and. presw<975.0) then ! 950mb
              rep_err = 1.44 - 0.2
           elseif(presw>=875.0 .and. presw<925.0) then ! 900mb
              rep_err = 1.44 - 0.2
           elseif(presw>=825.0 .and. presw<875.0) then ! 850mb
              rep_err = 1.37 - 0.2
           elseif(presw>=775.0 .and. presw<825.0) then ! 800mb
              rep_err = 1.26 - 0.2 
           elseif(presw>=725.0 .and. presw<775.0) then ! 750mb
              rep_err = 1.14 - 0.2
           elseif(presw>=675.0 .and. presw<725.0) then ! 700mb
              rep_err = 1.04 - 0.2 
           elseif(presw>=625.0 .and. presw<675.0) then ! 650mb
              rep_err = 0.98 - 0.2
           elseif(presw>=575.0 .and. presw<625.0) then ! 600mb
              rep_err = 0.95 - 0.2
           elseif(presw>=525.0 .and. presw<575.0) then ! 550mb
              rep_err = 0.93 - 0.2
           elseif(presw>=475.0 .and. presw<525.0) then ! 500mb
              rep_err = 0.92 - 0.2
           elseif(presw>=425.0 .and. presw<475.0) then ! 450mb
              rep_err = 0.92 - 0.2
           elseif(presw>=375.0 .and. presw<425.0) then ! 400mb
              rep_err = 0.94 - 0.2
           elseif(presw>=325.0 .and. presw<375.0) then ! 350mb
              rep_err = 0.98 - 0.2
           elseif(presw>=275.0 .and. presw<325.0) then ! 300mb
              rep_err = 1.07 - 0.2
           elseif(presw>=225.0 .and. presw<275.0) then ! 250mb
              rep_err = 1.18 - 0.2
           elseif(presw>=175.0 .and. presw<225.0) then ! 200mb
              rep_err = 1.29 - 0.2
           elseif(presw>=125.0 .and. presw<175.0) then ! 150mb
              rep_err = 1.35 - 0.2
           elseif(presw>=87.5 .and. presw<125.0) then ! 100mb
              rep_err = 1.38 - 0.2
           elseif(presw>=62.5 .and. presw<87.5) then ! 75mb
              rep_err = 1.38 - 0.2
           elseif(presw>=45.0 .and. presw<62.5) then ! 50mb
              rep_err = 1.35 - 0.2
           elseif(presw>=35.0 .and. presw<45.0) then ! 40mb
              rep_err = 1.31 - 0.2
           elseif(presw>=25.0 .and. presw<35.0) then ! 30mb
              rep_err = 1.30 - 0.2
           elseif(presw>=15.0 .and. presw<25.0) then ! 20mb
              rep_err = 1.34 - 0.2
           elseif(presw>=7.5 .and. presw<15.0) then ! 10mb
              rep_err = 1.40 - 0.2
           elseif(presw>=4.5 .and. presw<7.5) then ! 5mb
              rep_err = 1.45 - 0.2
           elseif(presw>=3.5 .and. presw<4.5) then ! 4mb
              rep_err = 1.47 - 0.2
           elseif(presw>=2.5 .and. presw<3.5) then ! 3mb
              rep_err = 1.49 - 0.2
           elseif(presw>=1.5 .and. presw<2.5) then ! 2mb
              rep_err = 1.50 - 0.2
           elseif(presw<1.5) then ! 0-1mb
              rep_err = 1.50 - 0.2
           endif
end subroutine rep_err_t

! define the representation error for rh, according to pressure levels
subroutine rep_err_rh(presw,rep_err)
           use kinds, only: r_kind

           real(r_kind), intent(in) :: presw  ! mb
           real(r_kind), intent(out) :: rep_err ! C/K

           if(presw>=1075.0)  then ! 1100 mb select the closest level 
              rep_err = 0.61 - 0.3 ! percent/10
           elseif(presw>=1025.0 .and. presw<1075.0) then ! 1050mb
              rep_err = 0.66 - 0.3
           elseif(presw>=975.0 .and. presw<1025.0) then ! 1000mb
              rep_err = 0.74 - 0.3
           elseif(presw>=925.0 .and. presw<975.0) then ! 950mb
              rep_err = 0.84 - 0.3
           elseif(presw>=875.0 .and. presw<925.0) then ! 900mb
              rep_err = 0.94 - 0.3
           elseif(presw>=825.0 .and. presw<875.0) then ! 850mb
              rep_err = 1.04 - 0.3
           elseif(presw>=775.0 .and. presw<825.0) then ! 800mb
              rep_err = 1.16 - 0.3
           elseif(presw>=725.0 .and. presw<775.0) then ! 750mb
              rep_err = 1.27 - 0.3
           elseif(presw>=675.0 .and. presw<725.0) then ! 700mb
              rep_err = 1.38 - 0.3
           elseif(presw>=625.0 .and. presw<675.0) then ! 650mb
              rep_err = 1.49 - 0.3
           elseif(presw>=575.0 .and. presw<625.0) then ! 600mb
              rep_err = 1.56 - 0.3
           elseif(presw>=525.0 .and. presw<575.0) then ! 550mb
              rep_err = 1.60 - 0.3
           elseif(presw>=475.0 .and. presw<525.0) then ! 500mb
              rep_err = 1.63 - 0.3
           elseif(presw>=425.0 .and. presw<475.0) then ! 450mb
              rep_err = 1.67 - 0.3
           elseif(presw>=375.0 .and. presw<425.0) then ! 400mb
              rep_err = 1.71 - 0.3
           elseif(presw>=325.0 .and. presw<375.0) then ! 350mb
              rep_err = 1.75 - 0.3
           elseif(presw>=275.0 .and. presw<325.0) then ! 300mb
              rep_err = 1.80 - 0.3
           elseif(presw>=225.0 .and. presw<275.0) then ! 250mb
              rep_err = 1.86 - 0.3
           elseif(presw>=175.0 .and. presw<225.0) then ! 200mb
              rep_err = 1.91 - 0.3
           elseif(presw>=125.0 .and. presw<175.0) then ! 150mb
              rep_err = 1.95 - 0.3
           elseif(presw>=87.5 .and. presw<125.0) then ! 100mb
              rep_err = 1.98 - 0.3
           elseif(presw>=62.5 .and. presw<87.5) then ! 75mb
              rep_err = 1.99 - 0.3
           elseif(presw<62.5) then ! 0-50mb
              rep_err = 2.00 - 0.3
           endif
end subroutine rep_err_rh
