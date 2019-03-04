subroutine read_dlbufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full, nobs)
    !$$$  subprogram documentation block

!   subprogram: read_dlbufr  read dlbufr data (u,v and w) which is in prepbufr format
!    prgmmr: jjh               date: 2016-12-20

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
    use deter_sfc_mod, only: deter_sfc2
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
    integer(i_kind) lunin,k,n,maxobs,levs
    integer(i_kind) nc
    integer(i_kind) ihh,idd,idate,iret,im,iy
    integer(i_kind) kx,nchanl,nreal,ilat,ilon
    integer(i_kind) cat
    integer(i_kind) nmsg                ! the number of message
    integer(i_kind) iyyyymm
    integer(i_kind),dimension(5):: idate5
    integer(i_kind)  pqm,wqm,zqm
    integer(i_kind) minobs,minan
    integer(i_kind) idomsfc

    real(r_kind) toff,t4dv,timeobs,time,zeps
    real(r_kind) usage
    real(r_kind) uob,vob,wob,zob
    real(r_kind) woe,dlat,dlon,dlat_earth,dlon_earth
    real(r_kind) elev
    real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
    real(r_kind),allocatable,dimension(:,:):: cdata_all
    real(r_kind) time_correction
    real(r_kind) tsavg,ff10,sfcr,zz

    real(r_double) rstation_id
    real(r_double),dimension(6):: hdr
    real(r_double),dimension(2,mxlv):: qcmark,obserr
    real(r_double),dimension(5,mxlv):: obsdat
    real(r_double),dimension(1,mxlv):: levdat


    data hdstr /'SID XOB YOB DHR TYP ELV'/ 
    data levstr /'ZOB'/
    data obstr /'CAT ZOB UOB VOB WOB'/
    data qcstr /'WQM ZQM'/
    data oestr /'WOE ZOE'/

    data lunin / 62 /

    write(6,*) 'READ_DLBUFR: INSIDE THE READ_DLBUFR'
    ! Initialize variables
    nreal = 25

!   check the obs in convinfo only
    do nc=1,nconvtype
!        write(6,*) 'READ_DLBUFR: nc=',nc,'obstype=',obstype,'ioctype(nc)=',ioctype(nc),'ictype(nc)=',ictype(nc)
        ! obstype uvw in convinfo file, type =298 and use flag should be 1
        if( trim(obstype) == trim(ioctype(nc)) .and. trim(obstype) == 'dlw' .and. icuse(nc)==1) then
            kx = ictype(nc) ! obs type
            ! nc is the index of this obs(obstype,and ictype(nc)) in convinfo file
            write(6,*) 'READ_DLBUFR: kx =',kx,', nc= ',nc
            exit
        else if (nc == nconvtype) then
            write(6,*) 'READ_DLBUFR: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USERFLAG IS NOT EQUAL TO 1'
            write(6,*) 'READ_DLBUFR: ABORT READING DLBUFR, NO OBS IS READ IN!'
            return
        end if
    end do

    write(6,*) 'READ_DLBUFR: FINISHING READING CONVINFO CHECK'

    ! set usage parameter
    usage = zero

    ! start reading the dlbufr file, some basic check and get observation amount
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
    nmsg = 0 ! total number of messages in dlbufr file 
    disterrmax = -9999.0_r_kind
 
    msg_report:  do while(ireadmg(lunin,subset,idate) == 0)
                if(subset == 'DLWNDR') then
                      nmsg = nmsg+1
                else
                      cycle msg_report
                endif
                !    Time offset
                if(nmsg == 1) call time_4dvar(idate,toff)

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
        write(6,*) 'READ_DLBUFR: ERROR - NO DATA HAS BEEN READ! EXIT'
        return
    endif

    write(6,*)'READ_DLBUFR: messages = ',nmsg,' maxobs = ',nread

    ! organize the data into an array for further processing, consistent with setupw.f90
    allocate(cdata_all(nreal,nread))
    cdata_all = 0 
 
    open(lunin,file=trim(infile),form='unformatted')
    call openbf(lunin,'IN',lunin)
    call datelen(10)
 
    msg_loop:  do while(ireadmg(lunin,subset,idate) == 0)
               if(subset /= 'DLWNDR') then
                    cycle msg_loop
               endif
                levs = 0
                subset_loop: do while(ireadsb(lunin) == 0)
                                call ufbint(lunin,hdr,6,1,iret,hdstr)
                                call ufbint(lunin,obsdat,5,mxlv,levs,obstr) !obs
                                call ufbint(lunin,qcmark,2,mxlv,levs,qcstr) !qm
                                call ufbint(lunin,obserr,2,mxlv,levs,oestr) !oerr 
                                
                                if (levs <=0) then
                                    cycle subset_loop
                                endif

                                if(kx/=hdr(5)) then
                                    write(6,*) 'READ_DLBUFR: ERROR - TYPE IN BUFR FILE is ', hdr(5),', BUT INFO FILE IS', kx
                                    cycle subset_loop 
                                endif

                                if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) then
                                    write(6,*) 'READ_DLBUFR: ERROR - LAT AND LONG INVALID, LONG = ', hdr(2),', LAT = ', hdr(3)
                                    cycle subset_loop 
                                endif 
                                ! check the lat and long values
                                if(hdr(2) == r360) hdr(2)=hdr(2)-r360
                                if(hdr(2) < zero) hdr(2)=hdr(2)+r360
                                dlon_earth=hdr(2)*deg2rad
                                dlat_earth=hdr(3)*deg2rad
      
                                ! domain check
                                if(regional)then
                                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
                                    if(diagnostic_reg) then
                                        call txy2ll(dlon,dlat,rlon00,rlat00)
                                        cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                                              (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                                        cdist=max(-one,min(cdist,one))
                                        disterr=acos(cdist)*rad2deg
                                        disterrmax=max(disterrmax,disterr)
                                    endif
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
                                    idate5(5)=iadatemn(5) !JJH changed from iadate to iadatemn 2018/05/03 
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
                                !print*,'READ_DLBUFR:hdr(1:4)=',hdr(1),'-',hdr(2),'-',hdr(3),'-',hdr(3),'-',hdr(4),'timeobs=',timeobs,'toff=',toff,'t4dv=',t4dv,'time_correction=',time_correction,'timeobs=',timeobs,'time=',time,'iadatemn=',iadatemn,'ctwind(nc)=',ctwind(nc),'twindin =',twindin,'real(abs(time))=',real(abs(time)),'real(twindin)=',real(twindin)

                                if (l4dvar) then
                                   if ((t4dv<zero.OR.t4dv>winlen)) cycle subset_loop ! outside time window
                                else
                                   if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin))) then
                                      !print *,'READ_DLBUFR!!!!!!!!!!! outside time window'
                                      cycle subset_loop ! outside time window
                                   endif
                                endif

                                loop_obs:do k=1,levs
                                            wqm = qcmark(1,k)
                                            cat = obsdat(1,k)
                                            zob = obsdat(2,k)
                                            uob = obsdat(3,k)
                                            vob = obsdat(4,k)
                                            wob = obsdat(5,k)
                                            woe = obserr(1,k) ! wind obs error
                                            elev = hdr(6)
                                            rstation_id = hdr(1)
                                            !write(6,*) 'READ_DLBUFR: level,lon,lat,dhr,zob,uob,vob,wob,wqm,woe=',k,hdr(2),hdr(3),hdr(4),zob,uob,vob,wob,wqm,woe
                                            !set some simple check
                                            if(uob>1.0e+3_r_kind .or. vob>1.0e+3_r_kind .or. woe>1.0e+3_r_kind .or. woe<0.0_r_kind) then
                                                cycle loop_obs
                                            endif
                                            ! Get information from surface file necessary for conventional data here
                                            call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

                                            ndata = ndata+1

                                            cdata_all(1,ndata)=woe                     ! wind error
                                            cdata_all(2,ndata)=dlon                    ! grid relative longitude
                                            cdata_all(3,ndata)=dlat                    ! grid relative latitude
                                            cdata_all(4,ndata)=-999.0                  ! ln(pressure in cb), no observed pressure
                                            cdata_all(5,ndata)=zob                     ! height of observation (10m wind)
                                            cdata_all(6,ndata)=uob                     ! u obs
                                            cdata_all(7,ndata)=vob                     ! v obs
                                            cdata_all(8,ndata)=rstation_id             ! station id
                                            cdata_all(9,ndata)=t4dv                    ! time
                                            cdata_all(10,ndata)=nc                     ! type (index in convinfo file)
                                            cdata_all(11,ndata)=elev                   ! station elevation
                                            cdata_all(12,ndata)=wqm                    ! quality mark
                                            cdata_all(13,ndata)=woe                    ! original obs error
                                            cdata_all(14,ndata)=usage                  ! usage parameter
                                            cdata_all(15,ndata)=idomsfc                ! dominate surface type
                                            cdata_all(16,ndata)=tsavg                  ! skin temperature
                                            cdata_all(17,ndata)=ff10                   ! 10 meter wind factor
                                            cdata_all(18,ndata)= sfcr                  ! surface roughness
                                            cdata_all(19,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                                            cdata_all(20,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                                            cdata_all(21,ndata)=zz                     ! terrain height at ob location
                                            cdata_all(22,ndata)=0                      ! provider name
                                            cdata_all(23,ndata)=0                      ! subprovider name
                                            cdata_all(24,ndata)=cat                    ! cat
                                            cdata_all(25,ndata)=0                      ! non linear qc parameter
                                end do loop_obs

                end do subset_loop
    end do msg_loop
    !call closbf(lunin)
    !close(lunin)

    if (ndata == 0) then
        write(6,*)'READ_DLBUFR: NO DATA VALID'
        call closbf(lunin)
    endif

    nodata = nread-ndata
    print*, 'CALL COUNT DLBUFR:', ndata,nreal,ilat,ilon,nobs
    call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)

    ! write observation to scratch file
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((cdata_all(k,n),k=1,nreal),n=1,ndata)

    write(6,*) 'FINISHED READ_DLBUFR,nobstype,ndata,nodata,nobs=',obstype,ndata,nodata,nobs

    deallocate(cdata_all)
    close(lunin)
    close(55)

    write(*,*) 'READ_DLBUFR:FINISHED'
    ! End of routine
    return


end subroutine read_dlbufr

