subroutine read_goesimgasr(mype,val_img,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_goesimg                    read goes imager data
!   prgmmr: su, xiujuan      org: np23                date: 2002-02-26
!
! abstract:  This routine reads GOES imager radiance (brightness
!            temperature) files.  Optionally, the data are thinned to
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2017-07-26  Nebuda - SIMTB based on read_goesimg, read binary file with all sky radiance 
!                        not cloud cleared. 
!
!   input argument list:
!     mype     - mpi task id
!     val_img  - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     nrec_start - first subset with useful information
!
!   output argument list:
!     nread    - number of BUFR GOES imager observations read
!     ndata    - number of BUFR GOES imager profiles retained for further processing
!     nodata   - number of BUFR GOES imager observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind, r_single
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,txy2ll,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,one,rad2deg,r60inv,r60
  use obsmod, only: bmiss
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,thin4d
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter, nst_gsi, nstinfo
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin,nrec_start
  integer(i_kind) ,intent(inout) :: ndata,nodata
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_img
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical         ,intent(in   ) :: dval_use

! Declare local parameters
  integer(i_kind),parameter:: nimghdr=13
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical outside,iuse,assim

  character(8) subset

  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,next
  integer(i_kind) nmind,lnbufr,idate,ilat,ilon,maxinfo
  integer(i_kind) ireadmg,ireadsb,iret,nreal,nele,itt
  integer(i_kind) itx,i,k,isflg,kidsat,n,iscan,idomsfc
  integer(i_kind) idate5(5)
  integer(i_kind),allocatable,dimension(:)::nrec

! integer(i_kind) nreal_bin4, nobs_bin4
  integer*4 nreal_bin4, nobs_bin4 
  integer(i_kind) istatus

  real(r_kind) dg2ew,sstime,tdiff,t4dv,sfcr
  real(r_kind) dlon,dlat,timedif,crit1,dist1
  real(r_kind) dlon_earth,dlat_earth
  real(r_kind) pred
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_single),allocatable,dimension(:):: data_bin4
! for clarity using named local variables
  real(r_kind) :: tb_asr(3)


  real(r_double),dimension(nimghdr) :: hdrgoesarr       !  goes imager header
  real(r_double),dimension(3,6) :: dataimg              !  goes imager data

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest

  logical :: allchnmiss

! like sevasr using -> xzhang 10/8/2015
  real(r_kind) :: clat,clon
  real(r_kind):: earth_radius,sat_height,lat_center,lon_center !!! satellite position
  real(r_kind):: cosgeo,geox,scan_dis,azimu,satzen,temp1,temp2
  real(r_kind) :: utc_hour,sun_zenith,sun_azimuth !!!,cldfrac
  integer(i_kind) :: doy,mon,m,nnn
  integer(i_kind),dimension(12):: mlen,mday

! For solar zenith/azimuth angles calculation
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/

!**************************************************************************
! Initialize variables

!should make this a separate routine
  m = 0
  do mon=1,12
     mday(mon) = m
     m = m + mlen(mon)
  end do
  earth_radius=6378.0_r_kind
!x  sat_height=36000.0_r_kind
  sat_height=35800.0_r_kind
! 35786 WMO OSCAR

! 31 + 4 channels
  maxinfo=35
  lnbufr = 10
  disterrmax=zero
  ntest=0
  dg2ew = r360*deg2rad

  ilon=3
  ilat=4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

  ndata=0
  nodata=0
  nchanl=4       ! the channel number 2,3,4,5
! nchanl_fields = 1 
  ilath=8        ! the position of latitude in the header
  ilonh=9        ! the position of longitude in the header
  ilzah=10       ! satellite zenith angle
  iszah=11       ! solar zenith angle

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_img=zero


! Make thinning grids
  call makegrids(rmesh,ithin)


! Open bin4 file.
  istatus=-1
  open(lnbufr,file=trim(infile),form='unformatted',access='sequential', &
       convert='little_endian',err=9999)
  istatus=0
  read (lnbufr,err=9998) nreal_bin4, nobs_bin4
  istatus=1

  if (allocated(data_bin4)) deallocate(data_bin4)
  allocate(data_bin4(nreal_bin4))

! Allocate arrays to hold all data for given satellite
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

  lon_center=0. !position of satellite
  if(jsatid == 'g13') then
     kidsat = 257
     lon_center=-75.0 !position of satellite
  endif
  if(jsatid == 'g15') then
     kidsat = 259
     lon_center=-135.0 !position of satellite
  endif

  next=0
  nrec=999999
  irec=0

! Big loop over bin4 file - treat like all obs in 1 message
! do not know how parallel read is partitioned
! read_msg: do while(IREADMG(lnbufr,subset,idate) >= 0)
!    irec=irec+1
!    if(irec < nrec_start)cycle read_msg
!    next=next+1
!    if(next == npe_sub)next=0
!    if(next /= mype_sub)cycle

!    read_loop: do while (IREADSB(lnbufr) == 0)
        read_loop: do nnn = 1, nobs_bin4

!       Read through each record
        read (lnbufr) data_bin4

        if(nint(data_bin4(2)) /= kidsat) cycle read_loop

!compute sat zenith
          clat=data_bin4(4)
          clon=data_bin4(3)
          if(clon > 180_r_kind) clon = clon-360.0_r_kind
          lat_center=0
          cosgeo=cos(clat*deg2rad)*cos(lat_center*deg2rad)* &
                 cos((clon-lon_center)*deg2rad)+ &
                 sin(clat*deg2rad)*sin(lat_center*deg2rad)
          cosgeo=max(-1.0_r_kind,min(1.0_r_kind,cosgeo))
          geox = acos(cosgeo)
          scan_dis=earth_radius*sin(geox)
          azimu=atan(scan_dis/(earth_radius*(1-cosgeo)+sat_height))
          satzen=geox/deg2rad+azimu/deg2rad

! 5     idate5(1) = hdr(2)     ! year
! 6     idate5(2) = hdr(3)     ! month
! 7     idate5(3) = hdr(4)     ! day
! 8     idate5(4) = hdr(5)     ! hours
! 9     idate5(5) = hdr(6)     ! minutes
! 10 seconds                            

          !!!solar zenith angle
           utc_hour=real(data_bin4(8),r_kind)+real(data_bin4(9),r_kind)*r60inv+real(data_bin4(10),r_kind)*r60inv*r60inv
          doy = mday( int(data_bin4(6)) ) + int(data_bin4(7))
          if ((mod( int(data_bin4(5)),4)==0).and.( int(data_bin4(6)) > 2))  then
           doy = doy + 1
          end if

          call zensun(doy,utc_hour,clat,clon,sun_zenith,sun_azimuth) ! output solar zenith angles are between -90 and 90
          sun_zenith = 90.-sun_zenith                ! make sure solar zenith angles are between 0 and 180

! seviri allowed bigger angle
!         if ( satzen > r65 ) cycle read_loop
          if ( satzen > r60 ) cycle read_loop

!       Convert obs location from degrees to radians
        if (clon>=r360) clon=clon-r360
        if (clon< zero) clon=clon+r360

        dlon_earth=clon*deg2rad
        dlat_earth=clat*deg2rad

!       If regional, map obs lat,lon to rotated grid.
        if(regional)then

!          Convert to rotated coordinate.  dlon centered on 180 (pi), 
!          so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)

           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,dlon00,dlat00)
              ntest=ntest+1
              cdist=sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00))
              cdist=max(-one,min(cdist,one))
              disterr=acos(cdist)*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if

!          Check to see if in domain.  outside=.true. if dlon_earth,
!          dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!       Global case
        else
           dlon=dlon_earth
           dlat=dlat_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
        idate5(1) = data_bin4(5)     ! year
        idate5(2) = data_bin4(6)     ! month
        idate5(3) = data_bin4(7)     ! day
        idate5(4) = data_bin4(8)     ! hours
        idate5(5) = data_bin4(9)     ! minutes

        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(hdrgoesarr(7),r_kind)*r60inv)*r60inv
        sstime = real(nmind,r_kind) + real(hdrgoesarr(7),r_kind)*r60inv
        tdiff=(sstime-gstime)*r60inv

        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

        if (thin4d) then
           crit1=0.01_r_kind
        else
           timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
           crit1=0.01_r_kind+timedif
        endif
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if(.not. iuse)cycle read_loop

        nread=nread+nchanl

        do n=1,3
          tb_asr(n) = data_bin4(10+n) + 273.15 ! FIX THIS
        enddo

! CHECK THIS 3 or nchanl
        allchnmiss=.true.
        do n=1,3  ! only 3 IR channels read
           if(tb_asr(n)<500.)  then
              allchnmiss=.false.
           end if
        end do
        if(allchnmiss) cycle read_loop


!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                         

        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
            ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)


!       Set common predictor parameters

        crit1=crit1+rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Set data quality predictor 
!SIMTB no thinning so clear sky preference does not matter
        pred =(10.0_r_kind-dataimg(2,1)/10.0_r_kind)+dataimg(3,3)*10.0_r_kind  ! clear sky and
                                                                 ! bt std as quality indicater

!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+pred 
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse) cycle read_loop

!       Map obs to grids
        if(hdrgoesarr(1) == 256_r_double) then
           dataimg(1,5)=dataimg(1,6)              ! use  brightness tem. 6 not 5
           dataimg(3,5)=dataimg(3,6)              ! use BT tem. var. 6 not 5 
        endif
        iscan = nint(hdrgoesarr(ilzah))+1.001_r_kind ! integer scan position
        
!
!       interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
        if ( nst_gsi > 0 ) then
           tref  = ts(0)
           dtw   = zero
           dtc   = zero
           tz_tr = one
           if ( sfcpct(0) > zero ) then
              call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
           endif
        endif

!       Transfer information to work array
        data_all( 1,itx) = data_bin4(1)               ! satellite id
        data_all( 2,itx) = t4dv                       ! analysis relative time
        data_all( 3,itx) = dlon                       ! grid relative longitude
        data_all( 4,itx) = dlat                       ! grid relative latitude
        data_all( 5,itx) = satzen*deg2rad             ! satellite zenith angle (radians)
        data_all( 6,itx) = bmiss
        data_all( 7,itx) = bmiss                      ! clear sky amount - unknown
        data_all( 8,itx) = iscan                      ! integer scan position
        data_all( 9,itx) = sun_zenith                 ! solar zenith angle
        data_all(10,itx) = bmiss
        data_all(11,itx) = sfcpct(0)                  ! sea percentage of
        data_all(12,itx) = sfcpct(1)                  ! land percentage
        data_all(13,itx) = sfcpct(2)                  ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                  ! snow percentage
        data_all(15,itx)= ts(0)                       ! ocean skin temperature
        data_all(16,itx)= ts(1)                       ! land skin temperature
        data_all(17,itx)= ts(2)                       ! ice skin temperature
        data_all(18,itx)= ts(3)                       ! snow skin temperature
        data_all(19,itx)= tsavg                       ! average skin temperature
        data_all(20,itx)= vty                         ! vegetation type
        data_all(21,itx)= vfr                         ! vegetation fraction
        data_all(22,itx)= sty                         ! soil type
        data_all(23,itx)= stp                         ! soil temperature
        data_all(24,itx)= sm                          ! soil moisture
        data_all(25,itx)= sn                          ! snow depth
        data_all(26,itx)= zz                          ! surface height
        data_all(27,itx)= idomsfc + 0.001_r_kind      ! dominate surface type
        data_all(28,itx)= sfcr                        ! surface roughness
        data_all(29,itx)= ff10                        ! ten meter wind factor
        data_all(30,itx)= dlon_earth*rad2deg          ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth*rad2deg          ! earth relative latitude (degrees)

        if(dval_use)then
           data_all(36,itx) = val_img
           data_all(37,itx) = itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref         ! foundation temperature
           data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
        endif

!       Transfer observation location and other data to local arrays

!       do k=1,nchanl
! nreal = maxinfo + nstinfo
        k = 1  ! do not have channel 2 info
!          data_all(k+31,itx)=bmiss
           data_all(k+nreal,itx)=bmiss
        do k=2,nchanl ! fill channel 3,4,5
           data_all(k+nreal,itx)=tb_asr(k-1)
!          data_all(k+31+nchanl,itx)=0.1 ! hardwired standard dev to pass QC
        end do
        nrec(itx)=irec

     enddo read_loop
! enddo read_msg

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

! If no observations read, jump to end of routine.

  do n=1,ndata
     do k=1,nchanl
        if(data_all(k+nreal,n) > tbmin .and. &
           data_all(k+nreal,n) < tbmax)nodata=nodata+1
    end do
  end do
  if(dval_use .and. assim)then
     do n=1,ndata
       itt=nint(data_all(37,n))
       super_val(itt)=super_val(itt)+val_img
     end do
  end if

! Write final set of "best" observations to output file
  call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

! Deallocate local arrays
  deallocate(data_all,nrec)

! Deallocate satthin arrays
900 continue
  call destroygrids
  call closbf(lnbufr)

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_GOESIMG:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

9998 continue
     if (istatus == 0) then
       write(6,*) 'READ_GOESIMG: Problems reading file ',trim(infile)
     endif
9999 continue
     if (istatus == -1) then
       write(6,*) 'READ_GOESIMG: Problems opening file ',trim(infile)
     endif

  return
end subroutine read_goesimgasr
