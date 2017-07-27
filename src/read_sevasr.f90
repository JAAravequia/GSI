subroutine read_sevasr(mype,val_sev,ithin,rmesh,jsatid,&
     gstime,infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sevasr                  read seviri bufr data
!   prgmmr: liu, haixia             org: np23                date: 2009-08-10
!
! abstract:  This routine reads BUFR format SEVIRI 1b radiance (brightness
!            temperature) files, which are bufrized from the NESDIS 1b data.  Optionally, the
!            data are thinned to a specified resolution using simple
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2017-07-25  Nebuda - SIMTB based on original bufr read read_seviri  
!                        process all sky seviri from real*4 binary file
!
!   input argument list:
!     mype     - mpi task id
!     val_sev  - weighting factor applied to super obs
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
!     nread    - number of BUFR SEVIRI 1b observations read
!     ndata    - number of BUFR SEVIRI 1b profiles retained for further processing
!     nodata   - number of BUFR SEVIRI 1b observations retained for further processing
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
  use constants, only: deg2rad,zero,one,rad2deg,r60inv
  use obsmod, only: offtime_data,bmiss
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: l4dvar,l4densvar,iadatebgn,iadateend,iwinbgn,winlen,thin4d
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter, nst_gsi, nstinfo
  use mpimod, only: npe
  use cloud_efr_mod, only: microphysics
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype,jsatid
  character(len=20),intent(in):: sis
  integer(i_kind),intent(in):: mype,lunout,ithin,nrec_start
  integer(i_kind),intent(inout):: ndata,nodata
  integer(i_kind),intent(inout):: nread
  integer(i_kind),dimension(npe),intent(inout):: nobs
  real(r_kind),intent(in):: rmesh,gstime,twind
  real(r_kind),intent(inout):: val_sev
  integer(i_kind),intent(in) :: mype_root
  integer(i_kind),intent(in) :: mype_sub
  integer(i_kind),intent(in) :: npe_sub
  integer(i_kind),intent(in) :: mpi_comm_sub
  logical        ,intent(in) :: dval_use

! Declare local parameters
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r65=65.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical outside,iuse,assim,clrsky,allsky

  character(8) subset,subcsr,subasr
  character(80):: hdrsevi             ! seviri header

  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,next
  integer(i_kind) nchanl_fields
  integer(i_kind) nmind,lnbufr,idate,ilat,ilon,nhdr,nchn,ncld,nbrst,jj,kk
  integer(i_kind) ireadmg,ireadsb,iret,nreal,nele,itt
  integer(i_kind) itx,i,k,isflg,kidsat,n,iscan,idomsfc
  integer(i_kind) idate5(5),maxinfo
  integer(i_kind),allocatable,dimension(:)::nrec
  integer(i_kind) nn, nnn
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
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_single),allocatable,dimension(:):: data_bin4
! for clarity using named local variables
  real(r_kind) :: tb_asr(11)
  real(r_kind) :: tb_clr(11)
  real(r_kind) :: tb_cld(11)
  real(r_kind) :: tb_hig(11)
  real(r_kind) :: tb_mid(11)
  real(r_kind) :: tb_low(11)

  real(r_kind) rclrsky
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest

  logical :: allchnmiss

!!!xzhang 10/8/2015
  real(r_kind) :: clat,clon
  real(r_kind):: earth_radius,sat_height,lat_center,lon_center !!! satellite position
  real(r_kind):: cosgeo,geox,scan_dis,azimu,satzen,temp1,temp2
  real(r_kind) :: utc_hour,sun_zenith,sun_azimuth !!!,cldfrac
  integer(i_kind) :: doy,mon,m
  integer(i_kind),dimension(12):: mlen,mday

! For solar zenith/azimuth angles calculation
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/


!**************************************************************************
! Initialize variables
   m = 0
  do mon=1,12
     mday(mon) = m
     m = m + mlen(mon)
  end do
  earth_radius=6378.0_r_kind
!x  sat_height=36000.0_r_kind
  sat_height=35800.0_r_kind
! 35786 WMO OSCAR 
!x  maxinfo=31
!  maxinfo=34
  lnbufr = 10
  disterrmax=zero
  ntest=0
  dg2ew = r360*deg2rad
  

!**************************************************************************
! Initialize variables
! maxinfo=31
!simtb_v4 added cloud info - 4 more fields 
! % cloud amount in 4x4 pixel segment: total, hig, mid, low 
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

! HLIU: NEED TO confirm
  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

  nread=0
  ndata=0
  nodata=0
  nchanl=8                     ! the channel number
  nchanl_fields=6              ! number of channel dependent fields to pass to setuprad


! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((trim(nusis(i))==trim(sis)) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_sev=zero

! Make thinning grids
  call makegrids(rmesh,ithin)

!-- Rewritten read of bin real*4 file
! Open bin4 file.
  istatus = -1
  open(lnbufr,file=trim(infile),form='unformatted',access='sequential', &
       convert='little_endian',err=9999)
  istatus = 0
  read (lnbufr,err=9998) nreal_bin4, nobs_bin4
  istatus = 1
  if (allocated(data_bin4)) deallocate(data_bin4)
  allocate(data_bin4(nreal_bin4))

! Allocate arrays to hold all data for given satellite
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
!SIMTB 6 channel dependent brightness temps
! all, clear, cloudy, high, mid, low
! nele  = nreal   + nchanl
  nele  = nreal   + nchanl_fields*nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))


  if(jsatid == 'm08') kidsat = 55
  if(jsatid == 'm09') kidsat = 56
  if(jsatid == 'm10') kidsat = 57

  nrec=999999
  irec=0
  next=0
! Big loop over bin4 file - treat like all obs in 1 message

        read_loop: do nnn = 1, nobs_bin4

!       Read through each record
        read (lnbufr) data_bin4

        if(nint(data_bin4(2)) /= kidsat) cycle read_loop

!compute sat zenith
          clat=data_bin4(4)
          clon=data_bin4(3)
          if(clon > 180_r_kind) clon = clon-360.0_r_kind
          lat_center=0
          lon_center=0.0 !position of satellite
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

          if ( satzen > r65 ) cycle read_loop

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
! 5     idate5(1) = hdr(2)     ! year
! 6     idate5(2) = hdr(3)     ! month
! 7     idate5(3) = hdr(4)     ! day
! 8     idate5(4) = hdr(5)     ! hours
! 9     idate5(5) = hdr(6)     ! minutes
! 10 seconds                            
        idate5(1) = data_bin4(5)     ! year
        idate5(2) = data_bin4(6)     ! month
        idate5(3) = data_bin4(7)     ! day
        idate5(4) = data_bin4(8)     ! hours
        idate5(5) = data_bin4(9)     ! minutes
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(data_bin4(10),r_kind)*r60inv)*r60inv
        sstime = real(nmind,r_kind) + real(data_bin4(10),r_kind)*r60inv
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

        rclrsky=bmiss
        if(data_bin4(11)>= zero .and. data_bin4(11) <= 100.0_r_kind ) then
           rclrsky=100.0_r_kind - data_bin4(11)  ! total  2d cloud percent
        end if


        do n=1,11  
          nn = 15+(n-1)*6
          tb_asr(n) = data_bin4(nn)
          tb_clr(n) = data_bin4(nn+1)
          tb_cld(n) = data_bin4(nn+2)
          tb_low(n) = data_bin4(nn+3)
          tb_mid(n) = data_bin4(nn+4)
          tb_hig(n) = data_bin4(nn+5)
        enddo
        allchnmiss=.true.
        do n=4,11  ! only 8 IR channels read
           if(tb_asr(n)<500.)  then
              allchnmiss=.false.
           end if
        end do
        if(allchnmiss) cycle read_loop

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

!       isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                         


        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
           ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

        crit1=crit1+rlndsea(isflg)
!       call checkob(dist1,crit1,itx,iuse)
!       if(.not. iuse)cycle read_loop


!       Set common predictor parameters
!test
        pred=zero
!test        
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

        crit1 = crit1+pred  
        call finalcheck(dist1,crit1,itx,iuse)

        if(.not. iuse)cycle read_loop

        iscan = nint(satzen)+1.001_r_kind ! integer scan position HLIU check this
 
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
        ndata=ndata+1
        data_all( 1,itx) = data_bin4(1)                     ! satellite id
        data_all( 2,itx) = t4dv                       ! analysis relative time
        data_all( 3,itx) = dlon                       ! grid relative longitude
        data_all( 4,itx) = dlat                       ! grid relative latitude
        data_all( 5,itx) = satzen*deg2rad         ! satellite zenith angle (radians)
        data_all( 6,itx) = bmiss                      ! satellite azimuth angle (radians)
        data_all( 7,itx) = rclrsky                    ! clear sky amount
        data_all( 8,itx) = iscan                      ! integer scan position
        data_all( 9,itx) = sun_zenith                 ! solar zenith angle
        data_all(10,itx) = bmiss                      ! solar azimuth angle
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
        data_all(30,itx) = dlon_earth*rad2deg         ! earth relative longitude (degrees)
        data_all(31,itx) = dlat_earth*rad2deg         ! earth relative latitude (degrees)
        data_all(32,itx) = data_bin4(11)              ! total 2d cloud amount in 4x4 pixel segment
        data_all(33,itx) = data_bin4(12)              ! low cloud amount 
        data_all(34,itx) = data_bin4(13)              ! mid cloud amount 
        data_all(35,itx) = data_bin4(14)              ! hig cloud amount 

        if(dval_use)then
!          data_all(32,itx) = val_sev
!          data_all(33,itx) = itt
           data_all(maxinfo-1,itx) = val_sev
           data_all(maxinfo,itx) = itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref         ! foundation temperature
           data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
        endif

        kk = nreal
        do k=1,nchanl
           jj=k+3  ! skip 3 vis channels
           kk=kk+1
           data_all(kk,itx)=tb_asr(jj)      ! all-sky radiance for chn 4,5,6,7,8,9,10,11
           kk=kk+1
           data_all(kk,itx)=tb_clr(jj)      ! clear
           kk=kk+1
           data_all(kk,itx)=tb_cld(jj)      ! cloud
           kk=kk+1
           data_all(kk,itx)=tb_low(jj)      ! low cloud
           kk=kk+1
           data_all(kk,itx)=tb_mid(jj)      ! mid cloud
           kk=kk+1
           data_all(kk,itx)=tb_hig(jj)      ! hig cloud
        end do
!       nrec(itx)=irec

!    End of satellite read block
     enddo read_loop
! enddo read_msg

  close(lnbufr)

! endif ! root pe check

! call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
!    nele,itxmax,nread,ndata,data_all,score_crit,nrec)

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

    do n=1,ndata
       do k=1,nchanl
          if(data_all(k+nreal,n) > tbmin .and. &
             data_all(k+nreal,n) < tbmax)nodata=nodata+1
       end do
    end do
    if(dval_use .and. assim)then
       do n=1,ndata
          itt=nint(data_all(33,n))
          super_val(itt)=super_val(itt)+val_sev
       end do
    end if

!   Write retained data to local file
    call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) nchanl_fields
    write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
  deallocate(data_all,nrec)
  deallocate(data_bin4)

! Deallocate satthin arrays
900 continue
  call destroygrids

! Print data counts
! write(6,9000) infile,sis,nread,rmesh,ndata
!000 format(' READ_SEVIRI:  infile=',a10,&
!       '   sis=',a20,&
!       '   nread=',i10, &
!       '   rmesh=',f7.3,'   ndata=',i10)

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_SEVIRI:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

9998 continue
     if (istatus == 0) then
       write(6,*) 'READ_SEVASR: Problems reading file ',trim(infile)
     endif
9999 continue
     if (istatus == -1) then
       write(6,*) 'READ_SEVASR: Problems opening file ',trim(infile)
     endif

! End of routine
  return
end subroutine read_sevasr
