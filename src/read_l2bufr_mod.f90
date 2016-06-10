module read_l2bufr_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    read_l2bufr_mod module for processing level2 radar bufr files
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: Process level 2 radar bufr files, converting radial wind observations
!             to superobs, which are read by subroutine read_superwinds.
!             Because these files can be very big (5-10 Gb), some additions were
!             made to bufrlib to allow use of mpi-io routines when reading the
!             bufr file.  This reduced processing time from 5-8 minutes to
!             60-90 seconds for a 5Gb test file.
!
! program history log:
!   2005-08-01  parrish
!   2006-04-21  parrish, complete rewrite.
!   2006-05-22  parrish, fix bug which causes infinite loop when no data pass initial checks
!   2007-10-24  parrish  add l2superob_only option
!   2009-04-18  woollen  improve mpi_io interface with bufrlib routines
!   2009-11-24  parrish  change time variable from regional_time (passed from gridmod) to
!                          iadate (passed from obsmod), to prevent all radar data being tossed.
!   2011-07-04  todling  - fixes to run either single or double precision
!
! subroutines included:
!   sub initialize_superob_radar - initialize superob parameters to defaults
!   sub radar_bufr_read_all      - process input level 2 bufr file and write out superobs
!
! Variable Definitions:
!   def del_azimuth     - azimuth range for superob box  (default 5 degrees)
!   def del_elev        - elevation angle range for superob box  (default .05 degrees)
!   def del_range       - radial range for superob box  (default 5 km)
!   def del_time        - 1/2 time range for superob box  (default .5 hours)
!   def elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!   def minnum                  - minimum number of samples needed to make a superob
!   def range_max       - max radial range to use in constructing superobs  (default 100km)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: initialize_superob_radar
  public :: radar_bufr_read_all
! set passed variables to public
  public :: read_l2rw_processed
  public :: range_max,del_time,l2superob_only,elev_angle_max,del_azimuth
  public :: minnum,del_range,del_elev

  integer(i_kind) minnum
  real(r_kind) del_azimuth,del_elev,del_range,del_time,elev_angle_max,range_max
  logical l2superob_only

contains

  subroutine initialize_superob_radar
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    initialize_superob_radar
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    use constants, only: quarter,half,five
    implicit none

    del_azimuth=five            !   (5 degrees)
    del_elev=quarter            !   (.25 degrees for elevation angle bin)
    del_range=5000._r_kind      !  (5 km)
    del_time=half               !   (hours)
    elev_angle_max=five         !  recommended by S. Liu to avoid heavy convection problems
    minnum=50
    range_max=100000._r_kind    !  (100km)
    l2superob_only=.false.

  end subroutine initialize_superob_radar

  subroutine radar_bufr_read_all(npe,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radar_bufr_read_all
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!   2010-08-30  treadon - changes for reproducibile results
!
!   input argument list:
!     mype     - mpi task id
!     npe
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$  end documentation block

    use kinds, only: r_double,r_quad
    use constants, only: zero,half,one,two,rearth,deg2rad,rad2deg,zero_quad,one_quad
    use mpimod, only: mpi_comm_world,mpi_min,mpi_sum,mpi_real8,ierror,mpi_real16
    use mpimod, only: mpi_max,mpi_integer4
    use obsmod,only: iadate,ndat,dsis,dfile,dtype
    use mpeu_util, only: IndexSet, IndexSort
    implicit none

    integer(i_kind),intent(in):: npe,mype

    integer(i_kind),parameter:: max_num_radars=150
    integer(i_kind),parameter:: n_gates_max=4000
    real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind
    real(r_kind),parameter:: r8     = 8.0_r_kind
    real(r_kind),parameter:: r89_5  = 89.5_r_kind
    real(r_kind),parameter:: r90    = 90.0_r_kind
    real(r_kind),parameter:: r125   = 125.0_r_kind
    real(r_kind),parameter:: r250   = 250.0_r_kind
    real(r_kind),parameter:: r360   = 360.0_r_kind
    real(r_kind),parameter:: r720   = 720.0_r_kind
    real(r_kind),parameter:: r99999 = 99999._r_kind
    real(r_kind),parameter:: rinv60 = 1.0_r_kind/60.0_r_kind
    real(r_double),parameter:: r1e5_double = 1.0e5_r_double


    integer(i_kind) nazbin,nrbin,nelbin
    integer(i_kind) i,ibyte,idate,inbufr,iret,isubset,krad,levs,lundx,n_gates
    integer(i_kind) k,iii,j,jj,jjall,jjj,numzzzz,num_radars_0,iloc
    integer(i_kind) iyref,imref,idref,ihref
    integer(i_kind) iazbin,irbin,ielbin
    integer(i_kind) nminref,nminthis
    integer(i_kind) num_radars,ireadsb,ireadmg,next
    integer(i_kind) nsuper,nsuperall
    integer(i_kind) nthisrad,nthisbins
    integer(i_kind) idups,idups0
    integer(i_kind) nradials_in,nradials_fail_angmax,nradials_fail_time,nradials_fail_elb
    integer(i_kind) nradials_in1,nradials_fail_angmax1,nradials_fail_time1,nradials_fail_elb1
    integer(i_kind) nobs_in,nobs_badvr,nobs_badsr,nobs_lrbin,nobs_hrbin,nrange_max,irad
    integer(i_kind) nobs_in1,nobs_badvr1,nobs_badsr1,nobs_lrbin1,nobs_hrbin1,nrange_max1
    integer(i_kind) num_radars_max,num_radars_min
    integer(i_kind):: histo_el
    integer(i_kind) idate5(5)
    integer(i_kind),allocatable,dimension(:) :: indx,icount
    integer(i_kind),allocatable,dimension(:,:) :: ibins,ibins2

    real(r_double) hdr(10),hdr2(12),rwnd(3,n_gates_max),rdisttest(n_gates_max)

    real(r_kind) delaz,delel,delr,t
    real(r_kind) ddiffmin,ddiffmin0,distfact,range
    real(r_kind) stn_lat,stn_lon,stn_hgt,stn_az,stn_el
    real(r_kind) timemax,timemin
    real(r_kind) timemax1,timemin1
    real(r_kind) this_stalat,this_stalon,this_stahgt
    real(r_kind) rlon0,clat0,slat0,rlonglob,rlatglob,clat1,caz0,saz0,cdlon,sdlon,caz1,saz1
    real(r_kind) this_stalatr,thisazimuthr,thistiltr
    real(r_kind) thisrange,thisazimuth,thistilt,thisvr,thisvr2
    real(r_kind) corrected_tilt
    real(r_kind) thiserr,thistime,thislat,thislon,corrected_azimuth
    real(r_kind) rad_per_meter,thishgt
    real(r_kind) rlonloc,rlatloc
    real(r_kind) a43,aactual,b,c,selev0,celev0,epsh,erad,h,ha
    real(r_kind) celev,selev,gamma
    real(r_kind) vrmax,vrmin,errmax,errmin
    real(r_kind) vrmaxall,vrminall,errmaxall,errminall
    real(r_kind) delazmmax,rdelaz,rdelr,rdelel
    real(r_kind) delazmmaxall
    real(r_kind) deltiltmaxall,deltiltmax
    real(r_kind) deltiltminall,deltiltmin
    real(r_kind) deldistmaxall,deldistmax
    real(r_kind) deldistminall,deldistmin
    real(r_kind),dimension(max_num_radars) :: stn_lat_table,stn_lon_table, &
                 stn_hgt_table,master_lat_table,master_lon_table,master_hgt_table
    real(r_kind),dimension(max_num_radars*npe) :: stn_lat_table_all,stn_lon_table_all, &
                 stn_hgt_table_all

    real(r_quad) thiscount
    real(r_quad),dimension(6) :: binsx
    real(r_quad),allocatable,dimension(:,:,:) :: bins,bins_work
!            bins(1,...) radial distance
!            bins(2,...) azimuth
!            bins(3,...) elev angle
!            bins(4,...) vr
!            bins(5,...) vr**2
!            bins(6,...) relative time
!            ibins(...) count

!       bins(  :,   :,   :)
!            var  rbin*azbin*elbin rad#
    character(8) chdr,chdr2,subset
    character(10) date
    character(20) infile
    character(4),dimension(max_num_radars):: stn_id_table,master_stn_table
    character(4) stn_id_table_all(max_num_radars*npe)
    character(4) stn_id,this_staid
    character(4*max_num_radars) cstn_id_table,cmaster_stn_table
    equivalence(stn_id_table(1),cstn_id_table)
    equivalence(master_stn_table(1),cmaster_stn_table)
    equivalence (chdr,hdr(1))
    equivalence (chdr2,hdr2(1))

    logical rite

    ! define infile if using either option for radial winds.
    do i=1,ndat
       if (trim(dtype(i))=='rw' .and. trim(dsis(i))=='rw')then
          infile=trim(dfile(i))
       else if (trim(dtype(i))=='rw' .and. trim(dsis(i))=='l2rw')then
          infile=trim(dfile(i))
       else 
          return ! Go back to gsisub if nothing to do.
       end if 
    end do    
 
    rad_per_meter= one/rearth
    erad = rearth
    nazbin=nint(r360/del_azimuth)
    nrbin=nint(range_max/del_range)
    nelbin=nint(elev_angle_max/del_elev)
    delaz=r360/nazbin
    delr=range_max/nrbin
    delel=elev_angle_max/nelbin
    rdelaz=one/delaz
    rdelr =one/delr
    rdelel=one/delel

    num_radars=0
    do i=1,max_num_radars
       stn_id_table(i)='ZZZZ'
    end do
    stn_lat_table=r99999
    stn_lon_table=r99999
    stn_hgt_table=r99999

    rite = .false.
    if (mype==0) rite=.true.
    
!   Open bufr file with openbf to initialize bufr table, etc in bufrlib
    inbufr=10
    open(inbufr,file=infile,form='unformatted')
    rewind inbufr
    lundx=inbufr
    call openbf(inbufr,'IN',lundx)
    call datelen(10)
    if(l2superob_only) then
       write(date,'( i10)') idate
       read (date,'(i4,3i2)') iyref,imref,idref,ihref
       if(rite) write(6,*)' create superobs only, radar file date = ',iyref,imref,idref,ihref
    else
       iyref=iadate(1)              !????  add mods so can be used in global mode
       imref=iadate(2)
       idref=iadate(3)
       ihref=iadate(4)
       if(rite) write(6,*)' using restart file date = ',iyref,imref,idref,ihref
    end if
    if(rite) write(6,*)'RADAR_BUFR_READ_ALL: analysis time is ',iyref,imref,idref,ihref
    idate5(1)=iyref
    idate5(2)=imref
    idate5(3)=idref
    idate5(4)=ihref
    idate5(5)=0          ! minutes
    call w3fs21(idate5,nminref)

!    Do an initial read of a bit of data to infer what multiplying factor is for 
!    radial distance.  There is a possible ambiguity, where the scaling is either 
!    125 or 250. If the minimum difference between gate distances is 2, then factor 
!    is 125, if = 1 then factor is 250.

    idups=0
    nobs_in=0
    ddiffmin=huge(ddiffmin)
    next=0
    do while(ireadmg(inbufr,subset,idate)>=0)
       next=next+1
       if(next == npe)next=zero
       if(next /= mype)cycle
       read(subset,'(2x,i6)')isubset
       if(isubset>6033) then
          iret=6034
          exit
       end if
       do while (ireadsb(inbufr)==0)
          call ufbint(inbufr,rdisttest,1,n_gates_max,n_gates,'DIST125M')
          if(n_gates>1) then
             do i=1,n_gates-1
                if(nint(abs(rdisttest(i+1)-rdisttest(i)))==0) then
                   idups=idups+1
                else
                   ddiffmin=min(abs(rdisttest(i+1)-rdisttest(i)),ddiffmin)
                end if
             end do
          end if
          call ufbint(inbufr,hdr2,12,1,levs,'SSTN CLAT CLON HSMSL HSALG ANEL YEAR MNTH DAYS HOUR MINU SECO')
          if(hdr2(6)>elev_angle_max) cycle
          idate5(1)=nint(hdr2(7)) ; idate5(2)=nint(hdr2(8)) ; idate5(3)=nint(hdr2(9))
          idate5(4)=nint(hdr2(10)) ; idate5(5)=nint(hdr2(11))
          call w3fs21(idate5,nminthis)
	  t=(real(nminthis-nminref,r_kind)+real(nint(hdr2(12)),r_kind)*rinv60)*rinv60
          if(abs(t)>del_time) cycle
          nobs_in=nobs_in+n_gates
          stn_id=chdr2 
          ibyte=index(cstn_id_table,stn_id)
          if(ibyte==0) then
             num_radars=num_radars+1
             if(num_radars>max_num_radars) then
                write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
                     'bufr file--increase parameter max_num_radars'
                call stop2(99)
             end if
             stn_lat=hdr2(2)
             stn_lon=hdr2(3)
             stn_hgt=hdr2(4)+hdr2(5)
             stn_id_table(num_radars)=stn_id
             stn_lon_table(num_radars)=stn_lon
             stn_lat_table(num_radars)=stn_lat
             stn_hgt_table(num_radars)=stn_hgt
          end if
       end do
    end do

    call mpi_allreduce(num_radars,num_radars_max,1,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    if(num_radars_max<=0) then
       if(rite) write(6,*)'RADAR_BUFR_READ_ALL:  NO RADARS KEPT IN radar_bufr_read_all, ',&
            'continue without level 2 data'
       call closbf(inbufr)
       return
    end if
    call mpi_reduce(num_radars,num_radars_min,1,mpi_integer4,mpi_min,0,mpi_comm_world,ierror)
    if(rite) write(6,*)' min,max num_radars=',num_radars_min,num_radars_max

!  Create master station list

!  First gather all stn id and lat,lon,hgt lists
    call mpi_allgather(stn_id_table,max_num_radars,mpi_integer4, &
         stn_id_table_all,max_num_radars,mpi_integer4,mpi_comm_world,ierror)
    call mpi_allgather(stn_lat_table,max_num_radars,mpi_real8, &
         stn_lat_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)
    call mpi_allgather(stn_lon_table,max_num_radars,mpi_real8, &
         stn_lon_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)
    call mpi_allgather(stn_hgt_table,max_num_radars,mpi_real8, &
         stn_hgt_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)

!   Create unique master list of all radar names,lats,lons
    jj=0
    do j=1,max_num_radars*npe
       if(stn_id_table_all(j) /= 'ZZZZ')then
          jj=jj+1
          stn_id_table_all(jj)=stn_id_table_all(j)
          stn_lat_table_all(jj)=stn_lat_table_all(j)
          stn_lon_table_all(jj)=stn_lon_table_all(j)
          stn_hgt_table_all(jj)=stn_hgt_table_all(j)
       end if
    end do
    jjall=jj
    num_radars_0=0
    jj=1
    outer: do j=1,jjall
        num_radars_0=num_radars_0+1
        if(num_radars_0 > max_num_radars) then
	  write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
	     'bufr file--increase parameter max_num_radars'
          call stop2(99)
        end if
        master_stn_table(num_radars_0)=stn_id_table_all(jj)
        master_lat_table(num_radars_0)=stn_lat_table_all(jj)
        master_lon_table(num_radars_0)=stn_lon_table_all(jj)
        master_hgt_table(num_radars_0)=stn_hgt_table_all(jj)
        numzzzz=0
        do jjj=jj+1,jjall
          if(stn_id_table_all(jjj) /= 'ZZZZ')then
	     if(stn_id_table_all(jjj)==master_stn_table(num_radars_0)) then
	        stn_id_table_all(jjj)='ZZZZ'
	     else
	        if(numzzzz == 0)numzzzz=jjj
	     end if
	  end if
        end do
        if(numzzzz==0) exit outer
        jj=numzzzz
    end do outer
    allocate(indx(num_radars_0))
    call indexset(indx)
    call indexsort(indx,master_stn_table)

    call mpi_allreduce(ddiffmin,ddiffmin0,1,mpi_real8,mpi_min,mpi_comm_world,ierror)
    call mpi_allreduce(idups,idups0,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
    distfact=zero
    if(nint(ddiffmin0)==1)     distfact=r250
    if(nint(ddiffmin0)==2)     distfact=r125
    if(distfact==zero) then
       write(6,*)'RADAR_BUFR_READ_ALL:  problem with level 2 bufr file, ',&
	    'gate distance scale factor undetermined, going with 125'
       distfact=r125
    end if

    timemax=-huge(timemax)
    timemin=huge(timemin)
    nradials_in=0
    nradials_fail_angmax=0
    nradials_fail_time=0
    nradials_fail_elb=0
    nobs_badvr=0
    nobs_badsr=0
    nobs_lrbin=0
    nobs_hrbin=0
    nrange_max=0
    nthisrad=nrbin*nazbin*nelbin
    nthisbins=6*nthisrad

! reopen and reread the file for data this time

    call closbf(inbufr)
    open(inbufr,file=infile,form='unformatted')
    call openbf(inbufr,'IN',inbufr)

    allocate(bins(6,nthisrad,num_radars_0),ibins(nthisrad,num_radars_0))
    bins=zero_quad
    ibins=0

    next=0
    do while(ireadmg(inbufr,subset,idate)>=0)
       next=next+1
       if(next == npe)next=zero
       if(next /= mype)cycle
       read(subset,'(2x,i6)')isubset
       if(isubset>6033) then
	  iret=6034
	  exit
       end if
       do while (ireadsb(inbufr)==0)
	  call ufbint(inbufr,hdr,10,1,levs, &
	       'SSTN YEAR MNTH DAYS HOUR MINU SECO ANAZ ANEL QCRW')
	  nradials_in=nradials_in+1
	  if(hdr(9)>elev_angle_max) then
	     nradials_fail_angmax=nradials_fail_angmax+1
	     cycle
	  end if
	  idate5(1)=nint(hdr(2)) ; idate5(2)=nint(hdr(3)) ; idate5(3)=nint(hdr(4))
	  idate5(4)=nint(hdr(5)) ; idate5(5)=nint(hdr(6))
	  call w3fs21(idate5,nminthis)
	  t=(real(nminthis-nminref,r_kind)+real(nint(hdr(7)),r_kind)*rinv60)*rinv60
	  timemax=max(t,timemax)
	  timemin=min(t,timemin)
	  if(abs(t)>del_time) then
	     nradials_fail_time=nradials_fail_time+1
	     cycle
	  end if
	  stn_az=r90-hdr(8)
	  stn_el=hdr(9)
	  iazbin=stn_az*rdelaz
	  iazbin=mod(iazbin,nazbin)
	  if(iazbin<0) iazbin=iazbin+nazbin
	  iazbin=iazbin+1
	  if(iazbin<=0.or.iazbin>nazbin) then
	     write(6,*)'RADAR_BUFR_READ_ALL:  error in getting iazbin, program stops'
	     call stop2(99)
	  end if
	  ielbin=ceiling(stn_el*rdelel)
	  if(ielbin<1.or.ielbin>nelbin) then
	     nradials_fail_elb=nradials_fail_elb+1
	     cycle
	  end if
	  stn_id=chdr 
	  ibyte=index(cmaster_stn_table,stn_id)
	  if(ibyte==0) then
	     write(6,*) ' index error in radar_bufr_read_all -- program stops -- ',ibyte,stn_id
	     call stop2(99)
	  else
	     krad=1+(ibyte-1)/4
	  end if

	  call ufbint(inbufr,rwnd,3,n_gates_max,n_gates,'DIST125M DMVR DVSW')
	  do i=1,n_gates
	     range=distfact*rwnd(1,i)
	     if(range>range_max) then
		nrange_max=nrange_max+1
		cycle
	     end if
	     if(rwnd(2,i)>r1e5_double) then
		nobs_badvr=nobs_badvr+1
		cycle
	     end if
	     if(rwnd(3,i)>r1e5_double) then
		nobs_badsr=nobs_badsr+1
		cycle
	     end if
	     irbin=ceiling(range*rdelr)
	     if(irbin<1) then
		nobs_lrbin=nobs_lrbin+1
		cycle
	     end if
	     if(irbin>nrbin) then
		nobs_hrbin=nobs_hrbin+1
		cycle
	     end if
	     iloc=nrbin*(nazbin*(ielbin-1)+(iazbin-1))+irbin
	     bins(1,iloc,krad)=bins(1,iloc,krad)+range
	     bins(2,iloc,krad)=bins(2,iloc,krad)+stn_az
	     bins(3,iloc,krad)=bins(3,iloc,krad)+stn_el
	     bins(4,iloc,krad)=bins(4,iloc,krad)+rwnd(2,i)
	     bins(5,iloc,krad)=bins(5,iloc,krad)+rwnd(2,i)**2
	     bins(6,iloc,krad)=bins(6,iloc,krad)+t
	     ibins(iloc,krad)=ibins(iloc,krad)+1
	  end do
	     
       end do          !  end do while
    end do             !  loop over blocks
    call closbf(inbufr)

    allocate(ibins2(nthisrad,num_radars_0))
    call mpi_allreduce(ibins,ibins2,nthisrad*num_radars_0,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
    deallocate(ibins)

    call mpi_reduce(nradials_in,nradials_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_angmax,nradials_fail_angmax1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_time,nradials_fail_time1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_elb,nradials_fail_elb1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_in,nobs_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badvr,nobs_badvr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badsr,nobs_badsr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_lrbin,nobs_lrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_hrbin,nobs_hrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nrange_max,nrange_max1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(timemax,timemax1,1,mpi_real8,mpi_max,0,mpi_comm_world,ierror)
    call mpi_reduce(timemin,timemin1,1,mpi_real8,mpi_min,0,mpi_comm_world,ierror)

    if(mype==0) then
    
       allocate(icount(num_radars_0))
       write(6,*)'RADAR_BUFR_READ_ALL:  num_radars_0 = ',num_radars_0
       do irad=1,num_radars_0
          krad=indx(irad)
          icount(krad)=0
          do i=1,nthisrad
            if(ibins2(i,krad) >= minnum)icount(krad)=icount(krad)+1
          end do
	  write(6,'(" master list radar ",i3," stn id,lat,lon,hgt,num = ",a4,2f10.2,f8.1,i10)') &
	       irad,master_stn_table(krad),master_lat_table(krad),master_lon_table(krad), &
               master_hgt_table(krad),icount(krad)
       end do
       write(6,*)'RADAR_BUFR_READ_ALL:  ddiffmin,distfact,idups=',ddiffmin0,distfact,idups0
       write(6,*)' nthisrad=',nthisrad
       write(6,*)' nthisbins=',nthisbins
       write(6,*)' timemin,max=',timemin1,timemax1
       write(6,*)' nradials_in=',nradials_in1
       write(6,*)' nradials_fail_angmax=',nradials_fail_angmax1
       write(6,*)' nradials_fail_time=',nradials_fail_time1
       write(6,*)' nradials_fail_elb=',nradials_fail_elb1
       write(6,*)' nobs_in=',nobs_in1
       write(6,*)' nobs_badvr=',nobs_badvr1
       write(6,*)' nobs_badsr=',nobs_badsr1
       write(6,*)' nobs_lrbin=',nobs_lrbin1
       write(6,*)' nobs_hrbin=',nobs_hrbin1
       write(6,*)' nrange_max=',nrange_max1

!    Print out histogram of counts by ielbin to see where angles are
       do ielbin=1,nelbin
          histo_el=0
          do krad=1,num_radars_0
	     do iazbin=1,nazbin
		do irbin=1,nrbin
		   iloc=nrbin*(nazbin*(ielbin-1)+(iazbin-1))+irbin
		   histo_el=histo_el+ibins2(iloc,krad)
		end do
	     end do
	  end do
	  write(6,'(" ielbin,histo_el=",i6,i20)')ielbin,histo_el
       end do

!   Prepare to create superobs and write out.
       open(inbufr,file='radar_supobs_from_level2',form='unformatted',iostat=iret)
       rewind inbufr
       nsuperall=0
       vrmaxall=-huge(vrmaxall)
       vrminall=huge(vrminall)
       errmaxall=-huge(errmaxall)
       errminall=huge(errminall)
       delazmmaxall=-huge(delazmmaxall)
       deltiltmaxall=-huge(deltiltmaxall)
       deldistmaxall=-huge(deldistmaxall)
       deltiltminall=huge(deltiltminall)
       deldistminall=huge(deldistminall)
    end if
    allocate(bins_work(6,nthisrad,npe))
    do irad=1,num_radars_0
       krad=indx(irad)
       if(r_double==r_quad) then
          call mpi_gather(bins(1,1,krad),nthisbins,mpi_real8 ,bins_work,nthisbins, &
                       mpi_real8 ,0,mpi_comm_world,ierror)
       else
          call mpi_gather(bins(1,1,krad),nthisbins,mpi_real16,bins_work,nthisbins, &
                       mpi_real16,0,mpi_comm_world,ierror)
       endif
       if(mype == 0)then
    
!   Create superobs and write out.
	  nsuper=0
	  vrmax=-huge(vrmax)
	  vrmin=huge(vrmin)
	  errmax=-huge(errmax)
	  errmin=huge(errmin)
	  delazmmax=-huge(delazmmax)
	  deltiltmax=-huge(deltiltmax)
	  deldistmax=-huge(deldistmax)
	  deltiltmin=huge(deltiltmin)
	  deldistmin=huge(deldistmin)
	  this_stalat=master_lat_table(krad)
	  if(abs(this_stalat)>r89_5) cycle
	  this_stalon=master_lon_table(krad)
	  rlon0=deg2rad*this_stalon
	  this_stalatr=this_stalat*deg2rad
	  clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
	  this_staid=master_stn_table(krad)
	  this_stahgt=master_hgt_table(krad)
	  do iii=1,nthisrad
	     if(ibins2(iii,krad) < minnum) cycle

	     thiscount=one_quad/real(ibins2(iii,krad),r_quad)
             do i=1,6
               binsx(i)=bins_work(i,iii,1)
             end do
             do k=2,npe
               do i=1,6
	         binsx(i)=binsx(i)+bins_work(i,iii,k)
               end do
             end do
             do i=1,6
               binsx(i)=binsx(i)*thiscount
             end do
	     thisrange=  binsx(1)
	     thisazimuth=binsx(2)
	     thistilt=binsx(3)
	     thisvr=binsx(4)
	     vrmax=max(vrmax,thisvr)
	     vrmin=min(vrmin,thisvr)
	     thisvr2=binsx(5)
	     thiserr=sqrt(abs(thisvr2-thisvr**2))
	     errmax=max(errmax,thiserr)
	     errmin=min(errmin,thiserr)
	     thistime=binsx(6)

!            Compute obs height here
!            Use 4/3rds rule to get elevation of radar beam
!            (if local temperature, moisture available, then vertical position 
!             might be estimated with greater accuracy by ray tracing )

	     aactual=erad+this_stahgt
	     a43=four_thirds*aactual
	     thistiltr=thistilt*deg2rad
	     selev0=sin(thistiltr)
	     celev0=cos(thistiltr)
	     b=thisrange*(thisrange+two*aactual*selev0)
	     c=sqrt(aactual*aactual+b)
	     ha=b/(aactual+c)
	     epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
	     h=ha-epsh
	     thishgt=this_stahgt+h
	     
!            Get corrected tilt angle
	     celev=celev0
	     selev=selev0
	     if(thisrange>=one) then
		celev=a43*celev0/(a43+h)
		selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
	     end if
	     corrected_tilt=atan2(selev,celev)*rad2deg
	     deltiltmax=max(corrected_tilt-thistilt,deltiltmax)
	     deltiltmin=min(corrected_tilt-thistilt,deltiltmin)
	     gamma=half*thisrange*(celev0+celev)
	     deldistmax=max(gamma-thisrange,deldistmax)
	     deldistmin=min(gamma-thisrange,deldistmin)

!            Get earth lat lon of superob
	     thisazimuthr=thisazimuth*deg2rad
	     rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
             rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
             call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
             thislat=rlatglob*rad2deg
             thislon=rlonglob*rad2deg

!            Keep away from poles, rather than properly deal with polar singularity
	     if(abs(thislat)>r89_5) cycle

!            Get corrected azimuth
             clat1=cos(rlatglob)
             caz0=cos(thisazimuthr)
             saz0=sin(thisazimuthr)
             cdlon=cos(rlonglob-rlon0)
             sdlon=sin(rlonglob-rlon0)
             caz1=clat0*caz0/clat1
             saz1=saz0*cdlon-caz0*sdlon*slat0
             corrected_azimuth=atan2(saz1,caz1)*rad2deg
             delazmmax=max(min(abs(corrected_azimuth-thisazimuth-r720),&
                  abs(corrected_azimuth-thisazimuth-r360),&
                  abs(corrected_azimuth-thisazimuth     ),&
                  abs(corrected_azimuth-thisazimuth+r360),&
                  abs(corrected_azimuth-thisazimuth+r720)),delazmmax)

             write(inbufr) this_staid,this_stalat,this_stalon,this_stahgt, &
                  thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,&
                  thiserr,corrected_tilt
             nsuper=nsuper+1
          end do
          if(nsuper > 0)then
            write(6,*)' for radar ',this_staid,' nsuper=',nsuper,' delazmmax=',delazmmax
            write(6,*)' vrmin,max=',vrmin,vrmax,' errmin,max=',errmin,errmax
            write(6,*)' deltiltmin,max=',deltiltmin,deltiltmax,' deldistmin,max=',deldistmin,deldistmax
            vrminall=min(vrminall,vrmin)
            vrmaxall=max(vrmaxall,vrmax)
            errminall=min(errminall,errmin)
            errmaxall=max(errmaxall,errmax)
            delazmmaxall=max(delazmmaxall,delazmmax)
            deltiltmaxall=max(deltiltmaxall,deltiltmax)
            deldistmaxall=max(deldistmaxall,deldistmax)
            deltiltminall=min(deltiltminall,deltiltmin)
            deldistminall=min(deldistminall,deldistmin)
            nsuperall=nsuperall+nsuper
          end if
       end if
    end do

    if(mype == 0)then
       write(6,*)' total number of superobs written=',nsuperall
       write(6,*)'  vrmin,maxall=',vrminall,vrmaxall
       write(6,*)' errmin,maxall=',errminall,errmaxall
       write(6,*)' delazmmaxall=',delazmmaxall
       write(6,*)' deltiltmin,maxall=',deltiltminall,deltiltmaxall
       write(6,*)' deldistmin,maxall=',deldistminall,deldistmaxall
       close(inbufr)
    end if
    deallocate(bins_work,bins,ibins2)
    if(l2superob_only) then
       call mpi_finalize(ierror)
       stop
    end if

end subroutine radar_bufr_read_all

subroutine read_l2rw_processed(nread,ndata,nodata,infile,lunout,obstype,twind, &
sis,hgtl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radar                    read radar radial winds
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads radar radial wind files.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!
! program history log:
!   2015-10-19  lippi   - Modified from read_radar to only process l2 radial
!                         wind obs. and skip vad checks.
!
!   input argument list:
!     infile   - file from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     hgtl_full- 3d geopotential height on full domain grid
!
!   output argument list:
!     nread    - number of doppler lidar wind observations read
!     ndata    - number of doppler lidar wind profiles retained for further
!     processing
!     nodata   - number of doppler lidar wind observations retained for further
!     processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor!
!     domain


  use kinds, only: r_kind,r_single,r_double,i_kind,i_byte
  use constants, only: zero,zero_single,half,one,two,three,deg2rad,rearth,rad2deg, &
      one_tenth,r10,r1000,r60inv,r100,r400,grav_equator, &
      eccentricity,somigliana,grav_ratio,grav, &
      semi_major_axis,flattening,two
  use qcmod, only: erradar_inflate,vadfile,newvad
  use obsmod, only: iadate,l_foreaft_thin
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy,nsig
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional,cmaq_regional,wrf_mass_regional
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype,ithin_conv,rmesh_conv,pmesh_conv
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,ges_prslavg
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model
  use mpimod, only: npe

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=20),intent(in  ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full

! Declare local parameters
  integer(i_kind),parameter:: maxlevs=1500
  integer(i_kind),parameter:: maxdat=22
  integer(i_kind),parameter:: maxvad=500
! integer(i_kind),parameter:: maxvadbins=20
  integer(i_kind),parameter:: maxvadbins=15
  real(r_kind),parameter:: r4_r_kind = 4.0_r_kind


  real(r_kind),parameter:: dzvad=304.8_r_kind  !  vad reports are every 1000 ft=304.8 meters
  real(r_kind),parameter:: r3_5 = 3.5_r_kind
  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: r90 = 90.0_r_kind
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r150 = 150.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r50000 = 50000.0_r_kind
  real(r_kind),parameter:: r60 = 60.0_r_kind
  real(r_kind),parameter:: r75 = 75.0_r_kind
  real(r_kind),parameter:: r92 = 92.6e03_r_kind
  real(r_kind),parameter:: r89_5  = 89.5_r_kind
  real(r_kind),parameter:: r2 = 2.0_r_kind
  real(r_kind),parameter:: r71 = 71.0_r_kind
  real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind

! Declare local variables
  logical good,outside,good0,lexist1,lexist2

  character(10) date
  character(80) hdrstr(2),datstr(2)
  character(8) subset,subset_check(3)
  character(30) outmessage
  character(255) filename

  integer(i_kind) lnbufr,i,j,k,maxobs,icntpnt,iiout,n,istop
  integer(i_kind) nmrecs,ibadazm,ibadtilt,ibadrange,ibadwnd,ibaddist,ibadheight,ibadvad,kthin
  integer(i_kind) iyr,imo,idy,ihr,imn,isc,ithin
  integer(i_kind) ibadstaheight,ibaderror,notgood,idate,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) novadmatch,ioutofvadrange
  integer(i_kind) iy,im,idd,ihh,iret,levs,mincy,minobs,kx0,kxadd,kx
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) ivad,ivadz,nvad,idomsfc
  real(r_kind) timeb,rmesh,usage,ff10,sfcr,skint,t4dv,t4dvo,toff
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error,wqm
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges

  real(r_kind),dimension(maxdat):: cdata
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id
  real(r_double),dimension(12):: hdr
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)
  real(r_double),dimension(7,maxlevs):: radar_obs
!  real(r_double),dimension(4,maxlevs):: vad_obs

  real(r_double),dimension(2,maxlevs):: fcst_obs

!  character(8) vadid(maxvad)
!  real(r_kind) vadlat(maxvad),vadlon(maxvad),vadqm(maxvad,maxvadbins)
!  real(r_kind) vadu(maxvad,maxvadbins),vadv(maxvad,maxvadbins)
!  real(r_kind) vadcount(maxvad,maxvadbins)
!  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2,vadcount2,vadwgt2
!  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2_5,vadcount2_5,vadwgt2_5
!  real(r_kind),dimension(maxvad,maxvadbins)::vadfit3,vadcount3,vadwgt3
  real(r_kind) zob,vadqmmin,vadqmmax
  integer(i_kind) level2(maxvad),level2_5(maxvad),level3(maxvad),level3_tossed_by_2_5(maxvad)
  integer(i_kind) loop,numcut
  integer(i_kind) numhits(0:maxvad)
  real(r_kind) timemax,timemin,errmax,errmin
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) xscale,xscalei
  integer(i_kind) max_rrr,nboxmax
  integer(i_kind) irrr,iaaa,iaaamax,iaaamin
  integer(i_byte),allocatable::nobs_box(:,:,:,:)
  real(r_kind) dlonvad,dlatvad,vadlon_earth,vadlat_earth
  real(r_kind) this_stalat,this_stalon,this_stahgt,thistime,thislat,thislon
  real(r_kind) azm0,elev0,range0,rotang
  real(r_kind) thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
  integer(i_kind) nsuper2_in,nsuper2_kept
  real(r_kind) errzmax
  real(r_kind) thisfit,thisvadspd,thisfit2,uob,vob,thiswgt

  integer(i_kind),allocatable,dimension(:):: isort

! following variables are for fore/aft separation
  real(r_kind) tdrele1,tdrele2,tdrele3
  integer(i_kind) nswp,firstbeam,nforeswp,naftswp,nfore,naft,nswptype,irec
  logical foreswp,aftswp

  data lnbufr/10/
  data hdrstr(1) / 'CLAT CLON SELV ANEL YEAR MNTH DAYS HOUR MINU MGPT' /
  data hdrstr(2) / 'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL' /
  data datstr(1) / 'STDM SUPLAT SUPLON HEIT RWND RWAZ RSTD' /
  data datstr(2) / 'DIST HREF DMVR DVSW' /

  data ithin / -9 /
  data rmesh / -99.999_r_kind /

!***********************************************************************************

  eradkm=rearth*0.001_r_kind
  maxobs=2e6
  nreal=maxdat
  nchanl=0
  ilon=2
  ilat=3
  iaaamax=-huge(iaaamax)
  iaaamin=huge(iaaamin)
  dlatmax=-huge(dlatmax)
  dlonmax=-huge(dlonmax)
  dlatmin=huge(dlatmin)
  dlonmin=huge(dlonmin)

  allocate(cdata_all(maxdat,maxobs),isort(maxobs))

  isort = 0
  cdata_all=zero

!2.
! Initialize variables
! vad_leash=.1_r_kind
!  vad_leash=.3_r_kind
 !xscale=5000._r_kind
 !xscale=10000._r_kind
  xscale=20000._r_kind
!  write(6,*)'READ_L2RW_PROCESSED:  set vad_leash,xscale=',vad_leash,xscale
!  write(6,*)'READ_L2RW_PROCESSED:  set
!  maxvadbins,maxbadbins*dzvad=',maxvadbins,&
!     maxvadbins*dzvad
  xscalei=one/xscale
  max_rrr=nint(100000.0_r_kind*xscalei)
  nboxmax=1

  kx0=22500

  nmrecs=0
  irec=0

  errzmax=zero
!  nvad=0
!  vadlon=zero
!  vadlat=zero
! First read in all vad winds so can use vad wind quality marks to decide
! which radar data to keep
! Open, then read bufr data
!********************************************************************************
!if(1==0)then !lippi
!!end if !lippi
!6.
!!if(1==0)then !lippi
!! Update vadqm table
!!end if !lippi
!7.
!!if(1==0)then !lippi
!! Print vadwnd table
!end if !lippi
!********************************************************************************

!  Allocate thinning grids around each radar
!  space needed is nvad*max_rrr*max_rrr*8*max_zzz
!
!      max_rrr=20
!      maxvadbins=20
!      nvad=150
!      space=150*20*20*8*20 = 64000*150=9600000  peanuts

  allocate(nobs_box(max_rrr,8*max_rrr,maxvadbins,nvad))
  nobs_box=0
! Set level2_5 to 0.  Then loop over routine twice, first looking for
! level 2.5 data, and setting level2_5=count of 2.5 data for any 2.5 data
! available that passes the vad tests.  The second pass puts in level 3
! data where it is available and no level 2.5 data was saved/available
! (level2_5=0)

!  vadfit2=zero
!  vadfit2_5=zero
!  vadfit3=zero
!  vadwgt2=zero
!  vadwgt2_5=zero
!  vadwgt3=zero
!  vadcount2=zero
!  vadcount2_5=zero
!  vadcount3=zero
  level2=0
  level2_5=0
  level3=0
  level3_tossed_by_2_5=0
  subset_check(1)='NC006002'
  subset_check(2)='NC006001'

! First process any level 2 superobs.
! Initialize variables.
  ikx=0
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype))ikx = i
  end do

  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  loop=0

  numhits=0
  ibadazm=0
  ibadwnd=0
  ibaddist=0
  ibadheight=0
  ibadstaheight=0
  iheightbelowsta=0
  iheightbelowsta=0
  ibaderror=0
  ibadvad=0
  ibadfit=0
  ioutofvadrange=0
  kthin=0
  novadmatch=0
  notgood=0
  notgood0=0
  nsuper2_in=0
  nsuper2_kept=0

  if(loop==0) outmessage='level 2 superobs:'
!8.
! Open sequential file containing superobs
  open(lnbufr,file='radar_supobs_from_level2',form='unformatted')
!  open(lnbufr,file='l2rwbufr_out',form='unformatted')
  rewind lnbufr

 ! dist2max=-huge(dist2max)
 ! dist2min=huge(dist2min)

! Loop to read superobs data file
  do
     read(lnbufr,iostat=iret)this_staid,this_stalat,this_stalon,this_stahgt, &
        thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
     if(iret/=0) exit
     nsuper2_in=nsuper2_in+1

     !write(6,*) this_staid,this_stalat,this_stalon,this_stahgt, &
     !           thistime,thislat,thislon,thishgt,thisvr,&
     !           corrected_azimuth,thiserr,corrected_tilt

     dlat_earth=this_stalat    !station lat (degrees)
     dlon_earth=this_stalon    !station lon (degrees)
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)
     staheight=this_stahgt    !station elevation
     tiltangle=corrected_tilt*deg2rad

!********************************************************************************
!9.
!if(1==0) then !lippi
!!    Find vad wind match
!end if !lippi
!********************************************************************************

     t4dvo=toff+thistime
     timemax=max(timemax,t4dvo)
     timemin=min(timemin,t4dvo)

!    Exclude data if it does not fall within time window
     if (l4dvar.or.l4densvar) then
        if (t4dvo<zero .OR. t4dvo>winlen) cycle
     else
        timeo=thistime
        if(abs(timeo)>half ) cycle
     endif

!    Get observation (lon,lat).  Compute distance from radar.
     dlat_earth=thislat
     dlon_earth=thislon
     if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if(dlon_earth<zero ) dlon_earth=dlon_earth+r360

     dlat_earth = dlat_earth*deg2rad
     dlon_earth = dlon_earth*deg2rad
     if(regional) then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

     clonh=cos(dlon_earth)
     slonh=sin(dlon_earth)
     clath=cos(dlat_earth)
     slath=sin(dlat_earth)
     cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
     cdist=max(-one,min(cdist,one))
     dist=eradkm*acos(cdist)
     irrr=nint(dist*1000*xscalei)
     if(irrr<=0 .or. irrr>max_rrr) cycle

!    Extract radial wind data
     height= thishgt
     rwnd  = thisvr
     azm_earth = corrected_azimuth
!10.
     if(regional) then
        cosazm_earth=cos(azm_earth*deg2rad)
        sinazm_earth=sin(azm_earth*deg2rad)
        call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
        azm=atan2(sinazm,cosazm)*rad2deg
     else
        azm=azm_earth
     end if
!11.
     iaaa=azm/(r360/(r8*irrr))
     iaaa=mod(iaaa,8*irrr)
     if(iaaa<0) iaaa=iaaa+8*irrr
     iaaa=iaaa+1
     iaaamax=max(iaaamax,iaaa)
     iaaamin=min(iaaamin,iaaa)
!12.
     error = erradar_inflate*thiserr
     errmax=max(error,errmax)
!13.
     if(thiserr>zero) errmin=min(error,errmin)
!    Perform limited qc based on azimuth angle, radial wind
!    speed, distance from radar site, elevation of radar,
!    height of observation, observation error, and goodness of fit to vad wind
     good0=.true.
     if(abs(azm)>r400) then
        ibadazm=ibadazm+1; good0=.false.
     end if
     if(abs(rwnd)>r200) then
        ibadwnd=ibadwnd+1; good0=.false.
     end if
     if(dist>r400) then
        ibaddist=ibaddist+1; good0=.false.
     end if
     if(staheight<-r1000.or.staheight>r50000) then
        ibadstaheight=ibadstaheight+1; good0=.false.
     end if
     if(height<-r1000.or.height>r50000) then
        ibadheight=ibadheight+1; good0=.false.
     end if
     if(height<staheight) then
        iheightbelowsta=iheightbelowsta+1 ; good0=.false.
     end if
     if(thiserr>r6 .or. thiserr<=zero) then
        ibaderror=ibaderror+1; good0=.false.
     end if
     good=.true.
     if(.not.good0) then
        notgood0=notgood0+1
        cycle
     else

!********************************************************************************
!14.
!if(1==0) then !lippi
!!       Check fit to vad wind and vad wind quality mark
!  end if !lippi
!********************************************************************************
     end if
!15.
!     good=.true. !lippi

!    If data is good, load into output array
     if(good) then
        nsuper2_kept=nsuper2_kept+1
        level2(ivad)=level2(ivad)+1
!lippi
!        nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1
        ndata    =min(ndata+1,maxobs)
        nodata   =min(nodata+1,maxobs)  !number of obs not used (no meaninghere)
        usage = zero
        if(icuse(ikx) < 0)usage=r100
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
           if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if

        call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)

        cdata(1) = error             ! wind obs error (m/s)
        cdata(2) = dlon              ! grid relative longitude
        cdata(3) = dlat              ! grid relative latitude
        cdata(4) = height            ! obs absolute height (m)
        cdata(5) = rwnd              ! wind obs (m/s)
        cdata(6) = azm*deg2rad       ! azimuth angle (radians)
        cdata(7) = t4dv              ! obs time (hour)
        cdata(8) = ikx               ! type
        cdata(9) = tiltangle         ! tilt angle (radians)
        cdata(10)= staheight         ! station elevation (m)
        cdata(11)= rstation_id       ! station id
        cdata(12)= usage             ! usage parameter
        cdata(13)= idomsfc           ! dominate surface type
        cdata(14)= skint             ! skin temperature
        cdata(15)= ff10              ! 10 meter wind factor
        cdata(16)= sfcr              ! surface roughness
        cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        cdata(19)=dist               ! range from radar in km (used to estimatebeam spread)
        cdata(20)=zsges              ! model elevation at radar site
        cdata(21)=thiserr
        cdata(22)=two

!       if(vadid(ivad)=='0303LWX') then
!          dist2max=max(dist2max,dist)
!          dist2min=min(dist2min,dist)
!       end if

        do i=1,maxdat
           cdata_all(i,ndata)=cdata(i)
        end do

     else
        notgood = notgood + 1
     end if

  end do

  close(lnbufr) ! A simple unformatted fortran file should not be mixed with bufr I/O
  write(6,*)'READ_L2RW_PROCESSED:  ',trim(outmessage),' reached eof on 2 superob radar file'
!16.
  write(6,*)'READ_L2RW_PROCESSED: nsuper2_in,nsuper2_kept=',nsuper2_in,nsuper2_kept
  write(6,*)'READ_L2RW_PROCESSED: # no vad match   =',novadmatch
  write(6,*)'READ_L2RW_PROCESSED: # out of vadrange=',ioutofvadrange
  write(6,*)'READ_L2RW_PROCESSED: # bad azimuths=',ibadazm
  write(6,*)'READ_L2RW_PROCESSED: # bad winds   =',ibadwnd
  write(6,*)'READ_L2RW_PROCESSED: # bad dists   =',ibaddist
  write(6,*)'READ_L2RW_PROCESSED: # bad stahgts =',ibadstaheight
  write(6,*)'READ_L2RW_PROCESSED: # bad obshgts =',ibadheight
  write(6,*)'READ_L2RW_PROCESSED: # bad errors  =',ibaderror
  write(6,*)'READ_L2RW_PROCESSED: # bad vadwnd  =',ibadvad
  write(6,*)'READ_L2RW_PROCESSED: # bad fit     =',ibadfit
  write(6,*)'READ_L2RW_PROCESSED: # num thinned =',kthin
  write(6,*)'READ_L2RW_PROCESSED: # notgood0    =',notgood0
  write(6,*)'READ_L2RW_PROCESSED: # notgood     =',notgood
  write(6,*)'READ_L2RW_PROCESSED: # hgt belowsta=',iheightbelowsta
  write(6,*)'READ_L2RW_PROCESSED: timemin,max   =',timemin,timemax
  write(6,*)'READ_L2RW_PROCESSED: errmin,max    =',errmin,errmax
  write(6,*)'READ_L2RW_PROCESSED: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
  write(6,*)'READ_L2RW_PROCESSED: iaaamin,max,8*max_rrr=',iaaamin,iaaamax,8*max_rrr

!********************************************************************************
!if(1==0) then !lippi
!!  Next process level 2.5 and 3 superobs
!!  Bigger loop over first level 2.5 data, and then level3 data
!end if !lippi started on line 822

!if(1==0) then !lippi
!! Write out vad statistics
!end if !lippi

!if(1==0) then !lippi tail doppler
!end if !lippi tail doppler
!********************************************************************************
! Write observation to scratch file
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
  deallocate(cdata_all)

900 continue

  return

end subroutine read_l2rw_processed

end module read_l2bufr_mod

SUBROUTINE tllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    tllv             
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none

  real(r_kind),intent(in   ) :: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(  out) :: tlm,tph

  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM-TLMO
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC+STPH0*SPH
  TLM=ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH-STPH0*CC)

END SUBROUTINE tllv

SUBROUTINE invtllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invtllv
!
!   prgrmmr:
!
! abstract:  inverse of tllv:  input ALM,APH is rotated lon,lat
!                   output is earth lon,lat, TLM,TPH
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none

  real(r_kind),intent(in   ) :: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(  out) :: tlm,tph

  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC-STPH0*SPH
  TLM=tlmo+ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH+STPH0*CC)
  
END SUBROUTINE invtllv
