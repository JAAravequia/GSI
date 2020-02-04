subroutine read_viscams(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_viscams.f90
!   prgmmr: Runhua Yang                               date: 2019-07-05
!
! abstract:  to read in visibility estimation through Image Analytics. Apply nonlinear Transformation to the data.
!
! program history log:
!   2019-07-05 Yang  - use read_mitm_mxtm.f90 and read_satmar as templates
!   2020-01-07 Yang  - compute the inflation values (see function computenorm).
!                      The inflating value is converted into an integer and stored in visqm.
!                      Remove qcthresd since the revised capping value is 10 miles.
!
!   input argument list:
!     infile   - unit from which to read data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     gstime   - analysis time in minutes from reference date ???
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of obs read 
!     ndata    - number of obs retained for further processing
!     nodata   - number of obs retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f95/2003
!   machine:  WCOSS
!
!$$$

!

  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,half,&
      three,four,r60inv,r10,r100,r2000,t0c
  use convinfo, only: nconvtype,  icuse,ictype,ioctype,ctwind
  use gridmod, only: regional,nlon,nlat,tll2xy,txy2ll,&
      rlats,rlons,twodvar_regional
  use deter_sfc_mod, only: deter_sfc2
  use obsmod, only: ianldate,bmiss,hilbert_curve
  use qcmod, only:  pvis,pcldch,scale_cv,estvisoe,vis_thres
  use nltransf, only: nltransf_forward
  use gsi_4dvar, only: iwinbgn,time_4dvar

  use sfcobsqc,only: init_rjlists,get_usagerj,destroy_rjlists
  use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
  use hilbertcurve,only: init_hilbertcurve, accum_hilbertcurve, &
                         apply_hilbertcurve,destroy_hilbertcurve
  use mpimod, only:npe
  use gsi_io, only: verbose
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  integer(i_kind)                       ,intent(in   ) :: lunout
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: gstime,twind
  integer(i_kind),dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: adjcoef =1.0_r_kind

! Declare local variables
  character(len=12) :: myname

  character(len=8) :: c_prvstg='veiacams'
  character(len=8) :: c_sprvstg='veiacams'
  character(len=82):: filename
! initializing c_station_id 
  character(len=8) :: c_station_id='VEIA1000'

  integer(i_kind) :: nreal,i,lunin
  integer(i_kind) :: visqm
  integer(i_kind) :: nmind
  integer(i_kind) :: idomsfc
  integer(i_kind) :: nc,k,ilat,ilon,nchanl
  integer(i_kind) :: idate,iout,maxobs,icount,ierr
  integer(i_kind) :: stnelev4, nid
  integer(i_kind) :: sunangle,veiaqc,index
  real(r_kind) :: dlat,dlon,dlat_earth,dlon_earth,toff,t4dv
  real(r_kind) :: dlat_earth_deg,dlon_earth_deg
  real(r_kind) :: visoe,tdiff,tempvis,visout
  real(r_kind) :: rminobs,pob,dlnpob,obval
  real(r_kind) :: stnelev
  real(r_kind)  :: rlon4,rlat4,obvalmi,rnid
  real(r_kind) :: usage,tsavg,ff10,sfcr,zz
  real(r_kind) :: rinflate 
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out

  integer(i_kind) :: thisobtype_usage   
  integer(i_kind) :: invalidkx, invalidob, n_outside
  integer(i_kind) :: ivisdate(5),y4m2d2,h2m2s2

  real(r_double) :: udbl,vdbl,rrnid
  logical :: outside
  logical lhilbert
  logical  linvalidkx, linvalidob
  logical  fexist
  logical print_verbose

  real(r_double) :: rstation_id
  real(r_double) :: r_prvstg
  real(r_double) :: r_sprvstg

  ! hardwired kx for webcams
  integer(i_kind) :: kx=999_i_kind   

  equivalence(rstation_id, c_station_id)

  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  print_verbose=.true.
  if(verbose)print_verbose=.true.

  lunin=11_i_kind
  myname='READ_VISCAMS'
  nreal=18
  visqm=0

  nchanl=0
  ilon=2
  ilat=3

  nc=0
  conv: do i=1,nconvtype                     
     if(trim(obstype) == trim(ioctype(i)) .and. ictype(i)==kx) then
       nc=i
       exit conv
     end if
  end do conv

  if(nc > 0)then
     write(6,*) myname,' found ',nc, ' matching obstypes in convinfo. proceed  with ob processing'
  else
     write(6,*) myname,' no matching obstype found in convinfo ',obstype
     return
  end if

  ! Try opening text file, if unable, print error to the screen
  !  and return to read_obs.F90
  open(lunin,file=trim(infile),form='formatted',iostat=ierr)
  if (ierr/=0) then
     write(6,*)myname,':ERROR: Trouble opening input file: ',trim(infile),' returning to read_obs.F90...'
     return
  end if


  ! Find number of reports
  maxobs = 0
! read in the header 
  read(lunin,90) filename
  readloop: do 
      read(lunin,91,end=101) nid,rlat4,rlon4,stnelev4,y4m2d2,h2m2s2,obvalmi,sunangle,veiaqc
      maxobs=maxobs+1
  end do readloop
101 continue
   write(6,*)myname,': maxobs=',maxobs

  if (maxobs == 0) then
     write(6,*)myname,': No reports found.  returning to read_obs.F90...'
     return
  end if

  lhilbert = twodvar_regional .and. hilbert_curve

  call init_rjlists
  if (lhilbert) call init_hilbertcurve(maxobs)
  if (twodvar_regional) call init_ndfdgrid
  allocate(cdata_all(nreal,maxobs))
  cdata_all=zero
  iout=0
  invalidkx=0
  invalidob=0
  n_outside=0

  rewind(lunin)
! read in the header 
  read(lunin,90) filename
  loop_readobs: do icount=1,maxobs
      read(lunin,91,end=101) nid,rlat4,rlon4,stnelev4,y4m2d2,h2m2s2,obvalmi,sunangle,veiaqc
90 format(a82)
91 format(I4,5X,F7.4,4X,F9.4,3X,I4,3X,I8,3X,I6,5X,F5.2,7x,I3,3x,I3)
!--------------------------------------------------------------------
! Information from Mike (viscams provider):
!
!  1) if the sunangle>105,i.e.,the sun is 15 degree below the horizontal level, the observation quality is not good.
!  2) veiaqc is a metrics of the quality, from 1-100
!  3) capping value: 
!         vis_capping value =5 miles before Dec. 9 2019
!         vis_capping value =10 miles after Dec. 9 2019
!--------------------------------------------------------------------
      if (sunangle>=105 )  cycle loop_readobs
!     compute the rinflate value
      index=veiaqc
      if (index < 26) index=26
      if (index >100) index=100
      rinflate=computenorm(index,adjcoef)
      visqm=int(rinflate*10)


      !   date arrary
      ivisdate(1)=y4m2d2/10000            ! four-digit year
      ivisdate(2)=mod(y4m2d2,10000)/100   ! two-digit months
      ivisdate(3)=mod(y4m2d2,100)         ! two-digit day
      ivisdate(4)=h2m2s2/10000            ! two-digit hour
      ivisdate(5)=mod(h2m2s2,10000)/100   ! two-digit minute

!     write (6,*) myname,'y4m2d2*10000=', y4m2d2*100,  ivisdate(4)
!     write (6,*) myname,'idate=', idate,t4dv,tdiff

      call w3fs21(ivisdate, nmind)
      rminobs=real(nmind,r_double)
      t4dv =(rminobs-real(iwinbgn,r_kind))*r60inv ! wrt the beginning of 4dvar window (6hr window length)
      tdiff=(rminobs-gstime)*r60inv   ! note: both rminobs and gstime is w.r.t.  a reference time
                                      ! so, tdiff is the departure of
                                      ! observation time from
                                      ! the analysis time
! TEST
!       write (6,*) myname, 'date=', ivisdate(1),ivisdate(2),ivisdate(3),ivisdate(4),ivisdate(5)
!       write (6,*) myname,'usagerj rminobs,iwindbgn,gstime', rminobs, real(iwinbgn,r_kind),gstime
!       write (6,*) myname,'4 get_usagerj t4dv,tdiff', t4dv,tdiff

! don't include the data that falls out the analysis time window
       if (abs(tdiff)>ctwind(nc).or.(abs(tdiff)>twind)) cycle loop_readobs
       rnid=float(nid)
       rrnid=dble(rnid)
       write(c_station_id,'(f8.4)') rrnid

!      write(6,*)myname,': rnid,dble(nid)=', rnid,rrnid,'c_station_id=', c_station_id
       
! convert obvalmi from mile to meter, then use obval thereafter
!............................................................
      obval=obvalmi*1600.
      stnelev=real(stnelev4)
!
!-------------------------------------------
       if (kx /= 999) then
          linvalidkx=.true.
          invalidkx=invalidkx+1
          write(6,*)myname,': Invalid report type: icount,kx ',icount,kx
       else
          linvalidkx=.false.
       endif

      if (obval > r0_1_bmiss) then
         linvalidob=.true.
         invalidob=invalidob+1
         write(6,*)myname,': Invalid ob value: icount,obval ',icount,obval
      else
         linvalidob=.false.
      endif

      dlat_earth_deg=real(rlat4,kind=r_kind)
      dlon_earth_deg=real(rlon4,kind=r_kind)
      if(abs(dlat_earth_deg)>r90 .or. abs(dlon_earth_deg)>r360) cycle loop_readobs
      if (dlon_earth_deg == r360) dlon_earth_deg=dlon_earth_deg-r360
      if (dlon_earth_deg < zero)  dlon_earth_deg=dlon_earth_deg+r360

!     write(6,*)myname,'dlon_earth_deg dlat_earth_deg', dlon_earth_deg,dlat_earth_deg
      dlon_earth=dlon_earth_deg*deg2rad
      dlat_earth=dlat_earth_deg*deg2rad

      outside=.false. !need this on account of global gsi
      if(regional)then
         call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
         if(outside) n_outside=n_outside+1
      else
         dlat = dlat_earth
         dlon = dlon_earth
         call grdcrd1(dlat,rlats,nlat,1)
         call grdcrd1(dlon,rlons,nlon,1)
      endif
         write(6,*)myname,': OUTSIDE, m_outside', outside,n_outside
      if (linvalidkx .or. linvalidob .or. outside)  cycle loop_readobs

! count the valid observation
!--------------------------------
      iout=iout+1

! Set usage variable   
      usage = zero
      if(icuse(nc) <= 0)usage=100._r_kind

      udbl=0._r_double
      vdbl=0._r_double
!
! compute idate in min. but with 00 in minute
!-----------------

      idate= y4m2d2*100_i_kind + ivisdate(4)
 

      call get_usagerj(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                         dlon_earth,dlat_earth,idate,tdiff, udbl,vdbl,usage) 
!     write (6,*) myname,'after get_usagerj'


! Get information from surface file necessary for conventional data here
      call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

      if(lhilbert) &
      call accum_hilbertcurve(usage,c_station_id,c_prvstg,c_sprvstg, &
           dlat_earth,dlon_earth,dlat,dlon,t4dv,toff,nc,kx,iout)

      pob=101.0_r_kind  ! Assume 1010 mb = 101.0 cb
      dlnpob=log(pob)   ! ln(pressure in cb)

!viscams:
!      visoe is in NLTR space, and is read in from the namelist. 
!......................................................................

! inflate the observation error 
      visoe=estvisoe*(one+rinflate)
!      write (6,*) myname,'inflating', rinflate,visoe
      cdata_all(1,iout)=visoe                   ! visibility error (cb)
      cdata_all(2,iout)=dlon                    ! grid relative longitude
      cdata_all(3,iout)=dlat                    ! grid relative latitude
!......................................................................
! simple QC check: if an observation vis is negative, assign it as bmiss
! if obs. = zero, reassign it as one_r_kind
! about bmiss:
! #ifdef ibm_sp !  real(r_kind), parameter:: bmiss = 1.0e11_r_kind !#else
!  real(r_kind), parameter:: bmiss = 1.0e9_r_kind !#endif
! in setupvis: missing data is checked and assigned not use in muse
! visthres is much smaller than bmiss
! do NLTR when this formula holds: (obval> zero .and. obval<=vis_thres)
!......................................................................
! 1/7/2020: the capping value=10 miles, equal to vis_thres
!-------------------------------------------------------------------------------

      if(obval < zero)   cdata_all(4,iout)=bmiss
      if(obval > vis_thres) cdata_all(4,iout)=bmiss
      if(obval == zero)  obval=max(obval,one)
      if(obval> zero .and. obval <= vis_thres) then
        tempvis=obval
        call nltransf_forward(tempvis,visout,pvis,scale_cv)
        cdata_all(4,iout) = visout
      endif
!----------------------------------------------------

      cdata_all(5,iout)=rstation_id             ! station id
      cdata_all(6,iout)=t4dv                    ! time
      cdata_all(7,iout)=nc                      ! type
      cdata_all(8,iout)=visoe*three             ! max error
      cdata_all(9,iout)=visqm                   ! quality mark   ! 10 times of inflating value
      cdata_all(10,iout)=usage                  ! usage parameter
      if (lhilbert) thisobtype_usage=10         ! save INDEX of where usage is stored 
                                                !for hilbertcurve cross validation (if requested)
      cdata_all(11,iout)=idomsfc                ! dominate surface type
      cdata_all(12,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
      cdata_all(13,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
      cdata_all(14,iout)=stnelev                ! station elevation (m)
      cdata_all(15,iout)=stnelev                ! observation height (m) use station elevatio
      cdata_all(16,iout)=zz                     ! terrain height at ob location
      cdata_all(17,iout)=r_prvstg               ! provider name
      cdata_all(18,iout)=r_sprvstg              ! subprovider name

      if(lhilbert) &
        call apply_hilbertcurve(maxobs,obstype,cdata_all(thisobtype_usage,1:maxobs))
  enddo loop_readobs


  nread=maxobs
  ndata=iout
  nodata=iout
  !  write(6,*) 'read_viscams: WANT TO print ndata=iout !!!', iout,ndata
  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     do k=1,nreal
        cdata_out(k,i)=cdata_all(k,i)
     end do
  end do

  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) cdata_out

  deallocate(cdata_all)
  deallocate(cdata_out)

  call destroy_rjlists
  if (lhilbert) call destroy_hilbertcurve
  if (twodvar_regional) call destroy_ndfdgrid

  close(lunin)

contains

  pure function computenorm(index,adjcoef) result (rinflate)

  use kinds, only: r_kind,i_kind

  implicit none
  real(r_kind),parameter :: sigma=1.0 ! head of veia data
  integer(i_kind),parameter :: nx=75   !
  integer(i_kind), intent(in) :: index
  real(r_kind), intent(in) :: adjcoef
  real(r_kind) ::  rinflate, s2,cs2,y1,y2,y,x,x2,delta,pi
!
  pi=3.1416_r_kind

!---------------------------------------------------------------------------------------------------------
! Tentative formula of inflating values:
! 1. curve is  
!     f(x)= exp -[(x^2)/2*(sigma*sigma)] /[sqrt(2*pi)*sigma]
!     sigma = 1.0 
! 2. make the range of confidence index proportional to the range of 3*sigma,so
!     the x interal is:  
!     delta=3.0*sigma/float(nx) 
!      here, nx is the range of confidence index, max-min
!     minimum confidence index =26, max confidence index = 100
! 3.  tuning the parameters or the formula as: make the inflating value large if an
!     confidence index closes to the minimum; and make the inflating value small if an index closes 100.
!---------------------------------------------------------------------------------------------------------

  delta=3.0*sigma/float(nx)
  s2= sigma*sigma

  x=float(index-26)*delta
  x2=x*x
  cs2=2.0*s2
  y1=exp(-1.*x2/cs2)
  y2= sqrt(2.0*pi)*sigma
  y= y1/y2
  rinflate=y*adjcoef
  end function computenorm

end subroutine read_viscams
