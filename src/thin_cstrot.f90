module thin_cstrot
!$$$ subprogram documenation block
!                .      .    .                                       .
! subprogram:    cstrot
!   prgmmr: Zhu          org: jcsda                date: 2015-06-05
!
! abstract:  This module is used for CSTROT satellite data thinning.
!
! program history log:
!   2015-06-05 zhu    - start, collect all CSTROT code in this module.
!
! Subroutines Included:
!   sub thinning_std
!   sub thinning_params
!   sub random_pts
!   sub distancell
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind), parameter :: nr_max=20        !Max nesting target/domain numbers
  integer(i_kind), parameter :: tot_sensor = 59  !Total CSTROT thinning sensors
  integer(i_kind), parameter :: mcha = 616       !Max channels (using iasi)
  integer(i_kind), parameter :: ssnum = 2        !sensors using std_ang

! set default to private
  private
! set subroutines to public
  public :: thinning_std
  public :: thinning_params
  public :: random_pts
  public :: distancell
  
! set passed variables to public
  public :: thin_method, represent, thin_reso1, thin_reso2
  public :: thin_skip1, thin_skip2, npts, coast_ice
  public :: ntarget, ndomain, slonc, slatc, sradius
  public :: lonw, lone, lats, latn
  public :: std1all, std2ratio, stdang_all, nthband_all, thbandx_all
  public :: sis_all, nch_all, nsensor

  integer(i_kind):: thin_method,represent
  real(r_kind):: thin_reso1,thin_reso2
  integer(i_kind):: thin_skip1,thin_skip2
  integer(i_kind):: npts(5),coast_ice(2)

  integer(i_kind):: ntarget, ndomain
  real(r_kind), dimension(nr_max):: slonc,slatc,sradius
  real(r_kind), dimension(nr_max):: lonw,lone,lats,latn

  real(r_kind), dimension(mcha,tot_sensor)   :: std1all
  real(r_kind), dimension(     tot_sensor)   :: std2ratio
  real(r_kind), dimension(200,mcha,ssnum)    :: stdang_all !(angle,band,sensor)
  integer(i_kind), dimension(      tot_sensor) :: nthband_all
  integer(i_kind), dimension(mcha, tot_sensor) :: thbandx_all

  character(20), dimension(    tot_sensor) :: sis_all
  integer(i_kind), dimension(  tot_sensor) :: nch_all
  integer(i_kind) :: nsensor
  character(5) :: ssa(ssnum)
  integer(i_kind) :: sfov(ssnum),cha(ssnum)

contains

subroutine thinning_std(sis,obstype,ndata,nchanl,nreal,nele,itxmax,data_all, &
                        thin_std,thin_use,nthband,thbandx)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    thinning_std        thinning satellite data with std/mean
!   prgmmr: zhu          org: jcsda                date: 2014-05-08
!
! abstract:  This routine compute satellite data mean and standary deviation for
! thinning
!
! program history log:
! 2014-05-08  zhu start
!
!   input argument list:
!     ndata    - task specific number of observations keep for assimilation
!     nchanl   - channel number for the sensor
!     nreal    - maxinfo + nstinfo
!     nele     - total number of data elements  (nele  = nreal   + nchanl)
!     itxmax   - maximum number of observations
!     data_all - observation data array
!
!   output argument list:
!     thin_std - std and mean for data_all
!     thin_use - thinning resluts, use or not: +/-1 regular thinning, -5:
!     overlay,
!         +/-4: within ntarget, +/-3 within ndomain, +/-2 coastline+sea_edge
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,two,one,deg2rad,rad2deg
  implicit none

! Declare passed variables
  character(20)                      ,intent(in   ) :: sis
  character(len=*)                   ,intent(in   ) :: obstype
  integer(i_kind)                    ,intent(in   ) :: ndata,itxmax
  integer(i_kind)                    ,intent(in   ) :: nchanl,nreal,nele
  real(r_kind),dimension(nele,itxmax),intent(inout) :: data_all
  real(r_kind),dimension(nchanl,itxmax),   intent(inout) :: thin_std
  integer(i_kind),dimension(nchanl,itxmax),  intent(inout) :: thin_use
  integer(i_kind)                    ,intent(inout) :: nthband
  integer(i_kind),dimension(nchanl)  ,intent(inout) :: thbandx

! Declare local variables
  integer(i_kind):: i,j,k,l,m,n,mm,mx,nx
  integer(i_kind):: n_used,n_overlay,n_bad_ch
  integer(i_kind):: dll,ii,jj

  real(r_kind),dimension(ndata):: ttime,tlon,tlat
  real(r_kind),dimension(nchanl,ndata):: tball
  real(r_kind),allocatable,dimension(:,:):: tb_tim,tb2d
  real(r_kind),allocatable,dimension(:,:,:):: tb_mean,tb_stdv
  integer(i_kind),allocatable,dimension(:,:):: tb_num, tb_num2, fov_mean
  integer(i_kind),allocatable,dimension(:,:,:):: tb_num1,tb_idx
  integer(i_kind),allocatable,dimension(:) :: nkeep
  integer(i_kind),allocatable,dimension(:) :: isflg
  real(r_kind),allocatable,dimension(:,:):: isflg_mean,isflg_stdv

  real(r_kind),dimension(nchanl):: std1,std2
  real(r_kind) :: stdx
  real(r_kind) :: min_tb, max_tb
  integer(i_kind), dimension(0:7) :: nctx
  integer(i_kind):: nct_all
  integer(i_kind):: num_min, num_max
  integer(i_kind):: lu,pts
  integer(i_kind), dimension(nchanl):: sfc_band       !0-sfc, >=1-air

  integer(i_kind)               :: nr
  integer(i_kind)               :: nkeep0                !The closest point to keep
  real(r_kind)                  :: slon1, slat1, plon2, plat2, disx, dismin
  real(r_kind),dimension(0:3)   :: sfcpct

  integer(i_kind),dimension(ndata)         :: iscan
  real(r_kind),allocatable,dimension(:,:)  :: stdang

  integer(i_kind)                          :: ifov, icha, iallo, xfov, n_ch

  logical lgtarget, lgdomain, lgcoast, replace

  print*,'---ZT6 call thinning_std'

  sfc_band(:) = 1            !first set all bands as sensitive to atmosphere
  if(obstype.eq.'atms') then
     print*,'--- zt: set sfc_band for:',obstype
     sfc_band(1:4) = 0       !window channels
     sfc_band(16) = 0
  endif
  if(obstype.eq.'amsua') then
     print*,'--- zt: set sfc_band for:',obstype
     sfc_band(1:3) = 0       !window channels
     sfc_band(15) = 0
  endif
  if(obstype.eq.'amsub'.or.obstype.eq.'mhs') then
     print*,'--- zt: set sfc_band for:',obstype
     sfc_band(1) = 0       !window channels
  endif

print*,'**ZT6 nr_max,thin_method,thin_reso1,represent:',nr_max,thin_method,thin_reso1,represent
print*,'**ZT6 npts,coast_ice:',npts,coast_ice
print*,'**ZT6 ssnum,ssa,sfov,cha:',ssnum,ssa,sfov,cha

  iallo = 0
  do m=1,ssnum
     if(sis(1:5).eq.ssa(m)) then
        ifov = sfov(m)
        icha = cha(m)
        if(icha.ne.nchanl) then
          print*,'####thinning_std, stdang_icha to read is not match.',icha,nchanl
          stop
        endif
        allocate(stdang(ifov,nchanl))
        iallo = 1
        exit
     endif
  enddo
  if(iallo.eq.0) then
    ifov = 2                           !not angle idependency
    allocate(stdang(ifov,nchanl))      !used for std1, std2
    iallo = 1
  endif

  if(thin_method.eq.1) dll = thin_reso1      !using 2x2 degree box
  if(thin_method.eq.2) dll = thin_reso2      !using 2x2 degree box
  ii=360/dll + 1
  jj=180/dll + 1
  allocate(tb_mean(nchanl,ii,jj),tb_stdv(nchanl,ii,jj),tb_num1(nchanl,ii,jj))
  allocate(tb_tim(ii,jj),tb2d(ii,jj))
  allocate(tb_num(ii,jj),tb_num2(ii,jj),fov_mean(ii,jj))
  allocate(isflg_mean(ii,jj),isflg_stdv(ii,jj))
  allocate(isflg(ndata))

  write(6,*) '--- ZT32 thin_std for sis,ndata,ii,jj:',sis,ndata,ii,jj

!.............. compute mean,min,max ...............

  do n=1,ndata
     ttime(n)  = data_all(2,n)        !obs time hour
     iscan(n)  = data_all(8,n)        !scan position
     tlon(n)   = data_all(30,n)
     tlat(n)   = data_all(31,n)
     tball(1:nchanl,n) = data_all(nreal+1:nreal+nchanl,n)

     sfcpct(0) = data_all(11,n)       ! sea percentage of
     sfcpct(1) = data_all(12,n)       ! land percentage
     sfcpct(2) = data_all(13,n)       ! sea ice percentage
     sfcpct(3) = data_all(14,n)       ! snow percentage

     isflg(n) = 0
     if(sfcpct(0) > 0.99_r_kind)then
        isflg(n) = 0
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg(n) = 1
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg(n) = 2
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg(n) = 3
     else
        isflg(n) = 4
     end if
  end do

  fov_mean(:,:) = 0
  tb_num(:,:) = 0
  tb_num1(:,:,:) = 0
  tb_num2(:,:) = 0
  tb_mean(:,:,:) = 0.0
  tb_stdv(:,:,:) = 0.0

  isflg_mean(:,:) = 0.0
  isflg_stdv(:,:) = 0.0

  n_overlay = 0
  n_used = 0
  n_bad_ch = 0

  do n=1,ndata

    if(tlon(n).gt.180.0) tlon(n)=tlon(n)-360.0
    i=floor(tlon(n)/dll)+180/dll + 1
    j=floor(tlat(n)/dll)+90/dll + 1

    if(n.le.5) print*,'===ttime(n)=',n,ttime(n)
    if(n/1000*1000.eq.n) print*,'===ttime(n)=',n,ttime(n)
    if(tb_num(i,j).eq.0) tb_tim(i,j)=ttime(n)
    if(tb_num(i,j).gt.0) then
       if( abs( tb_tim(i,j)-ttime(n) ).gt.1.0 ) then
          n_overlay = n_overlay + 1
          goto 100
       end if
    end if
    do l=1,nchanl
       if(tball(l,n).ge.100.0.and.tball(l,n).lt.400.0) then
         tb_mean(l,i,j) = tb_mean(l,i,j) + tball(l,n)
         tb_num1(l,i,j) = tb_num1(l,i,j) + 1
       else
         n_bad_ch = n_bad_ch + 1
       endif
    end do

    if(isflg(n).ge.0.and.isflg(n).le.4) then
       isflg_mean(i,j) = isflg_mean(i,j) + isflg(n)
       tb_num2(i,j) = tb_num2(i,j) + 1
    endif

    fov_mean(i,j) = fov_mean(i,j) + iscan(n)

    tb_num(i,j) = tb_num(i,j) + 1
    n_used = n_used + 1

 100 continue

  end do

  do j=1,jj
  do i=1,ii
     do l=1,nchanl
        if(tb_num1(l,i,j).ge.1) then
           tb_mean(l,i,j) = tb_mean(l,i,j) / float(tb_num1(l,i,j))
        endif
     end do

     if(tb_num2(i,j).ge.1) then
        isflg_mean(i,j) = isflg_mean(i,j) / float(tb_num2(i,j))
     endif

     if(tb_num(i,j).ge.1) then
        fov_mean(i,j) = NINT( fov_mean(i,j) / float(tb_num(i,j)) )
     endif
  end do
  end do

  write(6,*) '--- n_overlay points:',n_overlay
  write(6,*) '--- n_used points:',n_used
  write(6,*) '--- n_bad_ch points:',n_bad_ch

  do l=1,nchanl
     tb2d(:,:) = tb_mean(l,:,:)
     min_tb = minval(tb2d, mask=tb2d.gt.0.001)
     max_tb = maxval(tb2d)
     write(6,1000) l,min_tb,max_tb
  end do
1000 format('--- l, min/max(tb_mean(l,i,j)):',i3,1x,g12.5,1x,g12.5)
     min_tb = minval(isflg_mean)
     max_tb = maxval(isflg_mean)
     write(6,1002) min_tb,max_tb
1002 format('--- min/max(isflg_mean(i,j)):',g12.5,1x,g12.5)
     min_tb = minval(fov_mean)
     max_tb = maxval(fov_mean)
     write(6,*) '--- min/max(fov_mean(i,j)):',sis,min_tb,max_tb

!...... compute standary deviation ............

  num_min=minval(tb_num)
  num_max=maxval(tb_num)
  write(6,*) '...#1 num_min,num_max=',num_min,num_max
  allocate(tb_idx(num_max,ii,jj))
  tb_idx(:,:,:)=0
  tb_num(:,:) = 0
  tb_num1(:,:,:) = 0
  tb_num2(:,:) = 0

  do n=1,ndata

    if(tlon(n).gt.180.0) tlon(n)=tlon(n)-360.0
    i=floor(tlon(n)/dll)+180/dll + 1
    j=floor(tlat(n)/dll)+90/dll + 1

    if(tb_num(i,j).eq.0) tb_tim(i,j)=ttime(n)
    if(tb_num(i,j).gt.0) then
       if( abs( tb_tim(i,j)-ttime(n) ).gt.1.0 ) then
          goto 200
       end if
    end if
    do l=1,nchanl
       if(tball(l,n).ge.100.0.and.tball(l,n).lt.400.0) then
          tb_stdv(l,i,j)=tb_stdv(l,i,j)+(tb_mean(l,i,j)-tball(l,n))**2
          tb_num1(l,i,j) = tb_num1(l,i,j) + 1
       endif
    end do

    if(isflg(n).ge.0.and.isflg(n).le.4) then
       isflg_stdv(i,j) = isflg_stdv(i,j) + (isflg_mean(i,j)-isflg(n))**2
       tb_num2(i,j) = tb_num2(i,j) + 1
    endif

    tb_num(i,j) = tb_num(i,j) + 1
    m = tb_num(i,j)
    tb_idx(m,i,j) = n

 200 continue

  end do

  do j=1,jj
  do i=1,ii
     do l=1,nchanl
        if(tb_num1(l,i,j).ge.1) then
           tb_stdv(l,i,j)=sqrt( tb_stdv(l,i,j)/float(tb_num1(l,i,j)) )
        else
           tb_stdv(l,i,j) = -99.9
        endif

     end do

     if(tb_num2(i,j).ge.1) then
        isflg_stdv(i,j) = sqrt( isflg_stdv(i,j)/float(tb_num2(i,j)) )
     else
        isflg_stdv(i,j) = -99.9
     endif
  end do
  end do

  do l=1,nchanl
     tb2d(:,:) = tb_stdv(l,:,:)
     min_tb = minval(tb2d)
     max_tb = maxval(tb2d)
     write(6,1001) l,min_tb,max_tb
  end do
1001 format('--- l, min/max(tb_stdv(l,i,j)):',i3,1x,g12.5,1x,g12.5)
     min_tb = minval(isflg_stdv)
     max_tb = maxval(isflg_stdv)
     write(6,1003) min_tb,max_tb
1003 format('--- min/max(isflg_stdv(i,j)):',g12.5,1x,g12.5)


  num_min=minval(tb_idx)
  num_max=maxval(tb_idx)
  write(6,*) '...tb_idx(min/max)=',num_min,num_max
  num_min=minval(tb_num)
  num_max=maxval(tb_num)
  write(6,*) '...#2 num_min,num_max=',num_min,num_max

j=jj/2
write(6,*) '---ZT32-thin, tb_num:',tb_num(:,j)
write(6,*) '---ZT32-thin, fov_mean:',fov_mean(:,j)
write(6,*) '---ZT32-thin, tb_stdv:',tb_stdv(1,:,j)

!................ thinning using std ...................

  thin_use(:,:) = -1        !initialization with not use
  thin_std(:,:) = -9.9      !initialization missing value
! lgtarget = .false.
! lgdomain = .false.
! lgcoast  = .false.
  allocate(nkeep(num_max))

  do n=1,nsensor
     if(trim(adjustl(sis_all(n))) == trim(adjustl(sis))) then
        n_ch = nch_all(n)
        print*,'+++Extract std1_2 data for:',trim(adjustl(sis_all(n))),n_ch,nchanl
        if(n_ch /= nchanl) then
           write(*,*) 'In Thinning_Params, wrong n_ch read in for:',sis
           stop
        endif

        if(obstype.eq.'amsua'.or.obstype.eq.'atms') then
           if(obstype.eq.'amsua') m =1
           if(obstype.eq.'atms')  m= 2
           do l=1,n_ch
              stdang(1:ifov,l) = stdang_all(1:ifov,l,m)
           end do
        else
           stdang(1,1:n_ch) = std1all(1:n_ch,n)
           stdang(2,1:n_ch) = stdang(1,1:n_ch) * std2ratio(n)
        endif

        nthband = nthband_all(n)
        thbandx(1:nthband) = thbandx_all(1:nthband,n)

        exit
     end if
  end do

  print*,'ddd thinning_std: nthband,thbandx=',nthband,thbandx(1:nthband)

  nctx(:)=0

  do k=1,nthband

     if(nthband.lt.nchanl) l=thbandx(k)           !channel used for thinning
     if(nthband.eq.nchanl) l = k

     print*,'ddd compute STD for sis,channel:',sis,l
     print*,'ddd1 ifov,nchanl:',ifov,nchanl
     print*,'ddd2 stdang(*,l):',stdang(:,l)

     do j=1,jj
     do i=1,ii

        lgtarget = .false.
        lgdomain = .false.
        lgcoast  = .false.

        mm = tb_num(i,j)
        stdx = tb_stdv(l,i,j)

        iallo = 0
        do m=1,ssnum
           if(sis(1:5).eq.ssa(m)) then
             xfov = fov_mean(i,j)
             do n = 1, nchanl
                std1(n) = stdang(xfov,n)
                std2(n) = stdang(xfov,n)*2.0 
             enddo
             iallo = 1
             exit
           endif
        enddo
        if(iallo.eq.0) then
          std1(:) = stdang(1,:)
          std2(:) = stdang(2,:)
          iallo = 1
        endif

        if (mm.eq.0.or.stdx.lt.0.0) then
           nctx(0) = nctx(0) + 1
        else if (mm.eq.1) then
           nx = tb_idx(mm,i,j)
           thin_use(l,nx) = 1
           thin_std(l,nx) = stdx
           if(represent.eq.2) data_all(l,nx) = tb_mean(l,i,j)
           nctx(1) = nctx(1) + 1
        else
           do m = 1,mm
              nx = tb_idx(m,i,j)
              thin_std(l,nx) = stdx
             !if(represent.eq.2) data_all(l,nx) = tb_mean(l,i,j)
           enddo

           !----- select more points in target domains -----
           if(ntarget.ge.1) then
              do nr=1,ntarget
                 if(slonc(nr).gt.180.0) slonc(nr)=slonc(nr)-360.0
                 slon1 = slonc(nr)*deg2rad
                 slat1 = slatc(nr)*deg2rad
                 plon2 = ( (i-1)*dll - 180.0 )*deg2rad
                 plat2 = ( (j-1)*dll -  90.0 )*deg2rad

                 call distancell(slon1,slat1,plon2,plat2,disx)
                 if(disx.le.sradius(nr)) then
                    lgtarget = .true.
                    pts = npts(4)
                    nctx(5) = nctx(5) + pts
                    if(mod(nctx(5),500).eq.0) then
                       write(*,*) '***ZT6a-test',i,j,slon1,slat1,plon2,plat2,disx,sradius(nr)
                    end if

                    goto 300
                 endif
              end do
           endif
           !----- end of target domain selection -----------

           !----- select more points in interested domain -------
           if(ndomain.ge.1) then
              do nr=1,ndomain
                 plon2 = (i-1)*dll - 180.0
                 plat2 = (j-1)*dll -  90.0
                 if(lonw(nr).gt.180.0) lonw(nr)=lonw(nr)-360.0
                 if(lone(nr).gt.180.0) lone(nr)=lone(nr)-360.0

                 if(lonw(nr).le.lone(nr)) then
                    if(plon2.ge.lonw(nr).and.plon2.lt.lone(nr)) then
                    if(plat2.ge.lats(nr).and.plat2.lt.latn(nr)) then
                       lgdomain = .true.
                       pts = npts(5)
                       nctx(7) = nctx(7) + pts
                       goto 300
                    endif
                    endif
                 else
                    print*,'****** Double check: your selected domain covers 180E/180W'
                    if(plon2.le.lonw(nr).and.plon2.gt.lone(nr)) then
                    if(plat2.ge.lats(nr).and.plat2.lt.latn(nr)) then
                       lgdomain = .true.
                       pts = npts(5)
                       nctx(7) = nctx(7) + pts
                       goto 300
                    endif
                    endif
                 endif
              end do
           endif
           !----- end of select more points in interested domain -------

           !----- select less points along coastline and sea-ice
           if(sfc_band(l).eq.0) then
             if(coast_ice(1).eq.1.or.coast_ice(2).eq.1) then
             if(isflg_stdv(i,j).gt.0.2) then
                lgcoast = .true.
                pts = npts(1)
                nctx(6) = nctx(6) + pts
                goto 300
             endif
             endif
           endif
           !----- end of select less points along coastline and sea-ice

           if (stdx.ge.0.0.and.stdx.lt.std1(l)) then
              pts = npts(1)
              nctx(2) = nctx(2) + pts
           else if (stdx.ge.std1(l).and.stdx.lt.std2(l)) then
              pts = npts(2)
              nctx(3) = nctx(3) + pts
           else
              pts = npts(3)
              nctx(4) = nctx(4) + pts
           end if

 300       continue

           !----- initialization thin-data if within target/domain/coast
           if(lgtarget) then
             do m = 1,mm
                nx = tb_idx(m,i,j)
                thin_use(l,nx) = -4
                thin_std(l,nx) = stdx
             enddo
           endif
           if(lgdomain) then
             do m = 1,mm
                nx = tb_idx(m,i,j)
                thin_use(l,nx) = -3
                thin_std(l,nx) = stdx
             enddo
           endif
           if(lgcoast) then
             do m = 1,mm
                nx = tb_idx(m,i,j)
                thin_use(l,nx) = -2
                thin_std(l,nx) = stdx
             enddo
           endif

           !----- if specifed 0-pts, then no selection
           if(pts.eq.0 .and. .not.lgtarget .and. .not.lgdomain .and. .not.lgcoast) then
             do m = 1,mm
                nx = tb_idx(m,i,j)
                thin_use(l,nx) = -5
                thin_std(l,nx) = stdx
             enddo 
             goto 310
           endif

           if(mm.lt.pts) pts=mm

           !----- R=3 random points representation
           if(represent.eq.3) call random_pts(mm,pts,nkeep,num_max,i,j)

           !----- R=2 use average tb value
           if(represent.eq.2) then
             call random_pts(mm,pts,nkeep,num_max,i,j)
             do m=1,pts
                mx = nkeep(m)
                nx = tb_idx(mx,i,j)
                data_all(l,nx) = tb_mean(l,i,j)
             enddo
           endif

           !----- R=1 closest point representation (at lest one point is the closest one)
           if(represent.eq.1) then
             !..... a) select random points
             call random_pts(mm,pts,nkeep,num_max,i,j)
             !..... b) find closest point, and replace 1st point in a)
             dismin=-999.9            !initial distance with less than 0
             replace=.true.
             do m=1,mm
                nx = tb_idx(m,i,j)
                slon1 = tlon(nx)*deg2rad
                slat1 = tlat(nx)*deg2rad
                plon2 = ( (i-1)*dll - 180.0 )*deg2rad
                plat2 = ( (j-1)*dll -  90.0 )*deg2rad
                call distancell(slon1,slat1,plon2,plat2,disx)
                if(disx.lt.dismin.or.dismin.lt.0.0) then
                   dismin = disx
                   nkeep0 = m
                endif
             end do
             do m=1,pts
                if(nkeep(m).eq.nkeep0) replace=.false.
             end do
             if(replace) nkeep(1) = nkeep0
           endif

           do m=1,pts
              mx = nkeep(m)
              nx = tb_idx(mx,i,j)
              thin_use(l,nx) = 1
              if(lgtarget) thin_use(l,nx) = 4
              if(lgdomain) thin_use(l,nx) = 3
              if(lgcoast)  thin_use(l,nx) = 2
              thin_std(l,nx) = stdx
           end do

        end if

 310    continue

     end do
     end do

  end do

nct_all = sum(nctx)
write(6,*) '---ZT32-thin, nctx,nct_all:', nctx,nct_all

!.............. end, and deallocate ...........
  deallocate(tb_mean,tb_stdv)
  deallocate(tb_tim,tb_num,tb2d)
  deallocate(tb_num1,tb_num2)
  deallocate(nkeep,tb_idx)
  deallocate(isflg,isflg_mean,isflg_stdv)
  deallocate(stdang,fov_mean)

! End of routine
  return

end subroutine thinning_std

subroutine random_pts(mm,pts,nkeep,num_max,i,j)

!-------- Generate un-repeat m# from base n#.   --- Tong Zhu 05/16/2014

  Implicit None

! Declare passed variables
  integer(i_kind)                    ,intent(in   ) :: mm,pts
  integer(i_kind)                    ,intent(in   ) :: num_max,i,j
  integer(i_kind),dimension(num_max),intent(inout)  :: nkeep

! Declare local variables
  integer(i_kind):: n_chosen, this, tmp
  integer(i_kind):: m,n
  integer(i_kind), Dimension( 1:mm ) :: choices
  integer(i_kind), Dimension( 1 )     :: seed
  real(r_kind):: a

  choices = (/ ( n, n = 1, mm ) /)

  n_chosen = 0

  seed(1) = i * j

  call random_seed (put=seed)

  do n = 1, pts
     call random_number( a )
     this = a * ( mm - n_chosen ) + 1
    !write( 6, * ) 'a,this,chosen ', a,this,choices( this )
     nkeep( n ) = choices( this )
     tmp = choices( this )
     choices( this ) = choices( mm - n_chosen )
     choices( mm - n_chosen ) = tmp
     n_chosen = n_chosen + 1
  end do

  return
  
end subroutine random_pts

subroutine Thinning_Params
!
!-------- Read parameters for CSTROT thinning scheme ------- Tong Zhu 06/02/2014
!
  use kinds, only: r_kind,i_kind
  use file_utility, only : get_lun

  Implicit None

! Declare local variables

  real(r_kind), allocatable,dimension(:)         :: stdang_x
  character(5)                                   :: sis_5i
  integer(i_kind),allocatable,dimension(:)       :: nchx, thbandx
  real(r_kind),   allocatable,dimension(:)       :: stdx
  integer(i_kind)                                :: n_ch, nthband
  character(6)                                   :: string1,string2

  character(90) :: cline
  integer(i_kind) :: lu,ios
  integer(i_kind) :: i,m,n,l

     print*,'------ call Thinning_Params'
     ssa =(/'amsua','atms_'/)
     sfov=(/     30,     96/)
     cha =(/     15,     22/)

     lu=get_lun()

     open(lu,file='thinning_param.txt',form='formatted',status='old', &
          iostat=ios)
     if(ios /= 0) then
          write(*,*) 'Unable to open thinning_param.txt'
          stop
     endif

     cline=''
     do while (cline(1:18).ne.'######## Section_1')
        read(lu,'(a90)',iostat=ios) cline
     end do
     read(lu,'(a90)',iostat=ios) cline
     read(lu,*) thin_method
     read(lu,'(a90)',iostat=ios) cline
     read(lu,*) ntarget
     read(lu,'(a90)',iostat=ios) cline
     read(lu,*) ndomain
     print*,'---thin_method,ntarget,ndomain:',thin_method,ntarget,ndomain

     if(ntarget.ge.1) then
       do while (cline(1:18).ne.'######## Section_2')
          read(lu,'(a90)',iostat=ios) cline
       end do
       read(lu,'(a90)',iostat=ios) cline
       do i=1,ntarget
          read(lu,*) slonc(i),slatc(i),sradius(i)
          print*,'---slonc(i),slatc(i),sradius(i):',slonc(i),slatc(i),sradius(i)
       end do
     end if

     if(ndomain.ge.1) then
       do while (cline(1:18).ne.'######## Section_3')
          read(lu,'(a90)',iostat=ios) cline
       end do
       read(lu,'(a90)',iostat=ios) cline
       do i=1,ndomain
          read(lu,*) lonw(i),lone(i),lats(i),latn(i)
          print*,'---lonw(i),lone(i),lats(i),latn(i):',lonw(i),lone(i),lats(i),latn(i)
       end do
     end if

     if(thin_method.eq.1) then
!------ Thinning by standard deviation
        do while (cline(1:18).ne.'######## Section_4')
           read(lu,'(a90)',iostat=ios) cline
        end do
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) thin_reso1
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) represent
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) npts(1:3)
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) npts(4:5)
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) coast_ice
        print*,'---thin_reso1,represent:',thin_reso1,represent
        print*,'---npts,coast_ice:',npts,coast_ice

     else if(thin_method.eq.2) then
!------ Thinning by regression
        do while (cline(1:18).ne.'######## Section_5')
           read(lu,'(a90)',iostat=ios) cline
        end do
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) thin_reso2
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) represent
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) npts(1:3)
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) npts(4:5)
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) coast_ice
        print*,'---thin_reso2,represent:',thin_reso2,represent
        print*,'---npts,coast_ice:',npts,coast_ice

     else if(thin_method.eq.3) then
!----- Thinning by skipping points
       do while (cline(1:18).ne.'######## Section_6')
          read(lu,'(a90)',iostat=ios) cline
       end do
       read(lu,'(a90)',iostat=ios) cline
       read(lu,*) thin_skip1
       read(lu,'(a90)',iostat=ios) cline
       read(lu,*) thin_skip2
       print*,'---thin_skip1,thin_skip2',thin_skip1,thin_skip2

     else

       print*,'**** Wrong selection, no thinning method available for number:',thin_method
       stop

     endif

     close(lu)

     if(thin_method.eq.1) then

        lu=get_lun()

        open(lu,file='thinning_std.txt',form='formatted',status='old', &
             iostat=ios)
        if(ios /= 0) then
             write(*,*) 'Unable to open thinning_std.txt'
             stop
        endif

        cline=''
        read(lu,'(a90)',iostat=ios) cline
        read(lu,*) nsensor
        print*,'----- All sensors to read from thinning_std.txt:',nsensor

        stdang_all(:,:,:) = 999.9
        std1all(:,:) = 99.9
        std2ratio(:) = 1.0
        sis_all(:)   = ''
        nch_all(:)   = 0

        do n=1,nsensor

           read(lu,'(/,a90)',iostat=ios) cline
           sis_all(n)   = cline(41:60)
           sis_all(n) = adjustl(sis_all(n))
           sis_5i = sis_all(n)(1:5)
           string1      = cline(61:66)
           string2      = cline(67:72)

           read(string1,*) n_ch
           nch_all(n)   = n_ch
           read(string2,*) std2ratio(n)
           print*,'...read std1_std2 for sensor:',trim(sis_all(n))
           print*,'...nch_all(n),std2ratio(n):',nch_all(n),std2ratio(n)

           allocate( nchx(nch_all(n)), stdx(nch_all(n)), thbandx(nch_all(n)) )
           thbandx(:)   = 0

           read(lu,'(a90)',iostat=ios) cline
           read(lu,'(20I6)') nchx
           read(lu,'(A21,I3)') cline,nthband
           if(nthband.eq.n_ch) then
              read(lu,'(20I6)') thbandx(1)
              thbandx(:)=thbandx(1)
              print*,'...nthband,thbandx:',nthband,thbandx(1)
           endif
           if(nthband.lt.n_ch) then
              read(lu,'(20I6)') (thbandx(i),i=1,nthband)
              print*,'...nthband,thbandx:',nthband,(thbandx(i),i=1,nthband)
           endif
           nthband_all(n)   = nthband
           thbandx_all(1:nthband,n) = thbandx(1:nthband)

           read(lu,'(a90)',iostat=ios) cline

           if(sis_5i.eq.'amsua'.or.sis_5i.eq.'atms_') then

              if(sis_5i.eq.'amsua') m =1
              if(sis_5i.eq.'atms_') m= 2
              allocate( stdang_x(sfov(m)) )
              print*,'---sfov(m):',sfov(m)
              print*,'---here1 for sis_5i:',sis_5i

              do l=1,n_ch
                 read(lu,'(a90)',iostat=ios) cline
                 read(lu,'(20F6.2)') stdang_x
                 print*,'---l,stdang_x:',l,stdang_x
                 stdang_all(1:sfov(m),l,m) = stdang_x(1:sfov(m))
              end do

              deallocate(stdang_x)

           else

              print*,'---here2 for sis:',trim(sis_all(n))
              read(lu,'(20F6.2)') stdx
              std1all(1:n_ch,n) = stdx(1:n_ch)

           endif

           deallocate(nchx, stdx, thbandx)

        end do      !----------do n=1,nsensor

        close(lu)

     endif

  return

end subroutine Thinning_Params

subroutine distancell(lon1,lat1,lon2,lat2,distll)

!
!------ compute the distance of 2 points on the Earth surface
!------ T.Zhu 06/08/2014
!
  use kinds, only: r_kind,i_kind

  Implicit None

! Declare passed variables
  real(r_kind), intent(in   ) :: lon1,lat1,lon2,lat2  !(in radians)
  real(r_kind), intent(inout) :: distll               !(in km)

! Declare local parameters
  Real, Parameter :: rad_earth = 6371.0               !km(mean)
 !Real, Parameter :: rad_earth = 6356.752             !km(poles)
 !Real, Parameter :: rad_earth = 6378.137             !km(equator)

! Declare local variables
  real(r_kind) :: dlat, dlon
  real(r_kind) :: haversin_lat, haversin_lon, arcsin_sqrt

  dlat = lat1-lat2
  dlon = lon1-lon2

  haversin_lat = sin(dlat/2.0)**2
  haversin_lon = sin(dlon/2.0)**2

  arcsin_sqrt = asin( sqrt(haversin_lat + cos(lat1)*cos(lat2)*haversin_lon) )
  distll=2*rad_earth*arcsin_sqrt

  return

  end subroutine distancell

end module thin_cstrot
