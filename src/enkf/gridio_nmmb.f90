module gridio

use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                         nemsio_getheadvar,nemsio_realkind,nemsio_intkind,&
                         nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
use params, only: nlons,nlats,reducedgrid,nlevs,pseudo_rh, &
                  cliptracers,nlons,nlats,datestring,datapath, &
                  nbackgrounds,fgfileprefixes,anlfileprefixes
use kinds, only: i_kind,r_double,r_kind,r_single
use constants, only: zero,one,cp,fv,rd,grav,zero,max_varname_length
use gridinfo, only: npts,wind2mass,mass2wind
use mpisetup, only: nproc
use mpeu_util, only: getindex

implicit none
private
public :: readgriddata, writegriddata
contains

subroutine readgriddata(nanal,cvars3d,cvars2d,nc3d,nc2d,grdin,qsat)
implicit none
character(len=500) :: filename
character(len=3) charnanal
integer, intent(in) :: nanal
integer, intent(in) :: nc2d,nc3d
character(len=max_varname_length), dimension(nc2d), intent(in) :: cvars2d
character(len=max_varname_length), dimension(nc3d), intent(in) :: cvars3d
real(r_double), dimension(npts,nlevs,nbackgrounds), intent(out) :: qsat
real(r_single), dimension(npts,nc3d*nlevs+nc2d,nbackgrounds), intent(out) :: grdin
real(r_single), allocatable, dimension(:,:) :: pslg
real(r_kind) clip

real(nemsio_realkind) nems_wrk(nlons*nlats), nems_wrk2(nlons*nlats),&
                      f_ice(nlons*nlats),f_rain(nlons*nlats),clwmr(nlons*nlats), &
                      field1(nlevs)
!real(r_single) :: ak(nlevs),bk(nlevs), pt
real(r_single),dimension(nlevs+1,3,2) :: nems_vcoord
real(r_single), allocatable, dimension(:) :: psg, ak, bk
real(r_single) aeta1(nlevs),aeta2(nlevs),pt,pdtop
type(nemsio_gfile) :: gfile
logical ice
integer(i_kind) iret,k,kk,nb,ii
integer :: u_ind, v_ind, t_ind, q_ind, oz_ind, cw_ind, qr_ind, qli_ind, dbz_ind, w_ind
integer :: ps_ind

u_ind   = getindex(cvars3d, 'u')   !< indices in the state var arrays
v_ind   = getindex(cvars3d, 'v')   ! U and V (3D)
t_ind   = getindex(cvars3d, 'tv')  ! Tv (3D)
q_ind   = getindex(cvars3d, 'q')   ! Q (3D)
oz_ind  = getindex(cvars3d, 'oz')  ! Oz (3D)
cw_ind  = getindex(cvars3d, 'ql')  ! CW (3D)
qr_ind  = getindex(cvars3d, 'qr')  ! QR (3D)
qli_ind = getindex(cvars3d, 'qli')  ! QLI (3D)
dbz_ind = getindex(cvars3d, 'dbz')  ! dBZ (3D)
w_ind   = getindex(cvars3d, 'w')  ! W (3D)

ps_ind  = getindex(cvars2d, 'ps')  ! Ps (2D)

if( cw_ind * qr_ind * qli_ind > 0 ) then
  write(6,*)'NMMB gridio/readgriddata: please make sure state variables ql, qr, qli all in anavinfo'
  call stop2(23)
end if

backgroundloop: do nb=1,nbackgrounds

write(charnanal,'(i3.3)') nanal
filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal

call nemsio_init(iret=iret)
if(iret/=0) then
   write(6,*)'NMMB gridio/readgriddata: nmmb model: problem with nemsio_init, iret=',iret
   call stop2(23)
end if
call nemsio_open(gfile,filename,'READ',iret=iret)
if (iret/=0) then
   write(6,*)'NMMB gridio/readgriddata: nmmb model: problem with nemsio_open, iret=',iret
   call stop2(23)
end if

! get surface pressure and pressure on model levels
!call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
!if (iret/=0) then
!   write(6,*)'NMMB gridio/readgriddata: NMMB model: problem with nemsio_readrecv(ps), iret=',iret
!   call stop2(23)
!endif
!allocate(psg(nlons*nlats),pslg(nlons*nlats,nlevs))
!psg = 0.01*nems_wrk + pt ! surface pressure, units of hPa

!psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.


! get surface pressure and pressure on model levels
call nemsio_getheadvar(gfile,'PT',pt,iret)
pt = 0.01*pt
call nemsio_getheadvar(gfile,'PDTOP',pdtop,iret)
pdtop = 0.01*pdtop
call nemsio_getheadvar(gfile,'SGML1',field1,iret)
do k=1,nlevs
  aeta1(k)=field1(nlevs+1-k)
enddo
call nemsio_getheadvar(gfile,'SGML2',field1,iret)
do k=1,nlevs
  aeta2(k)=field1(nlevs+1-k)
  aeta1(k) = aeta1(k) + aeta2(k)
enddo
call nemsio_readrecv(gfile,'dpres','hybrid sig lev',1,nems_wrk,iret=iret)
if (iret/=0) then
   write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(dpres), iret=',iret
   call stop2(23)
endif
allocate(psg(nlons*nlats),pslg(nlons*nlats,nlevs))
psg = 0.01*nems_wrk + pt ! surface pressure, units of hPa
if (ps_ind > 0) then
  grdin(:,nc3d*nlevs+ps_ind,nb) = psg
end if
! pressure on model levels
do k=1,nlevs
   pslg(:,k) = aeta1(k)*pdtop + aeta2(k)*(psg - pdtop - pt) + pt
enddo

!call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
!if ( iret /= 0 ) then
!   write(6,*)' NMMB gridio:  ***ERROR*** problem reading header ', &
!      'vcoord, Status = ',iret
!   call stop2(99)
!endif

!allocate(ak(nlevs),bk(nlevs))

!if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
!   ak = zero
!   bk = nems_vcoord(1:nlevs,1,1)
!elseif ( idvc == 1 ) then                     ! sigma coordinate
!   ak = zero
!   bk = nems_vcoord(1:nlevs,2,1)
!elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
!   ak = 0.01_r_kind*nems_vcoord(1:nlevs,1,1) ! convert to mb
!   bk = nems_vcoord(1:nlevs,2,1)
!else
!   write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
!   call stop2(85)
!endif
if (nanal .eq. 1) then
   print *,'time level ',nb
   print *,'---------------'
endif
!if (ps_ind > 0) then
!  grdin(:,nc3d*nlevs+ps_ind,nb) = psg
!endif
! pressure on model levels
!do k=1,nlevs
!   pslg(:,k)=ak(k)+bk(k)*psg
!   if (nanal .eq. 1) print *,'nemsio, min/max pressi',k,minval(pslg(:,k)),maxval(pslg(:,k))
!enddo
!deallocate(ak,bk)
! get u,v
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   call wind2mass(nems_wrk,nlons,nlats)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(ugrd), iret=',iret
      call stop2(23)
   endif
   if (u_ind > 0) then
     grdin(:,nlevs*(u_ind-1)+k,nb) = nems_wrk
   endif
   call nemsio_readrecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   call wind2mass(nems_wrk,nlons,nlats)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(vgrd), iret=',iret
      call stop2(23)
   endif
   if (v_ind > 0) then
     grdin(:,nlevs*(v_ind-1)+k,nb) = nems_wrk
   endif
enddo
ice = .false. ! calculate qsat w/resp to ice?
clip = tiny(grdin(1,1,1))
! get sensible temperature and humidity
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_readrecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(spfh), iret=',iret
      call stop2(23)
   endif
   if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
   if (t_ind > 0) then
     grdin(:,nlevs*(t_ind-1)+k,nb) = nems_wrk*(1. + fv*nems_wrk2)
   endif
   if (q_ind > 0) then
     grdin(:,nlevs*(q_ind-1)+k,nb) = nems_wrk2
   endif
enddo
! compute qsat
if (pseudo_rh) then
   call genqsat1(grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb),qsat(:,:,nb),pslg,grdin(:,(t_ind-1)*nlevs+1:t_ind*nlevs,nb),ice,npts,nlevs)
else
   qsat(:,:,nb) = 1._r_double
end if
! other tracers
if (oz_ind > 0) then 
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(o3mr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       grdin(:,(oz_ind-1)*nlevs+k,nb) = nems_wrk
    enddo
endif
if (cw_ind > 0) then ! qr_ind and qli_ind should be also > 1
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       clwmr = nems_wrk

       where( clwmr < 1.e-12_r_kind )
          clwmr = 0.0_r_kind
       end where

       call nemsio_readrecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_rain = nems_wrk

       call nemsio_readrecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_ice = nems_wrk
       ! ==== convert to hydrometeors
       do ii = 1, nlons*nlats
         grdin(ii,k+(qli_ind-1)*nlevs,nb) = clwmr(ii) * f_ice(ii) ! qli
         grdin(ii,k+(qr_ind-1)*nlevs,nb) = f_rain(ii) * clwmr(ii) * (1.0_r_kind - f_ice(ii)) ! qr
         grdin(ii,k+(cw_ind-1)*nlevs,nb) = (1.0_r_kind - f_rain(ii)) * clwmr(ii) * &
                                          (1.0_r_kind -f_ice(ii)) ! ql
       end do

    enddo
endif

if ( dbz_ind > 0 )then
   do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(refl_10cm), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       grdin(:,(dbz_ind-1)*nlevs+k,nb) = nems_wrk
    enddo
endif

if ( w_ind > 0 )then
   do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(refl_10cm), iret=',iret
          call stop2(23)
       endif
       grdin(:,(w_ind-1)*nlevs+k,nb) = nems_wrk
    enddo
endif
  
deallocate(psg,pslg)
call nemsio_close(gfile, iret=iret)

end do backgroundloop ! loop over backgrounds to read in

end subroutine readgriddata

subroutine writegriddata(nanal,cvars3d,cvars2d,nc3d,nc2d,grdin)

implicit none

character(len=500):: filename
integer, intent(in) :: nanal
integer, intent(in) :: nc2d,nc3d
character(len=max_varname_length), dimension(nc2d), intent(in) :: cvars2d
character(len=max_varname_length), dimension(nc3d), intent(in) :: cvars3d

real(r_single), dimension(npts,nc3d*nlevs+nc2d,nbackgrounds), intent(inout) :: grdin
character(len=3) charnanal
integer(nemsio_intkind) iret,nfhour,jdate(7),idat(3),ihrst,nfminute,ntimestep,nfsecond
integer iadate(4),idate(4),k,kk,nb, ii
integer,dimension(8):: ida,jda
integer :: u_ind, v_ind, t_ind, q_ind, oz_ind, cw_ind, qr_ind, qli_ind, dbz_ind, w_ind
integer :: ps_ind
real(r_double),dimension(5):: fha
real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2,psg, clwmr,f_rain,f_ice
real(r_single) pdtop,pt
real(r_kind) clip
type(nemsio_gfile) :: gfile


u_ind   = getindex(cvars3d, 'u')   !< indices in the state var arrays
v_ind   = getindex(cvars3d, 'v')   ! U and V (3D)
t_ind   = getindex(cvars3d, 'tv')  ! Tv (3D)
q_ind   = getindex(cvars3d, 'q')   ! Q (3D)
oz_ind  = getindex(cvars3d, 'oz')  ! Oz (3D)
cw_ind  = getindex(cvars3d, 'ql')  ! CW (3D)
qr_ind  = getindex(cvars3d, 'qr')  ! QR (3D)
qli_ind = getindex(cvars3d, 'qli')  ! QLI (3D)
dbz_ind = getindex(cvars3d, 'dbz')  ! dBZ (3D)
w_ind   = getindex(cvars3d, 'w')  ! W (3D)

ps_ind  = getindex(cvars2d, 'ps')  ! Ps (2D)

if( cw_ind * qr_ind * qli_ind > 0 ) then
  write(6,*)'NMMB gridio/readgriddata: please make sure state variables ql, qr, qli all in anavinfo'
  call stop2(23)
end if

clip = tiny(grdin(1,1,1))

! First guess file should be copied to analysis file at scripting
! level; only variables updated by EnKF are changed
backgroundloop: do nb=1,nbackgrounds

write(charnanal,'(i3.3)') nanal
filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal

call nemsio_init(iret=iret)
if(iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_init, iret=',iret
   call stop2(23)
end if
call nemsio_open(gfile,filename,'RDWR',iret=iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_open, iret=',iret
   call stop2(23)
end if

call nemsio_getheadvar(gfile,'idate',idate,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_getheadvar, iret=',iret
   call stop2(23)
end if
call nemsio_getheadvar(gfile,'nfhour',nfhour,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_getheadvar, iret=',iret
   call stop2(23)
end if
! update header information
! Compute analysis time from guess date and forecast length.
fha=zero; ida=0; jda=0
fha(2)=nfhour    ! relative time interval in hours
ida(1)=idate(4) ! year
ida(2)=idate(2) ! month
ida(3)=idate(3) ! day
ida(4)=0                ! time zone
ida(5)=idate(1) ! hour
ida(6)=idate(5) ! minute
call w3movdat(fha,ida,jda)
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
iadate(1)=jda(5) ! hour
iadate(2)=jda(2) ! mon
iadate(3)=jda(3) ! day
iadate(4)=jda(1) ! year
iadate(5)=jda(6) ! minute
if (nproc .eq. 0) then
  print *,'nfhour = ',nfhour
  print *,'idate = ',idate
  print *,'iadate = ',iadate
end if
idat = 0
jdate = 0
jdate(1)=jda(1)    !  new year
jdate(2)=jda(2)    !  new month
jdate(3)=jda(3)    !  new day
jdate(4)=jda(5)    !  new hour
jdate(5)=jda(6)    !  new minute
idat(3)=jdate(1)       !  forecast starting year
idat(2)=jdate(2)       !  forecast starting month
idat(1)=jdate(3)       !  forecast starting day  
ihrst=jdate(4)         !  forecast starting hour (0-23)
call nemsio_setheadvar(gfile,'idate',jdate,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(jdate), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'idat',idat,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(idat), iret=',iret
   call stop2(23)
end if
nfhour = 0; nfminute = 0; nfsecond = 0; ntimestep = 0
call nemsio_setheadvar(gfile,'nfhour',nfhour,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfhour), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'nfminute',nfminute,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfminute), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'nfsecondn',nfsecond,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfsecondn), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'ihrst',ihrst,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(ihrst), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'ntimestep',ntimestep,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(ntimestep), iret=',iret
   call stop2(23)
end if


! update u,v
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(ugrd), iret=',iret
      call stop2(23)
   endif
   if (u_ind > 0) then
     nems_wrk2 = grdin(:,(u_ind-1)*nlevs + k,nb)
     call mass2wind(nems_wrk2,nlons,nlats)
     nems_wrk = nems_wrk + nems_wrk2
   endif
   call nemsio_writerecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(ugrd), iret=',iret
      call stop2(23)
   endif

   call nemsio_readrecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(vgrd), iret=',iret
      call stop2(23)
   endif
   if (v_ind > 0) then
     nems_wrk2 = grdin(:,(v_ind-1)*nlevs + k,nb)
     call mass2wind(nems_wrk2,nlons,nlats)
     nems_wrk = nems_wrk + nems_wrk2
   endif
   call nemsio_writerecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(vgrd), iret=',iret
      call stop2(23)
   endif
enddo

clip = tiny(grdin(1,1,1))
! update sensible temperature and humidity
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_readrecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(spfh), iret=',iret
      call stop2(23)
   endif
   nems_wrk = nems_wrk*(1. + fv*nems_wrk2) 
   if (t_ind > 0) then 
      nems_wrk = nems_wrk + grdin(:,(t_ind-1)*nlevs+k,nb)
   endif
   if (q_ind > 0) then
      nems_wrk2 = nems_wrk2 + grdin(:,(q_ind-1)*nlevs+k,nb)
   endif
   if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
   ! nems_wrk is now updated Tv, convert back to T
   nems_wrk = nems_wrk/(1. + fv*nems_wrk2)
   call nemsio_writerecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_writerecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(spfh), iret=',iret
      call stop2(23)
   endif
enddo
! update other tracers
if (oz_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(o3mr), iret=',iret
          call stop2(23)
       endif
       nems_wrk = nems_wrk + grdin(:,(oz_ind-1)*nlevs+k,nb)
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(o3mr), iret=',iret
         call stop2(23)
       endif
    enddo
endif
if (cw_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       clwmr = nems_wrk

       where( clwmr < 1.e-12_r_kind )
          clwmr = 0.0_r_kind
       end where

       call nemsio_readrecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(f_rain), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       f_rain = nems_wrk

       call nemsio_readrecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(f_ice), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       f_ice = nems_wrk
       ! ==== convert to hydrometeors
       do ii = 1, nlons*nlats
         grdin(ii,k+(qli_ind-1)*nlevs,nb) = min(grdin(ii,k+(qli_ind-1)*nlevs,nb) + clwmr(ii) * f_ice(ii) , 0.015_r_kind)! qli
         grdin(ii,k+(qr_ind-1)*nlevs,nb)  = min(grdin(ii,k+(qr_ind-1)*nlevs,nb) + &
                                            f_rain(ii) * clwmr(ii) * (1.0_r_kind -f_ice(ii)), 0.015_r_kind) ! qr
         grdin(ii,k+(cw_ind-1)*nlevs,nb)  = min(grdin(ii,k+(cw_ind-1)*nlevs,nb) + (1.0_r_kind - f_rain(ii)) * clwmr(ii) * &
                                            (1.0_r_kind -f_ice(ii)) , 0.015_r_kind) ! ql
       end do

       where( grdin(:,k+(cw_ind-1)*nlevs,nb) <= 1.0e-8_r_kind )
         grdin(:,k+(cw_ind-1)*nlevs,nb) = 0.0_r_kind
       end where

       where( grdin(:,k+(qli_ind-1)*nlevs,nb) <= 1.0e-8_r_kind )
         grdin(:,k+(qli_ind-1)*nlevs,nb) = 0.0_r_kind
       end where

       where( grdin(:,k+(qr_ind-1)*nlevs,nb) <= 1.0e-8_r_kind )
         grdin(:,k+(qr_ind-1)*nlevs,nb) = 0.0_r_kind
       end where

       clwmr = grdin(:,k+(cw_ind-1)*nlevs,nb) + grdin(:,k+(qr_ind-1)*nlevs,nb) + &
               grdin(:,k+(qli_ind-1)*nlevs,nb)
       nems_wrk =  clwmr

       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
          call stop2(23)
       endif

       do ii = 1, nlons*nlats
         if( clwmr (ii) .gt. 0.0_r_kind )then
           f_ice(ii) = ( grdin(ii,k+(qli_ind-1)*nlevs,nb) )/clwmr(ii)
         else
           f_ice(ii) = 0.0_r_kind
         end if
       end do

       nems_wrk = f_ice
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(f_rain), iret=',iret
          call stop2(23)
       endif

       do ii = 1, nlons*nlats
         if( clwmr (ii) .gt. ( grdin(ii,k+(qli_ind-1)*nlevs,nb) ) .and. &
             clwmr (ii) .gt. 0.0_r_kind )then
            f_rain(ii) = grdin(ii,k+(qr_ind-1)*nlevs,nb)/( clwmr (ii) - (grdin(ii,k+(qli_ind-1)*nlevs,nb) ) )
         else
            f_rain(ii) = 0.0_r_kind
         end if
       end do

       nems_wrk = f_rain
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(f_ice), iret=',iret
          call stop2(23)
       endif

    enddo
endif

if (dbz_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       where (nems_wrk < 0.0 ) nems_wrk = 0.0
       nems_wrk = nems_wrk + grdin(:,(dbz_ind-1)*nlevs+k,nb)
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
         call stop2(23)
       endif
    enddo
endif

if (w_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       nems_wrk = nems_wrk + grdin(:,(w_ind-1)*nlevs+k,nb)
       call nemsio_writerecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
         call stop2(23)
       endif
    enddo
endif

call nemsio_close(gfile, iret=iret)

end do backgroundloop ! loop over backgrounds to read in

end subroutine writegriddata

end module gridio
