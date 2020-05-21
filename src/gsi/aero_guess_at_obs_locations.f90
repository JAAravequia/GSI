subroutine aero_guess_at_obs_locations(&
     obstime,data_s,nchanl,nreal,nsig,n_aerosols,&
     aero,aero_names)
  ! from M. Pagowski, added to this branch by C. Martin - 2/21/2019

  use kinds, only: r_kind,i_kind
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_chemguess_mod, only: gsi_chemguess_bundle   
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use gridmod, only: istart,jstart,nlon,nlat,lon1
  use constants, only: max_varname_length, zero, one
  use mpimod, only: mype
  use guess_grids, only: hrdifsig,nfldsig

  implicit none

! Declare passed variables

  integer(i_kind), intent(in   ) :: nchanl,nreal,nsig, n_aerosols
  real(r_kind), intent(in   ) :: obstime

  real(r_kind),dimension(nreal+nchanl), intent(in   ) ::data_s
  character(len=max_varname_length), dimension(n_aerosols), intent(in   ) :: aero_names

  real(r_kind),dimension(nsig,n_aerosols), intent(  out) :: aero


  integer(i_kind):: j,k,m1,ix,ix1,ixp,iy,iy1,iyp,ii
  integer(i_kind):: itsig,itsigp

  real(r_kind):: w00,w01,w10,w11,dx,dy
  real(r_kind):: delx,dely,delx1,dely1,dtsig,dtsigp

  integer(i_kind):: ilon, ilat, ier

  real(r_kind),pointer,dimension(:,:,:)::aeroges_itsig =>null()
  real(r_kind),pointer,dimension(:,:,:)::aeroges_itsigp=>null()

  m1=mype+1

  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)

  dx  = data_s(ilat)                 ! grid relative latitude
  dy  = data_s(ilon)                 ! grid relative longitude

! Set spatial interpolation indices and weights
  ix1=dx
  ix1=max(1,min(ix1,nlat))
  delx=dx-ix1
  delx=max(zero,min(delx,one))
  ix=ix1-istart(m1)+2
  ixp=ix+1
  if(ix1==nlat) then
     ixp=ix
  end if
  delx1=one-delx

  iy1=dy
  dely=dy-iy1
  iy=iy1-jstart(m1)+2
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(m1)+2
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(m1)+2
  end if
  iyp=iy+1
  dely1=one-dely

  w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely


! Get time interpolation factors for sigma files
  if(obstime > hrdifsig(1) .and. obstime < hrdifsig(nfldsig))then
     do j=1,nfldsig-1
        if(obstime > hrdifsig(j) .and. obstime <= hrdifsig(j+1))then
           itsig=j
           itsigp=j+1
           dtsig=((hrdifsig(j+1)-obstime)/(hrdifsig(j+1)-hrdifsig(j)))
        end if
     end do
  else if(obstime <=hrdifsig(1))then
     itsig=1
     itsigp=1
     dtsig=one
  else 
     itsig=nfldsig
     itsigp=nfldsig
     dtsig=one
  end if
  dtsigp=one-dtsig

  ier=0

  if(n_aerosols>0)then
     if(size(gsi_chemguess_bundle)==1) then
        do ii=1,n_aerosols
           call gsi_bundlegetpointer(gsi_chemguess_bundle(1),aero_names(ii),aeroges_itsig ,ier) 
           do k=1,nsig
              aero(k,ii) =(aeroges_itsig(ix ,iy ,k)*w00+ &
                   aeroges_itsig(ixp,iy ,k)*w10+ &
                   aeroges_itsig(ix ,iyp,k)*w01+ &
                   aeroges_itsig(ixp,iyp,k)*w11)
           end do
        enddo
     else
        do ii=1,n_aerosols
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itsig ),aero_names(ii),aeroges_itsig ,ier) 
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itsigp),aero_names(ii),aeroges_itsigp,ier) 
           do k=1,nsig
              aero(k,ii) =(aeroges_itsig (ix ,iy ,k)*w00+ &
                   aeroges_itsig (ixp,iy ,k)*w10+ &
                   aeroges_itsig (ix ,iyp,k)*w01+ &
                   aeroges_itsig (ixp,iyp,k)*w11)*dtsig + &
                   (aeroges_itsigp(ix ,iy ,k)*w00+ &
                   aeroges_itsigp(ixp,iy ,k)*w10+ &
                   aeroges_itsigp(ix ,iyp,k)*w01+ &
                   aeroges_itsigp(ixp,iyp,k)*w11)*dtsigp
           end do
        enddo
     endif

  endif

end subroutine aero_guess_at_obs_locations
