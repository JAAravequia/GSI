module observer_enkf

private
public init_observer_enkf, calc_interp, calc_linhx, calc_linhx_modens,&
       destroy_observer_enkf
integer, allocatable, dimension(:) ::  kindx

contains

subroutine init_observer_enkf
   use statevec, only: slevels, nsdim, ns2d, ns3d
   use params,   only: nlevs
   implicit none
   integer nn,n,k,nl
   allocate(kindx(nsdim))
   nn = 0
   do n=1,ns3d
     if (n .eq. 1) then
       nl = slevels(n)
     else
       nl = slevels(n)-slevels(n-1)
     endif
     !print *,'ns3d,levs',n,nl
     do k=1,nl
        nn = nn + 1
        kindx(nn) = k
        ! FIXME - deal with state variables with nlevs+1 levels (like prse)
        if (kindx(nn) > nlevs) kindx(nn)=nlevs
     enddo
   enddo
   do n=1,ns2d ! 2d fields are treated as surface fields.
     nn = nn + 1
     kindx(nn) = 1
   enddo
   return
end subroutine init_observer_enkf

subroutine destroy_observer_enkf
 if (allocated(kindx)) deallocate(kindx)
end subroutine destroy_observer_enkf

subroutine calc_interp(rlat, rlon, time, interp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract:
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
  use gridinfo, only: latsgrd, lonsgrd
  use constants, only: zero,one,pi
  use intweight
  implicit none

! Declare passed variables
  real(r_single), intent(in) :: rlat, rlon ! observation lat and lon in radians
  real(r_single), intent(in) :: time   ! observation time relative to middle of window
  type(intw), intent(out)    :: interp
! H (x_ens)

! Declare local variables
  integer(i_kind) :: ix, iy, it, ixp, iyp, itp
  real(r_kind)    :: delx, dely, delxp, delyp, delt, deltp

  ! find interplation indices and deltas
  ix = 0
  do while (latsgrd(ix*nlons+1) >= rlat)
    ix = ix + 1
    if (ix == nlats-1) exit
  enddo
  ix  = min(ix,   nlats-1)
  ix  = min(ix,   nlats-1)
  ixp = max(ix-1, 0)

  if (ixp /= ix) then
     delx = (rlat - latsgrd(ix*nlons+1)) /                     &
              (latsgrd(ixp*nlons + 1) - latsgrd(ix*nlons+1))
  else
     delx = one
  endif
  delx = max(zero,min(delx,one))

  iyp = 1
  do while (iyp <= nlons .and. lonsgrd(ix*nlons + iyp) <= rlon)
    iyp = iyp + 1
  enddo
  iy = iyp - 1
  if(iy < 1)     iy = iy + nlons
  if(iyp > nlons) iyp = iyp - nlons
  if(iy > nlons) iy = iy - nlons

  if (iy /= nlons) then
     dely = (rlon - lonsgrd(ix*nlons + iy)) /                  &
              (lonsgrd(ix*nlons + iyp) - lonsgrd(ix*nlons + iy))
  else
     dely = (rlon - lonsgrd(ix*nlons + iy)) /                  &
              (lonsgrd(ix*nlons + iyp) + 2*pi - lonsgrd(ix*nlons + iy))
  endif

  it = 1
  do while (time + fhr_assim > nhr_state(it) .and. it < nstatefields)
    it = it + 1
  enddo
  itp = it
  it = max(1,itp-1)
  if (it /= itp) then
     delt = (time + fhr_assim - nhr_state(it)) /               &
               (nhr_state(itp) - nhr_state(it))
  else
     delt = one
  endif

  deltp = one - delt
  delxp = one - delx
  delyp = one - dely

  interp%ind(1) = ix*nlons  + iy;      interp%w(1) = delxp*delyp
  interp%ind(2) = ixp*nlons + iy;      interp%w(2) = delx *delyp
  interp%ind(3) = ix*nlons  + iyp;     interp%w(3) = delxp*dely
  interp%ind(4) = ixp*nlons + iyp;     interp%w(4) = delx * dely

  interp%tind(1) = it;    interp%tw(1) = deltp
  interp%tind(2) = itp;   interp%tw(2) = delt

  return
end subroutine calc_interp

subroutine calc_linhx(hx, dens, interp, dhx_dx, hx_ens, nx, ny, nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract: 
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use sparsearr, only: sparr
  use intweight
  implicit none

! Declare passed variables
  integer(i_kind), intent(in) :: nx, ny, nz
  real(r_single), intent(in)  :: hx           ! H(x_mean)
  real(r_single), dimension(nx, ny, nz), intent(in) :: dens !npts,nsdim,nstatefields
  type(intw),     intent(in)  :: interp
  type(sparr),    intent(in)  :: dhx_dx       ! dH(x)/dx |x_mean profiles
  real(r_single), intent(out) :: hx_ens       ! H (x_ens)

! Declare local variables
  integer(i_kind) :: i, j, k

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     do k = 1, 4
       hx_ens = hx_ens + dhx_dx%val(i) * interp%w(k) *                      &
             ( dens( interp%ind(k), j, interp%tind(1) ) * interp%tw(1)  +   &
               dens( interp%ind(k), j, interp%tind(2) ) * interp%tw(2) )
     enddo
  enddo

  return
end subroutine calc_linhx

subroutine calc_linhx_modens(hx, dens, interp, dhx_dx, hx_ens, nx, ny, nz, vscale) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract:
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single,r_double
  use params, only: neigv, nlevs
  use sparsearr, only: sparr
  use intweight
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nx, ny, nz
  real(r_single)                         ,intent(in   ) :: hx   ! H(x_mean)
  real(r_single), dimension(nx, ny, nz),  intent(in   ) :: dens  ! x_ens - x_mean, state vector space
  type(intw),                             intent(in   ) :: interp
  type(sparr)                            ,intent(in   ) :: dhx_dx   ! dH(x)/dx |x_mean profiles
  real(r_single)                         ,intent(  out) :: hx_ens(neigv)   ! H (x_ens)
  real(r_double),dimension(neigv,nlevs+1),intent(in   ) :: vscale
! vertical scaling (for modulated ens)
  integer(i_kind) i,j,k,klev

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     klev = kindx(j)
     do k = 1, 4
       hx_ens(:) = hx_ens(:) + dhx_dx%val(i) * interp%w(k) *                                  &
             ( dens( interp%ind(k), j, interp%tind(1) ) * vscale(:,klev) * interp%tw(1)  +   &
               dens( interp%ind(k), j, interp%tind(2) ) * vscale(:,klev) * interp%tw(2) )
     enddo
  enddo

  return
end subroutine calc_linhx_modens


end module observer_enkf
