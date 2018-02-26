module observer_enkf

private
public init_observer_enkf, calc_linhx, calc_interp

contains

subroutine init_observer_enkf
   return
end subroutine init_observer_enkf

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
  use gridinfo, only: npts, latsgrd, lonsgrd
  use constants, only: zero,one,pi
  use intweight
  use mpisetup
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

subroutine calc_linhx(hx, dens, interp, dhx_dx, hx_ens, nx, ny, nz, iobs)
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
!  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
!  use gridinfo, only: npts, latsgrd, lonsgrd
!  use statevec, only: nsdim
  use sparsearr, only: sparr
  use intweight
  use mpisetup
  implicit none

! Declare passed variables
  integer(i_kind), optional, intent(in) :: iobs
  integer(i_kind), intent(in) :: nx, ny, nz
  real(r_single), intent(in)  :: hx           ! H(x_mean)
  real(r_single), dimension(nx, ny, nz), intent(in) :: dens !npts,nsdim,nstatefields), intent(in) :: dens         ! x_ens - x_mean, state vector space
  type(intw),     intent(in)  :: interp
  type(sparr),    intent(in)  :: dhx_dx       ! dH(x)/dx |x_mean profiles
  real(r_single), intent(out) :: hx_ens       ! H (x_ens)

! Declare local variables
  integer(i_kind) :: i, j, k

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
!  if (nproc == 0) print *, dhx_dx%nnz
!  print *, nproc, ' in calc_hx: ', hx_ens
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
!     print *, nproc, ' in calc_hx: ', i, j
     do k = 1, 4
!       print *, nproc, ' in calc_hx: ', i, j, k
!       print *, nproc, ' in calc_hx: dhx: ', dhx_dx%val(i)
!       print *, nproc, ' in calc_hx: interpw: ', interp%w(k)
!       print *, nproc, ' in calc_hx: interpi: ', interp%ind(k)
!       print *, nproc, ' in calc_hx: interpti: ', interp%tind(1), interp%tind(2)
!       print *, nproc, ' in calc_hx: dens size: ', nx ,ny,nz
!       print *, nproc, ' in calc_hx: dens i: ', interp%ind(k), j, interp%tind(1)
       hx_ens = hx_ens + dhx_dx%val(i) * interp%w(k) *                      &
             ( dens( interp%ind(k), j, interp%tind(1) ) * interp%tw(1)  +   &
               dens( interp%ind(k), j, interp%tind(2) ) * interp%tw(2) )
!       if (present(iobs)) then
!         if (iobs ==1) print *, 'observer: ', i, k, nproc+1, dhx_dx%val(i), interp%w(k), dens(interp%ind(k), j, interp%tind(1) )
!       endif
     enddo
  enddo

  return
end subroutine calc_linhx

end module observer_enkf
