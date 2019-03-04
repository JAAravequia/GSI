!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
!ROUTINE:  This module calculate the dew point observation error
! 
! abstract: For dew point observations
!
! program history log:
!   2017-09-10 Junjun Hu, CIMMS/OU/NOAA/NSSL 
!
! attributes:
!   language: f90
!
! 
module dewpoint_obs_err_mod

use kinds, only: r_kind
use met_mod, only : rh_and_temp_to_dewpoint, temp_and_dewpoint_to_rh

implicit none
private

public :: dewpt_error_from_rh_and_temp
public :: rh_error_from_dewpt_and_temp

contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   dewpt_error_from_rh_and_temp - computes estimated uncertainty in
!           a dewpoint observation, when the dewpoint is derived from
!           temperature and relative humidity observations
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function dewpt_error_from_rh_and_temp(tmpk, rh_in)

! reference:  Lin and Hubbard 2004, Journal of Applied Meteorology, 821-825

real(r_kind)             :: dewpt_error_from_rh_and_temp
real(r_kind), intent(in) :: tmpk                   ! temperature (Kelvin)
real(r_kind), intent(in) :: rh_in                  ! relative humidity (0.00-1.00)

real(r_kind), parameter  :: rh_error = 0.05_r_kind     ! guess for instrument + representativeness error
real(r_kind), parameter  :: t_error = 1.0_r_kind       ! guess for instrument + representativeness error

real(r_kind), parameter  :: rh_min = 0.20_r_kind
real(r_kind), parameter  :: rh_max = 1.00_r_kind
real(r_kind), parameter  :: delta_rh = 0.01_r_kind     ! perturbation for finite differencing
real(r_kind), parameter  :: delta_t  = 0.1_r_kind      ! perturbation for finite differencing
real(r_kind)             :: rh
real(r_kind)             :: rh1
real(r_kind)             :: rh2
real(r_kind)             :: t1,td1
real(r_kind)             :: t2,td2
real(r_kind)             :: td_deriv_rh
real(r_kind)             :: td_deriv_t


if ( ( rh_in < 0.00_r_kind ) .or. ( rh_in > 1.00_r_kind ) ) then
  print*,'dewpt_error_from_rh_and_temp:  bad rh ',rh_in
  stop
end if

! The uncertainty in dewpoint derived from temperature and relative humidity grows without bound
! as the relative humidity approaches 0.  A lower bound on the input relative humidity will be
! applied to keep the calculation bounded and to acknowledge uncertainty in the relative humidity
! measurement.

rh = max(rh_in, rh_min)

! finite-difference approximations of derivatives
rh1 = max(rh - delta_rh, rh_min)
rh2 = min(rh + delta_rh, rh_max)
t1 = tmpk - delta_t
t2 = tmpk + delta_t
call rh_and_temp_to_dewpoint(rh2, tmpk, td2)
call rh_and_temp_to_dewpoint(rh1, tmpk, td1)
td_deriv_rh = ( td2 - td1 ) / (rh2-rh1)
call rh_and_temp_to_dewpoint(rh, t2, td2)
call rh_and_temp_to_dewpoint(rh, t1, td1)
td_deriv_t = ( td2 - td1 ) / (t2-t1)

dewpt_error_from_rh_and_temp = sqrt ( td_deriv_t*td_deriv_t * t_error*t_error &
                                     +td_deriv_rh*td_deriv_rh * rh_error*rh_error)

return
end function dewpt_error_from_rh_and_temp


! GSR  - Added rh error function following above
function rh_error_from_dewpt_and_temp(tmpk, dewpt)

! reference:  Lin and Hubbard 2004, Journal of Applied Meteorology, 821-825

real(r_kind)             :: rh_error_from_dewpt_and_temp
real(r_kind), intent(in) :: tmpk                   ! temperature (Kelvin)
real(r_kind), intent(in) :: dewpt                  ! dewpoint temperature (Kelvin) 

real(r_kind), parameter  :: td_error = 1.50_r_kind     ! guess for instrument + representativeness error
real(r_kind), parameter  :: t_error = 1.0_r_kind       ! guess for instrument + representativeness error

real(r_kind), parameter  :: delta_td = 0.1_r_kind      ! perturbation for finite differencing
real(r_kind), parameter  :: delta_t  = 0.1_r_kind      ! perturbation for finite differencing
real(r_kind)             :: td1
real(r_kind)             :: td2
real(r_kind)             :: t1
real(r_kind)             :: t2
real(r_kind)             :: rh_deriv_td
real(r_kind)             :: rh_deriv_t
real(r_kind)             :: rh1
real(r_kind)             :: rh2


if ( ( dewpt > tmpk ) ) then
  print*,'rh_error_from_dewpt_and_temp:  bad dewpt ',dewpt, tmpk
  stop
end if

! finite-difference approximations of derivatives
td1 = dewpt - delta_td
td2 = min (dewpt + delta_td, tmpk + delta_t)
t1 = tmpk - delta_t
t2 = tmpk + delta_t
call temp_and_dewpoint_to_rh(tmpk,td2,rh2)
call temp_and_dewpoint_to_rh(tmpk,td1,rh1)
rh_deriv_td = (rh2 - rh1) / (td2-td1)

call temp_and_dewpoint_to_rh(t2,dewpt,rh2)
call temp_and_dewpoint_to_rh(t1,dewpt,rh1)
rh_deriv_t = (rh2 - rh1) / (t2-t1)

rh_error_from_dewpt_and_temp = sqrt ( rh_deriv_t*rh_deriv_t * t_error*t_error &
                                     +rh_deriv_td*rh_deriv_td * td_error*td_error)

return
end function rh_error_from_dewpt_and_temp

end module dewpoint_obs_err_mod

