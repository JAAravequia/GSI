module intweight
!$$$  module documentation block
!            .      .    .                                       .
! module:  intweight
!  prgmmr: shlyaeva
!
!
! program history log:
!   2017-02-17  shlyaeva - initial code
!
! attributes:
!   language: f90
!   machine:
!
!$$$

use kinds
implicit none
private

public intw
public size
public writearray, readarray


type intw
   integer(i_kind), dimension(4) :: ind
   real(r_single),  dimension(4) :: w
   integer(i_kind), dimension(2) :: tind
   real(r_single),  dimension(2) :: tw
end type intw

! interfaces
! writing out sparse array to array
interface writearray
  module procedure writearray_intw
end interface

! reading sparse array from array
interface readarray
  module procedure readarray_intw
end interface

interface size
  module procedure size_intw
end interface



contains
! private subroutines

! returns "size" 8 + 4
integer function size_intw(this)
  type(intw), intent(in) :: this

  size_intw = 12
end function size_intw

! writing out sparse array to array
subroutine writearray_intw(this, array, ierr)
  type(intw), intent(in)                   :: this
  real(r_single), dimension(:), intent(out) :: array
  integer(i_kind), optional, intent(out)  :: ierr

  integer(i_kind) :: ind

  if (present(ierr)) ierr = 0
  if (size(array) < size_intw(this)) then
    if (present(ierr)) ierr = -1
    return
  endif

  ind = 1
  array(ind:ind+3) = this%ind
  ind = ind + 4
  array(ind:ind+3) = this%w
  ind = ind + 4
  array(ind:ind+1) = this%tind
  ind = ind + 2
  array(ind:ind+1) = this%tw

end subroutine writearray_intw

! reading sparse array from array
subroutine readarray_intw(this, array)
  type(intw), intent(out)              :: this
  real(r_single), dimension(:), intent(in) :: array

  integer(i_kind) :: ind

  ind = 1
  this%ind  = array(ind:ind+3)
  ind = ind + 4
  this%w    = array(ind:ind+3)
  ind = ind + 4
  this%tind = array(ind:ind+1)
  ind = ind + 2
  this%tw   = array(ind:ind+1)

end subroutine readarray_intw

end module intweight
