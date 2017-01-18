module sparsearr
!$$$  module documentation block
!            .      .    .                                       .
! module:  sparsearr
!  prgmmr: shlyaeva
!
! abstract: define sparse array (for saving the jacobian for EnKF) and basic routines
!
! program history log:
!   2016-11-29  shlyaeva - initial code
!
! subroutines included:
!   sub new
!   sub delete
!   sub writearray
!   sub readarray
!
! functions included:
!   size
!
! attributes:
!   language: f90
!   machine:
!
!$$$

use kinds
implicit none
private

public sparr, sparr2
public new, delete, size
public writearray, readarray

! general sparse array type
! saves all non-zero elements and their indices
type sparr
   integer(i_kind) :: nnz                              ! number of non-zero elements
   real(r_kind), dimension(:), allocatable    :: val  ! values of non-zero elements
   integer(i_kind), dimension(:), allocatable :: ind  ! indices of non-zero elements
end type sparr

! sparse array with dense subarrays type
! saves all non-zero elements and start and end indices of the dense
! subarrays
! i.e. for array  
! index 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
! value 0 0 0 1 2 3 0 0 0  0  0  4  5  6  7  0  0  0  0  0
! nind:    2
! st_ind:  4, 12
! end_ind: 6, 15
! val:     1, 2, 3, 4, 5, 6, 7 
type sparr2
   integer(i_kind) :: nnz
   integer(i_kind) :: nind                                   ! number of indices
   integer(i_kind), dimension(:), allocatable :: st_ind      ! start indices for dense subarrays
   integer(i_kind), dimension(:), allocatable :: end_ind     ! end indices for dense subarrays
   real(r_kind), dimension(:), allocatable    :: val         ! values of non-zero elements
end type sparr2

! interfaces
! constructor
interface new
  module procedure init_sparr2
end interface

! destructor
interface delete
  module procedure cleanup_sparr2
end interface

! writing out sparse array to array
interface writearray
  module procedure writearray_sparr2
  module procedure writearray_r_sparr2
end interface

! reading sparse array from array
interface readarray
  module procedure readarray_sparr2
end interface

interface size
  module procedure size_sparr2
end interface



contains
! private subroutines

! constructor for sparr2
subroutine init_sparr2(this, nnz, nind)
  type(sparr2), intent(out) :: this
  integer, intent(in) :: nnz, nind

  this%nnz  = nnz
  this%nind = nind
  if (allocated(this%st_ind))   deallocate(this%st_ind)
  if (allocated(this%end_ind))  deallocate(this%end_ind)
  if (allocated(this%val))      deallocate(this%val)

  allocate(this%st_ind(nind), this%end_ind(nind), this%val(nnz))

end subroutine init_sparr2

! destructor for sparr2
subroutine cleanup_sparr2(this)
  type(sparr2), intent(inout) :: this

  if (allocated(this%st_ind))   deallocate(this%st_ind)
  if (allocated(this%end_ind))  deallocate(this%end_ind)
  if (allocated(this%val))      deallocate(this%val)
  this%nnz  = 0
  this%nind = 0
end subroutine cleanup_sparr2

! returns "size" (2 + 2*nind + nnz) of sparr2
integer function size_sparr2(this)
  type(sparr2), intent(in) :: this

  size_sparr2 = 2 + this%nnz + 2*this%nind
end function size_sparr2

! writing out sparse array to array
subroutine writearray_sparr2(this, array, ierr)
  type(sparr2), intent(in)                :: this
  real(r_single), dimension(:), intent(out) :: array
  integer(i_kind), optional, intent(out)  :: ierr
 
  integer(i_kind) :: ind

  if (present(ierr)) ierr = 0
  if (size(array) < size_sparr2(this)) then
    if (present(ierr)) ierr = -1
    return
  endif

  ind = 1
  array(ind) = this%nnz
  ind = ind + 1
  array(ind) = this%nind
  ind = ind + 1
  array(ind:ind+this%nind-1) = this%st_ind
  ind = ind + this%nind
  array(ind:ind+this%nind-1) = this%end_ind
  ind = ind + this%nind
  array(ind:ind+this%nnz-1)  = this%val

end subroutine writearray_sparr2

! writing out sparse array to array
subroutine writearray_r_sparr2(this, array, ierr)
  type(sparr2), intent(in)                :: this
  real(r_kind), dimension(:), intent(out) :: array
  integer(i_kind), optional, intent(out)  :: ierr

  integer(i_kind) :: ind

  if (present(ierr)) ierr = 0
  if (size(array) < size_sparr2(this)) then
    if (present(ierr)) ierr = -1
    return
  endif

  ind = 1
  array(ind) = this%nnz
  ind = ind + 1
  array(ind) = this%nind
  ind = ind + 1
  array(ind:ind+this%nind-1) = this%st_ind
  ind = ind + this%nind
  array(ind:ind+this%nind-1) = this%end_ind
  ind = ind + this%nind
  array(ind:ind+this%nnz-1)  = this%val

end subroutine writearray_r_sparr2


! reading sparse array from array
subroutine readarray_sparr2(this, array)
  type(sparr2), intent(out)              :: this
  real(r_single), dimension(:), intent(in) :: array

  integer(i_kind) :: ind, nnz, nind

  ind = 1
  nnz = array(ind)
  ind = ind + 1
  nind = array(ind)
  ind = ind + 1

  call init_sparr2(this, nnz, nind)

  this%st_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%end_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%val = array(ind:ind+nnz-1)

end subroutine readarray_sparr2


end module sparsearr
