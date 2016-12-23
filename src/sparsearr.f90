module sparsearr
use kinds
implicit none

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

end module sparsearr
