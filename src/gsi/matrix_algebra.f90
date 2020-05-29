subroutine matrix_algebra()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: matrix_algebra

!   prgmmr: k apodaca <Karina.Apodaca@noaa.gov>
!      org: CSU/CIRA, Data Assimilation Group
!           Now at: UMiami/CIMAS and NOAA/AOML/HRD
!     date: 2020-05-22
!
! abstract:  Subroutines for calculating the diagonal, inverse, and
!            inverse transpose of a matrix are included.
!
!            These algebraic operations are applied to the background
!            state vector, the first guess, and the analysis increment.
!            All are necessary for applying non-Gaussian-based DA.
!
! program history log:
!   2017-09-08  k apodaca  -  first version of diagonal matrix
!   calculation
!   2020-05-22  k apodaca  -  condense multiple matrix operations into a
!                             single routine


! Declare local variables
      integer, parameter :: N = 3
      integer :: i
      integer, target :: dat(N*N)  
      integer, pointer :: mat(:,:)
      integer, pointer :: diag(:)

! File(s) with diagonal output
      character :: diag_bstatev*40
      character :: diag_1stguess*40
      character :: diag_deltacv*40

! Open file with array for local diagonal estimation

      write(post_file,199)mype
199   format('1st_guess_',i3.3,'.bin')
      open(unit=200,file=trim(diag_bstatev),form='formatted',action='read')
      
      write(post_file,200)mype
200   format('btatev_',i3.3,'.bin')
      open(unit=200,file=trim(diag_1stguess),form='formatted',action='read')

      write(post_file,201)mype
201   format('deltacv_',i3.3,'.bin')
      open(unit=201,file=trim(diag_deltacv),form='formatted',action='read')

! Calculate diagonal
  subroutine diagonal

      dat = [( i, i=1,size(dat) )]
      mat(1:N,1:N) => dat
      diag => dat(1::N+1)
      print *, "Approach 1: Matrix:"
      do i = 1, size(mat,dim=1)
         print "(*(g0,1x))", mat(:,i)
      end do
      print *, "Diagonal elements: ", diag
      mat => null()
      diag => null()
close(unit=199)
close(unit=200)
close(unit=201)


! Calculate transpose
  subroutine transpose
! Calculate inverse
  subroutine inverse
end subroutine matrix_algebra
