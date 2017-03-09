module converr_raw
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_raw
!   prgmmr: su          org: np2                date: 2017-03-07
! abstract:  This module contains variables and routines related
!            to the assimilation of rawinsonde observation error based on
!            instrument types 
!
! program history log:
!   2007-03-07  su  - original code  
!
! Subroutines Included:
!   sub converr_raw_read      - allocate arrays for and read in conventional error table 
!   sub converr_raw_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_raw             -  the array to hold the error table
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only:r_kind,i_kind,r_single
use constants, only: zero
use obsmod, only : oberrflg_raw 
implicit none

! set default as private
  private
! set subroutines as public
  public :: converr_raw_read
  public :: converr_raw_destroy
! set passed variables as public
  public :: etabl_raw

  integer(i_kind),save:: ietabl,inst,lcount,iflag,k,m
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_raw

contains


  subroutine converr_raw_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_err      read conventional information file
!
!     prgmmr:    su    org: np2                date: 2017-03-07
!
! abstract:  This routine reads the conventional error table file
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
     use constants, only: half
     implicit none

     integer(i_kind),intent(in   ) :: mype

     integer(i_kind):: ier

     allocate(etabl_raw(200,33,5))

     etabl_raw=1.e9_r_kind
      
     ietabl=19
     open(ietabl,file='errtable_raw',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR:  ***WARNING*** raw obs error table ("errtable") not available in GSI'
        lcount=0
        oberrflg_raw=.false.
        return
     endif

     rewind ietabl
     etabl_raw=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl,100,IOSTAT=iflag,end=120) inst
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        do k=1,33
           read(ietabl,110)(etabl_raw(inst,k,m),m=1,5)
110        format(1x,5e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR:  ***WARNING*** raw obs error table not available in GSI'
        oberrflg_raw=.false.
     endif

     close(ietabl)

     return
  end subroutine converr_raw_read


subroutine converr_raw_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_raw_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2017-03-07
!
! abstract:  This routine destroys arrays from converr file
!
! program history log:
!   2007-03-07  su 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
     implicit none

     deallocate(etabl_raw)
     return
  end subroutine converr_raw_destroy

end module converr_raw



