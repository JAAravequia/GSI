module converr_td
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_td
!   prgmmr: su          org: np2                date: 2007-03-15
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations error to read
!            temperature error table, the structure is different from current
!            one, the first 
!
! program history log:
!   2007-03-15  su  - original code - move reading observation error table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have the new error information 
!
! Subroutines Included:
!   sub converr_t_read      - allocate arrays for and read in conventional error table 
!   sub converr_t_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_t             -  the array to hold the error table
!   def ptabl_t             -  the array to have vertical pressure values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only:r_kind,i_kind,r_single
use constants, only: zero
use obsmod, only : oberrflg 
implicit none

! set default as private
  private
! set subroutines as public
  public :: converr_td_read
  public :: converr_td_destroy
! set passed variables as public
  public :: etabl_td,ptabl_td,isuble_td,maxsub_td

  integer(i_kind),save:: ietabl_td,itypex,itypey,lcount,iflag,k,m,n,maxsub_td
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_td
  real(r_kind),save,allocatable,dimension(:)  :: ptabl_td
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_td

contains


  subroutine converr_td_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_t_read      read error table 
!
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine reads the conventional error table file
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2013-05-14  guo     -- add status and iostat in open, to correctly
!                          handle the error case of "obs error table not
!                          available to 3dvar".
!   2015-03-06  yang    -- add ld=300, the size of the error table.
!                          Remove the original calculation to get error table
!                          array index. ld=300 is sufficient for current conventional
!                          observing systems.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
     use constants, only: half
     implicit none
     integer(i_kind),    parameter :: ld=300  ! max number of convent. observing systems
     integer(i_kind),intent(in   ) :: mype
     integer(i_kind):: ier

     maxsub_td=5
     allocate(etabl_td(ld,33,6),isuble_td(ld,5))

     etabl_td=1.e9_r_kind
      
     ietabl_td=19 !CHANGE
     open(ietabl_td,file='errtable_td',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR_td:  ***WARNING*** obs error table ("errtable") not available to 3dvar.'
        lcount=0
        oberrflg=.false.
        return
     endif

     rewind ietabl_td
     etabl_td=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl_td,100,IOSTAT=iflag,end=120) itypey
!        write(6,*) 'READ_TD_TABLE iflag=, itypey=', iflag,itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ietabl_td,105,IOSTAT=iflag,end=120) (isuble_td(itypex,n),n=1,5)
105     format(8x,5i12)
        do k=1,33
           read(ietabl_td,110)(etabl_td(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR_TD:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     else
        if(mype == 0) then
           write(6,*)'CONVERR_T:  using observation errors from user provided table'
        endif
        allocate(ptabl_td(34))
! use itypex to get pressure values.  itypex is the last valid observation type
        if (itypex > 0 ) then
           ptabl_td=zero
           ptabl_td(1)=etabl_td(itypex,1,1)
           do k=2,33
              ptabl_td(k)=half*(etabl_td(itypex,k-1,1)+etabl_td(itypex,k,1))
           enddo
           ptabl_td(34)=etabl_td(itypex,33,1)
        else
            write(6,*)'ERROR IN CONVERR_T: NO OBSERVATION TYPE READ IN'
            return
        endif
     endif

     close(ietabl_td)

     return
  end subroutine converr_td_read


subroutine converr_td_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_t_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from converr_t file
!
! program history log:
!   2007-03-15  su 
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

     deallocate(etabl_td,ptabl_td,isuble_td)
     return
  end subroutine converr_td_destroy

end module converr_td

