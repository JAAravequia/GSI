SUBROUTINE MOVINGA(PV,KV,KW,PVA)

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2006, EUMETSAT, All Rights Reserved.


!*** *Movinga* - Moving average of REAL array
!        Phil Watts  ECMWF 24/01/02

!    PURPOSE
!    -------
!    Calculate the moving average (smoothing filter) of array
!    No error checking supplied.

!**  INTERFACE.
!    ---------
!**  *CALL* * MOVINGA( )*
!    WHERE V     : Input array to be averaged
!          NV    : Number of elements in V
!          NW    : Window width for filter
!          VA    : Averaged array

!**  EXTERNALS
!    ---------
!    NONE

!    MODIFICATIONS
!    -------------
!    01/02/06   A.Collard   1.0   Original export version.
!    13/01/15   R.Eresmaa   2.1   Make array size specifications implicit.

IMPLICIT NONE

!* 0.1 global variables
INTEGER, INTENT(IN)    :: KV     ! length of V
REAL,    INTENT(IN)    :: PV(:)  ! original array
INTEGER, INTENT(IN)    :: KW     ! length of averaging window
REAL,    INTENT(INOUT) :: PVA(:) ! averaged array
!* 0.2 local variables
INTEGER :: INJ,J,I

PVA(:)=0

DO I = 1,KV  ! loop over array elements
  INJ=0
  DO J=I-KW/2,I+KW/2,1  ! loop over window
    IF (J > 0 .AND. J < (KV+1)) then ! if window element exists in original
                                     ! array
      INJ=INJ+1
      PVA(I)=PVA(I)+PV(J)            ! add value
    ENDIF
  ENDDO
  PVA(I)=PVA(I)/REAL(INJ)            ! mean value
ENDDO

END SUBROUTINE MOVINGA
