SUBROUTINE Heapsort(n, a, index)

!
!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2006, EUMETSAT, All Rights Reserved.


!**** Heapsort
!        A.Collard   ECMWF   01/02/06

!    PURPOSE
!    -------
!    Basic heapsort algorithm.

!    INTERFACE
!    ---------
!    *CALL* * HEAPSORT( )*
!    WHERE N     : Length of input array
!          A     : Real input array
!          INDEX : Output ranked array


!    MODIFICATIONS
!    -------------
!    16/05/06   A.Collard   1.0   Original version.


IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(in)    :: n
REAL,    INTENT(in)    :: a(:)
INTEGER, INTENT(inout) :: index(:)

INTEGER :: i,j,right,left, idx
REAL :: tmp

!------------------------------------------

IF (n <= 1) RETURN
left  = n/2+1
right = n

LOOP: DO
   IF (left > 1) THEN
      left = left - 1
      idx  = index(left)
   ELSE
      idx = index(right)
      index(right) = index(1)
      right = right - 1
      if (right == 1) then
         index(1) = idx
         Exit LOOP
      endif
   ENDIF
   tmp = a(idx)
   i = left
   j = 2*left
   DO WHILE (j <= right)
      IF (j < right) THEN
         IF (a(index(j)) < a(index(j+1))) j = j + 1
      END IF
      IF (tmp < a(index(j))) THEN
         index(i) = index(j)
         i = j
         j = 2*j
      ELSE
         j = right + 1
      END IF
   END DO
   index(i) = idx
END DO LOOP

END SUBROUTINE Heapsort
