SUBROUTINE Aerosol_Detect (&
&    K__SENSOR,     &
&    K__NCHANS,     &
&    K__CHANID,     &
&    P__OBSBTs,     &
&    K__AerosolPresent)

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2006, EUMETSAT, All Rights Reserved.

!**** Aerosol Detection
!        A. Collard  ECMWF 17/05/06

!     PURPOSE
!     -------
!     Look for aerosol signal in IR radiances

!**   INTERFACE
!     ---------
!     aerosol_detect is called from cloud_detect_wrapper

!     METHOD
!     ------
!     A unique theoretically derived aerosol signal is sought through
!     observed brightness temperature differences in two spectral
!     locations within long-wave window region.

!     EXTERNALS
!     ---------

!     MODIFICATIONS
!     -------------
!     17/05/06  1.0 Original code.                                   A. Collard
!     19/10/06  1.1 Modification to present channels test.           A. Collard
!     13/01/15  2.1 Make array size specifications implicit.         R. Eresmaa
!     10/11/15  2.2 New algorithm.             J. Letertre-Danczak & R. Eresmaa
!     29/12/16  2.3 Make sensor-independent.   J. Letertre-Danczak & R. Eresmaa

USE YOMAERO, ONLY : S__Aerosol_Detect_Setup

IMPLICIT NONE

! Subroutine arguments

INTEGER,INTENT(IN)  :: K__SENSOR         ! sensor
INTEGER,INTENT(IN)  :: K__NCHANS         ! no. of channels
INTEGER,INTENT(IN)  :: K__CHANID(:)      ! Channel IDs
REAL,   INTENT(IN)  :: P__OBSBTS(:)      ! Observed radiances
INTEGER,INTENT(OUT) :: K__AerosolPresent ! Aerosol flag

! Local variables

INTEGER           :: J, I__TB, I__K, I__Test
INTEGER           :: I__MaxChans
INTEGER           :: I__Num_Aerosol_Chans
INTEGER           :: I__Aerosol

INTEGER, POINTER  :: I__Aerosol_Chans(:)
INTEGER           :: I__M
INTEGER           :: I__Mean_Aerosol_Chans

REAL              :: Z_DIFF_Aerosol
REAL, POINTER     :: Z__TBD(:)
REAL, ALLOCATABLE :: Z_TBM(:)

INTEGER, ALLOCATABLE :: I__NumFoundChans_1(:)
INTEGER              :: I__Mean2_Aerosol_Chans

!-----------------------------------
! Initialise
!-----------------------------------

K__AerosolPresent = 0

I__MaxChans = &
&      MAXVAL(S__Aerosol_Detect_Setup(K__SENSOR) % N__Num_Aerosol_Chans(:))

!-----------------------------------
! Loop through tests
!-----------------------------------

ALLOCATE(Z_TBM(I__MaxChans))
ALLOCATE(I__NumFoundChans_1(I__MaxChans))
I__Aerosol=0

TestLoop : DO I__Test = &
&                1, S__Aerosol_Detect_Setup(K__SENSOR) % N__Num_Aerosol_Tests

   I__Num_Aerosol_Chans = &
&        S__Aerosol_Detect_Setup(K__SENSOR) % N__Num_Aerosol_Chans(I__Test)
   I__Aerosol_Chans => S__Aerosol_Detect_Setup(K__SENSOR) % &
&        N__Aerosol_Chans(I__Test,1:I__Num_Aerosol_Chans)
   Z__TBD => S__Aerosol_Detect_Setup(K__SENSOR) % &
&        R__Aerosol_TBD(I__Test,1:I__Num_Aerosol_Chans)
   I__Mean_Aerosol_Chans = S__Aerosol_Detect_Setup(K__SENSOR) % &
&        N__Mean_Aerosol_Chans
   I__Mean2_Aerosol_Chans = int(I__Mean_Aerosol_Chans/2)+1

   DO I__TB=1, I__Num_Aerosol_Chans
      I__NumFoundChans_1(I__TB)=0
      Z_TBM(I__TB)=0
   ENDDO

   DO I__K=1,K__NCHANS
      IF (P__OBSBTS(I__K) <= 0.) CYCLE
      DO I__TB=1, I__Num_Aerosol_Chans
         DO J=1, I__Mean_Aerosol_Chans
            I__M = I__Aerosol_Chans(I__TB)-I__Mean2_Aerosol_Chans+J
            IF (I__M == K__CHANID(I__K)) THEN
               Z_TBM(I__TB) = Z_TBM(I__TB)+P__OBSBTS(I__K)
               I__NumFoundChans_1(I__TB) = I__NumFoundChans_1(I__TB)+1
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO I__TB=1, I__Num_Aerosol_Chans
      IF(I__NumFoundChans_1(I__TB)==0) CYCLE TestLoop
      Z_TBM(I__TB) = Z_TBM(I__TB)/I__NumFoundChans_1(I__TB)
   ENDDO

   Z_DIFF_Aerosol=Z_TBM(1)-Z_TBM(2)
   IF (Z_DIFF_Aerosol <= Z__TBD(1)) I__Aerosol=I__Aerosol+1
   Z_DIFF_Aerosol=Z_TBM(3)-Z_TBM(4)
   IF (Z_DIFF_Aerosol <= Z__TBD(2)) I__Aerosol=I__Aerosol+1

ENDDO TestLoop

IF (I__Aerosol==2) K__AerosolPresent=1

DEALLOCATE(Z_TBM,I__NumFoundChans_1)
NULLIFY(I__Aerosol_Chans)
NULLIFY(Z__TBD)

END SUBROUTINE Aerosol_Detect
