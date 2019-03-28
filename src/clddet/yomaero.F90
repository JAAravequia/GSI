MODULE YOMAERO

!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 25 November 1998, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2016, EUMETSAT, All Rights Reserved.


!*** Yomaero
!    J. Letertre-Danczak  ECMWF 06/10/15

!    Purpose
!    -------
!    Set up aerosol detection structures.

!    MODIFICATIONS
!    -------------
!    21/12/16   R.Eresmaa   2.3   Import to NWPSAF CADS V2.3

USE YOMIASI, ONLY : JP__Min_Sensor_Index, &
                    JP__Max_Sensor_Index

IMPLICIT NONE

SAVE

TYPE Aerosol_Detect_Type
   INTEGER         :: M__Sensor               ! Unique ID for sensor

   INTEGER         :: N__Num_Aerosol_Tests    ! No. of aerosol detection tests

   INTEGER,POINTER :: N__Num_Aerosol_Chans(:) ! No. of aerosol test channels

   INTEGER,POINTER :: N__Aerosol_Chans(:,:)   ! List of aerosol test channels
   INTEGER         :: N__Mean_Aerosol_Chans   ! Boxcar averaging window width
   REAL,POINTER    :: R__Aerosol_TBD(:,:)     ! Aerosol test thresholds

END TYPE Aerosol_Detect_Type

TYPE(Aerosol_Detect_Type) :: &
&             S__Aerosol_Detect_Setup(JP__Min_Sensor_Index:JP__Max_Sensor_Index)

END MODULE YOMAERO
