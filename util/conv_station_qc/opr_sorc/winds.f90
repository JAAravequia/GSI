!C---------------------------------------------------------------------
!C  DD FF >> U V
!C---------------------------------------------------------------------
      SUBROUTINE UV(DD,FF,U,V)
      DATA  CONV2R/0.017453293/
 
      IF(FF.LE.0.0)  THEN
         U = 0.0
         V = 0.0
      ELSE
         U = -FF * SIN(DD*CONV2R)
         V = -FF * COS(DD*CONV2R)
      END IF
      RETURN
      END
!C-----------------------------------------------------------------------
!C  PROGRAM TO COMPUTE WIND SPEED AND DIRECTION FROM U AND V COMPONENTS
!C-----------------------------------------------------------------------
      SUBROUTINE UV2DS(U,V)
 
      DATA PI180  /.0174532/
      DATA RAD2DG /57.29578/
      DATA VMAX   /  99999./
 
!C-----------------------------------------------------------------------
!C-----------------------------------------------------------------------
 
!C  RETURN THE SPEED AND DIRECTION IN THE WIND COMPONENT VARAIBLES
!C  --------------------------------------------------------------
 
      DU = U
      DV = V
 
      IF(DU.EQ.0. .AND. DV.EQ.0.) THEN
         DD = 360.
      ELSE IF(DU.EQ.0.) THEN
         IF(DV.GT.0.) DD = 180.
         IF(DV.LT.0.) DD = 360.
      ELSE IF(DV.EQ.0.) THEN
         IF( DU.GT.0.0 ) DD = 270.
         IF( DU.LT.0.0 ) DD = 90.
      ELSE
         DD = ATAN2(DU,DV) * RAD2DG + 180.
         DD = AMOD(DD,360.)
      ENDIF
      DS = SQRT(DU**2 + DV**2)
 
      U = DD
      V = DS
 
      RETURN
      END
