        !COMPILER-GENERATED INTERFACE MODULE: Thu Mar 31 19:35:36 2016
        MODULE FFTPACK_RADF2__genmod
          INTERFACE 
            SUBROUTINE FFTPACK_RADF2(IDO,L1,CC,CH,WA1)
              INTEGER(KIND=4) :: L1
              INTEGER(KIND=4) :: IDO
              REAL(KIND=4) :: CC(IDO,L1,2)
              REAL(KIND=4) :: CH(IDO,2,L1)
              REAL(KIND=4) :: WA1(IDO)
            END SUBROUTINE FFTPACK_RADF2
          END INTERFACE 
        END MODULE FFTPACK_RADF2__genmod
