        !COMPILER-GENERATED INTERFACE MODULE: Thu Mar 31 19:35:36 2016
        MODULE FFTPACK_RADF4__genmod
          INTERFACE 
            SUBROUTINE FFTPACK_RADF4(IDO,L1,CC,CH,WA1,WA2,WA3)
              INTEGER(KIND=4) :: L1
              INTEGER(KIND=4) :: IDO
              REAL(KIND=4) :: CC(IDO,L1,4)
              REAL(KIND=4) :: CH(IDO,4,L1)
              REAL(KIND=4) :: WA1(IDO)
              REAL(KIND=4) :: WA2(IDO)
              REAL(KIND=4) :: WA3(IDO)
            END SUBROUTINE FFTPACK_RADF4
          END INTERFACE 
        END MODULE FFTPACK_RADF4__genmod
