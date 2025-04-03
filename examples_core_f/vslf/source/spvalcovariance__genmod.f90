        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SPVALCOVARIANCE__genmod
          INTERFACE 
            FUNCTION SPVALCOVARIANCE(N,C,C_II,C_IJ,C_JJ)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: C
              REAL(KIND=4), INTENT(IN) :: C_II
              REAL(KIND=4), INTENT(IN) :: C_IJ
              REAL(KIND=4), INTENT(IN) :: C_JJ
              REAL(KIND=4) :: SPVALCOVARIANCE
            END FUNCTION SPVALCOVARIANCE
          END INTERFACE 
        END MODULE SPVALCOVARIANCE__genmod
