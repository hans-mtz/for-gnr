        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPVALCOVARIANCE__genmod
          INTERFACE 
            FUNCTION DPVALCOVARIANCE(N,C,C_II,C_IJ,C_JJ)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: C
              REAL(KIND=8), INTENT(IN) :: C_II
              REAL(KIND=8), INTENT(IN) :: C_IJ
              REAL(KIND=8), INTENT(IN) :: C_JJ
              REAL(KIND=4) :: DPVALCOVARIANCE
            END FUNCTION DPVALCOVARIANCE
          END INTERFACE 
        END MODULE DPVALCOVARIANCE__genmod
