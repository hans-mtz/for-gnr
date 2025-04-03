        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTERAWMOMENT__genmod
          INTERFACE 
            FUNCTION SCOMPUTERAWMOMENT(ORDER,MEAN,SIGMA)
              INTEGER(KIND=4), INTENT(IN) :: ORDER
              REAL(KIND=4), INTENT(IN) :: MEAN
              REAL(KIND=4), INTENT(IN) :: SIGMA
              REAL(KIND=4) :: SCOMPUTERAWMOMENT
            END FUNCTION SCOMPUTERAWMOMENT
          END INTERFACE 
        END MODULE SCOMPUTERAWMOMENT__genmod
