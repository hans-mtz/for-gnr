        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTERAWMOMENT__genmod
          INTERFACE 
            FUNCTION DCOMPUTERAWMOMENT(ORDER,MEAN,SIGMA)
              INTEGER(KIND=4), INTENT(IN) :: ORDER
              REAL(KIND=8), INTENT(IN) :: MEAN
              REAL(KIND=8), INTENT(IN) :: SIGMA
              REAL(KIND=4) :: DCOMPUTERAWMOMENT
            END FUNCTION DCOMPUTERAWMOMENT
          END INTERFACE 
        END MODULE DCOMPUTERAWMOMENT__genmod
