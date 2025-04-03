        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTEPVALSSKEWNESS__genmod
          INTERFACE 
            SUBROUTINE DCOMPUTEPVALSSKEWNESS(DIM,N,SKEW,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: SKEW(DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE DCOMPUTEPVALSSKEWNESS
          END INTERFACE 
        END MODULE DCOMPUTEPVALSSKEWNESS__genmod
