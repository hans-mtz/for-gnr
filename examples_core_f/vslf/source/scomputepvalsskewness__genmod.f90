        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSSKEWNESS__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSSKEWNESS(DIM,N,SKEW,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: SKEW(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE SCOMPUTEPVALSSKEWNESS
          END INTERFACE 
        END MODULE SCOMPUTEPVALSSKEWNESS__genmod
