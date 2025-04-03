        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTEPVALSVARIATION__genmod
          INTERFACE 
            SUBROUTINE DCOMPUTEPVALSVARIATION(DIM,N,VARIATION,A,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: VARIATION(DIM)
              REAL(KIND=8), INTENT(IN) :: A(DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE DCOMPUTEPVALSVARIATION
          END INTERFACE 
        END MODULE DCOMPUTEPVALSVARIATION__genmod
