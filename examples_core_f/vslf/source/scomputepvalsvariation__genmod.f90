        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSVARIATION__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSVARIATION(DIM,N,VARIATION,A,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: VARIATION(DIM)
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE SCOMPUTEPVALSVARIATION
          END INTERFACE 
        END MODULE SCOMPUTEPVALSVARIATION__genmod
