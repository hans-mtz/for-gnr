        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTEPVALSCOVARIANCE__genmod
          INTERFACE 
            SUBROUTINE DCOMPUTEPVALSCOVARIANCE(DIM,N,COV,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: COV(DIM,DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: PVALS(DIM,DIM)
            END SUBROUTINE DCOMPUTEPVALSCOVARIANCE
          END INTERFACE 
        END MODULE DCOMPUTEPVALSCOVARIANCE__genmod
