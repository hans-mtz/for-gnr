        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSVARIANCE__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSVARIANCE(DIM,N,COV,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: COV(DIM,DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM,DIM)
            END SUBROUTINE SCOMPUTEPVALSVARIANCE
          END INTERFACE 
        END MODULE SCOMPUTEPVALSVARIANCE__genmod
