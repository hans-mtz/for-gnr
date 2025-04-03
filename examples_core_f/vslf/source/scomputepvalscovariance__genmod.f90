        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSCOVARIANCE__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSCOVARIANCE(DIM,N,COV,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: COV(DIM,DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM,DIM)
            END SUBROUTINE SCOMPUTEPVALSCOVARIANCE
          END INTERFACE 
        END MODULE SCOMPUTEPVALSCOVARIANCE__genmod
