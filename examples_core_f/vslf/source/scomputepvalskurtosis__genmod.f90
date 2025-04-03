        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSKURTOSIS__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSKURTOSIS(DIM,N,KURT,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: KURT(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE SCOMPUTEPVALSKURTOSIS
          END INTERFACE 
        END MODULE SCOMPUTEPVALSKURTOSIS__genmod
