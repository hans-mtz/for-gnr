        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTEPVALSRAWMOMENTS__genmod
          INTERFACE 
            SUBROUTINE DCOMPUTEPVALSRAWMOMENTS(DIM,N,RAW,ORDER,A,C,PVALS&
     &)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: RAW(DIM)
              INTEGER(KIND=4), INTENT(IN) :: ORDER
              REAL(KIND=8), INTENT(IN) :: A(DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE DCOMPUTEPVALSRAWMOMENTS
          END INTERFACE 
        END MODULE DCOMPUTEPVALSRAWMOMENTS__genmod
