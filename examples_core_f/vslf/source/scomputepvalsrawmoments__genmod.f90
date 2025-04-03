        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCOMPUTEPVALSRAWMOMENTS__genmod
          INTERFACE 
            SUBROUTINE SCOMPUTEPVALSRAWMOMENTS(DIM,N,RAW,ORDER,A,C,PVALS&
     &)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: RAW(DIM)
              INTEGER(KIND=4), INTENT(IN) :: ORDER
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE SCOMPUTEPVALSRAWMOMENTS
          END INTERFACE 
        END MODULE SCOMPUTEPVALSRAWMOMENTS__genmod
