        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DCOMPUTEPVALSCENTRALMOMENTS__genmod
          INTERFACE 
            SUBROUTINE DCOMPUTEPVALSCENTRALMOMENTS(DIM,N,CENTRAL,ORDER,A&
     &,C,PVALS)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(IN) :: CENTRAL(DIM)
              INTEGER(KIND=4), INTENT(IN) :: ORDER
              REAL(KIND=8), INTENT(IN) :: A(DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: PVALS(DIM)
            END SUBROUTINE DCOMPUTEPVALSCENTRALMOMENTS
          END INTERFACE 
        END MODULE DCOMPUTEPVALSCENTRALMOMENTS__genmod
