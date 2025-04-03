        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGENERATEGAUSSIANMVDATA__genmod
          INTERFACE 
            FUNCTION SGENERATEGAUSSIANMVDATA(DIM,N,X,A,C)
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(OUT) :: X(DIM,N)
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4) :: SGENERATEGAUSSIANMVDATA
            END FUNCTION SGENERATEGAUSSIANMVDATA
          END INTERFACE 
        END MODULE SGENERATEGAUSSIANMVDATA__genmod
