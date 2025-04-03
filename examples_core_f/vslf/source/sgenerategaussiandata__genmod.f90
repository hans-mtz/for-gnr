        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGENERATEGAUSSIANDATA__genmod
          INTERFACE 
            FUNCTION SGENERATEGAUSSIANDATA(DIM,N,X,A,SIGMA)
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(OUT) :: X(DIM,N)
              REAL(KIND=4), INTENT(IN) :: A
              REAL(KIND=4), INTENT(IN) :: SIGMA
              REAL(KIND=4) :: SGENERATEGAUSSIANDATA
            END FUNCTION SGENERATEGAUSSIANDATA
          END INTERFACE 
        END MODULE SGENERATEGAUSSIANDATA__genmod
