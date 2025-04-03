        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGENERATEGAUSSIANDATA__genmod
          INTERFACE 
            FUNCTION DGENERATEGAUSSIANDATA(DIM,N,X,A,SIGMA)
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=8), INTENT(OUT) :: X(DIM,N)
              REAL(KIND=8), INTENT(IN) :: A
              REAL(KIND=8), INTENT(IN) :: SIGMA
              REAL(KIND=4) :: DGENERATEGAUSSIANDATA
            END FUNCTION DGENERATEGAUSSIANDATA
          END INTERFACE 
        END MODULE DGENERATEGAUSSIANDATA__genmod
