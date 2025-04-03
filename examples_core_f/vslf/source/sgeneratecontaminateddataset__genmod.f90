        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGENERATECONTAMINATEDDATASET__genmod
          INTERFACE 
            FUNCTION SGENERATECONTAMINATEDDATASET(DIM,N,X,A,C,RATIO,M,  &
     &COEFF)
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(OUT) :: X(N,DIM)
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              INTEGER(KIND=4), INTENT(IN) :: RATIO
              REAL(KIND=4), INTENT(IN) :: M
              REAL(KIND=4), INTENT(IN) :: COEFF
              REAL(KIND=4) :: SGENERATECONTAMINATEDDATASET
            END FUNCTION SGENERATECONTAMINATEDDATASET
          END INTERFACE 
        END MODULE SGENERATECONTAMINATEDDATASET__genmod
