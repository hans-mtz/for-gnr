        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGENERATEGAUSSIANMVDATABLOCK__genmod
          INTERFACE 
            FUNCTION SGENERATEGAUSSIANMVDATABLOCK(DIM,N,X,STREAM,A,T)
              USE MKL_VSL_TYPE
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(OUT) :: X(DIM,N)
              TYPE (VSL_STREAM_STATE), INTENT(IN) :: STREAM
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: T(DIM,DIM)
              REAL(KIND=4) :: SGENERATEGAUSSIANMVDATABLOCK
            END FUNCTION SGENERATEGAUSSIANMVDATABLOCK
          END INTERFACE 
        END MODULE SGENERATEGAUSSIANMVDATABLOCK__genmod
