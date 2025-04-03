        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGENERATEGAUSSIANMVDATABLOCK__genmod
          INTERFACE 
            FUNCTION DGENERATEGAUSSIANMVDATABLOCK(DIM,N,X,STREAM,A,T)
              USE MKL_VSL_TYPE
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=8), INTENT(OUT) :: X(DIM,N)
              TYPE (VSL_STREAM_STATE), INTENT(IN) :: STREAM
              REAL(KIND=8), INTENT(IN) :: A(DIM)
              REAL(KIND=8), INTENT(IN) :: T(DIM,DIM)
              REAL(KIND=4) :: DGENERATEGAUSSIANMVDATABLOCK
            END FUNCTION DGENERATEGAUSSIANMVDATABLOCK
          END INTERFACE 
        END MODULE DGENERATEGAUSSIANMVDATABLOCK__genmod
