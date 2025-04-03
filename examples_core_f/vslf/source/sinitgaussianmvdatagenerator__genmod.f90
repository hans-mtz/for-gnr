        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SINITGAUSSIANMVDATAGENERATOR__genmod
          INTERFACE 
            FUNCTION SINITGAUSSIANMVDATAGENERATOR(DIM,C,T,STREAM)
              USE MKL_VSL_TYPE
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: T(DIM,DIM)
              TYPE (VSL_STREAM_STATE), INTENT(OUT) :: STREAM
              REAL(KIND=4) :: SINITGAUSSIANMVDATAGENERATOR
            END FUNCTION SINITGAUSSIANMVDATAGENERATOR
          END INTERFACE 
        END MODULE SINITGAUSSIANMVDATAGENERATOR__genmod
