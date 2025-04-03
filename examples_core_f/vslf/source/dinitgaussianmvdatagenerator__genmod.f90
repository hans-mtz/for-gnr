        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DINITGAUSSIANMVDATAGENERATOR__genmod
          INTERFACE 
            FUNCTION DINITGAUSSIANMVDATAGENERATOR(DIM,C,T,STREAM)
              USE MKL_VSL_TYPE
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=8), INTENT(OUT) :: T(DIM,DIM)
              TYPE (VSL_STREAM_STATE), INTENT(OUT) :: STREAM
              REAL(KIND=4) :: DINITGAUSSIANMVDATAGENERATOR
            END FUNCTION DINITGAUSSIANMVDATAGENERATOR
          END INTERFACE 
        END MODULE DINITGAUSSIANMVDATAGENERATOR__genmod
