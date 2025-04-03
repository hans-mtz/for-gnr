        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGENERATEMISSINGVALUESINPUT__genmod
          INTERFACE 
            FUNCTION DGENERATEMISSINGVALUESINPUT(DIM,N,X,EPS,NPATT,PATT,&
     &A,C,MISS_VALS,NMISS_VALS)
              INTEGER(KIND=4), INTENT(IN) :: NPATT
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=8), INTENT(OUT) :: X(N,DIM)
              REAL(KIND=8), INTENT(IN) :: EPS
              INTEGER(KIND=4), INTENT(IN) :: PATT(DIM*NPATT)
              REAL(KIND=8), INTENT(IN) :: A(DIM)
              REAL(KIND=8), INTENT(IN) :: C(DIM,DIM)
              INTEGER(KIND=4), INTENT(OUT) :: MISS_VALS(N)
              INTEGER(KIND=4), INTENT(OUT) :: NMISS_VALS
              REAL(KIND=4) :: DGENERATEMISSINGVALUESINPUT
            END FUNCTION DGENERATEMISSINGVALUESINPUT
          END INTERFACE 
        END MODULE DGENERATEMISSINGVALUESINPUT__genmod
