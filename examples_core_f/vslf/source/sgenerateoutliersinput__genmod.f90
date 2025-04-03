        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGENERATEOUTLIERSINPUT__genmod
          INTERFACE 
            FUNCTION SGENERATEOUTLIERSINPUT(DIM,N,X,EPS,A,A_OUTL,C,     &
     &OUTLINDEX,CNTOUTL)
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: DIM
              REAL(KIND=4), INTENT(OUT) :: X(DIM,N)
              REAL(KIND=4), INTENT(IN) :: EPS
              REAL(KIND=4), INTENT(IN) :: A(DIM)
              REAL(KIND=4), INTENT(IN) :: A_OUTL(DIM)
              REAL(KIND=4), INTENT(IN) :: C(DIM,DIM)
              REAL(KIND=4), INTENT(OUT) :: OUTLINDEX(N)
              INTEGER(KIND=4), INTENT(OUT) :: CNTOUTL
              REAL(KIND=4) :: SGENERATEOUTLIERSINPUT
            END FUNCTION SGENERATEOUTLIERSINPUT
          END INTERFACE 
        END MODULE SGENERATEOUTLIERSINPUT__genmod
