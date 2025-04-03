        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGENERATEUNIFORMDATA__genmod
          INTERFACE 
            FUNCTION DGENERATEUNIFORMDATA(N,X,A,B)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(OUT) :: X(N)
              REAL(KIND=8), INTENT(IN) :: A
              REAL(KIND=8), INTENT(IN) :: B
              REAL(KIND=4) :: DGENERATEUNIFORMDATA
            END FUNCTION DGENERATEUNIFORMDATA
          END INTERFACE 
        END MODULE DGENERATEUNIFORMDATA__genmod
