        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DSWEEP__genmod
          INTERFACE 
            SUBROUTINE DSWEEP(K,DIM,G)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: K
              REAL(KIND=8), INTENT(OUT) :: G(DIM,DIM)
            END SUBROUTINE DSWEEP
          END INTERFACE 
        END MODULE DSWEEP__genmod
