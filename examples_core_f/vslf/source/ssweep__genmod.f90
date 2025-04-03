        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 10 14:37:01 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SSWEEP__genmod
          INTERFACE 
            SUBROUTINE SSWEEP(K,DIM,G)
              INTEGER(KIND=4), INTENT(IN) :: DIM
              INTEGER(KIND=4), INTENT(IN) :: K
              REAL(KIND=4), INTENT(OUT) :: G(DIM,DIM)
            END SUBROUTINE SSWEEP
          END INTERFACE 
        END MODULE SSWEEP__genmod
