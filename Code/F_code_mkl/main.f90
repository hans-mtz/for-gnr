PROGRAM MAIN
  USE MATRIX
  USE RANDOM
  IMPLICIT NONE

  REAL(8) :: A(30000,3),B(30000),c(3),e(30000),AAinv(3,3),AB(3)
  INTEGER :: i,j
  INTEGER :: tin,tout,hz,n
  REAL(8) :: tim

  DO i=1,30000
     DO j=1,3
        A(i,j)=Sample_Uniform(0.0d0,1.0d0)
     END DO
     e=Sample_Normal(0.0d0,1.0d0)
  END DO
  WRITE(*,*) "A"

  CALL SYSTEM_CLOCK(count_rate=hz) 
  CALL SYSTEM_CLOCK(count=tin) 
  c=(/1.0d0,-0.5d0,-1.0d0/)
  B=MATMUL(A,c) + e
  AAinv=MATMUL(TRANSPOSE(A),A)
  AAinv=Matrix_Inverse_symmetric(AAinv)
  AB=MATMUL(TRANSPOSE(A),B)
  C=MATMUL(AAinv,AB)
  CALL SYSTEM_CLOCK(count=tout) 
  tout=tout-tin
  tim=DBLE(tout)/DBLE(hz)
  WRITE(*,*) tim,c

  CALL SYSTEM_CLOCK(count_rate=hz) 
  CALL SYSTEM_CLOCK(count=tin) 
  c=(/1.0d0,-0.5d0,-1.0d0/)
  B=MMUL(A,c) + e
  AAinv=MMUL(TRANSPOSE(A),A)
  AAinv=Matrix_Inverse_symmetric(AAinv)
  AB=MMUL(TRANSPOSE(A),B)
  C=MMUL(AAinv,AB)
  CALL SYSTEM_CLOCK(count=tout) 
  tout=tout-tin
  tim=DBLE(tout)/DBLE(hz)
  WRITE(*,*) tim,c

  CALL SYSTEM_CLOCK(count_rate=hz) 
  CALL SYSTEM_CLOCK(count=tin) 
  c=(/1.0d0,-0.5d0,-1.0d0/)
  B=MMUL(A,c) + e
  c=OLS(A,B)
  CALL SYSTEM_CLOCK(count=tout) 
  tout=tout-tin
  tim=DBLE(tout)/DBLE(hz)
  WRITE(*,*) tim,c

  WRITE(*,*) c
  WRITE(*,*) ab
  WRITE(*,*)
  WRITE(*,*) OUTER_PRODUCT(c,AB)
  WRITE(*,*)
  WRITE(*,*) OUTER_PRODUCT_M(c,AB)




END PROGRAM MAIN
