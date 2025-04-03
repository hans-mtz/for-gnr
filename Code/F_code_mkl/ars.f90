MODULE ARS
  USE NRUTIL, ONLY : ASSERT
  IMPLICIT NONE
  PRIVATE 
  PUBLIC :: Accelerated_Random_Search
  
  INTEGER(4), SAVE :: kiss1_ars=123456789,kiss2_ars=362436069,kiss3_ars=521288629,kiss4_ars=380116160,seed_ars,zigjz_ars
  LOGICAL, SAVE :: initialized=.FALSE.
CONTAINS
  
  SUBROUTINE Accelerated_Random_Search(xm,c,bound,nexp,nrest,func,xm_init,id,k1)
    ! Minimize a function func. It returns xm, the point at the minimum
    ! using accelerated random search. nexp is the number of radius expansions before it automatically
    ! restarts. nrest is the number of restarts before the algorithm stops and reports the lowest point
    ! so far. bound contains the upper and lower bounds for each element of xm. c is a contraction parameter between (0,1)
    ! if xm_init==.TRUE. the program uses the values in xm as initial centering for the ball 
    ! k1 is an optional argument to set the seed of the generator, id is used for I/O in mpi    
    IMPLICIT NONE
    REAL(8), INTENT(INOUT) :: xm(:)
    REAL(8), INTENT(IN) :: c,bound(:,:)
    INTEGER, INTENT(IN) :: nexp,nrest,id
    INTEGER, INTENT(IN), OPTIONAL :: k1(4)
    LOGICAL, INTENT(IN) :: xm_init
    INTERFACE
       REAL(8) FUNCTION FUNC(x)
         IMPLICIT NONE
         REAL(8), INTENT(IN) :: x(:)
       END FUNCTION FUNC
    END INTERFACE
    REAL(8) :: rho,fm,fx,fy,r,l,u,y(SIZE(xm)),x(SIZE(xm)),d(size(xm))
    INTEGER :: ne,nr,j,sxm
    LOGICAL :: used
    CALL ASSERT(c>0.0d0,c<1.0d0,"ARS contraction parameter should be in (0,1)")
    IF (.NOT.INITIALIZED) THEN
       IF (PRESENT(k1)) THEN
          CALL SET_SEED_ARS(k1(1),k1(2),k1(3),k1(4))
       ELSE
          CALL SET_SEED_ARS()
       END IF
       INITIALIZED=.TRUE.
    END IF
    sxm=SIZE(xm)
    rho=1.0d-8
    fm=1.0d300
    nr=-1
    ne=-1
    r=1.0d0
    used=.FALSE.
    DO
       IF (ne==-1) THEN
          IF ((.NOT.used).AND.(xm_init)) THEN
             DO j=1,sxm
                d(j)=bound(j,2)-bound(j,1)
                x(j)=xm(j)
             END DO
             used=.FALSE.
          ELSE
             DO j=1,sxm
                d(j)=bound(j,2)-bound(j,1)
                x(j)=Sample_Uniform_ARS(bound(j,1),bound(j,2))
             END DO
          END IF
          r=1.0d0
          ne=0
          fx=func(x)
          IF (id==0) WRITE(*,*) "initial value",fx
       END IF
       DO j=1,sxm
          l=MAX(x(j)-r*d(j),bound(j,1))
          u=MIN(x(j)+r*d(j),bound(j,2))
          y(j)=Sample_Uniform_ARS(l,u)
       END DO
       fy=func(y)
       IF (fy<fx) THEN
          x=y
          fx=fy
          r=1.0d0
          IF ((id==0).AND.(fx<fm)) THEN
             WRITE(*,*) "Current minimum after",ne," expansions:",fx
             OPEN(52312,file="point.out")
             DO j=1,sxm
                WRITE(52312,'(F32.16)') x(j)
             END DO
             CLOSE(52312)
          END IF
       ELSE
          r=r*c
       END IF
       IF (r<rho) THEN
          ne=ne+1
          r=1.0d0
          IF (id==0) WRITE(*,*) ne
       END IF
       IF (ne>nexp) THEN
          ne=-1
          nr=nr+1
          IF (fm>=fx) THEN
             xm=x
             fm=fx
          END IF
          IF (id==0) WRITE(*,*) "Expanded: Current minimum after",nr," restarts",fm
       END IF
       IF (nr>=nrest) EXIT
    END DO
    xm=x
  END SUBROUTINE Accelerated_Random_Search

  SUBROUTINE Set_Seed_ARS(k1,k2,k3,k4)
    IMPLICIT NONE
    INTEGER(4), INTENT(IN), OPTIONAL :: k1,k2,k3,k4
    INTEGER :: i
    IF (PRESENT(k1)) THEN
       kiss1_ARS=k1
       kiss2_ARS=k2
       kiss3_ARS=k3
       kiss4_ARS=k4
    ELSE
       CALL SYSTEM_CLOCK(seed_ARS)
       kiss1_ARS=SHR3_ARS()
       kiss2_ARS=SHR3_ARS()
       kiss3_ARS=SHR3_ARS()
       kiss4_ARS=SHR3_ARS()
    END IF
  END SUBROUTINE Set_Seed_ARS

  ! Generate random 32-bit integers
  INTEGER FUNCTION shr3_ARS( )
    zigjz_ARS = seed_ARS
    seed_ARS = IEOR(seed_ARS,ISHFT(seed_ARS,13))
    seed_ARS = IEOR(seed_ARS,ISHFT(seed_ARS,-17))
    seed_ARS = IEOR(seed_ARS,ISHFT(seed_ARS,5))
    shr3_ARS = zigjz_ARS + seed_ARS
  END FUNCTION shr3_ARS
  
  INTEGER(4) FUNCTION KISS_ARS ()
    ! The  KISS (Keep It Simple Stupid) random number generator. Combines:
    ! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
    ! (2) A 3-shift shift-register generator, period 2^32-1,
    ! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
    !  Overall period>2^123;
    kiss1_ARS = 69069 * kiss1_ARS + 1327217885
    kiss2_ARS = m (m (m (kiss2_ARS, 13), - 17), 5)
    kiss3_ARS = 18000 * iand (kiss3_ARS, 65535) + ishft (kiss3_ARS, - 16)
    kiss4_ARS = 30903 * iand (kiss4_ARS, 65535) + ishft (kiss4_ARS, - 16)
    kiss_ARS = kiss1_ARS + kiss2_ARS + ishft (kiss3_ARS, 16) + kiss4_ARS
  CONTAINS
    INTEGER FUNCTION m(k, n)
      INTEGER :: k, n
      m = ieor (k, ishft (k, n) )
    END FUNCTION m
  END FUNCTION KISS_ARS

  REAL(8) FUNCTION Sample_Uniform_ARS(lo,hi)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: lo,hi
    REAL(8) :: u
    u = (DBLE(KISS_ARS())+2147483649.0d0)/4294967297.0d0
    Sample_Uniform_ARS = lo + u*(hi-lo)
  END FUNCTION Sample_Uniform_ARS

END MODULE

