MODULE PARS
  USE NRUTIL, ONLY : ASSERT
  USE MPI
  IMPLICIT NONE
  PRIVATE 
  PUBLIC :: Parallel_Accelerated_Random_Search
  
  INTEGER(4), SAVE :: kiss1_pars=123456789,kiss2_pars=362436069,kiss3_pars=521288629,kiss4_pars=380116160,seed_pars,zigjz_pars
  LOGICAL, SAVE :: initialized=.FALSE.
CONTAINS
  
  SUBROUTINE Parallel_Accelerated_Random_Search(xm,c,bound,nexp,func,xm_init,id,numprocs,k1)
    ! Minimize a function func. It returns xm, the point at the minimum
    ! using accelerated random search. nexp is the number of radius expansions before it automatically
    ! restarts. bound contains the upper and lower bounds for each element of xm. c is a contraction parameter between (0,1)
    ! if xm_init==.TRUE. the program uses the values in xm as initial centering for the ball 
    ! k1 is an optional argument to set the seed of the generator, id is used for I/O in mpi    
    IMPLICIT NONE
    REAL(8), INTENT(INOUT) :: xm(:)
    REAL(8), INTENT(IN) :: c,bound(:,:)
    INTEGER, INTENT(IN) :: nexp,id,numprocs
    INTEGER, INTENT(IN), OPTIONAL :: k1(4)
    LOGICAL, INTENT(IN) :: xm_init
    INTERFACE
       REAL(8) FUNCTION FUNC(x)
         IMPLICIT NONE
         REAL(8), INTENT(IN) :: x(:)
       END FUNCTION FUNC
    END INTERFACE
    REAL(8) :: rho,fm,fx,fy,r,l,u,y(SIZE(xm)),x(SIZE(xm)),d(size(xm)),myx(SIZE(xm),numprocs),myfx(numprocs)
    INTEGER :: ne,j,sxm,ierr,h,ct
    LOGICAL :: used
    CALL ASSERT(c>0.0d0,c<1.0d0,"PARS contraction parameter should be in (0,1)")
    IF (.NOT.INITIALIZED) THEN
       IF (PRESENT(k1)) THEN
          CALL SET_SEED_PARS(id,k1(1),k1(2),k1(3),k1(4))
       ELSE
          CALL SET_SEED_PARS(id)
       END IF
       INITIALIZED=.TRUE.
    END IF
    sxm=SIZE(xm)
    rho=1.0d-12
    fm=1.0d300
    ne=-1
    r=1.0d0
    used=.FALSE.
    ct=0
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
                x(j)=Sample_Uniform_PARS(bound(j,1),bound(j,2))
             END DO
          END IF
          r=1.0d0
          ne=0
          fx=func(x)
       END IF
       DO j=1,sxm
          l=MAX(x(j)-r*d(j),bound(j,1))
          u=MIN(x(j)+r*d(j),bound(j,2))
          y(j)=Sample_Uniform_PARS(l,u)
       END DO
       fy=func(y)
       IF (fy<fx) THEN
          x=y
          fx=fy
          r=1.0d0
          IF (id==0) THEN
             WRITE(*,'(A,I3,A,I8,A,F16.8)') "Current minimum for cpu",id," after",ne," expansions:",fx
             OPEN(52312,file="point.out")
             DO j=1,sxm
                WRITE(52312,'(F32.16)') x(j)
             END DO
             CLOSE(52312)
          END IF
       ELSE
          r=r*c
       END IF
       IF (ct>10000) r=rho-1.0d0
       IF (r<rho) THEN
          WRITE(*,*) "cpu",id,"arrived and waiting",ct,"iterations"
          ct=0
          CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
          myx(:,id+1)=x
          myfx(id+1)=fx
          DO j=1,numprocs
             CALL MPI_BCAST(myx(:,j),sxm,MPI_REAL8,j-1,MPI_COMM_WORLD,ierr)
             CALL MPI_BCAST(myfx(j),1,MPI_REAL8,j-1,MPI_COMM_WORLD,ierr)
          END DO
          fm=myfx(1)
          h=1
          DO j=2,numprocs
             IF (myfx(j)<fm) THEN
                fm=myfx(j)
                h=j
             END IF
          END DO
          ! This is to allow more variation
!          IF (ne<nexp) THEN
!             IF (myfx(id+1)<fm*1.1d0) THEN
!                h=id+1
!                fm=myfx(id+1)
!             END IF
!          END IF
          ! It is not clear above should work is an experiment
          fx=fm
          x=myx(:,h)
          ne=ne+1
          r=1.0d0
       END IF
       IF (ne>=nexp) EXIT
       ct=ct+1
    END DO
    xm=x
  END SUBROUTINE Parallel_Accelerated_Random_Search

  SUBROUTINE Set_Seed_PARS(id,k1,k2,k3,k4)
    IMPLICIT NONE
    INTEGER(4), INTENT(IN), OPTIONAL :: k1,k2,k3,k4,id
    INTEGER :: i
    IF (PRESENT(k1)) THEN
       kiss1_PARS=k1+id
       kiss2_PARS=k2*id
       kiss3_PARS=k3-id
       kiss4_PARS=k4
    ELSE
       CALL SYSTEM_CLOCK(seed_PARS)
       seed_PARS=seed_PARS*(id+1) + id
       kiss1_PARS=SHR3_PARS()
       kiss2_PARS=SHR3_PARS()
       kiss3_PARS=SHR3_PARS()
       kiss4_PARS=SHR3_PARS()
    END IF
  END SUBROUTINE Set_Seed_PARS

  ! Generate random 32-bit integers
  INTEGER FUNCTION shr3_PARS( )
    zigjz_PARS = seed_PARS
    seed_PARS = IEOR(seed_PARS,ISHFT(seed_PARS,13))
    seed_PARS = IEOR(seed_PARS,ISHFT(seed_PARS,-17))
    seed_PARS = IEOR(seed_PARS,ISHFT(seed_PARS,5))
    shr3_PARS = zigjz_PARS + seed_PARS
  END FUNCTION shr3_PARS
  
  INTEGER(4) FUNCTION KISS_PARS ()
    ! The  KISS (Keep It Simple Stupid) random number generator. Combines:
    ! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
    ! (2) A 3-shift shift-register generator, period 2^32-1,
    ! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
    !  Overall period>2^123;
    kiss1_PARS = 69069 * kiss1_PARS + 1327217885
    kiss2_PARS = m (m (m (kiss2_PARS, 13), - 17), 5)
    kiss3_PARS = 18000 * iand (kiss3_PARS, 65535) + ishft (kiss3_PARS, - 16)
    kiss4_PARS = 30903 * iand (kiss4_PARS, 65535) + ishft (kiss4_PARS, - 16)
    kiss_PARS = kiss1_PARS + kiss2_PARS + ishft (kiss3_PARS, 16) + kiss4_PARS
  CONTAINS
    INTEGER FUNCTION m(k, n)
      INTEGER :: k, n
      m = ieor (k, ishft (k, n) )
    END FUNCTION m
  END FUNCTION KISS_PARS

  REAL(8) FUNCTION Sample_Uniform_PARS(lo,hi)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: lo,hi
    REAL(8) :: u
    u = (DBLE(KISS_PARS())+2147483649.0d0)/4294967297.0d0
    Sample_Uniform_PARS = lo + u*(hi-lo)
  END FUNCTION Sample_Uniform_PARS

END MODULE PARS

