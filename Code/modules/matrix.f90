! include 'mkl.fi'
! include 'lapack.f90'
MODULE MATRIX
   use iso_c_binding, only: c_int, c_double, c_float
  USE NRUTIL, ONLY : assert_eq,nrerror,MMUL,DOTP
  USE LAPACK95, ONLY : GESDD,GELS,SYTRF,SYTRI,POTRF,GETRF,GETRI,GELSY,POTRI,GELSD
  USE BLAS95, ONLY : GER
  IMPLICIT NONE

  INTERFACE Outer_Product
     MODULE PROCEDURE Outer_Product_r,Outer_Product_i
  END INTERFACE

  INTERFACE OLS
     MODULE PROCEDURE OLS_1,OLS_2,OLS_JIN,OLS_c
  END INTERFACE

!  PRIVATE :: Jacobi

CONTAINS

!  FUNCTION Outer_Product_r(a,b)
!    REAL(8), DIMENSION(:), INTENT(IN) :: a,b
!    REAL(8), DIMENSION(size(a),size(b)) :: Outer_Product_r
!    Outer_Product_r = spread(a,dim=2,ncopies=size(b)) * &
!         spread(b,dim=1,ncopies=size(a))
!  END FUNCTION Outer_Product_r

  FUNCTION Outer_Product_i(a,b)
    INTEGER, DIMENSION(:), INTENT(IN) :: a,b
    INTEGER, DIMENSION(size(a),size(b)) :: Outer_Product_i
    Outer_Product_i = spread(a,dim=2,ncopies=size(b)) * &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION Outer_Product_i

  FUNCTION Outer_Product_r(a,b)
    REAL(8), DIMENSION(:), INTENT(IN) :: a,b
    REAL(8), DIMENSION(size(a),size(b)) :: Outer_Product_r
    Outer_product_r=0.0d0
    CALL GER(Outer_Product_r,a,b)
  END FUNCTION Outer_Product_r

  FUNCTION Matrix_Inverse(ain)
    ! Inverts a square matrix by LU decomposition
    REAL(8), INTENT(IN) :: ain(:,:)
    REAL(8) :: Matrix_Inverse(SIZE(ain,1),SIZE(ain,2))
    INTEGER :: IPIV(SIZE(ain,1))
    Matrix_Inverse=ain
    CALL GETRF(Matrix_Inverse,IPIV)
    CALL GETRI(Matrix_Inverse,IPIV)
    ! Matrix_Inverse=Matrix_Inverse
    ! FORALL(i=1:n,j=1:n,i>j) Matrix_Inverse(i,j)=Matrix_Inverse(j,i)
  END FUNCTION Matrix_Inverse

  FUNCTION Matrix_Inverse_symmetric(ain)
    ! Inverts a symmetric matrix
    REAL(8), INTENT(IN) :: ain(:,:)
    REAL(8) :: Matrix_Inverse_symmetric(SIZE(ain,1),SIZE(ain,2))
    INTEGER :: IPIV(SIZE(ain,1)),i,j,n
    Matrix_Inverse_symmetric=ain
    n=SIZE(ain,1)
    CALL SYTRF(Matrix_Inverse_symmetric,'U',IPIV)
    CALL SYTRI(Matrix_Inverse_symmetric,IPIV)
    FORALL(i=1:n,j=1:n,i>j) Matrix_Inverse_symmetric(i,j)=Matrix_Inverse_symmetric(j,i)
  END FUNCTION Matrix_Inverse_symmetric

  FUNCTION Cholesky(AIN)
    IMPLICIT NONE
    REAL(8), DIMENSION(:,:), INTENT(IN) :: AIN
    REAL(8) :: A(SIZE(AIN,1),SIZE(AIN,2)),Cholesky(SIZE(AIN,1),SIZE(AIN,2))
    INTEGER :: i,j,n
    n=SIZE(ain,1)
    A=AIN
    CALL POTRF(A,'L')
    Cholesky=0.0d0
    FORALL(i=1:n,j=1:n,j<=i) Cholesky(i,j)=A(i,j)
  END FUNCTION Cholesky

  pure FUNCTION matrix_inverse_Cholesky(AIN) result(A)
   IMPLICIT NONE
   REAL(c_double), DIMENSION(:,:), INTENT(IN) :: AIN
   REAL(c_double) :: A(SIZE(AIN,1),SIZE(AIN,2))!,matrix_inverse_Cholesky(SIZE(AIN,1),SIZE(AIN,2))
   INTEGER(c_int) :: info,j,n
   n=SIZE(ain,1)
   A=AIN
   CALL POTRF(A,info=info)
   if (info.ne.0) error stop "Error inverting the matrix"
   call POTRI(A,info=info)
   if (info.ne.0) error stop "Error inverting the matrix"
   ! matrix_inverse_Cholesky=A
   ! FORALL(i=1:n,j=1:n,j<=i) matrix_inverse_Cholesky(i,j)=A(i,j)
 END FUNCTION matrix_inverse_Cholesky


  FUNCTION Determinant(B)
    ! Subroutine for evaluating the determinant of a matrix using
    ! the partial-pivoting Gaussian elimination scheme.
    IMPLICIT NONE
    REAL(8), INTENT (IN) :: B(:,:)
    INTEGER :: I,J,MSGN,N,INDX(SIZE(B,1))
    REAL(8) :: Determinant,A(SIZE(B,1),SIZE(B,1)),D
    A=B
    N=SIZE(B,1)
    CALL ELGS(A,N,INDX)
    D = 1.0
    DO I = 1, N
       D = D*A(INDX(I),I)
    END DO
    MSGN = 1
    DO I = 1, N
       DO WHILE (I.NE.INDX(I))
          MSGN = -MSGN
          J = INDX(I)
          INDX(I) = INDX(J)
          INDX(J) = J
       END DO
    END DO
    Determinant = MSGN*D
  CONTAINS
    SUBROUTINE ELGS (A,N,INDX)
      ! Subroutine to perform the partial-pivoting Gaussian elimination.
      ! A(N,N) is the original matrix in the input and transformed matrix
      ! plus the pivoting element ratios below the diagonal in the output.
      ! INDX(N) records the pivoting order.  Copyright (c) Tao Pang 2001.
      IMPLICIT NONE
      INTEGER, INTENT (IN) :: N
      INTEGER :: I,J,K,ITMP
      INTEGER, INTENT (OUT), DIMENSION (N) :: INDX
      REAL(8) :: C1,PI,PI1,PJ
      REAL(8), INTENT (INOUT), DIMENSION (N,N) :: A
      REAL(8), DIMENSION (N) :: C
      ! Initialize the index
      DO I = 1, N
         INDX(I) = I
      END DO
      ! Find the rescaling factors, one from each row
      DO I = 1, N
         C1= 0.0
         DO J = 1, N
            C1 = DMAX1(C1,ABS(A(I,J)))
         END DO
         C(I) = C1
      END DO
      ! Search the pivoting (largest) element from each column
      DO J = 1, N-1
         PI1 = 0.0
         DO I = J, N
            PI = ABS(A(INDX(I),J))/C(INDX(I))
            IF (PI.GT.PI1) THEN
               PI1 = PI
               K   = I
            ENDIF
         END DO
         ! Interchange the rows via INDX(N) to record pivoting order
         ITMP    = INDX(J)
         INDX(J) = INDX(K)
         INDX(K) = ITMP
         DO I = J+1, N
            PJ  = A(INDX(I),J)/A(INDX(J),J)
            ! Record pivoting ratios below the diagonal
            A(INDX(I),J) = PJ
            ! Modify other elements accordingly
            DO K = J+1, N
               A(INDX(I),K) = A(INDX(I),K)-PJ*A(INDX(J),K)
            END DO
         END DO
      END DO
    END SUBROUTINE ELGS
  END FUNCTION Determinant

   pure FUNCTION OLS_1(Xin,Yin)
      IMPLICIT NONE
      REAL(c_double), INTENT(IN) :: Xin(:,:),Yin(:)
      REAL(c_double) :: X(SIZE(Xin,1),SIZE(Xin,2)+1),Y(SIZE(Yin),1)
      REAL(c_double) :: OLS_1(SIZE(Xin,2)+1)
      INTEGER(c_int) :: info, r, work(1), lwork
      
      X(:,1) = 1.0d0  ! Add column of 1s
      ! ptr => Xin
      X(:,2:) = Xin ! Copy Xin to X starting from column 2
      Y(:,1)=Yin
      ! print*, "Size X:", shape(X), "; Size Y:", shape(Y)
      ! if (size(X,1)>10000) then
      !    ! print*, "calling GELSD"

      !    call gelsd(X,Y,rank=r,info=info)
      !    ! lwork=-1
      !    ! call dgetsls('N', size(X,1), size(X,2), 1, X, size(X,1), Y, size(Y,1), work, lwork, info)
      !    ! print*, "lwork", info, lwork, work(1)
      !    ! if (info==0) then
      !    !    lwork=work(1)
      !    ! else
      !    !    lwork=(size(X,2)+size(X,1))*50
      !    ! end if
      !    ! lwork=(size(X,2)+size(X,1))*50
      !    ! call dgetsls('N', size(X,1), size(X,2), 1, X, size(X,1), Y, size(Y,1), work, lwork, info)
      !    ! if (info==0) write(*,*) "Solved by GELSD",r
      ! else 
      !    ! print*, "calling GELS in OLS"
      !    CALL GELS(X,Y,info=info)
      !    ! if (info==0) write(*,*) "Solved by GELS"!, size(Y(1:(SIZE(Xin,2)+1),1)), size(OLS_1)
      ! end if
      ! CALL GELS(X,Y,info=info)
      call GELSY(X,Y,rank=r,info=info)
      ! print*, "info", info
      IF (info == 0) THEN
         OLS_1(:)=Y(1:(SIZE(Xin,2)+1),1)
         ! write(*,*) "gamma_hat", OLS_1
      ELSE
         ! WRITE(*,*) "Error: Unable to solve by GELS calling GELSY"!, info
         X(:,1) = 1.0d0  ! Add column of 1s
         X(:,2:) = Xin ! Copy Xin to X starting from column 2
         Y(:,1)=Yin
         info=0
         ! call GELS(X,Y,info=info)
         call GELSD(X,Y,rank=r,info=info)
         if (info == 0) then
            ! WRITE(*,*) "DONE: solve by GELSY"
            ! write(*,*) "gamma_hat", Y(1:(SIZE(Xin,2)+1),1)
            OLS_1(:)=Y(1:(SIZE(Xin,2)+1),1)
         else
            OLS_1=0.0d0
            error stop "OLS Error: Unable to solve"
         endif
      END IF
   END FUNCTION OLS_1

   pure FUNCTION OLS_c(Yin)
   IMPLICIT NONE
   REAL(c_double), INTENT(IN) :: Yin(:)
   REAL(c_double) :: X(SIZE(Yin,1),1),Y(SIZE(Yin),1)
   REAL(c_double) :: OLS_c
   INTEGER(c_int) :: info, r
   
   X(:,1) = 1.0d0  ! Add column of 1s
   Y(:,1)=Yin

   call GELSY(X,Y,rank=r,info=info)
   ! print*, "info", info
   IF (info == 0) THEN
      OLS_c=Y(1,1)
      ! write(*,*) "gamma_hat", OLS_1
   ELSE
      ! WRITE(*,*) "Error: Unable to solve by GELS calling GELSY"!, info
      X(:,1) = 1.0d0  ! Add column of 1s
      ! X(:,2:) = Xin ! Copy Xin to X starting from column 2
      Y(:,1)=Yin
      info=0
      ! call GELS(X,Y,info=info)
      call GELSD(X,Y,rank=r,info=info)
      if (info == 0) then
         ! WRITE(*,*) "DONE: solve by GELSY"
         ! write(*,*) "gamma_hat", Y(1:(SIZE(Xin,2)+1),1)
         OLS_c=Y(1,1)
      else
         OLS_c=0.0d0
         error stop "OLS Error: Unable to solve"
      endif
   END IF
END FUNCTION OLS_c

   FUNCTION OLS_2(Xin,Yin,ind,n)
      ! This function performs ordinary least squares regression using a subset of data.
      ! Xin: Input matrix of independent variables
      ! Yin: Input vector of dependent variable
      ! ind: Logical array indicating which data points to include in the regression
      ! n: Number of data points
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: Xin(:,:),Yin(:)
      LOGICAL, INTENT(IN) :: ind(:)
      INTEGER, INTENT(IN) :: n
      REAL(8) :: X(n,SIZE(Xin,2)),Y(n,1)
      REAL(8) :: OLS_2(SIZE(Xin,2))
      INTEGER :: j
      DO j=1,SIZE(Xin,2)
            X(:,j)=PACK(Xin(:,j),ind)
      END DO
      Y(:,1)=PACK(Yin,ind)
      CALL GELS(X,Y)
      OLS_2=Y(1:SIZE(Xin,2),1)
   END FUNCTION OLS_2

   FUNCTION OLS_JIN(Xin,Yin,Xc,Yc)
      ! This function performs ordinary least squares regression using a subset of data based on given thresholds.
      ! Xin: Input matrix of independent variables
      ! Yin: Input vector of dependent variable
      ! Xc: Threshold values for each independent variable
      ! Yc: Threshold value for the dependent variable
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: Xin(:,:),Yin(:)
      REAL(8), INTENT(IN) :: Xc(:),Yc
      REAL(8), POINTER :: X(:,:),Y(:,:)
      REAL(8) :: OLS_JIN(SIZE(Xin,2))
      INTEGER :: Xlog(SIZE(Xin,1),SIZE(Xin,2)),Ylog(SIZE(Xin,1)),obs_vec(SIZE(Xin,1))
      INTEGER :: j,n,k
      k=SIZE(Xin,2)
      Xlog=0
      Ylog=0
      WHERE (Yin>Yc) Ylog=1
      obs_vec=Ylog
      DO j=1,k
         WHERE (Xin(:,j)>Xc(j)) Xlog(:,j)=1
         obs_vec=obs_vec*Xlog(:,j)
      END DO
      n=SUM(obs_vec)
      ALLOCATE(X(n,k),Y(n,1))
      DO j=1,k
            X(:,j)=PACK(Xin(:,j),obs_vec==1)
      END DO
      Y(:,1)=PACK(Yin,obs_vec==1)
      CALL GELS(X,Y)
      OLS_JIN=Y(1:k,1)
      DEALLOCATE(X,Y)
   END FUNCTION OLS_JIN

  pure FUNCTION SVD(Xin)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: Xin(:,:)
    REAL(8) :: X(SIZE(Xin,1),SIZE(Xin,2))
    REAL(8) :: SVD(MIN(SIZE(Xin,1),SIZE(Xin,2)))
    X=Xin
    CALL GESDD(X,SVD)
  END FUNCTION SVD

  pure INTEGER FUNCTION RANK(Ain)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: Ain(:,:)
    REAL(8) :: A(SIZE(Ain,1),SIZE(Ain,2))
    REAL(8) :: S(MIN(SIZE(Ain,1),SIZE(Ain,2))),rc
    REAL(8), PARAMETER :: cond=1.0d-16
    INTEGER :: m
    A=Ain
    S=SVD(A)
    m=MAX(SIZE(Ain,1),SIZE(Ain,2))
    rc=S(1)*cond*DBLE(m)
    RANK=COUNT(S.GT.rc)
  END FUNCTION RANK

!!$  INTEGER FUNCTION Rank(A)
!!$    ! RETURNS THE RANK OF A MATRIX
!!$    IMPLICIT NONE
!!$    REAL(8), INTENT(IN) :: A(:,:)
!!$    REAL(8) :: AA(SIZE(A,2),SIZE(A,2)),EVAL(SIZE(A,2)),TOL,EVEC(SIZE(A,2),SIZE(A,2))
!!$    INTEGER :: N,nrot
!!$    TOL=1.0d-6
!!$    N = SIZE(A,2)
!!$    AA = MATMUL(TRANSPOSE(A),A)
!!$    CALL JACOBI(AA,EVAL,EVEC,nrot)
!!$    RANK = COUNT(EVAL>TOL)
!!$  END FUNCTION Rank

  SUBROUTINE Colinear(X,IND,N)
    ! CHECKS FOR LINEAR INDEPENDENCE IN A MATRIX
    ! IT RETURNS THE INDEX OF N LINEARLY INDEPENDENT COLUMNS OF THE MATRIX IN IND(1:N)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: X(:,:)
    INTEGER, INTENT(OUT) :: IND(SIZE(X,2)),N
    INTEGER :: h,NR,NC,IN(SIZE(X,2)),R,R1,NC1,RS
    NR = SIZE(X,1)
    NC = SIZE(X,2)
    N = RANK(X)
    DO h = 1, NC
       IN(h) = h
    END DO
    IF (N==NC) THEN
       IND = IN
    ELSE
       RS = N
       NC1=0
       DO h = 1, NC
          R = NC - h
          R1 = RANK(X(:,IN(h+1:NC)))
          IF ((R1<RS).and.(R1<R)) THEN
             NC1 = NC1 + 1
             IND(NC1) = h
             RS=RS-1
          ELSEIF ((R1.ge.RS).and.(R1<R)) THEN
             RS=RS
          ELSE
             IND(NC1+1:N) = IN(h+1:NC)
             EXIT
          END IF
       END DO
    END IF
  END SUBROUTINE Colinear

  SUBROUTINE Perfect_Predict_Probit(D,X,CON,IND,N)
    ! CHECKS FOR VARIABLES THAT PREDICT PERFECTLY IN A PROBIT MODEL
    ! IT RETURNS THE INDEX OF N COLUMNS OF THE MATRIX IN IND(1:N) MINUS THE ONES THAT PREDICT PERFECTLY
    ! CON IS THE LOCATION OF THE CONSTANT. IF NO CONSTANT MAKE CON=0
    INTEGER, INTENT(IN) :: CON
    REAL(8), INTENT(IN) :: X(:,:),D(:)
    INTEGER, INTENT(OUT) :: IND(SIZE(X,2)),N
    INTEGER :: c,NR,NC,N_DUMMY,SUMA,CHECK(SIZE(X,1))
    NR = SIZE(X,1)
    NC = SIZE(X,2)
    N=0
    DO c = 1, NC
       IF (c==CON) THEN
          N = N + 1
          IND(N) = c
       ELSE
          CHECK=0
          WHERE (X(:,c)==0.0d0) CHECK=1
          WHERE (X(:,c)==1.0d0) CHECK=1
          IF (SUM(CHECK)==NR) THEN
             N_DUMMY = COUNT(X(:,c)==0.0d0)
             SUMA = INT(SUM(D,X(:,c)==0.0d0))
             IF ((.NOT.SUMA==0) .AND. (.NOT.SUMA==N_DUMMY)) THEN
                N_DUMMY = COUNT(X(:,c)==1.0d0)
                SUMA = INT(SUM(D,X(:,c)==1.0d0))
                IF ((.NOT.SUMA==0) .AND. (.NOT.SUMA==N_DUMMY)) THEN
                   N = N + 1
                   IND(N) = c
                END IF
             END IF
          ELSE
             N = N + 1
             IND(N) = c
          END IF
       END IF
    END DO
  END SUBROUTINE Perfect_Predict_Probit

  SUBROUTINE Sort(Xin,xout,ind)
    IMPLICIT NONE
    !  Sorts xout into ascENDing order - Quicksort
    REAL (8), DIMENSION (:), INTENT (in) :: Xin
    INTEGER, INTENT(INOUT) :: ind(:)
    REAL (8), DIMENSION (SIZE(ind)), INTENT (Out) :: xout
    xout=XIN
    Call SUBSOR (xout, 1, Size (xout),ind)
    Call INSSORT (xout,ind)
  CONTAINS
    RECURSIVE SUBROUTINE SUBSOR (xout, IDEB1, IFIN1,ind)
      IMPLICIT NONE
      !  Sorts xout from IDEB1 to IFIN1
      REAL(8), DIMENSION (:), INTENT (InOut) :: xout
      INTEGER, INTENT (In) :: IDEB1, IFIN1
      INTEGER, INTENT(INOUT) :: ind(:)
      INTEGER, PARAMETER :: NINS = 16 ! Max for insertion sort
      INTEGER :: ICRS, IDEB, IDCR, IFIN, IMIL,IWRK,IPIV
      REAL(8) :: XPIV, XWRK
      IDEB = IDEB1
      IFIN = IFIN1
      !  IF we DOn't have enough values to make it worth while, we leave
      !  them unsorted, and the final insertion sort will take care of them
      IF ((IFIN - IDEB) > NINS) THEN
         IMIL = (IDEB+IFIN) / 2
         !  One chooses a pivot, median of 1st, last, and middle values
         IF (xout(IMIL) < xout(IDEB)) THEN
            XWRK = xout (IDEB)
            IWRK = IND (IDEB)
            xout (IDEB) = xout (IMIL)
            IND (IDEB) = IND (IMIL)
            xout (IMIL) = XWRK
            IND (IMIL) = IWRK
         END IF
         IF (xout(IMIL) > xout(IFIN)) Then
            XWRK = xout (IFIN)
            IWRK = IND(IFIN)
            xout (IFIN) = xout (IMIL)
            IND (IFIN) = IND (IMIL)
            xout (IMIL) = XWRK
            IND (IMIL) = IWRK
            IF (xout(IMIL) < xout(IDEB)) Then
               XWRK = xout (IDEB)
               IWRK = IND (IDEB)
               xout (IDEB) = xout (IMIL)
               IND (IDEB) = IND (IMIL)
               xout (IMIL) = XWRK
               IND (IMIL) = IWRK
            END IF
         END IF
         XPIV = xout (IMIL)
         IPIV = IND (IMIL)
         !  One exchanges values to put those > pivot in the END and
         !  those <= pivot at the beginning
         ICRS = IDEB
         IDCR = IFIN
         ECH2: DO
            DO
               ICRS = ICRS + 1
               IF (ICRS >= IDCR) Then
                  !  the first  >  pivot is IDCR
                  !  the last   <= pivot is ICRS-1
                  !  Note: IF one arrives here on the first iteration, then
                  !  the pivot is the maximum of the set, the last value is equal
                  !  to it, and one can reduce by one the size of the set to process,
                  !  as IF xout (IFIN) > XPIV
                  Exit ECH2
               END IF
               IF (xout(ICRS) > XPIV) Exit
            END DO
            DO
               IF (xout(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               IF (ICRS >= IDCR) Then
                  !  The last value < pivot is always ICRS-1
                  Exit ECH2
               END IF
            END DO
            XWRK = xout (IDCR)
            IWRK = IND (IDCR)
            xout (IDCR) = xout (ICRS)
            IND (IDCR) = IND (ICRS)
            xout (ICRS) = XWRK
            IND (ICRS) = IWRK
         END DO ECH2
         !  One now sorts each of the two sub-intervals
         Call SUBSOR (xout, IDEB1, ICRS-1,IND)
         Call SUBSOR (xout, IDCR, IFIN1,IND)
      END IF
      RETURN
    END SUBROUTINE SUBSOR

    SUBROUTINE INSSORT (xout,IND)
      IMPLICIT NONE
      !  Sorts xout into increasing order (Insertion sort)
      REAL(8), DIMENSION (:), INTENT (InOut) :: xout
      INTEGER, INTENT(INOUT) :: ind(:)
      INTEGER :: ICRS, IDCR,IWRK
      REAL(8) :: XWRK
      DO ICRS = 2, Size (xout)
         XWRK = xout (ICRS)
         IWRK = IND (ICRS)
         IF (XWRK >= xout(ICRS-1)) Cycle
         xout (ICRS) = xout (ICRS-1)
         IND (ICRS) = IND (ICRS-1)
         DO IDCR = ICRS - 2, 1, - 1
            IF (XWRK >= xout(IDCR)) Exit
            xout (IDCR+1) = xout (IDCR)
            IND (IDCR+1) = IND (IDCR)
         END DO
         xout (IDCR+1) = XWRK
         IND (IDCR+1) = IWRK
      END DO
      RETURN
    END SUBROUTINE INSSORT

  END SUBROUTINE Sort

!!$  SUBROUTINE jacobi(ain,d,v,nrot)
!!$    IMPLICIT NONE
!!$    REAL(8), DIMENSION(:,:), INTENT(IN) :: ain
!!$    REAL(8), INTENT(OUT) :: d(SIZE(AIN,1)),v(SIZE(AIN,1),SIZE(AIN,2))
!!$    INTEGER, INTENT(OUT) :: nrot
!!$    INTEGER :: i,j,ip,iq,n
!!$    REAL(8) :: c,g,h,s,sm,t,tau,theta,tresh,a(SIZE(AIN,1),SIZE(AIN,2))
!!$    REAL(8), DIMENSION(size(AIN,1)) :: b,z
!!$    LOGICAL :: upper(SIZE(AIN,1),SIZE(AIN,2))
!!$    n=assert_eq(size(AIN,1),size(AIN,2),'The routine only works for square symmetric matrices: jacobi')
!!$    A=AIN
!!$    v=0.0d0
!!$    upper=.FALSE.
!!$    FORALL(i=1:n,j=1:n,i<j) upper(i,j)=.TRUE.
!!$    FORALL(i=1:n)
!!$       v(i,i)=1.0D0
!!$       b(i)=A(i,i)
!!$    END FORALL
!!$    d=b
!!$    z=0.0d0
!!$    nrot=0
!!$    DO i=1,50
!!$       sm=SUM(ABS(a),upper)
!!$       IF (sm == 0.0d0) RETURN
!!$       tresh=MERGE(0.2d0*sm/n**2,0.0d0, i < 4 )
!!$       DO ip=1,n-1
!!$          DO iq=ip+1,n
!!$             g=100.0d0*abs(a(ip,iq))
!!$             IF ((i > 4) .and. (abs(d(ip))+g == abs(d(ip))).and.(abs(d(iq))+g == abs(d(iq)))) THEN
!!$                a(ip,iq)=0.0
!!$             ELSEIF (abs(a(ip,iq)) > tresh) THEN
!!$                h=d(iq)-d(ip)
!!$                IF (abs(h)+g == abs(h)) THEN
!!$                   t=a(ip,iq)/h
!!$                ELSE
!!$                   theta=0.5d0*h/a(ip,iq)
!!$                   t=1.0d0/(abs(theta)+sqrt(1.0d0+theta**2))
!!$                   IF (theta < 0.0) t=-t
!!$                END IF
!!$                c=1.0d0/dsqrt(1+t**2)
!!$                s=t*c
!!$                tau=s/(1.0d0+c)
!!$                h=t*a(ip,iq)
!!$                z(ip)=z(ip)-h
!!$                z(iq)=z(iq)+h
!!$                d(ip)=d(ip)-h
!!$                d(iq)=d(iq)+h
!!$                a(ip,iq)=0.0
!!$                CALL jrotate(a(1:ip-1,ip),a(1:ip-1,iq))
!!$                CALL jrotate(a(ip,ip+1:iq-1),a(ip+1:iq-1,iq))
!!$                CALL jrotate(a(ip,iq+1:n),a(iq,iq+1:n))
!!$                CALL jrotate(v(:,ip),v(:,iq))
!!$                nrot=nrot+1
!!$             END IF
!!$          END DO
!!$       END DO
!!$       b=b+z
!!$       d=b
!!$       z=0.0
!!$    END DO
!!$    CALL NRERROR('too many iterations in jacobi')
!!$  CONTAINS
!!$    SUBROUTINE jrotate(a1,a2)
!!$      REAL(8), DIMENSION(:), INTENT(INOUT) :: a1,a2
!!$      REAL(8), DIMENSION(size(a1)) :: wk1
!!$      wk1=a1
!!$      a1=a1-s*(a2+a1*tau)
!!$      a2=a2+s*(wk1-a2*tau)
!!$    END SUBROUTINE jrotate
!!$  END SUBROUTINE jacobi

END MODULE MATRIX
