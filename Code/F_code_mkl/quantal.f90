MODULE QUANTAL
  USE NRUTIL, ONLY : MMUL,DOTP
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: PROBIT
CONTAINS
  
  SUBROUTINE PROBIT(ddd,xxx,parameters,var,marginal,phat,miss)
    ! Uses other subroutines. D(n) is the observation indicator,
    ! X(n,k) is covariates (no constant is added, add your own)
    ! parameters is the estimated beta, it's standard error and t coefficient
    ! var is the variance covariance matrix of parameters
    ! marginal is the average marginal effect (average derivative, not derivative at average)
    ! phat is the predicted probability, it takes the value of -1 if it shoud be missing
    ! miss is a logical=true if phat=-1 (i.e missing) 
    USE MATRIX
    USE PROBABILITY
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: ddd(:),xxx(:,:)
    REAL(8), INTENT(OUT) :: parameters(size(xxx,2),3),var(size(xxx,2),size(xxx,2))
    REAL(8), INTENT(OUT) :: phat(size(ddd)),marginal(size(xxx,2))
    REAL(8), POINTER :: b0(:),gradient(:),hessian(:,:),var2(:,:),inc(:),me(:), &
         vme(:,:),db(:,:),iii(:),x1(:),x0(:),xhat(:,:),ddb(:)
    REAL(8), ALLOCATABLE :: vec(:),x(:,:),d(:),arg(:),xx(:,:)
    INTEGER, ALLOCATABLE :: ivec(:),indkk(:),dummykk(:),ind(:),dummy(:)
    REAL(8) :: cdf,pdf,factor,tol,con,w,ww,arg1,pdf1,pdf0,dd(size(ddd)),step,term1,term2
    REAL(8) :: value
    INTEGER :: j,nn,k,kk,n,i,h,s,ss,obs
    LOGICAL :: miss(size(ddd)),miss2(size(ddd))
    n = size(ddd)
    obs=n
    kk = size(xxx,2)
    ALLOCATE(dummykk(kk),indkk(kk),xx(n,kk))
    miss=.FALSE.
    xx = xxx
    dd = ddd
    tol = 0.000001d0
    k=0
    DO i = 1, kk
       miss2=.FALSE.
       ALLOCATE(vec(n),ivec(n))
       ivec=(/1:n:1/)
       vec=xx(1:n,i)
       IF (MEAN(vec,vec.ne.0.0d0).ne.1.0d0) THEN
          k=k+1
          indkk(k)=i	
          dummykk(k)=0
       ELSE
          w=MEAN(dd,vec.ne.0.0d0)
          ww=MEAN(dd,(1.0-vec).ne.0.0d0)
          IF (w==1) THEN
             miss2=xxx(:,i).ne.0.0d0
             WHERE ((miss==.false.).and.(miss2==.true.)) miss=.true.
             nn=COUNT(vec.ne.0.0d0)
             WRITE(*,'(A,I8,A,I10,A)') 'Variable',i,' predicts success perfectly. Dropping it plus ',nn,' observations'
             CALL SORT(vec,vec,ivec)
             xx(1:n,:)=xx(ivec,:)
             dd(1:n)=dd(ivec)
             n=n-nn
          ELSE IF (w==0) THEN
             miss2=xxx(:,i).ne.0.0
             WHERE ((miss==.false.).and.(miss2==.true.)) miss=.true.
             nn=COUNT(vec.ne.0.0)
             WRITE(*,'(A,I10,A,I10,A)') 'Variable',i,' predicts failure perfectly. Dropping it plus ',nn,' observations'
             CALL SORT(vec,vec,ivec)
             xx(1:n,:)=xx(ivec,:)
             dd(1:n)=dd(ivec)
             n=n-nn
          ELSE IF (ww==1) THEN
             miss2=(1.0-xxx(:,i)).ne.0.0d0	
             WHERE ((miss==.false.).and.(miss2==.true.)) miss=.true.
             nn=COUNT((1.0d0-vec).ne.0.0d0)
             WRITE(*,'(A,I10,A,I10,A)') 'Variable',i,' predicts success perfectly. Dropping it plus ',nn,' observations'
             CALL SORT(-vec,vec,ivec)
             xx(1:n,:)=xx(ivec,:)
             dd(1:n)=dd(ivec)
             n=n-nn
          ELSE IF (ww==0) THEN
             miss2=(1.0-xxx(:,i)).ne.0.0d0
             WHERE ((miss==.false.).and.(miss2==.true.)) miss=.true.
             nn=COUNT((1.0-vec).ne.0.0)
             WRITE(*,'(A,I10,A,I10,A)') 'Variable',i,' predicts failure perfectly. Dropping it plus ',nn,' observations'
             CALL SORT(-vec,vec,ivec)
             xx(1:n,:)=xx(ivec,:)
             dd(1:n)=dd(ivec)
             n=n-nn
          ELSE
             k=k+1
             indkk(k)=i	
             dummykk(k)=1
          END IF
       END IF
       DEALLOCATE(vec,ivec)
    END DO
    ALLOCATE(x(n,k),ind(k),dummy(k),xhat(obs,k))
    dummy=dummykk(1:k)
    ind=indkk(1:k)
    xhat=xx(:,ind)
    x=xx(1:n,ind)
    DEALLOCATE(indkk,dummykk,xx)
    ALLOCATE(indkk(k),dummykk(k),xx(n,k))
    dummykk=dummy
    xx=x
    CALL COLINEAR(X,indkk,k)
    DEALLOCATE(ind,dummy,x)
    ALLOCATE(ind(k),dummy(k),b0(k),arg(n),gradient(k),hessian(k,k),var2(k,k),x(n,k),d(n),inc(n),ddb(k))
    ind=indkk(1:k)
    dummy=dummykk(ind)
    x=xx(:,ind)
    d=dd(1:n)
    WRITE(*,'(A)') 'I kept after eliminating collinear variables: '
    DO i = 1, k
       WRITE(*,'(A,I10)') 'Variable',i
       IF (MEAN(x(:,i))==1.0d0) THEN
          ss=i
          dummy(i)=0
       END IF
    END DO
    con=1.0
    b0 = OLS(x,d)			! use OLS coefficents for starting values
    DO WHILE (con>tol)
       arg = MMUL(x,b0)
       gradient = 0.0		! clear before next iteration
       hessian = 0.0		! clear before next iteration	
       var2 = 0.0			! clear before next iteration
       value = 0.0			! clear before next iteration
       DO j = 1, n
          cdf = CDF_NORMAL(arg(j),0.0d0,1.0d0)
          IF (cdf<=0.0) THEN
             cdf=0.000001
          ELSE IF (cdf>=1.0) THEN
             cdf=0.999999
          END IF
          pdf = PDF_NORMAL(arg(j),0.0d0,1.0d0)
          factor = (d(j)-cdf)*pdf/(cdf*(1-cdf)) 
          gradient = gradient + factor*x(j,:)
          hessian = hessian - (factor*arg(j) + factor**2)*OUTER_PRODUCT(x(j,:),x(j,:))
          value = value + LOG((d(j)*cdf) + ((1-d(j))*(1-cdf)))
          var2 = var2 + OUTER_PRODUCT(factor*x(j,:),factor*x(j,:))
       END DO
       hessian = -hessian
       hessian = MATRIX_INVERSE(hessian)
       ! Stepsize determination
       step = 2.0d0
       term1 = 0.0d0
       term2 = 1.0d0
       ddb=MMUL(hessian,gradient)
       DO WHILE (term2 > term1)
          step = step/2.0d0
          term1 = prlike(b0+step*ddb,d,x)
          term2 = prlike(b0+step*ddb/2.0d0,d,x)
       END DO
       b0 = b0 + step*MMUL(hessian,gradient)
       con = DOTP(gradient,MMUL(hessian,gradient))
    END DO
    WRITE(*,'(A,I10,A)') 'I only used ',n,' observations'
    ALLOCATE(me(k),vme(k,k),db(k,k),iii(k),x1(k),x0(k))
    parameters=0.0
    parameters(ind,1) = b0
    var(ind,ind) = hessian
    parameters(ind,2:3) = REGSTAT(var(ind,ind),b0)
    
    arg = MMUL(x,b0)
    me=0.0d0
    vme=0.0d0
    db=0.0d0
    DO h = 1, k
       iii=0.0d0
       iii(h)=1.0d0
       IF (dummy(h)==0.0d0) THEN
          DO j = 1, n
             pdf = PDF_NORMAL(arg(j),0.0d0,1.0d0)
             me(h)=me(h)+(pdf*b0(h))
             db(h,:)=db(h,:)+(((iii-arg(j)*b0(h)*x(j,:))*pdf)/n)
          END DO
       ELSE
          DO j = 1, n
             x1=x(j,:)
             x0=x(j,:)
             x1(h)=1.0d0
             x0(h)=0.0d0
             arg1=CDF_NORMAL(DOTP(x1,b0),0.0d0,1.0d0)
             arg1=arg1-PDF_NORMAL(DOTP(x0,b0),0.0d0,1.0d0)
             pdf1=PDF_NORMAL(DOTP(x1,b0),0.0d0,1.0d0)
             pdf0=PDF_NORMAL(DOTP(x0,b0),0.0d0,1.0d0)
             me(h)=me(h)+arg1
             db(h,:)=db(h,:)+((pdf1*x1-pdf0*x0)/n)
          END DO
       END IF
    END DO
    me=me/n
    vme=MMUL(MMUL(db,hessian),TRANSPOSE(db))
    marginal=0.0
    marginal(ind)=me
    IF (ss>0) marginal(ss)=0.0
    phat=MMUL(xhat(:,ind),b0)
    DO j = 1, size(ddd)
       phat(j) = CDF_NORMAL(phat(j),0.0d0,1.0d0)
       IF (miss(j)) phat(j)=-1.0d0
    END DO
    
  CONTAINS
    
    FUNCTION MEAN(a,b)
      IMPLICIT NONE
      REAL(8), DIMENSION(:), INTENT(IN) :: a
      LOGICAL, OPTIONAL :: b(SIZE(a))
      REAL(8) :: MEAN
      IF (PRESENT(b)) THEN
         MEAN = SUM(a,b)/COUNT(b)
      ELSE
         MEAN = SUM(a)/SIZE(a)
      END IF
    END FUNCTION MEAN
    
    FUNCTION REGSTAT(a,b)
      IMPLICIT NONE
      REAL(8), INTENT(in) :: a(:,:),b(:)
      REAL(8) :: REGSTAT(size(b),2),temp(size(b)),one
      INTEGER :: j
      one=1.0
      DO j = 1, size(a,2)
         REGSTAT(j,1) = SQRT(a(j,j))
         temp(j) = b(j)/REGSTAT(j,1)
         REGSTAT(j,2) = 1-CDF_NORMAL(ABS(temp(j)),0.0d0,1.0d0)+CDF_NORMAL(-ABS(temp(j)),0.0d0,1.0d0)
      END DO
    END FUNCTION REGSTAT
    
    FUNCTION PRLIKE(b,y,x)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: y(:),b(:),x(:,:)
      REAL(8) :: i,cdf,vec(SIZE(X,2))
      REAL(8) :: PRLIKE
      INTEGER :: j
      i = 1.0d0
      PRLIKE=0.0d0
      DO j = 1, SIZE(Y)
         vec=x(j,:)
         cdf = CDF_NORMAL(DOTP(vec,b),0.0d0,1.0d0)
         PRLIKE = PRLIKE + y(j)*log(cdf)+(i-y(j))*log(i-cdf)
      END DO
    END FUNCTION PRLIKE
    
  END SUBROUTINE PROBIT
END MODULE QUANTAL
