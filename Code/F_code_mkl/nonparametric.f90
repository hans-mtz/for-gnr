MODULE NONPARAMETRIC
  IMPLICIT NONE
  
!!!! In all cases the gaussian kernel is true and epanechnikov is false. You want more, add your own
  
  PRIVATE gausskern,epankern
  
CONTAINS
  
  FUNCTION Kernel_Density(x,dom,h,kern)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x(:),dom(:),h
    LOGICAL, INTENT(IN) :: kern
    REAL(8) :: Kernel_Density(SIZE(dom)),k(SIZE(x))
    INTEGER :: j
    DO j = 1, SIZE(dom)
       IF (kern) THEN
          k = gausskern(dom(j),x,h)
       ELSE
          k = epankern(dom(j),x,h)
       END IF
       Kernel_Density(j) = SUM(k)
    END DO
    Kernel_Density = Kernel_Density / (h*DBLE(SIZE(X)))
  END FUNCTION Kernel_Density
	 
  FUNCTION Kernel_Regression(y,x,dom,h,kern)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x(:),y(:),dom(:),h
    LOGICAL, INTENT(IN) :: kern
    REAL(8) :: Kernel_Regression(SIZE(dom)),k(SIZE(x)),w(SIZE(x))
    INTEGER :: j
    DO j = 1, SIZE(dom)
       IF (kern) THEN
          k = gausskern(dom(j),x,h)
       ELSE
          k = epankern(dom(j),x,h)
       END IF
       w=k/SUM(k)
       Kernel_Regression(j)=SUM(w*y)
    END DO
  END FUNCTION Kernel_Regression

  FUNCTION Local_Linear_Regression(y,x,dom,h,kern)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x(:),y(:),dom(:),h
    LOGICAL, INTENT(IN) :: kern
    REAL(8) :: k(SIZE(x)),dist(SIZE(X)),sn1,sn2,Local_Linear_Regression(SIZE(dom)),num,denom
    INTEGER :: j
    DO j =1, SIZE(DOM)
       dist = dom(j)-x
       IF (kern) THEN
          k = gausskern(dom(j),x,h)
       ELSE
          k = epankern(dom(j),x,h)
       END IF
       sn1=SUM(k*dist)
       sn2=SUM(k*dist*dist)
       dist=k*(sn2-(dist*sn1))
       num=SUM(dist*Y)
       denom=SUM(dist)
       Local_Linear_Regression(j)=num/denom
    END DO
  END FUNCTION Local_Linear_Regression

  FUNCTION Derivative_Local_Linear_Regression(y,x,dom,h,kern)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x(:),y(:),dom(:),h
    LOGICAL, INTENT(IN) :: kern
    REAL(8) :: k(SIZE(x)),dist(SIZE(X)),w(SIZE(X)),arg(SIZE(X)),sn1,sn2,Derivative_Local_Linear_Regression(SIZE(dom)),num,denom
    REAL(8) :: dk(SIZE(x)),dsn1,dsn2,dwj,dwjy,wy
    INTEGER :: j
    DO j = 1, SIZE(DOM)
       dist = dom(j)-x
       arg = dist/h
       IF (kern) THEN
          k = gausskern(dom(j),x,h)
       ELSE
          k = epankern(dom(j),x,h)
       END IF
       sn1=SUM(k*dist)
       sn2=SUM(k*dist*dist)
       w=k*(sn2-(dist*sn1))
       IF (kern) THEN
          dk = -arg*k/h
       ELSE
          dk = -1.5d0*arg*k/h
       END IF
       dsn1 = SUM(k+(dist*dk))
       dsn2 = SUM((2.0d0*k*dist)+(dist*dist*dk))
       dwj = SUM((k*(dsn2-(dist*dsn1)-sn1))+((sn2-dist*sn1)*dk))
       dwjy = SUM((((k*(dsn2-(dist*dsn1)-sn1))+((sn2-dist*sn1)*dk))*y))
       wy = SUM(w*y)
       num = SUM(w)*dwjy - dwj*wy
       denom = SUM(w*w)
       Derivative_Local_Linear_Regression(j)=num/denom
    END DO
  END FUNCTION Derivative_Local_Linear_Regression
  
  
  FUNCTION gausskern(dom,x,h)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: dom,x(:),h
    REAL(8), PARAMETER :: root2pi = 2.506628274631000502415765284811D0
    REAL(8) :: gausskern(SIZE(x))
    gausskern = (dom-x)/h
    gausskern = EXP(-0.5d0*gausskern*gausskern)/root2pi
  END FUNCTION gausskern
  
  FUNCTION epankern(dom,x,h)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: dom,x(:),h
    REAL(8) :: epankern(SIZE(x))
    epankern = (dom-x)/h
    WHERE (ABS(epankern)>1.0d0) epankern=1.0d0
    epankern = 0.75d0*(1-epankern*epankern)
  END FUNCTION epankern
  
END MODULE NONPARAMETRIC
