MODULE RANDOM
  USE NRUTIL, ONLY : NRERROR,NR_SMALL,NR_BIG,NR_EPS,MMUL
  USE LAPACK95, ONLY : POTRF
  IMPLICIT NONE

  PRIVATE :: P_Cholesky_Ran,P_CDF_Normal_Ran!,P_CDF_Normal_Inverse_Ran

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                         PARAMETERS FOR:
  ! Marsaglia & Tsang generator for random normals & random exponentials.
  ! Translated from C by Alan Miller (amiller@bigpond.net.au)
  ! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating
  ! random variables', J. Statist. Software, v5(8).
  ! This is an electronic journal which can be downloaded from:
  ! http://www.jstatsoft.org/v05/i08
  ! N.B. It is assumed that all integers are 32-bit.
  ! N.B. The value of zigm2 has been halved to compensate for the lack of
  !      unsigned integers in Fortran.
  ! Latest version - February 3 2004
  ! Dec 18, 2007 changed the uniform random number generator to use KISS instead

  INTEGER(4), SAVE :: kiss1=123456789,kiss2=362436069,kiss3=521288629,kiss4=916191069
  REAL(8), PARAMETER  :: zigm1=2147483648.0d0,zigm2=2147483648.0d0,half=0.5d0
  REAL(8) :: zigdn=3.442619855899d0,zigtn=3.442619855899d0,zigvn=0.00991256303526217d0, &
       zigq,zigde=7.697117470131487d0,zigte=7.697117470131487d0,zigve=0.003949659822581572d0
  INTEGER(4), SAVE :: zigiz,zigjz,seed,zigkn(0:127),zigke(0:255),zighz
  REAL(8), SAVE :: zigwn(0:127),zigfn(0:127),zigwe(0:255),zigfe(0:255)
  LOGICAL, SAVE :: initialized=.FALSE.

  INTERFACE HALTON
    MODULE PROCEDURE HALTON1,HALTONMUL
  END INTERFACE HALTON


CONTAINS

  SUBROUTINE Set_Seed(k1,k2,k3,k4)
    IMPLICIT NONE
    INTEGER(4), INTENT(IN), OPTIONAL :: k1,k2,k3,k4
    INTEGER :: i
    IF (PRESENT(k1)) THEN
       kiss1=k1
       kiss2=k2
       kiss3=k3
       kiss4=k4
    ELSE
       CALL SYSTEM_CLOCK(seed)
       kiss1=SHR3()
       kiss2=SHR3()
       kiss3=SHR3()
       kiss4=SHR3()
    END IF
    zigdn=3.442619855899d0
    zigtn=3.442619855899d0
    zigvn=0.00991256303526217d0
    zigde=7.697117470131487d0
    zigte=7.697117470131487d0
    zigve=0.003949659822581572d0

    !  Tables for NORMAL
    zigq = zigvn*EXP(half*zigdn*zigdn)
    zigkn(0) = (zigdn/zigq)*zigm1
    zigkn(1) = 0
    zigwn(0) = zigq/zigm1
    zigwn(127) = zigdn/zigm1
    zigfn(0) = 1.0d0
    zigfn(127) = EXP( -half*zigdn*zigdn )
    DO  i = 126, 1, -1
       zigdn = SQRT( -2.0d0 * LOG( zigvn/zigdn + EXP( -half*zigdn*zigdn ) ) )
       zigkn(i+1) = (zigdn/zigtn)*zigm1
       zigtn = zigdn
       zigfn(i) = EXP(-half*zigdn*zigdn)
       zigwn(i) = zigdn/zigm1
    END DO
    !  Tables for EXPONENTIAL
    zigq = zigve*EXP( zigde )
    zigke(0) = (zigde/zigq)*zigm2
    zigke(1) = 0
    zigwe(0) = zigq/zigm2
    zigwe(255) = zigde/zigm2
    zigfe(0) = 1.0d0
    zigfe(255) = EXP( -zigde )
    DO  i = 254, 1, -1
       zigde = -LOG( zigve/zigde + EXP( -zigde ) )
       zigke(i+1) = zigm2 * (zigde/zigte)
       zigte = zigde
       zigfe(i) = EXP( -zigde )
       zigwe(i) = zigde/zigm2
    END DO
    initialized = .TRUE.
  END SUBROUTINE Set_Seed

  ! Generate random 32-bit integers
  INTEGER(4) FUNCTION shr3()
    zigjz = seed
    seed = IEOR(seed,ISHFT(seed,13))
    seed = IEOR(seed,ISHFT(seed,-17))
    seed = IEOR(seed,ISHFT(seed,5))
    shr3 = zigjz + seed
  END FUNCTION shr3

  INTEGER(4) FUNCTION KISS ()
    ! The  KISS (Keep It Simple Stupid) random number generator. Combines:
    ! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
    ! (2) A 3-shift shift-register generator, period 2^32-1,
    ! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
    !  Overall period>2^123;
    IF (.NOT.INITIALIZED) CALL SET_SEED()
    kiss1 = 69069 * kiss1 + 1327217885
    kiss2 = m (m (m (kiss2, 13), - 17), 5)
    kiss3 = 18000 * iand (kiss3, 65535) + ishft (kiss3, - 16)
    kiss4 = 30903 * iand (kiss4, 65535) + ishft (kiss4, - 16)
    kiss = kiss1 + kiss2 + ishft (kiss3, 16) + kiss4
  CONTAINS
    INTEGER FUNCTION m(k, n)
      INTEGER :: k, n
      m = ieor (k, ishft (k, n) )
    END FUNCTION m
  END FUNCTION KISS

  REAL(8) FUNCTION Sample_Uniform(a,b)
    !INTEGER(4) ::  ival,zigjz
    REAL(8), INTENT(IN) :: a,b
    IF (a>b) CALL NRERROR('upper limit lower than lower limit: Sample_Uniform')
    Sample_Uniform = (DBLE(KISS())+2147483649.0d0)/4294967297.0d0
    Sample_Uniform = Sample_Uniform*(b-a) + a
  END FUNCTION Sample_Uniform

  !  Generate random normals
  REAL(8) FUNCTION Sample_Normal(mu,var)
    REAL(8), INTENT(IN) :: mu,var
    REAL(8), PARAMETER ::  r = 3.442620d0
    REAL(8) :: x,y
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: Sample_Normal')
    zighz = KISS()
    zigiz = IAND(zighz,127)
    IF(ABS(zighz) < zigkn(zigiz)) THEN
       Sample_Normal = zighz * zigwn(zigiz)
       Sample_Normal = Sample_Normal*SQRT(var) + mu
    ELSE
       DO
          IF(zigiz==0) THEN
             DO
                x = -0.2904764d0*LOG(Sample_Uniform(0.0d0,1.0d0))
                y = -LOG(Sample_Uniform(0.0d0,1.0d0))
                IF( y+y >= x*x ) EXIT
             END DO
             Sample_Normal = r+x
             IF (zighz<=0) Sample_Normal = -Sample_Normal
             Sample_Normal = Sample_Normal*SQRT(var) + mu
             RETURN
          END IF
          x = zighz * zigwn(zigiz)
          IF (zigfn(zigiz) + Sample_Uniform(0.0d0,1.0d0)* &
               (zigfn(zigiz-1)-zigfn(zigiz))< EXP(-half*x*x)) THEN
             Sample_Normal = x
             Sample_Normal = Sample_Normal*SQRT(var) + mu
             RETURN
          END IF
          zighz = KISS()
          zigiz = IAND(zighz,127)
          IF(ABS(zighz) < zigkn(zigiz)) THEN
             Sample_Normal = zighz * zigwn(zigiz)
             Sample_Normal = Sample_Normal*SQRT(var) + mu
             RETURN
          END IF
       END DO
    END IF
  END FUNCTION Sample_Normal

  !  Generate random exponentials
  REAL(8) FUNCTION Sample_Exponential(a)
    REAL(8), INTENT(IN) :: a
    REAL(8)  ::  x
    IF (a<0.0d0) CALL NRERROR('a has to be positive: Sample_Exponential')
    zigjz = KISS()
    zigiz = IAND(zigjz,255)
    IF (ABS(zigjz) < zigke(zigiz)) THEN
       Sample_Exponential = (ABS(zigjz) * zigwe(zigiz))*a
       RETURN
    END IF
    DO
       IF (zigiz==0) THEN
          Sample_Exponential = (7.69711 - LOG(Sample_Uniform(0.0d0,1.0d0)))*a
          RETURN
       END IF
       x = ABS(zigjz) * zigwe(zigiz)
       IF (zigfe(zigiz) + Sample_Uniform(0.0d0,1.0d0)*(zigfe(zigiz-1) - zigfe(zigiz)) < EXP(-x)) THEN
          Sample_Exponential = x*a
          RETURN
       END IF
       zigjz = KISS()
       zigiz = IAND(zigjz,255)
       IF (ABS(zigjz) < zigke(zigiz)) THEN
          Sample_Exponential = (ABS(zigjz)*zigwe(zigiz))*a
          RETURN
       END IF
    END DO
  END FUNCTION Sample_Exponential

  REAL(8) FUNCTION Sample_Gamma(ain,b)
    ! Returns a number distributed as a gamma distribution with shape parameter a and inverse scale b.
    ! such that mean=a/b and var=a/(b*b)
    ! That is P(x) = [(b^a)/Gamma(a)] * [x^(a-1)] * exp(-x*b) for x>0 and a>=1.
    ! Uses the algorithm in
    ! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
    ! gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: ain,b
    REAL(8) :: c,d,u,v,x,correction,a
    correction=1.0d0
    a=ain
    IF (ain<0.0d0) CALL NRERROR('a has to be positive: Sample_Gamma')
    IF (b<0.0d0) CALL NRERROR('b has to be positive: Sample_Gamma')
    IF (a<1.0d0) THEN
       correction=Sample_Uniform(0.0d0,1.0d0)**(1.0d0/ain)
       a = ain + 1.0
    END IF
    d = a - 1.0d0/3.0d0
    c = 1.0d0/SQRT(9.0d0*d)
    ! Start of main loop
    DO
       ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
       DO
          x = Sample_Normal(0.0d0,1.0d0)
          v = (1.0d0 + c*x)**3
          IF (v > 0.0d0) EXIT
       END DO
       ! Generate uniform variable U
       u = Sample_Uniform(0.0d0,1.0d0)
       IF (u < 1.0d0 - 0.0331d0*x**4.0d0) THEN
          Sample_Gamma = correction*d*v
          EXIT
       ELSE IF (LOG(u) < 0.5d0*(x**2.0d0) + d*(1.0d0 - v + LOG(v))) THEN
          Sample_Gamma = correction*d*v
          EXIT
       END IF
    END DO
    Sample_Gamma=Sample_Gamma/b
  END FUNCTION Sample_Gamma

  REAL(8) FUNCTION Sample_Beta(a,b)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: a,b
    REAL(8) :: x,y
    x=Sample_Gamma(a,1.0d0)
    y=Sample_Gamma(b,1.0d0)
    Sample_Beta=x/(x+y)
  END FUNCTION Sample_Beta

  REAL(8) FUNCTION Sample_Chi2(df)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: df
    Sample_Chi2=2.0d0*Sample_Gamma(0.5d0*df,1.0d0)
  END FUNCTION Sample_Chi2

  REAL(8) FUNCTION Sample_Mixture_Gamma(a,b,p)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: a(:),b(:),p(:)
    REAL(8) :: u,cdf,x
    INTEGER :: j
    cdf=0.0d0
    u=Sample_Uniform(0.0d0,1.0d0)
    DO j=1,SIZE(p)
       cdf=cdf+p(j)
       x=Sample_Gamma(a(j),b(j))
       IF (u<=cdf) THEN
          Sample_Mixture_GAMMA=x
          RETURN
       END IF
    END DO
  END FUNCTION Sample_Mixture_Gamma

  REAL(8) FUNCTION Sample_Mixture_Normal(mu,var,p)
    ! Returns 1 draws from a mixture of normals with nmix mixture components
    ! p vector of weights, mu vector of means and sigma vector of variances
    IMPLICIT NONE
    REAL(8), INTENT(in) :: p(:),mu(:),var(:)
    INTEGER :: i, j,nmix
    REAL(8) :: cdf(SIZE(p)+1),nor,u
    IF (ANY(var<0.0d0)) CALL NRERROR('variance has to be positive: Sample_Mixture_Normal')
    IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) CALL NRERROR('weights have to add to one: Sample_Mixture_Normal')
    IF (ANY(p<0.0d0)) CALL NRERROR('weights have to be positive: Sample_Mixture_Normal')
    nmix=SIZE(p)
    u = Sample_Uniform(0.0d0,1.0d0)
    nor = Sample_Normal(0.0d0,1.0d0)
    cdf=0.0D0
    DO i = 1, nmix
       cdf(i+1) = cdf(i) + p(i)
       IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
          Sample_Mixture_Normal=mu(i)+SQRT(var(i))*nor
          RETURN
       END IF
    END DO
  END FUNCTION Sample_Mixture_Normal

  REAL(8) FUNCTION Sample_Truncated_Mixture_Normal(mu,var,p,a,lb)
    ! Returns 1 draws from a mixture of normals with nmix mixture components
    ! p vector of weights, mu vector of means and sigma vector of variances
    ! that is truncated to be between a and infinity (lb=true) or -infinity and a (lb=false)
    ! !!!!!!!!!!!!!!!!!!!!!!!!!NOT the same as sampling from a !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!mixture of truncated normals!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! the weights need to be updated
    IMPLICIT NONE
    REAL(8), INTENT(in) :: p(:),mu(:),var(:),a
    LOGICAL, INTENT(IN) :: lb
    INTEGER :: i, j,nmix
    REAL(8) :: cdf(SIZE(p)+1),u,w(SIZE(p))
    IF (ANY(var<0.0d0)) CALL NRERROR('variance has to be positive: Sample_Truncated_Mixture_Normal')
    IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) CALL NRERROR('weights have to add to one: Sample_Truncated_Mixture_Normal')
    IF (ANY(p<0.0d0)) CALL NRERROR('weights have to be positive: Sample_Truncated_Mixture_Normal')
    nmix=SIZE(p)
    IF (lb) THEN
       DO i = 1, nmix
          w(i) = (1.0d0 - P_CDF_Normal_Ran(a,mu(i),var(i)))*p(i)
       END DO
    ELSE
       DO i = 1, nmix
          w(i) = P_CDF_Normal_Ran(a,mu(i),var(i))*p(i)
       END DO
    END IF
    w=w/SUM(w)
    u = Sample_Uniform(0.0d0,1.0d0)
    cdf=0.0D0
    DO i = 1, nmix
       cdf(i+1) = cdf(i) + w(i)
       IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
          Sample_Truncated_Mixture_Normal=Sample_Truncated_Normal_Geweke(mu(i),var(i),a,lb)
          RETURN
       END IF
    END DO
  END FUNCTION Sample_Truncated_Mixture_Normal

  REAL(8) FUNCTION Sample_Truncated_Normal_Geweke(mu,var,a,lb)
    ! Returns one draw from a truncated normal with underlying
    ! mean=mu and VARIANCE=var with truncation
    !           (a,+infty)    IF lb=TRUE
    !           (-infty,a)    IF lb=FALSE
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu,var,a
    LOGICAL, INTENT(IN) :: lb
    REAL(8), PARAMETER :: t4=0.45D0
    REAL(8) :: u,z,phi_z,c,temp
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: Sample_Truncated_Normal')
    c=((a-mu)/(SQRT(var)))
    IF (.NOT.lb) THEN
       c=-c
    END IF
    IF (c < t4) THEN
       ! normal rejection sampling
       DO
          u=Sample_Normal(0.0d0,1.0d0)
          IF (u > c) EXIT
       ENDDO
       temp=u
    ELSE
       ! exponential rejection sampling
       DO
          u = Sample_Uniform(0.0d0,1.0d0)
          z = Sample_Exponential(1.0d0/c)
          phi_z=EXP(-.5D0*(z*z))
          IF (u < phi_z) EXIT
       ENDDO
       temp=c + z
    ENDIF
    IF (.not.(lb)) THEN
       Sample_Truncated_Normal_Geweke = mu - (temp*SQRT(var))
    ELSE
       Sample_Truncated_Normal_Geweke = mu + (temp*SQRT(var))
    ENDIF
  END FUNCTION Sample_Truncated_Normal_Geweke

  REAL(8) FUNCTION Sample_Truncated_Normal(mu,var,a,lb)
    ! Returns one draw from a truncated normal with underlying
    ! mean=mu and VARIANCE=var with truncation
    !           (a,+infty)    IF lb=TRUE
    !           (-infty,a)    IF lb=FALSE
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu,var,a
    LOGICAL, INTENT(IN) :: lb
    REAL(8), PARAMETER :: t4=0.45D0
    REAL(8) :: u,cdf,aux
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: Sample_Truncated_Normal')
    u = Sample_Uniform(0.0d0,1.0d0)
    cdf=P_CDF_Normal_Ran(a,mu,var)
    IF (lb) THEN
       aux = (u*(1.0-cdf)) + cdf
    ELSE
       aux = u*cdf
    END IF
    Sample_Truncated_Normal = P_CDF_Normal_Inverse_Ran(aux,mu,var)
  END FUNCTION Sample_Truncated_Normal

  REAL(8) FUNCTION Sample_Truncated_Normal_Given_Uniform(mu,var,a,lb,u)
    ! Returns one draw from a truncated normal with underlying
    ! mean=mu and VARIANCE=var with truncation
    !           (a,+infty)    IF lb=TRUE
    !           (-infty,a)    IF lb=FALSE
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu,var,a,u
    LOGICAL, INTENT(IN) :: lb
    REAL(8), PARAMETER :: t4=0.45D0
    REAL(8) :: cdf,aux,o
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: Sample_Truncated_Normal')
    cdf=P_CDF_Normal_Ran(a,mu,var)
    IF (lb) THEN
       aux = (u*(1.0-cdf)) + cdf
    ELSE
       aux = u*cdf
    END IF
    o=P_CDF_Normal_Inverse_Ran(aux,mu,var)
    Sample_Truncated_Normal_Given_Uniform = o
  END FUNCTION Sample_Truncated_Normal_Given_Uniform

  REAL(8) FUNCTION Sample_Double_Truncated_Mixture_Normal(mu,var,p,a,b)
    ! Returns 1 draws from a mixture of normals with nmix mixture components
    ! p vector of weights, mu vector of means and sigma vector of variances
    ! that is truncated to be between a and b
    ! !!!!!!!!!!!!!!!!!!!!!!!!!NOT the same as sampling from a !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!mixture of truncated normals!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! the weights need to be updated
    IMPLICIT NONE
    REAL(8), INTENT(in) :: p(:),mu(:),var(:),a,b
    INTEGER :: i, j,nmix
    REAL(8) :: cdf(SIZE(p)+1),u,w(SIZE(p))
    IF (ANY(var<0.0d0)) CALL NRERROR('variance has to be positive: Sample_Double_Truncated_Mixture_Normal')
    IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) &
         CALL NRERROR('weights have to add to one: Sample_Double_Truncated_Mixture_Normal')
    IF (ANY(p<0.0d0)) CALL NRERROR('weights have to be positive: Sample_Double_Truncated_Mixture_Normal')
    nmix=SIZE(p)
    DO i = 1, nmix
       w(i) = (P_CDF_Normal_Ran(b,mu(i),var(i)) - P_CDF_Normal_Ran(a,mu(i),var(i)))*p(i)
    END DO
    w=w/SUM(w)
    u = Sample_Uniform(0.0d0,1.0d0)
    cdf=0.0D0
    DO i = 1, nmix
       cdf(i+1) = cdf(i) + w(i)
       IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
          Sample_Double_Truncated_Mixture_Normal=Sample_Double_Truncated_Normal(mu(i),var(i),a,b)
          RETURN
       END IF
    END DO
  END FUNCTION Sample_Double_Truncated_Mixture_Normal

  REAL(8) FUNCTION Sample_Double_Truncated_Normal_Geweke(mu,var,a,b)
    ! Generates one draw from truncated standard normal distribution (mu,sigma) on (a,b)
    IMPLICIT NONE
    REAL(8),INTENT(in) :: a,b,mu,var
    REAL(8) :: c,c1,c2,u(2),x,cdel,f1,f2,z,az,bz,eps
    REAL(8),PARAMETER :: t1=0.375D0,t2=2.18D0,t3=0.725D0,t4=0.45D0
    LOGICAL :: lflip
    REAL :: aaa
    INTEGER :: j
    eps=2.220446049250313D-016
    az=(a-mu)/SQRT(var)
    bz=(b-mu)/SQRT(var)
    c1=az
    c2=bz
    lflip=.false.
    IF (c1*c2<0.0D0) THEN
       IF ((f(c1)>t1) .and. (f(c2)>t1)) THEN
          cdel=c2-c1
          DO
             DO j = 1, 2
                u(j) = Sample_Uniform(0.0d0,1.0d0)
             END DO
             x=c1+cdel*u(1)
             IF (u(2)<f(x)) EXIT
          END DO
       ELSE
          DO
             x=Sample_Normal(0.0d0,1.0d0)
             IF ((x>c1) .and. (x<c2)) EXIT
          END DO
       END IF
    ELSE
       IF (c1<0.0D0) THEN
          c=c1
          c1=-c2
          c2=-c
          lflip=.true.
       END IF
       f1=f(c1)
       f2=f(c2)
       IF ((f2<eps) .or. (f1/f2>t2)) THEN
          IF (c1>t3) THEN
             !exponential rejection sampling
             c=c2-c1
             DO
                u(1) = Sample_Uniform(0.0d0,1.0d0)
                z = Sample_Exponential(1.0d0/c1)
                IF ((z<c) .and. (u(1)<f(z))) EXIT
             END DO
             x=c1+z
          ELSE
             !half-normal rejection sampling
             DO
                x=Sample_Normal(0.0d0,1.0d0)
                x=abs(x)
                IF ((x>c1) .and. (x<c2)) EXIT
             END DO
          END IF
       ELSE
          !uniform rejection sampling
          cdel=c2-c1
          DO
             DO j = 1, 2
                u(j) = Sample_Uniform(0.0d0,1.0d0)
             END DO
             x=c1+cdel*u(1)
             IF (u(2)<(f(x)/f1)) EXIT
          END DO
       END IF
    END IF
    IF (lflip) THEN
       Sample_Double_Truncated_Normal_Geweke=mu-(SQRT(var)*x)
    ELSE
       Sample_Double_Truncated_Normal_Geweke=mu+(SQRT(var)*x)
    END IF
  CONTAINS
    REAL(8) FUNCTION f(x)
      REAL(8) :: x
      f=dexp(-.5D0*(x*x))
    END FUNCTION f
  END FUNCTION Sample_Double_Truncated_Normal_Geweke

  REAL(8) FUNCTION Sample_Double_Truncated_Normal(mu,var,a,b)
    ! Returns one draw from a truncated normal with underlying
    ! mean=mu and VARIANCE=var with truncation
    !           (a,+infty)    IF lb=TRUE
    !           (-infty,a)    IF lb=FALSE
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu,var,a,b
    REAL(8) :: u,cdfa,cdfb,aux
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: Sample_Double_Truncated_Normal')
    u = Sample_Uniform(0.0d0,1.0d0)
    cdfa=P_CDF_Normal_Ran(a,mu,var)
    cdfb=P_CDF_Normal_Ran(b,mu,var)
    aux = (u*(cdfb-cdfa))+cdfa
    Sample_Double_Truncated_Normal = P_CDF_Normal_Inverse_Ran(aux,mu,var)
  END FUNCTION Sample_Double_Truncated_Normal

  FUNCTION Sample_Dirichlet(k,a)
    ! DIRICHLET
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: k
    REAL(8), DIMENSION(:), INTENT(IN) :: a
    REAL(8) :: Sample_Dirichlet(k)
    REAL(8) :: rg(k),sg
    INTEGER :: i
    DO i = 1, k
       rg(i) = Sample_Gamma(a(i),1.0d0)
    ENDDO
    sg=SUM(rg)
    Sample_Dirichlet = rg/sg
  END FUNCTION Sample_Dirichlet

  INTEGER FUNCTION Sample_Multinomial(p)
    ! INPUT  :   p  is kx1
    ! Returns a random variable sampled from a multinomial distribution with
    ! k categories having probabilities p
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: p(:)
    REAL(8) :: u,cdf
    INTEGER :: j,k
!    IF (ABS(SUM(p)-1.0d0)<1.0d-8) THEN
!       WRITE(*,*) p
!       CALL NRERROR("p does not sum to 1")
!    END IF
    k=SIZE(p)
    u = Sample_Uniform(0.0d0,1.0d0)
    cdf=0.0d0
    DO j=1,k-1
       IF ((u>cdf).AND.(u<=cdf+p(j))) THEN
          Sample_Multinomial=j
          RETURN
       END IF
       cdf=cdf+p(j)
    END DO
    IF (u>cdf) Sample_Multinomial=k
  END FUNCTION Sample_Multinomial

  FUNCTION Sample_Multivariate_Normal(mu,var)
    ! Returns one draw from a multivariate normal with mean mu and varcovar var
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu(:),var(:,:)
    REAL(8) :: cvar(SIZE(var,1),SIZE(var,2)),Sample_Multivariate_Normal(SIZE(mu))
    INTEGER :: j
    cvar=P_Cholesky_Ran(var)
    DO j = 1, SIZE(mu)
       Sample_Multivariate_Normal(j) = Sample_Normal(0.0d0,1.0d0)
    END DO
    Sample_Multivariate_Normal = mu + MMUL(cvar,Sample_Multivariate_Normal)
  END FUNCTION Sample_Multivariate_Normal

  REAL(8) FUNCTION Sample_Logit()
    IMPLICIT NONE
    REAL(8) :: u
    u=Sample_Uniform(0.0d0,1.0d0)
    Sample_Logit=LOG((1.0d0-u)/u)
  END FUNCTION Sample_Logit

  REAL(8) FUNCTION Sample_EV1()
    REAL(8) :: u
    u=Sample_Uniform(0.0d0,1.0d0)
    Sample_EV1=-LOG(-LOG(u))
  END FUNCTION Sample_EV1

  FUNCTION P_Cholesky_RAN(AIN)
    IMPLICIT NONE
    REAL(8), DIMENSION(:,:), INTENT(IN) :: AIN
    REAL(8) :: A(SIZE(AIN,1),SIZE(AIN,2)),P_Cholesky_RAN(SIZE(AIN,1),SIZE(AIN,2))
    INTEGER :: i,j,n
    n=SIZE(ain,1)
    A=AIN
    CALL POTRF(A,'L')
    P_Cholesky_RAN=0.0d0
    FORALL(i=1:n,j=1:n,j<=i) P_Cholesky_RAN(i,j)=A(i,j)
  END FUNCTION P_Cholesky_RAN

  REAL(8) FUNCTION P_CDF_Normal_Ran(zin,mu,var)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: zin
    REAL(8), INTENT(IN) :: mu,var
    REAL(8) :: zabs,p,arg,logpdf,z,std
    REAL(8), PARAMETER :: p0=220.2068679123761D0,p1=221.2135961699311D0,p2=112.0792914978709D0, &
         p3 = 33.91286607838300D0,p4 = 6.373962203531650D0,p5 = .7003830644436881D0, &
         p6 = .3526249659989109D-01,q0 = 440.4137358247522D0,q1 = 793.8265125199484D0, &
         q2 = 637.3336333788311D0,q3 = 296.5642487796737D0,q4 = 86.78073220294608D0, &
         q5=16.06417757920695D0,q6=1.755667163182642D0,q7=.8838834764831844D-1,cutoff = 7.071D0, &
         logroot2pi = 0.918938533204672780563271317078D0
    IF (var<0.0d0) CALL NRERROR('variance has to be positive: P_CDF_Normal_Ran')
    z=zin-mu
    zabs=ABS(z)
    IF (zabs<NR_Small) THEN
       P_CDF_Normal_Ran=0.5d0
       RETURN
    END IF
    std = SQRT(var)
    IF (std<NR_Small) THEN
       IF (zin-mu>0.0d0) THEN
          P_CDF_Normal_Ran = 1.0d0
       ELSE IF	(zin-mu<0.0d0) THEN
          P_CDF_Normal_Ran = 0.0d0
       END IF
    END IF
    zabs=zabs/std
    IF (z > 37.0D0) THEN
       P_CDF_Normal_Ran = 1.0D0
       RETURN
    ELSE IF (z < -37.0D0) THEN
       P_CDF_Normal_Ran = 0.0D0
       RETURN
    END IF
    arg = -0.5D0*zabs*zabs
    logpdf = -logroot2pi - LOG(std) + arg
    IF (zabs < cutoff) THEN
       p = arg + LOG(((((((p6*zabs + p5)*zabs + p4)*zabs + p3)*zabs + &
            p2)*zabs + p1)*zabs + p0)) - LOG((((((((q7*zabs + q6)*zabs + &
            q5)*zabs + q4)*zabs + q3)*zabs + q2)*zabs + q1)*zabs + &
            q0))
    ELSE
       p = logpdf - LOG((zabs + 1.0D0/(zabs + 2.0D0/(zabs + 3.0D0/(zabs + 4.0D0/ &
            (zabs + 0.65D0))))))
    END IF
    p = EXP(p)
    IF (z < 0.0D0) THEN
       P_CDF_Normal_Ran=p
       RETURN
    ELSE
       P_CDF_Normal_Ran = 1.0D0 - p
       RETURN
    END IF
    RETURN
  END FUNCTION P_CDF_Normal_Ran

  REAL(8) FUNCTION P_CDF_Normal_Inverse_Ran(P,muin,varin)
    !	Produces the normal deviate Z corresponding to a given lower
    !	tail area of P; Z is accurate to about 1 part in 10**16.
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: P
    REAL(8), OPTIONAL, INTENT(IN) :: muin,varin
    REAL(8) :: Q,R,mu,var
    REAL(8), PARAMETER :: ZERO=0.D0, ONE = 1.D0, HALF = 0.5D0,SPLIT1 = 0.425D0, SPLIT2 = 5.D0, &
         CONST1 = 0.180625D0, CONST2 = 1.6D0
    !	Coefficients for P close to 0.5
    REAL(8), PARAMETER :: A0 = 3.3871328727963666080D0,A1=1.3314166789178437745D+2, &
         A2=1.9715909503065514427D+3,A3=1.3731693765509461125D+4,A4=4.5921953931549871457D+4, &
         A5=6.7265770927008700853D+4,A6=3.3430575583588128105D+4,A7=2.5090809287301226727D+3, &
         B1=4.2313330701600911252D+1,B2=6.8718700749205790830D+2,B3=5.3941960214247511077D+3, &
         B4=2.1213794301586595867D+4,B5=3.9307895800092710610D+4,B6=2.8729085735721942674D+4, &
         B7=5.2264952788528545610D+3
    !	Coefficients for P not close to 0, 0.5 or 1.
    REAL(8), PARAMETER :: C0=1.42343711074968357734D0,C1=4.63033784615654529590D0, &
         C2=5.76949722146069140550D0,C3=3.64784832476320460504D0,C4=1.27045825245236838258D0, &
         C5=2.41780725177450611770D-1,C6=2.27238449892691845833D-2,C7=7.74545014278341407640D-4, &
         D1=2.05319162663775882187D0,D2=1.67638483018380384940D0,D3=6.89767334985100004550D-1, &
         D4=1.48103976427480074590D-1,D5=1.51986665636164571966D-2,D6=5.47593808499534494600D-4, &
         D7=1.05075007164441684324D-9
    !	CoefficientsforP near 0 or 1.
    REAL(8), PARAMETER :: E0=6.65790464350110377720D0,E1=5.46378491116411436990D0, &
         E2=1.78482653991729133580D0,E3=2.96560571828504891230D-1,E4=2.65321895265761230930D-2, &
         E5=1.24266094738807843860D-3,E6=2.71155556874348757815D-5,E7=2.01033439929228813265D-7, &
         F1=5.99832206555887937690D-1,F2=1.36929880922735805310D-1,F3=1.48753612908506148525D-2, &
         F4=7.86869131145613259100D-4,F5=1.84631831751005468180D-5,F6=1.42151175831644588870D-7, &
         F7=2.04426310338993978564D-15
    IF ((P>1.0D0).OR.(P<0.0D0)) CALL NRERROR('P has to be between zero and one: P_CDF_Normal_Inverse_Ran')
    mu=0.0d0
    IF (PRESENT(muin)) mu=muin
    var=1.0d0
    IF (PRESENT(varin)) THEN
       IF (varin<0.0d0) CALL NRERROR('variance has to be positive: P_CDF_Normal_Inverse_Ran')
       var=varin
    END IF
    Q = P - HALF
    IF (ABS(Q) .LE. SPLIT1) THEN
       R = CONST1 - Q * Q
       P_CDF_Normal_Inverse_Ran=Q*(((((((A7*R+A6)*R+A5)*R+A4)*R+A3)*R+A2)*R+A1)*R+ A0) / &
            (((((((B7 * R + B6) * R + B5) * R + B4) * R + B3)* R + B2) * R + B1) * R + ONE)
       P_CDF_Normal_Inverse_Ran=(SQRT(var)*P_CDF_Normal_Inverse_Ran)+mu
       RETURN
    ELSE
       IF (Q .LT. ZERO) THEN
          R = P
       ELSE
          R = ONE - P
       END IF
       R = SQRT(-LOG(R))
       IF (R .LE. SPLIT2) THEN
          R = R - CONST2
          P_CDF_Normal_Inverse_Ran=(((((((C7*R + C6) * R + C5) * R + C4) * R + C3)*R+C2)*R+ C1) * R + C0) / &
               (((((((D7 * R + D6) * R + D5) * R + D4) * R + D3)* R + D2) * R + D1) * R + ONE)
       ELSE
          R = R - SPLIT2
          P_CDF_Normal_Inverse_Ran=(((((((E7*R + E6) * R + E5) * R + E4) * R + E3)*R+E2)*R + E1) * R + E0) / &
               (((((((F7 * R + F6) * R + F5) * R + F4) * R + F3)* R + F2) * R + F1) * R + ONE)
       END IF
       IF (Q .LT. ZERO) P_CDF_Normal_Inverse_Ran = - P_CDF_Normal_Inverse_Ran
       P_CDF_Normal_Inverse_Ran=(SQRT(var)*P_CDF_Normal_Inverse_Ran)+mu
       RETURN
    END IF
  END FUNCTION P_CDF_Normal_Inverse_Ran

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!USE HALTON SEQUENCES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  INTEGER FUNCTION Prime(n)
    !*****************************************************************************80
    !
    ! PRIME returns the n'th PRIME_MAX prime numbers.
    !
    !  Discussion:
    !
    !    PRIME_MAX is 1600, and the largest prime stored is 13499.
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    INTEGER, PARAMETER :: prime_max=1600
    INTEGER :: NPVEC(prime_max)
    npvec(1:100) = (/ &
         2,    3,    5,    7,   11,   13,   17,   19,   23,   29, 31,   37,   41,   43,   47,   53,   59,   61,   67,   71, &
         73,   79,   83,   89,   97,  101,  103,  107,  109,  113, 127,  131,  137,  139,  149,  151,  157,  163,  167,  173, &
         179,  181,  191,  193,  197,  199,  211,  223,  227,  229, 233,  239,  241,  251,  257,  263,  269,  271,  277,  281, &
         283,  293,  307,  311,  313,  317,  331,  337,  347,  349, 353,  359,  367,  373,  379,  383,  389,  397,  401,  409, &
         419,  421,  431,  433,  439,  443,  449,  457,  461,  463, 467,  479,  487,  491,  499,  503,  509,  521,  523,  541 /)

    npvec(101:200) = (/ &
         547,  557,  563,  569,  571,  577,  587,  593,  599,  601, 607,  613,  617,  619,  631,  641,  643,  647,  653,  659, &
         661,  673,  677,  683,  691,  701,  709,  719,  727,  733, 739,  743,  751,  757,  761,  769,  773,  787,  797,  809, &
         811,  821,  823,  827,  829,  839,  853,  857,  859,  863, 877,  881,  883,  887,  907,  911,  919,  929,  937,  941, &
         947,  953,  967,  971,  977,  983,  991,  997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, &
         1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223 /)

    npvec(201:300) = (/ &
         1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, &
         1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, &
         1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, &
         1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, &
         1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987 /)

    npvec(301:400) = (/ &
         1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129, &
         2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287, &
         2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, &
         2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, &
         2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741 /)

    npvec(401:500) = (/ &
         2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, &
         2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, &
         3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, &
         3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, &
         3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571 /)

    npvec(501:600) = (/ &
         3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, 3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, &
         3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821, 3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, &
         3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, 4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057, &
         4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139, 4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, &
         4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297, 4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409 /)

    npvec(601:700) = (/ &
         4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, &
         4591, 4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751, &
         4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, 4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937, &
         4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, 5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087, &
         5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, 5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279 /)

    npvec(701:800) = (/ &
         5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387, 5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, &
         5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, 5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639, &
         5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693, 5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, &
         5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851, 5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, &
         5953, 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133 /)

    npvec(801:900) = (/ &
         6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, 6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, &
         6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, 6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473, &
         6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, 6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673, &
         6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761, 6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833, &
         6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, 6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997 /)

    npvec(901:1000) = (/ &
         7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, &
         7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253, 7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, &
         7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499, 7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, &
         7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723, &
         7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, 7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919 /)

    npvec(1001:1100) = (/ &
         7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017, 8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, &
         8117, 8123, 8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219, 8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, 8287, 8291, &
         8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, 8389, 8419, 8423, 8429, 8431, 8443, 8447, 8461, 8467, 8501, &
         8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597, 8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, &
         8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741, 8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831 /)

    npvec(1101:1200) = (/ &
         8837, 8839, 8849, 8861, 8863, 8867, 8887, 8893, 8923, 8929, 8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011, &
         9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109, 9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199, &
         9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, 9293, 9311, 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, &
         9391, 9397, 9403, 9413, 9419, 9421, 9431, 9433, 9437, 9439, 9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533, &
         9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631, 9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733 /)

    npvec(1201:1300) = (/ &
         9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, 9817, 9829, 9833, 9839, 9851, 9857, 9859, 9871, 9883, 9887, &
         9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973,10007, 10009,10037,10039,10061,10067,10069,10079,10091,10093,10099, &
         10103,10111,10133,10139,10141,10151,10159,10163,10169,10177, 10181,10193,10211,10223,10243,10247,10253,10259,10267,10271, &
         10273,10289,10301,10303,10313,10321,10331,10333,10337,10343, 10357,10369,10391,10399,10427,10429,10433,10453,10457,10459, &
         10463,10477,10487,10499,10501,10513,10529,10531,10559,10567, 10589,10597,10601,10607,10613,10627,10631,10639,10651,10657 /)

    npvec(1301:1400) = (/ &
         10663,10667,10687,10691,10709,10711,10723,10729,10733,10739,10753,10771,10781,10789,10799,10831,10837,10847,10853,10859, &
         10861,10867,10883,10889,10891,10903,10909,10937,10939,10949,10957,10973,10979,10987,10993,11003,11027,11047,11057,11059, &
         11069,11071,11083,11087,11093,11113,11117,11119,11131,11149,11159,11161,11171,11173,11177,11197,11213,11239,11243,11251, &
         11257,11261,11273,11279,11287,11299,11311,11317,11321,11329,11351,11353,11369,11383,11393,11399,11411,11423,11437,11443, &
         11447,11467,11471,11483,11489,11491,11497,11503,11519,11527,11549,11551,11579,11587,11593,11597,11617,11621,11633,11657 /)

    npvec(1401:1500) = (/ &
         11677,11681,11689,11699,11701,11717,11719,11731,11743,11777,11779,11783,11789,11801,11807,11813,11821,11827,11831,11833, &
         11839,11863,11867,11887,11897,11903,11909,11923,11927,11933,11939,11941,11953,11959,11969,11971,11981,11987,12007,12011, &
         12037,12041,12043,12049,12071,12073,12097,12101,12107,12109,12113,12119,12143,12149,12157,12161,12163,12197,12203,12211, &
         12227,12239,12241,12251,12253,12263,12269,12277,12281,12289,12301,12323,12329,12343,12347,12373,12377,12379,12391,12401, &
         12409,12413,12421,12433,12437,12451,12457,12473,12479,12487,12491,12497,12503,12511,12517,12527,12539,12541,12547,12553 /)

    npvec(1501:1600) = (/ &
         12569,12577,12583,12589,12601,12611,12613,12619,12637,12641,12647,12653,12659,12671,12689,12697,12703,12713,12721,12739, &
         12743,12757,12763,12781,12791,12799,12809,12821,12823,12829,12841,12853,12889,12893,12899,12907,12911,12917,12919,12923, &
         12941,12953,12959,12967,12973,12979,12983,13001,13003,13007,13009,13033,13037,13043,13049,13063,13093,13099,13103,13109, &
         13121,13127,13147,13151,13159,13163,13171,13177,13183,13187,13217,13219,13229,13241,13249,13259,13267,13291,13297,13309, &
         13313,13327,13331,13337,13339,13367,13381,13397,13399,13411,13417,13421,13441,13451,13457,13463,13469,13477,13487,13499 /)

    IF ((n<1).OR.(n>prime_max)) THEN
       prime=-1
       CALL NRERROR("n must be between 1 and 1600")
    ELSE
       prime=npvec(n)
    END IF
    RETURN
  END FUNCTION Prime

    SUBROUTINE HALTON1(i,r)
    !*****************************************************************************80
    !
    !! HALTON computes an element of a Halton sequence.
    !  Parameters:
    ! i is the number of the element of the sequence
    ! m is the size (dimension) of the sequence
    ! r(m) is the i'th element of the m-dimensional sequence
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(OUT) :: r

    INTEGER :: d,il,j,pr,t
    REAL(8) :: prinv

    t = ABS ( i )
    !
    !  Carry out the computation.
    !
    pr=PRIME(1)
    prinv = 1.0D0 / DBLE(pr)
    r=0.0d0
    DO WHILE (t.NE.0)
        pr=PRIME(1)
        d=MOD(t,pr)
        r=r+(DBLE(d)*prinv)
        prinv=prinv/DBLE(pr)
        t=(t/pr)
    END DO
    RETURN
    END SUBROUTINE HALTON1


  SUBROUTINE HALTONMUL(i,m,r)
    !*****************************************************************************80
    !
    !! HALTON computes an element of a Halton sequence.
    !  Parameters:
    ! i is the number of the element of the sequence
    ! m is the size (dimension) of the sequence
    ! r(m) is the i'th element of the m-dimensional sequence
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: i,m
    REAL(8), INTENT(OUT) :: r(:)

    INTEGER :: d,il,j,pr,t(m)
    REAL(8) :: prinv(m)

    t = ABS ( i )
    !
    !  Carry out the computation.
    !
    DO il = 1, m
       pr=PRIME(il)
       prinv(il) = 1.0D0 / DBLE(pr)
    END DO
    r=0.0d0
    DO WHILE (any(t.NE.0))
       DO j=1,m
          pr=PRIME(j)
          d=MOD(t(j),pr)
          r(j)=r(j)+(DBLE(d)*prinv(j))
          prinv(j)=prinv(j)/DBLE(pr)
          t(j)=(t(j)/pr)
       END DO
    END DO
    RETURN
  END SUBROUTINE HALTONMUL

  SUBROUTINE HALTON_ORDER(i,m,b,r)
    !*****************************************************************************80
    !
    !! HALTON computes an element of a Halton sequence.
    !  Parameters:
    ! i is the number of the element of the sequence
    ! m is the size (dimension) of the sequence
    ! b(m) is the prime order to use as base for the sequence
    ! r(m) is the i'th element of the m-dimensional sequence
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: i,m,b(:)
    REAL(8), INTENT(OUT) :: r(:)

    INTEGER :: d,il,j,pr,t(m)
    REAL(8) :: prinv(m)

    t = ABS ( i )
    !
    !  Carry out the computation.
    !
    DO il = 1, m
       pr=PRIME(b(il))
       prinv(il) = 1.0D0 / DBLE(pr)
    END DO
    r=0.0d0
    DO WHILE (any(t.NE.0))
       DO j=1,m
          pr=PRIME(b(j))
          d=MOD(t(j),pr)
          r(j)=r(j)+(DBLE(d)*prinv(j))
          prinv(j)=prinv(j)/DBLE(pr)
          t(j)=(t(j)/pr)
       END DO
    END DO
    RETURN
  END SUBROUTINE HALTON_ORDER

  REAL(8) FUNCTION Sample_Normal_Provide_Uniform(mu,var,uni)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: mu,var,uni
    Sample_Normal_Provide_Uniform = P_CDF_Normal_Inverse_Ran(uni,mu,var)
  END FUNCTION Sample_Normal_Provide_Uniform

END MODULE RANDOM
