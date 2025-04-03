!-----------------------------------------------------------------------
!##################################BEGIN GN#############################
! variables and parameters of GN that might need to be accessed
MODULE GN_MOD
  IMPLICIT NONE
  ! stptol in: step size for relative convergence test.
  ! reltol, abstol in: value relative/absolute convergence test.
  !    Setting accuracies too small wastes evaluations.
  !    Setting accuracies too large may not get the best solution.
  ! derivstp in: the step for derivatives.
  !    Must be large enough to get some change in the function.
  !    Must be small enough for some accuracy in the derivative.
  ! iprint in: degree of printout, if value is:
  !    0=none, 1=final f0 and x0, 2=iterations, 3=each x tried,
  !    4=all except Jacobian and Hessian, 5=all.
  ! ihist in: if > 0, write history file HistFile using unit ihist
  ! limit  in: maximum number of all evaluations allowed, approximate.
  ! tuning constants:
  !    ZLOW,ZHIGH change bounds of trust region (del) (.5,2).
  !    Small changes in the path set by them greatly affect results.
  !    ZCP Cauchy step size (1).
  !    ZCPMIN minimum Cauchy step size (0.1).
  !    ZCPMAX maximum Cauchy step size (1000).
  !    MXBAD number bad steps with rank-1 updates (1).
  ! NewtStep1st=.true. start with Newton step. Use on linear problems.
  ! mu0,del0,r0,H0,Jac0,iHOOKmax out: final values
  ! dfj/dxi=Jac0(j,i)
  ! iHOOKmax is max number of iterations in hook search
  ! iscale=0, no scaling
  ! iscale=1, variable scaling
  ! iscale=2, fixed scaling based on D0, which must be allocated
  !  and filled by user before call to GN
  
  ! FIXED PARAMETER
  REAL(8) :: ONE=1.0d0
  ! OUTPUT INFORMATION
  INTEGER, save :: iHOOKmax,iter,nconsecMax,nfcn
  REAL(8), save :: del0,hnorm,mu0
  REAL(8), save, dimension(:), allocatable :: D0,r0
  REAL(8), save, dimension(:,:), allocatable :: H0,Jac0
  ! USER PARAMETERS
  CHARACTER(len=30), save :: HistFile='GNHIST.TXT'
  LOGICAL :: NewtStep1st=.false.
  INTEGER :: ihist=0,iprint=0,iscale=0,limit=10000
  REAL(8) :: abstol=1d-11,derivstp=1d-4,reltol=3d-7,stptol=1d-8
  ! GN TUNING CONSTANTS
  INTEGER :: MXBAD=1
  REAL(8) :: ZCONSEC=1.005d0,ZHIGH=2d0,ZLOW=0.65d0
  REAL(8) :: ZCP=.6d0,ZCPMAX=1000,ZCPMIN=0.18d0

CONTAINS
  
  !-----------------------------------------------------------------------
  !+augmented Gauss-Newton nonlinear least squares
  SUBROUTINE GN(fcn,m,n,info,x0,f0)
    ! AUTHORS: Kenneth Klare (kklare@gmail.com)
    !      and Guthrie Miller (guthriemiller@gmail.com)
    ! OPEN SOURCE: may be freely used, with author achnowledgment,
    !    for any peaceful, beneficial purpose
    ! PURPOSE: Minimize sum of squares of residuals r using
    !    augmented Gauss-Newton step and Levenberg-Marquardt trust region.
    !    Uses finite-difference derivatives and 1-D Jacobian updates.
    ! KEYWORDS: Nonlinear Least squares, nonlinear minimization, robust,
    !           optimization, finite-difference derivatives,
    !           augmented Gauss-Newton, Levenberg-Marquardt.
    ! ARGUMENTS:
    ! fcn    in: A user FUNCION to compute r(1:m) from x(1:n).
    !    It is invoked: call FCN(m,n,x,r), where
    !    x      in. n parameters to evaluate residuals.
    !    r      out. m residuals to be reduced. DO NOT SQUARE THEM.
    !    It must not change m, n, nor x.
    !    Scale the output residuals:
    !       r(i)=(fit(x,i)-y(i))/yerror(i), where y is the data.
    !    To reject the given x, set the r very large, like sqrt(largest).
    !    The starting point (x0) must valid and not be rejected.
    !    An iteration has function and derivative evaluations.
    !    Updates use function evaluations only.
    !    The program is designed to minimize evaluation count.
    ! m      in: number of equations or fitting residuals, the size of r0.
    ! n      in: number of variables, the size of x0.
    !    Program handles overdetermined, m>n, and underdetermined, m<n,
    !    as well as linear and nonlinear equations (m=n).
    ! info   out: status sum of bits (LT 128) or single error code (GE 128)
    !    Notation: pT q=transpose of p times q, ||q||=norm(q)=sqrt(qT q).
    ! CONVERGENCES
    !     1 step less than tolerance (stptol).
    !     2 rT r relative improvement less than tolerance (reltol).
    !     8 rT r sum of squares less than absolute tolerance (abstol).
    ! FATAL ERRORS
    !    128 input error.
    !    129 Jacobian is zero--cannot proceed with unchanging function.
    !    130 evaluation limit exceeded without termination.
    !    133 hook search error: could not find a suitable mu in 30 tries.
    ! x0     in/out n-vector: initial guess and best position found.
    ! f0     out scalar: final sum of squares. Think chi-squared.
    ! ACCESSIBLE in GN_MOD:
    ! r0     out m-vector: final residuals at x0
    ! iter   out scalar: actual number of iterations taken
    ! nfcn   out scalar: actual number of function evaluations used
    ! mu0    out scalar: the "best" L-M parameter used
    ! del0   out scalar: the "best" trust region size
    ! D0     out n-vector: scale factor, input for iscale=2
    ! H0     out n-by-n matrix: the unaugmented Hessian
    ! Jac0   out m-by-n array: the "best" Jacobian
    ! LIMITATIONS:
    !    Strict descent limits use to reproducible data.
    !    x0, r0, and others must be 1-D vectors, not arrays.
    !    Large m and n will require large storage and round-off errors.
    !    Your residuals should not exceed sqrt(HUGE/m) to avoid overflows.
    ! RECOMMENDATIONS:
    !    Use double-precision calculation, REAL(8), if possible.
    !    Single precision can do 3 to 4 digits in x, at best.
    !    Give a good guess or you may find a secondary minimum.
    !    Scale parameters and residuals, so chi-square and relative
    !    errors are meaningful.
    ! CREDITS:
    !     * Dennis and Schnabel, "Numerical Methods for Unconstrained
    !     Optimization and Nonlinear Equations" (1983),
    !     for many ideas and the hook calculation, i.e., mu from phi.
    !     * IMSL ZXSSQ for the Jacobian update attributed in
    !     Fletcher, "Practical Methods of Optimization" (1980)
    !     to Barnes (1965) and Broyden (1965).
    !     * x(+) = x-(JacT Jac+mu I)**-1 JacT r augmented Gauss-Newton.
    !     * See this for allocations and vector operators in F90.
    !     <http://www.stanford.edu/class/me200c/tutorial_90/07_arrays.html>
    !     * See "Fortran90 for Fortran77 Programmers" by Clive Page
    !     <http://www.star.le.ac.uk/~cgp/f90course/f90.html> 11/26/2001
    ! DYNAMICALLY ALLOCATED:
    ! Jac  scratch, m-by-n array: Jacobian of derivatives w.r.t. variables.
    ! H    scratch, n-by-n array: JacT Jac Hessian without 2nd order derivs.
    ! L    scratch, n-by-n array: lower-triangle factor, L LT = H+mu*I.
    ! r    scratch, m vector of residuals. ChiSq=Sum(rT r).
    ! D,g,sN,s,x   scratch, n vectors.
    !    They are scale factor, gradient, Newton step, step tried, position.
    !    You must scale by scaling factors D.
    !    Use dfj/dxi=Jac(j,i)/D(i) H(i,j)/D(i)/D(j) g(j)/D(j) sN(j)*D(j).
    ! INTERFACE:
    IMPLICIT NONE
    INTERFACE
       SUBROUTINE FCN(m,n,x,r)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: m,n
         REAL(8), INTENT(IN) :: x(n)
         REAL(8), INTENT(OUT) :: r(m)
       END SUBROUTINE FCN
    END INTERFACE
!    external GN_CHOL,GN_LSOLVE
    INTRINSIC abs,epsilon,matmul,max,min
    INTRINSIC minval,sign,size,sqrt,sum,transpose
    INTEGER :: info,m,n
    REAL(8) :: f0,GN_CauchyDist_0,x0(n)
    ! INTERNAL:
    LOGICAL :: hook,take,done
    INTEGER :: i,iHOOK,j,nbad,nrank1,nconsec
    REAL(8) :: del,delFac,EPS,epsn12,f,sfp
    REAL(8) :: gnorm,hold,lastdel,mu,mulow,muhigh,muStart
    REAL(8) :: phi,phip,rr,scal,scal0,snewt,snorm,step,temp
    ! DYNAMIC:
    REAL(8), dimension(:,:), allocatable :: Jac,H,L
    REAL(8), dimension(:), allocatable :: D,g,r,s,sN,x

    if(min(limit,m,n).le.0) then
       write(*,*)'INPUT ERROR limit,m,n',limit,m,n
       info=128
       return
    endif
    ! For Fortran90 or better, allocate module arrays and local arrays.
    ! To avoid fragmentation of the stack: allocate globals, then locals,
    ! deallocate locals in reverse order, then globals in reverse order.
    ! Globals remain allocated until program is run next or you do it.
    ! Deallocate/allocate global arrays in GN_MOD.
    
    !  with iscale=2, user needs to include the statements:
    !      use GN_MOD, only:D0
    !      if(allocated(D0)) deallocate(D0)
    !      allocate(D0(n))
    !      D0=...
    !  before calling GN.
    !  Scale vector D0 needs to have all n elements positive.
    if(iscale.eq.2) then
       if(.not.allocated(D0)) then
          info=128
          write(*,*)'INPUT ERROR scale vector D0 not allocated'
       elseif(size(D0).lt.n.or.minval(D0).le.0) then
          info=128
          write(*,*)'INPUT ERROR bad scale vector D0',D0
       endif
    endif
    if(info.eq.128) return
    if(iscale.ne.2) then
       if(allocated(D0)) deallocate(D0)
       allocate(D0(n))
    endif
    if(allocated(Jac0)) deallocate(Jac0)
    if(allocated(H0)) deallocate(H0)
    if(allocated(r0)) deallocate(r0)
    allocate(r0(m),H0(n,n),Jac0(m,n))
    ! Allocate local arrays.
    allocate(Jac(m,n),H(n,n),L(n,n))
    allocate(r(m),D(n),g(n),s(n),sN(n),x(n))
    EPS=epsilon(ONE)
    epsn12=sqrt(n*EPS)
    x=x0
    D=1
    if(iscale.eq.2) D=D0(:n)
    nfcn=0
    nrank1=n
    hook=.false.
    take=.false.
    nconsec=0
    nconsecMax=0
    H=0
    if(ihist.gt.0) then
       open(ihist,file=HistFile,access="append")
       write(ihist,*)'iter,f,x'
    endif
    if(iprint.ge.4) then
       write(*,*)
       write(*,*)'GN: new Problem: m,n,NewtStep1st=',m,n,NewtStep1st
       write(*,*)'derivstp,stptol,reltol,abstol',derivstp,stptol,reltol,abstol
       if(iscale.eq.2) write(*,*) 'fixed scale D based on user D0',D
    endif
    
    ! REPEAT UNTIL info.ne.0 and have a new Jacobian.
    ! Do not indent main loop to keep width smaller---------------------
    MainLoop: do iter=1,limit ! termination on nfcn >= limit
       info=0
       nfcn=nfcn+1
       CALL FCN(m,n,x,r)
       f=sum(r**2)
       if(iter.gt.1) then
          take=f.lt.f0
          if(take) then
             nconsec=nconsec+1
             nconsecMax=max(nconsecMax,nconsec)
          else
             nconsec=0
          endif
       endif
       if(iprint.ge.4) write(*,*)
       if(iprint.ge.2) write(*,*) 'after call FCN: iter,nfcn,f,info,take',iter,nfcn,f,info,take
       if(iprint.ge.3) write(*,*) 'x',x
       if(iprint.ge.4) write(*,*) 'r',r
       if(iter.eq.1) then
          take=.true.
       elseif(iter.eq.2.and.NewtStep1st.and..not.take) then
          ! CAUCHY STEP is distance to Cauchy point in Newton direction.
          ! Here if Newton step didn't work.
          hook=.false.
          del=GN_CauchyDist(n,L,g,gnorm)
       else
          ! UPDATE TRUST REGION.
          ! Strict descent requires f<f0.
          ! Parabolic interpolation/extrapolation using f0, f and slope at x0.
          ! Update del to be distance to minimum, new del is delFac times old.
          lastdel=del
          if(abs(f-f0).le.max(reltol*max(f,f0),abstol)) info=2
          ! sfp is the step length s times the slope fp of f in the direction s.
          ! it would be the change in f for negligible curvature.
          ! sfp is negative because the step is in a decreasing direction.
          sfp=2*sum(g*s)
          rr=(f-f0)/sfp
          ! rr is bounded above by 1, when f < f0 and f behaves linearly,
          ! and unbounded below when f > f0
          delfac=0.5/max(1-rr,EPS)
          del=snorm*min(max(delFac,ZLOW),ZHIGH)
          ! we also increase trust radius based on number of consecutive takes.
          ! Test problem #36 illustrates the need for this.
          if(iprint.ge.4)then
             write(*,*)'Update trust region del,lastdel,rr,delFac',del,lastdel,rr,delFac
             if(nconsec.gt.0) write(*,*)' nconsec, ZCONSEC**nconsec',' Newdel=',nconsec,ZCONSEC**nconsec,del*ZCONSEC**nconsec
          endif
          del=del*ZCONSEC**nconsec
       endif
       
       if(take) then
          ! SAVE THE BEST.
          if(iprint.ge.4) write(*,*) 'saving best'
          nbad=0
          x0=x
          f0=f
          mu0=mu
          del0=del
          D0=D
          H0=H
          Jac0=Jac
          if(ihist.gt.0) write(ihist,*) iter,f0,x0
       elseif(iter.ne.2.or..not.NewtStep1st) then
          ! Overshot the mark. Try, then get a new Jacobian.
          hook=.true.
          if(nrank1.gt.0) nbad=nbad+1
          if(iprint.ge.4) write(*,*)'Overshot nrank1,nbad',nrank1,nbad
       endif
       
       ! Some convergence tests.
       if(iter.gt.1) then
          if(snorm.le.stptol) info=info+1
       endif
       if(f.lt.abstol) info=info+8
       done=(info.ne.0.and.nrank1.eq.0).or.info.ge.8
       if(.not.done.and.nfcn.ge.limit) info=130
       if(iter.gt.1.and.(done.or.info.ge.128)) exit MainLoop
       
       ! ---Jacobian dfi/dxj, gradient g=JacT r, Hessian H=JacT Jac+2nd order
       ! Jacobian update when not stale nor final, else a full Jacobian.
       if(nbad.le.MXBAD.and.info.eq.0.and.nrank1.lt.n) then
          if(.not.take) goto 40
          ! Rank-1 update to Jacobian. Jac(new) = Jac+((r-r0-Jac s) sT)/(sT s).
          if(iprint.ge.4) write(*,*) 'Rank=1 Jacobian update: nrank1,nbad,snorm',nrank1,nbad,snorm
          nrank1=nrank1+1
          Jac=Jac+matmul(reshape(r-r0-matmul(Jac,s),(/m,1/)),reshape(s/snorm**2,(/1,n/))) !outer product
          if(take) r0=r
       else
          ! Full Jacobian.
          ! Step away from zero to avoid crossing it.
          if(iprint.ge.4) write(*,*)'Full Jacobian: nrank1,nbad,take',nrank1,nbad,take
          nrank1=0
          nbad=0
          if(take) r0=r
          nfcn=nfcn+n
          if(iscale.eq.1) then
             ! variable scale
             scal0=sum((s*D)**2)
             D=max(0.5*(abs(x0)+D),stptol)
             if(iprint.ge.4) write(*,*) 'variable scale D',D
             scal=sum((s*D)**2)
             if(scal.gt.0) del=del*sqrt(scal0/scal)
          endif
          do j=1,n
             hold=x0(j)
             step=sign(derivstp,hold)
             x0(j)=x0(j)+step*D(j)
             CALL FCN(m,n,x0,r)
             Jac(:,j)=(r-r0)/step
             x0(j)=hold
          enddo
       endif
       
       ! Gradient and Hessian.
       g=matmul(transpose(Jac),r0)
       H=matmul(transpose(Jac),Jac)
       if(iprint.ge.5) then
          do i=1,m
             write(*,*) 'J',Jac(i,:)
          enddo
          do j=1,n
             write(*,*) 'H',H(j,:)
          enddo
       endif
       gnorm=sqrt(sum(g**2))
       if(iprint.ge.4) write(*,*) 'gnorm,g',gnorm,g
       ! L1 norm (max of row sums of abs) of H(i,j).
       ! H=JacT Jac symmetric.
       hnorm=0
       do j=1,n
          hnorm=max(hnorm,sum(abs(H(:,j))))
       enddo
       ! Get a small number for further augmentation, check underflow.
       hnorm=hnorm*epsn12
       if(hnorm.eq.0) then
          info=129
          exit MainLoop
       endif
       ! Find bad rows and ignore them.
       do j=1,n
          if(H(j,j).le.0) then
             H(j,:)=0
             H(:,j)=0
             H(j,j)=1
          endif
       enddo
       
       ! --- GET NEWTON STEP, H sN = -g ---
       mu=0
       ! solve (H + mu I)s = g for step s, possibly augmenting diagonal of H
       call GN_CHOL(n,mu,H,g,L,sN,hnorm)
       snewt=sqrt(sum(sN**2))
       if(snewt.le.del) hook=.false.
       if(iprint.ge.4) write(*,*) 'snewt,mu,sN',snewt,mu,sN
       
       if(iter.eq.1) then
          if(NewtStep1st) then
             ! Try Newton step first.
             del=snewt
             s=sN
             snorm=snewt
             if(iprint.ge.4) write(*,*)'Taking Newton step first'
             goto 100
          else
             ! more conservative approach, try Cauchy step first
             hook=.false.
             del=GN_CauchyDist(n,L,g,gnorm)
          endif
       endif
       
40     continue ! --- CHOOSE NEWTON OR HOOK STEPS -------------------
       if(.not.hook) then
          ! Allow Newton step to be up to the current trust radius del
          temp=1
          if(snewt.gt.del) temp=del/snewt
          s=sN*temp
          snorm=snewt*temp
          if(iprint.ge.4) write(*,*) 'Step in Newton direction:',' snewt,del,snorm',snewt,del,snorm
       else
          ! --- Hook search ---
          ! Find step of length del by finding mu
          ! that gives snorm = ||s|| = ||(H + mu I)**-1 g|| = del.
          ! Because del = ||(H + mu I)**-1 g|| and H is positive,
          ! del is less than ||g||/mu.
          muhigh=gnorm/del
          mulow=0
          ! mulow <= mu <= muhigh
          muStart=mu
          ! REPEAT UNTIL abs(del-||s||)<.05 or mulow>muhigh.
          HookLoop: do iHOOK=1,30
             call GN_CHOL(n,mu,H,g,L,s,hnorm)
             mulow=max(mulow,mu)
             snorm=sqrt(sum(s**2))
             if(abs(snorm-del).le.0.05d0*del.or.mulow.ge.muhigh) exit HookLoop
             phi=snorm-del
             ! phi<0 indicates mu is too large, use this mu to update muhigh
             if(phi.lt.0) muhigh=min(mu,muhigh)
             if(phi.gt.0) mulow=max(mu,mulow)
             ! Want sT (H + mu I)**-1 s = ||L**-1 s||**2.
             ! Start with x = L**-1 s.
             call GN_LSOLVE(n,L,x,s)
             phip=0
             if(snorm.gt.0) phip=-sum(x**2)/snorm
             ! As mu increases the step size decreases, so phip must be negative.
             if(phip.lt.0) mu=mu-(snorm/del)*(phi/phip)
             mu=max(min(mu,muhigh),mulow)
          enddo HookLoop
          ! End of REPEAT.
          if(iprint.ge.4.or.iHOOK.ge.30) then
             write(*,*) 'iHOOK,iHOOKmax,muStart,mu,lastdel,del,snorm',iHOOK,iHOOKmax,muStart,mu,lastdel,del,snorm
             info = 133
             if(iHOOK.ge.30) exit MainLoop
          endif
          iHOOKmax=max(iHOOKmax,iHOOK)
       endif
       
100    continue ! TAKE THE STEP.
       if(iprint.ge.4) write(*,*) 'Taking step s',s
       x=x0+s*D
    enddo MainLoop ! termination on nfcn >= limit
    if(iprint.ge.1) then
       write(*,'(" iter m n nf f info take",i4,2i3,i5,1pg21.14,i4,L2)') iter,m,n,nfcn,f,info,take
       write(*,*) 'x0',x0
    endif
    ! memory freed in reverse order from borrowed as in next line.
    deallocate(x,sN,s,g,D,r,L,H,Jac)
    if(iprint.ge.4) then
       write(*,*)'GN: finished Problem'
       write(*,*)
    endif
  end subroutine GN
  !-----------------------------------------------------------------------
  !+Cauchy distance is distance along gradient to minimum.
  ! Cauchy step is Cauchy distance in the Newton direction.
  ! Cauchy distance = -||g||**2/(gT H g) ||g||, gT H g = ||LT g||**2.
  ! Use ZCP * Cauchy distance.
  ! ||v|| = sqrt(vT v) is L2 or Euclidean norm of vector v.
  REAL(8) function GN_CauchyDist(n,L,g,gnorm)
!    use GN_MOD, only: ZCP,ZCPMIN,ZCPMAX,iprint
    implicit none
    intrinsic max,min,sum
    REAL(8) :: L(n,n),g(n),gnorm,del,temp
    INTEGER :: j,n
    if(gnorm.eq.0) then
       del=ZCPMIN
       return
    endif
    temp=0
    ! calculate temp = gT H g/||g||**2 = ||LT g||**2/||g||**2
    do j=1,n
       temp=temp+(sum(L(j:,j)*g(j:))/gnorm)**2
    enddo
    temp=ZCP*gnorm/temp
    del=max(ZCPMIN,min(temp,n*ZCPMAX))
    if(iprint.ge.4) write(*,*) 'Cauchy step,del,stpmax',temp,del,n*ZCPMAX
    GN_CauchyDist=del
  end function GN_CauchyDist
  !-----------------------------------------------------------------------
  !+Cholesky decomposition and solution, (H + (mu+add)*I) s = -g.
  ! Decomposition to lower triangle L without addition if it works.
  ! Break as soon as OK.
  subroutine GN_CHOL(n,mu,H,g,L,s,add)
    implicit none
    intrinsic sqrt,min,sum
    INTEGER :: n,i,iadd,j
    REAL(8) :: add,mu,tmp,H(n,n),g(n),L(n,n),s(n)
    loop1: do iadd=1,n
       do j=1,n
          do i=j,n
             L(i,j)=H(j,i)-sum(L(i,:j-1)*L(j,:j-1))
          enddo
          tmp=L(j,j)+mu
          if(tmp.le.0) then
             mu=mu+add
             cycle loop1
          endif
          tmp=sqrt(tmp)
          L(j,j)=tmp
          L(j+1:,j)=L(j+1:,j)/tmp
       enddo
       exit
    enddo loop1
    ! forward row reduction and backward substitution
    do i=1,n
       s(i)=(-g(i)-sum(L(i,:i-1)*s(:i-1)))/L(i,i)
    enddo
    do i=n,1,-1
       s(i)=(s(i)-sum(L(i+1:,i)*s(i+1:)))/L(i,i)
    enddo
  end subroutine GN_CHOL
  !-----------------------------------------------------------------------
  !+solve L x = y. x and y may be the same vector, L lower triangle.
  subroutine GN_LSOLVE(n,L,x,y)
    implicit none
    intrinsic sum
    INTEGER :: n,i
    REAL(8) :: L(n,n),x(n),y(n)
    do i=1,n
       x(i)=(y(i)-sum(L(i,:i-1)*x(:i-1)))/L(i,i)
    enddo
  end subroutine GN_LSOLVE
  !###############################END GN##################################
  !-----------------------------------------------------------------------

END MODULE GN_MOD
