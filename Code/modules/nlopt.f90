module nlopt
    use iso_c_binding, only: c_double, c_int, c_char
    implicit none
    private
    public :: minimize, control_type, NLOPT_GN_ISRES, NLOPT_LN_SBPLX, NLOPT_LN_PRAXIS, NLOPT_LN_NELDERMEAD, NLOPT_LN_NEWUOA, NLOPT_LN_COBYLA
    include 'nlopt.f'
    type :: control_type
        integer(c_int) :: algorithm, converged, maxevals
        real(c_double) :: tol, xtol, ftol, maxtime
        real(c_double), allocatable, dimension(:) :: ub, lb
    end type control_type

contains
    subroutine minimize(f,  params, fun_data, control)
        external f
        real(c_double), intent(inout) :: params(:)
        real(c_double), intent(in) :: fun_data(:,:)
        real(c_double) :: minf, maxtime
        type(control_type), intent(inout) :: control
        integer*8 opt
        integer(c_int) :: n, ires, nevals, maxevals
        real(c_double), allocatable :: grad(:), lb(:), ub(:)
        ! interface
        !     subroutine f(val, n , x, grad, need_grad, f_data)
        !         use iso_c_binding, only: c_int, c_double
        !         integer(c_int):: n, need_grad
        !         real(c_double):: val, f_data(:,:), x(n), grad(n)
        !     end subroutine f
        ! end interface

        n=size(params)
        ! allocate(x(n))
        allocate(grad(n))
        
        opt=0
        ! print*, "fun_data minimize routine", shape(fun_data)
        ! print*, "calling f in minimize", minf
        ! call f(minf,n,params,grad,0,fun_data(:,:))
        call nlo_create(opt,control%algorithm,n)
        ! write(*,*) "opt is ", opt
        if (opt==0) error stop "problem in creating nlo"

        if (control%algorithm==NLOPT_GN_ISRES) then
            allocate(lb(size(control%lb)), ub(size(control%ub)))
            call nlo_get_lower_bounds(ires,opt,lb)
            if (ires<0) error stop "Unsuccessful getting bounds"
            lb=control%lb
            call nlo_set_lower_bounds(ires,opt,lb)
            if (ires<0) error stop "Unsuccessful setting lower bounds"
            call nlo_get_lower_bounds(ires,opt,lb)
            write(*,"(a,*(G0.6,:,','))") "lb are ",lb
            
            call nlo_get_upper_bounds(ires,opt,ub)
            if (ires<0) error stop "Unsuccessful getting upper bounds"
            ub=control%ub
            call nlo_set_upper_bounds(ires,opt,ub)
            if (ires<0) error stop "Unsuccessful setting upper bounds"
            call nlo_get_upper_bounds(ires,opt,ub)
            write(*,"(a,*(G0.6,:,','))") "ub are ",ub

        end if

        ! call nlo_get_maxeval(maxevals,opt)
        ! maxevals=control%maxevals
        ! call nlo_set_maxeval(ires,opt, maxevals)
        ! if (ires<0) error stop "problem in setting maxevals nlo"

        
        call nlo_set_min_objective(ires,opt,f,fun_data)
        ! write(*,*) "ires is ", ires
        if (ires<0) error stop "problem in setting min nlo"
        
        call nlo_get_maxtime(maxtime,opt)
        maxtime=control%maxtime
        call nlo_set_maxtime(ires,opt, maxtime)
        if (ires<0) error stop "problem in setting maxtime nlo"

        call nlo_set_xtol_rel(ires, opt, control%tol)
        if (ires<0) error stop "problem in setting xtol nlo"

        ! call nlo_set_ftol_rel(ires, opt, control%tol**2.0D0)
        ! if (ires<0) error stop "problem in setting ftol nlo"

        call nlo_set_xtol_abs(ires, opt, control%tol)
        if (ires<0) error stop "problem in setting xtol abs nlo"

        ! call nlo_set_ftol_abs(ires, opt, control%tol**2.0D0)
        ! if (ires<0) error stop "problem in setting ftol abs nlo"

        call nlo_optimize(ires,opt,params,minf)
        ! if (opt.ne.0) error stop "problem optimizing nlo"
        if (ires.lt.0) then
            nevals=0
            call nlo_get_numevals(nevals, opt)
            write(*,*) 'num of evaluations', nevals
            write(*,*) 'nlopt failed! ires is ',ires
            control%converged=0
         else
            write(*,*) 'nlopt succeeded! ires is ',ires
            write(*,"(a,*(G0.6,:,','))") 'found min at ', params
            write(*,*) 'min val = ', minf
            control%converged=1
         endif
        
        call nlo_destroy(opt)
        
    end subroutine minimize
end module