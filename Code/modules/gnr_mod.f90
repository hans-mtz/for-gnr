!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! MODULE: GNR
!
!> @author
!> Hans Martinez}
!
! DESCRIPTION: 
!> GNR estimates gross output production function and productivity following GNR(2020)
!
! REVISION HISTORY:
! dd Mmm yyyy - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------
module gnr_mod
    use iso_c_binding, only: c_float, c_double, c_int, c_char
    use ieee_arithmetic
    use nrutil, only: mmul, assert_eq
    use stats!, only: .div., mean
    use matrix, only: OLS!, matrix_inverse_Cholesky
    use globvar, only: verbose!, lag_data
    ! use minimization
    ! use annealing
    implicit none
    intrinsic :: findloc

    private
    public :: gnr, gnr_CD
    type non_flex_type(n_coef,n_obs)
        integer(c_int), len :: n_coef, n_obs
        real(c_double) :: coeff(n_coef) , predict(n_obs), resid(n_obs)
    end type non_flex_type

    type, extends(non_flex_type) :: flex_type!(n_coef,n_obs)
        ! integer(c_int), len :: n_coef, n_obs
        real(c_double) :: lag_resid(n_obs)
    end type flex_type
    
contains

    function estimate_productivity_NLOPT(y, D, Z,id,time, flex_result) result(omega_out)
        ! use simplex
        ! use minimization
        use nlopt
        use globvar, only: script_Y
        implicit none
        real(c_double), intent(in) :: D(:,:), y(:), Z(:,:), id(:), time(:)!, script_Y(:)
        ! character(kind=c_char,len=*), intent(in) :: vars_names(:), flex_name
        type(flex_type(n_coef=*, n_obs=*)), intent(in) :: flex_result
        type(non_flex_type(n_coef=:, n_obs=:)), allocatable :: omega_out
        real(c_double) :: alpha_0(size(Z,2)+1), alpha_1(size(Z,2)),log_omega(size(Z,1)),&
            & g_data(size(Z,1),size(Z,2)+3)!, script_Y(size(Z,1))
        type(control_type) :: control
        ! external min_obj_fun_markov_nlopt


        ! Compute script_Y
        ! script_Y = compute_script_Y(y, D, vars_names, flex_name, flex_result)
        ! assuming lag vars are in same order than non-lagged vars
        ! lag_script_Y = compute_script_Y(lag_y, lag_D, vars_names, flex_name, flex_result, .true.) 

        if (verbose) call print_stats(script_Y,"Script Y")
        ! if (verbose) call print_stats(lag_script_Y, "Lag Script Y")
        ! set initial values
        if (size(Z,1).ne.size(script_Y)) error stop "Z and y not same size"
        alpha_0 = OLS(Z,script_Y)
        if (verbose) write(*,*) "OLS alpha_0 is ", alpha_0
        if (verbose) print*, "size alphas, ", size(alpha_1), size(alpha_0)
        alpha_1(:) = alpha_0(2:) !No constant

        ! val=min_obj_fun_markov(alpha_0(2:))
        ! if (verbose) write(*,*) "Obj Fun Markov initial val ",val
        g_data(:,1)=id
        g_data(:,2)=time
        g_data(:,3)=script_Y
        g_data(:,4:)=Z

        if (verbose) print*, "g_data estimate", shape(g_data)!, g_data(1:9,1:3)

        ! control%algorithm=NLOPT_GN_ISRES
        ! control%tol=1.0D-4
        ! control%converged=0
        control%maxtime=60*10 !seconds
        ! control%maxevals=300
        ! allocate(control%ub(size(alpha_1)),control%lb(size(alpha_1)))
        ! control%ub=maxval([maxval(alpha_1),1.0D0])
        ! control%lb=minval([minval(alpha_1),-1.0D0])
        ! print*, "Global optimizaion"
        ! call minimize(min_obj_fun_markov_nlopt, alpha_1, g_data, control)

        ! if (control%converged==0) alpha_1(:) = alpha_0(2:) !Global optimization not converged
        
        control%algorithm=NLOPT_LN_SBPLX
        control%tol=1.0D-6
        control%converged=0
        print*, "Local optimizaion"
        call minimize(min_obj_fun_markov_nlopt, alpha_1, g_data, control)
        ! call NELDER_MEADE(alpha_1,1.0d-8,min_obj_fun_markov,1,100*50)
        ! call NELDER_MEADE(alpha_1,1.0d-8,min_obj_fun_markov,1,100*50)
        ! call BFGS(alpha_1,1.0d-6,1.0d-6,min_obj_fun_markov,0,verbose)
        ! call BFGS(alpha_1,1.0d-8,1.0d-8,min_obj_fun_markov,0,verbose)
        if (verbose) write(*,*) "OUT alpha_1 is ", alpha_1
        ! val=min_obj_fun_markov(alpha_1)
        ! if (verbose) write(*,*) "Obj Fun Markov out val ",val

        allocate(non_flex_type(size(alpha_1),size(script_Y)) :: omega_out)
        omega_out%coeff=alpha_1
        ! Productivity is defined as exp(omega+errors)
        ! omega = script_Y-C(k,l)
        log_omega=script_Y-mmul(Z,alpha_1)
        if (verbose) call print_stats(log_omega, "log_omega")
        if (verbose) call print_stats(flex_result%resid, "flex_result %resids")
        omega_out%resid=exp(log_omega+flex_result%resid)


    end function estimate_productivity_NLOPT

    pure function adjust_coeff(strings, share_name,derive) result(vect_out)
        implicit none
        character(kind=c_char, len=*), intent(in) :: strings(:), share_name
        logical, intent(in), optional :: derive
        integer(c_int) :: count, i, len, pos
        real(c_double) :: vect_out(size(strings))
        do i = 1, size(strings)
            pos = 1
            len = len_trim(strings(i))
            if (present(derive) .and. .true.) then
                count = 0
            else
                count = 1
            endif
            do while (pos <= len)
                if (strings(i)(pos:pos) == trim(share_name)) then
                    count = count + 1
                    pos = pos + 1
                else
                    pos = pos + 1
                end if
            end do
            if (present(derive) .and. derive) then
                vect_out(i)=real(count)
            else
                vect_out(i)=1.0d0/real(count)
            endif 
        end do
    end function adjust_coeff

    function expand_matrix(X, degree) result(Y)
        implicit none
        real(c_double), intent(in) :: X(:,:)
        integer(c_int), intent(in) :: degree
        real(c_double), allocatable :: Y(:,:)
        integer(c_int) :: n, m, i, j, k, count, counter
        
        n = size(X, 1)
        m = size(X, 2)

        ! write(*,*) "Matrix inside", n,m
        count = 1
        
        ! Calculate the number of columns in the expanded matrix
        do i = 1, m
            count = count *i
        end do
        count=count+m

        ! write(*,*) "Expanded matrix will have these columns", count
        ! Allocate memory for the expanded matrix
        allocate(Y(n, count))
        
        ! Fill the expanded matrix with the combinations
        counter = 1
        do i = 1, m
            Y(:, counter) = X(:, i)
            counter = counter + 1
        end do
        ! write(*,*) "DONE with colums 1 to ", counter
        do i = 1, m
            do j = i, m
                do k = 2, degree
                    ! write(*,*) "counter", counter
                    Y(:, counter) = X(:, i) * X(:, j)
                    counter = counter + 1
                end do
            end do
        end do
        ! write(*,*) "DONE with colums 4 to ", counter
    end function expand_matrix

    pure function predict(coefficients, datain) result(predictions)
        implicit none
        real(c_double), intent(in) :: coefficients(:), datain(:, :)
        real(c_double), allocatable :: predictions(:), datapredict(:,:)
        integer(c_int) :: n, m
        integer :: istat
        character(len=60) :: err
        
        n = size(datain, 1)
        m = size(datain, 2)

        ! Allocate memory for the predictions
        allocate(predictions(n), stat=istat, errmsg=err)
        allocate(datapredict(n,m+1), stat=istat, errmsg=err)
        
        ! if (istat == 0) then
        !     print*, "arrays allocated"
        ! else
        !     print *, "Status : ",istat; print *, "Errmsg : ",err
        ! endif
        if (istat .ne. 0)  error stop "Error: Predict could not allocate memory"
        
        ! write(*,*) "Predict OK"
        ! Add a column of ones to the data
        datapredict(:,1)=1.0d0
        datapredict(:,2:)=datain
        
        ! write(*,*) "Coefficients dim", size(coefficients)
        ! write(*,*) "Matrix dims", size(datapredict,1), size(datapredict,2)
        ! Calculate the predictions
        istat=assert_eq(size(datapredict,2),size(coefficients),"predict")
        predictions = mmul(datapredict, coefficients)
        ! deallocate(predictions)
        ! deallocate(datapredict)

    end function predict

    pure function ln_D_E(g,d) result(s)
        implicit none
        
        real(c_double), intent(in) :: g(:), d(:,:)
        real(c_double) :: xb(size(d,1)), s(size(d,1)), epsilon
        integer(c_int) :: i, u

        u=assert_eq(size(d,2)+1,size(g),"ln_D_E")
        xb=predict(g,d)

        epsilon=tiny(xb(1))

        do i = 1, size(xb)
            if (xb(i) < epsilon) then
                ! s(i) = 690.77552789821368151024216786026954650879d0
                s(i) = log(epsilon)
            else
                s(i) = log(xb(i))
            end if
        end do
        ! write(*,*) "Log OK, s size", size(s)
    end function ln_D_E

    ! pure function min_obj_fun(G) result(val)
    !     use globvar, only: D, s
    !     implicit none
        
    !     real(c_double), intent(in) :: G(:)
    !     real(c_double) :: val, s_hat(size(s)), eps(size(s))
    !     ! external ln_D_E

    !     s_hat=ln_D_E(G,D)
    !     eps=s-s_hat

    !     val=mmul(eps,eps)
    !     ! val=norm2(eps)
        
    ! end function min_obj_fun

    subroutine min_obj_fun_nlopt(val,n, x, grad, need_grad, f_data) 
        use iso_c_binding
        use globvar, only: s,D
        implicit none
        integer(c_int) :: n, need_grad
        real(c_double) :: val, x(n), grad(n), f_data(:,:)
        real(c_double), allocatable, dimension(:) :: s_hat, eps!, s(size(f_data,1)), D(size(f_data,1),n-1)
        ! real(c_double), allocatable :: s(:), D(:,:)

        ! if (size(f_data,1)==0) error stop "f_data size is zero"
        ! s=f_data(:,1)
        ! D=f_data(:,2:)
        
        allocate(s_hat(size(D,1))) 
        allocate(eps(size(D,1)))

        if (need_grad.ne.0) then
            print*, "calling the gradient"
            grad=0.0d0
        endif
        ! external ln_D_E

        s_hat=ln_D_E(x,D)
        eps=s-s_hat

        val=mmul(eps,eps)!/size(eps)
        ! val=norm2(eps)
    
    end subroutine min_obj_fun_nlopt

    ! pure function min_obj_fun_markov(alpha) result(retval)
    !     use globvar, only: script_Y,Z,lag_script_Y,lag_Z
    !     use matrix, only: matrix_inverse_Cholesky
    !     implicit none
        
    !     real(c_double), intent(in) :: alpha(:)
    !     real(c_double) :: retval, w(size(Z,1)), lag_w(size(Z,1),3),&
    !         & delta(4), eta(size(Z,1)), moments(size(Z,2)), weight_mat(size(Z,2),size(Z,2))
    !     integer(c_int) :: i,j
    !     ! interface
    !     !     pure function matrix_inverse_Cholesky(A) result(Ainv)
    !     !         real(8), intent(in) :: A(:,:)
    !     !         real(8) :: Ainv(size(A,1),size(A,2))
    !     !     end function
    !     ! end interface

    !     ! integer :: istat
    !     ! character(len=60) :: err=" "
    !     !exp(alpha) !only positive numbers
    !     ! Note: no constant for C(k,l) only for Markov Process of productivity
    !     ! w=delta_0+delta_1 w_t-1+delta_2 w_t-1^2+delta_3 w_t-1^3+eta
    !     w=script_Y-mmul(Z,alpha) ! form omega for a guess of alpha
    !     ! print*, "w size", size(w)
    !     ! print*, "Z size", size(Z,2); print*, "alpha size", size(alpha)
    !     ! print*, "lag_Z size", size(lag_Z,2)
    !     lag_w(:,1)=lag_script_Y-mmul(lag_Z,alpha) ! form omega at t-1 for a guess of alpha
    !     lag_w(:,2)=lag_w(:,1)*lag_w(:,1) ! omega_t-1^2
    !     lag_w(:,3)=lag_w(:,2)*lag_w(:,1) ! omega_t-1^3
    !     ! print*, "lag_w size ", size(lag_w,1), size(lag_w,2)
    !     delta=OLS(lag_w,w) ! Productivity's Markov Process
    !     ! print*, "delta", delta
    !     ! deallocate(eta, stat=istat,errmsg=err)
    !     ! print *, "Status : ",istat; print *, "Errmsg : ",err !NAG compiler
    !     eta=w-predict(delta,lag_w) !getting eta, residuals of Markov Process
    !     ! print*, "eta size", size(eta)
    !     moments=mmul(eta,Z)/real(size(eta))
    !     ! weight_mat=mmul(Z, transpose(Z))
    !     ! print*, "weight matrix, ", weight_mat
    !     ! weight_mat=matrix_inverse_Cholesky(mmul(transpose(Z),Z)/real(size(eta)))
    !     ! FORALL(i=1:size(Z,2),j=1:size(Z,2),i.ne.j) weight_mat(i,j)=0.0d0 !Assuming independence
    !     weight_mat=0.0d0
    !     FORALL(i=1:size(Z,2),j=1:size(Z,2),i==j) weight_mat(i,j)=1.0d0
    !     weight_mat=matrix_inverse_Cholesky(weight_mat)
    !     retval=mmul(moments,mmul(weight_mat,moments))
    !     ! retval=norm2(moments)
    !     ! print*, "retval", retval
    ! end function min_obj_fun_markov

    subroutine min_obj_fun_markov_nlopt(val,n, x, grad, need_grad, obj_f_data) 
        use iso_c_binding
        use globvar, only: lag_data, id, time, script_Y, Z
        ! use matrix, only: matrix_inverse_Cholesky
        ! implicit none     
        integer(c_int) :: n, need_grad
        real(c_double) :: val, x(n), grad(n), obj_f_data(:,:)
        real(c_double) :: w(size(Z,1)), delta(4),  moments(n), &
            & weight_mat(n,n)!, script_Y(size(obj_f_data,1)),&
            !& Z(size(obj_f_data,1),n)!, time(size(obj_f_data,1)), id(size(obj_f_data,1))
        real(c_double), allocatable :: lag_script_Y(:,:), lag_Z(:,:), w_in(:), lag_w(:,:), eta(:), Z_in(:,:)
        integer(c_int) :: i,j
        logical :: mask(size(Z,1))

        if (need_grad .ne. 0) then
            grad=0.0d0 
        end if

        ! if (verbose) print*, "id", size(id),"time", size(time)
        
        ! if (size(obj_f_data,1)==0) error stop "no data"

        ! id = obj_f_data(:,1)
        ! time = obj_f_data(:,2)
        ! script_Y = obj_f_data(:,3)
        ! Z = obj_f_data(:,4:)


        mask=id==eoshift(id,shift=-1) .and. time==(eoshift(time,shift=-1)+1.0d0)

        ! if (verbose) print*, "mask size", count(mask)
        allocate(lag_script_Y(count(mask),1))
        allocate(lag_Z(count(mask),size(Z,2)))
        allocate(lag_w(count(mask),3))
        allocate(eta(count(mask)))
        allocate(Z_in(count(mask),size(Z,2)))

        lag_script_Y= lag_data(script_Y,id,time,1)
        ! if (verbose) print*, "lag_script_Y", size(lag_script_Y,1), size(lag_script_Y,2), shape(lag_script_Y)
        lag_Z=lag_data(Z,id,time,1)
        ! if (verbose) print*, "lag_Z", shape(lag_Z)
        ! call print_stats(lag_Z)

        ! integer :: istat
        ! character(len=60) :: err=" "
        !exp(alpha) !only positive numbers
        ! Note: no constant for C(k,l) only for Markov Process of productivity
        ! w=delta_0+delta_1 w_t-1+delta_2 w_t-1^2+delta_3 w_t-1^3+eta
        w=script_Y-mmul(Z,x) ! form omega for a guess of x
        ! print*, "w size", size(w)
        ! print*, "Z size", size(Z,2); print*, "x size", size(x)
        ! print*, "lag_Z size", size(lag_Z,2)
        lag_w(:,1)=lag_script_Y(:,1)-mmul(lag_Z,x) ! form omega at t-1 for a guess of alpha
        lag_w(:,2)=lag_w(:,1)*lag_w(:,1) ! omega_t-1^2
        lag_w(:,3)=lag_w(:,2)*lag_w(:,1) ! omega_t-1^3
        ! print*, "lag_w size ", size(lag_w,1), size(lag_w,2)
        
        w_in=pack(w,mask)
        ! if (verbose) print*, "w_in", shape(w_in)
        ! if (verbose) print*, "lag_w", shape(lag_w)
        delta=OLS(lag_w,w_in) ! Productivity's Markov Process
        ! if (verbose) print*, "delta", delta
        ! deallocate(eta, stat=istat,errmsg=err)
        ! print *, "Status : ",istat; print *, "Errmsg : ",err !NAG compiler
        eta=w_in-predict(delta,lag_w) !getting eta, residuals of Markov Process
        ! if (verbose) print*, "eta size", size(eta)
        Z_in=reshape(pack(Z,spread(mask,2,size(Z,2))),[count(mask),size(Z,2)])
        moments=mmul(eta,Z_in)/real(size(eta))
        ! weight_mat=mmul(Z, transpose(Z))
        ! if (verbose) print*, "moments, ", moments
        ! weight_mat=matrix_inverse_Cholesky(mmul(transpose(Z),Z)/real(size(eta)))
        ! FORALL(i=1:size(Z,2),j=1:size(Z,2),i.ne.j) weight_mat(i,j)=0.0d0 !Assuming independence
        weight_mat=0.0d0
        FORALL(i=1:n,j=1:n,i==j) weight_mat(i,j)=1.0d0
        ! weight_mat=matrix_inverse_Cholesky(weight_mat)
        val=sqrt(mmul(moments,mmul(weight_mat,moments)))
        ! retval=norm2(moments)
        ! print*, "retval", retval
    end subroutine min_obj_fun_markov_nlopt

    function compute_script_Y(y,D,flex_name,flex_result,lag) result(script_Y_type)
        use globvar, only: select_columns, script_Y, vars_names => vars_names_D
        implicit none
        
        real(c_double), intent(in) :: y(:), D(:,:)
        logical, intent(in), optional :: lag
        character(kind=c_char,len=*) , intent(in) :: flex_name!, vars_names(:)
        type(flex_type(n_coef=*,n_obs=*)), intent(in) :: flex_result
        real(c_double) :: gama_denom(size(D,2)+1),&
            & G_integrated(size(D,1)),gamma_adjusted(size(D,2)+1)
        integer(c_int) :: col
        type(flex_type(n_coef=:,n_obs=:)), allocatable :: script_Y_type
        ! Compute the integral
        ! Get denominator for gamma
        gama_denom=adjust_coeff(vars_names,flex_name)
        if (verbose) write(*,*) "gama_denom ", gama_denom, "vars_names ", vars_names
        col=select_columns(flex_name, vars_names(2:)) !D does not have a constant, vars_names DO
        if (verbose) write(*,*) "Column for m in D", col
        ! Integrate the function
        gamma_adjusted=flex_result%coeff * gama_denom
        ! write(*,*) "gamma adjusted ", gamma_adjusted
        if (verbose) write(*,*) "gamma adjusted size ", size(gamma_adjusted) 
        if (verbose) write(*,*) "D columns", size(D,2)
        G_integrated=predict(gamma_adjusted,D)*D(:,col)
        ! Construct script Y
        if (present(lag) .and. lag) then
            if (verbose) write(*,*) "Lagging the script_Y"
            script_Y=y-flex_result%lag_resid-G_integrated
            if (verbose) call print_stats(script_Y,"Script Y")
        else
            if (verbose) write(*,*) "Non-lagged script_Y"
            script_Y=y-flex_result%resid-G_integrated
        endif

        allocate(flex_type(size(gamma_adjusted),size(script_Y)) :: script_Y_type)

        script_Y_type%predict=G_integrated
        script_Y_type%resid=script_Y
        script_Y_type%coeff=gamma_adjusted
        
    end function compute_script_Y

    function get_elasticities(D,Z,flex_name,non_flex_names,flex_in, omega_in, script_Y_in) result(non_flex_out)
        ! use iso_c_binding, only: c_double, c_char
        ! use iso_varying_string, only: varying_string
        use globvar, only: select_columns, str_contains, vars_names_D, vars_names_Z
        implicit none
        real(c_double), intent(in) :: D(:,:), Z(:,:)
        type(flex_type(n_coef=*,n_obs=*)), intent(in) :: flex_in, script_Y_in
        character(kind=c_char, len=*), intent(in) :: flex_name, non_flex_names(:) !,vars_names_D(:), vars_names_Z(:), 
        type(non_flex_type(n_coef=*,n_obs=*)), intent(in) :: omega_in
        integer :: i, flex_D_col, non_flex_D_col,non_flex_Z_col, n, m, j, k
        real(c_double) :: aux(size(vars_names_D)), aux_G(size(D,1)), aux_Z(size(Z,1)), non_flex_out(size(D,1),size(non_flex_names))
        real(c_double) :: G_adjusted_coeffs(size(vars_names_D),size(non_flex_names)), Z_adjusted_coeffs(size(vars_names_Z),size(non_flex_names))
        real(c_double), allocatable :: aux_G_coeff(:), aux_D(:,:), aux_ZZ(:,:), aux_GG(:,:)
        character(kind=c_char,len=:), dimension(:), allocatable :: select_non_flex_D, select_non_flex_Z
        integer(c_int), allocatable :: non_flex_D_cols(:), non_flex_Z_cols(:)

        if (size(D,1) .ne. size(Z,1)) error stop "D and Z must have the same number of observations"
        
        ! Integrate the flex input coefficients
        ! Adjust coefficients flex input
        ! aux = flex_in%coeff * adjust_coeff(vars_names_D, flex_name)
        ! if (verbose) print*, flex_name, " Adjusted coeffs factor integrate G flex", adjust_coeff(vars_names_D, flex_name)
        flex_D_col = select_columns(flex_name,vars_names_D(2:)) ! D, no constant, names_D do

        ! allocate(non_flex_type(*,*) :: non_flex_out(size(non_flex_names)))
        
        do i = 1, size(non_flex_names)
            ! Derive the non flex input coefficients
            G_adjusted_coeffs(:,i) = script_Y_in%coeff * adjust_coeff(vars_names_D, non_flex_names(i), .true.)
            if (verbose) print*, non_flex_names(i), "Adjusted coeffs factor derivate G ",adjust_coeff(vars_names_D, non_flex_names(i), .true.)
            Z_adjusted_coeffs(:,i) = omega_in%coeff * adjust_coeff(vars_names_Z, non_flex_names(i), .true.)
            if (verbose) print*, "Adjusted coeffs factor derivate Z ",adjust_coeff(vars_names_Z, non_flex_names(i), .true.)
            ! Compute the elasticities

            ! Select variables containing the non-flex input in D and Z
            n=size(str_contains(non_flex_names(i), vars_names_D))
            m=size(str_contains(non_flex_names(i), vars_names_Z))
            ! allocate(flex_type(n_coef=n+m,n_obs=size(D,1)) :: non_flex_out)
            allocate(select_non_flex_D(n), mold=vars_names_D)
            allocate(select_non_flex_Z(m), mold=vars_names_D)
            ! Select variables containing the non-flex input in D and Z
            select_non_flex_D = str_contains(non_flex_names(i), vars_names_D) !array of strings
            select_non_flex_Z = str_contains(non_flex_names(i), vars_names_Z) !array of strings

            if (verbose) print*, "Selected columns for ", non_flex_names(i), " in D ",select_non_flex_D
            if (verbose) print*, "Selected columns for ", non_flex_names(i), " in Z ",select_non_flex_Z
            ! Select # of column in D and Z containing selected variables
            non_flex_D_cols = select_columns(select_non_flex_D, vars_names_D(2:)) !Array of integers
            non_flex_Z_cols = select_columns(select_non_flex_Z, vars_names_Z) !Array of integers
            non_flex_D_col = select_columns(non_flex_names(i),vars_names_D(2:)) !Array of integers
            non_flex_Z_col = select_columns(non_flex_names(i),vars_names_Z) !Array of integers

            if (verbose) print*, non_flex_D_cols, non_flex_D_col
            if (verbose) print*, non_flex_Z_cols, non_flex_Z_col
            ! Compute the elasticity G non-constant
            ! print*, "Size mmul", size(mmul(D(:, non_flex_D_cols),G_adjusted_coeffs(non_flex_D_cols,i)),1)
            ! allocate(aux_G_coeff(size(non_flex_D_cols)))
            ! aux_G_coeff = G_adjusted_coeffs(non_flex_D_cols,i)
            ! print*, " vector size is ",size(aux_G_coeff)
            ! allocate(aux_D(size(D,1),size(select_non_flex_D)))
            ! call select_columns_m(select_non_flex_D,vars_names_D,D,aux_D)
            ! do n=1,size(non_flex_D_cols)
            !     print*, n, non_flex_D_cols(n)
            !     aux_D(:,n) = D(:, non_flex_D_cols(n))
            ! end do
            allocate(aux_ZZ(size(Z,1),size(non_flex_Z_cols)))
            allocate(aux_GG(size(Z,1),size(non_flex_D_cols)))
            if (verbose) print*, "Z adjusted coeffs", Z_adjusted_coeffs(non_flex_Z_cols,i)
            !     do k=1,size(Z(:,non_flex_Z_cols),1)
            !         if (abs(Z(k,non_flex_Z_col)) < tiny(Z(1,1))) then
            !             aux_ZZ(k,j)=Z(k, non_flex_Z_cols(j))/sign(tiny(Z(1,1)),Z(k,non_flex_Z_col))
            !         else
            !             aux_ZZ(k,j)=Z(k, non_flex_Z_cols(j))/Z(k,non_flex_Z_col)
            !         end if
            !     end do
            ! end do
            do j=1,size(Z(:,non_flex_Z_cols),2)
                if (select_non_flex_Z(j)==non_flex_names(i)) then
                    aux_ZZ(:,j)=1.0d0
                    cycle
                end if
                aux_ZZ(:,j)=Z(:, non_flex_Z_cols(j)) .div. Z(:,non_flex_Z_col)
            enddo
            ! aux_ZZ(:,1)=1.0d0
            aux_Z = mmul(aux_ZZ,Z_adjusted_coeffs(non_flex_Z_cols,i)) !
            ! aux_Z = aux_Z/Z(:,non_flex_Z_col)
            ! do j=1,size(aux_Z,1)
            !     if (abs(Z(j, non_flex_Z_col)) < epsilon(aux_Z(1))) then
            !         aux_Z(j)= aux_Z(j) /epsilon(aux_Z(1))
            !     else
            !         aux_Z(j) = aux_Z(j) /Z(j, non_flex_Z_col)
            !     end if

            ! end do
            
            ! print*, "Problem is not Z"

            if (verbose) print*, "Matrix", size(D(:,non_flex_D_cols),1), size(D(:,non_flex_D_cols),2)
            if (verbose) print*, "non_flex_D_cols, ", non_flex_D_cols, "; non_flex+1, ",non_flex_D_cols+1
            if (verbose) print*, "G adjusted coeffs, ", G_adjusted_coeffs(non_flex_D_cols+1,i)
            do j=1,size(D(:,non_flex_D_cols),2)
                if (select_non_flex_D(j)==non_flex_names(i)) then
                    aux_GG(:,j)=D(:, flex_D_col) 
                    cycle
                end if
                aux_GG(:,j) = D(:,non_flex_D_cols(j))*D(:, flex_D_col) .div. D(:, non_flex_D_col)
            end do
            ! aux_GG(:,1)=D(:, flex_D_col)  

            ! do j=1,size(D(:,non_flex_D_cols),2)
            !     if (select_non_flex_D(j)==non_flex_names(i)) then
            !         aux_GG(:,j)=D(:, flex_D_col)  
            !         cycle
            !     end if
            !     do k=1,size(D(:,non_flex_D_cols),1)
            !         if (abs(D(k,non_flex_D_cols(j))) < tiny(D(1,1))) then
            !             aux_GG(k,j)=Z(k, non_flex_D_cols(j))/sign(tiny(D(1,1)),Z(k,non_flex_D_cols(j)))
            !         else
            !             aux_GG(k,j)=D(k,non_flex_D_cols(j))*D(k, flex_D_col)  /D(k, non_flex_D_col)
            !         end if
            !     end do
            ! end do
            aux_G = mmul(aux_GG,G_adjusted_coeffs(non_flex_D_cols+1,i))! G Coeffs include a constant
            ! print*, "Problem is not G"

            ! do j=1,size(aux_G,1)
            !     if (abs(D(j, non_flex_D_col)) < epsilon(aux_G(1))) then
            !         aux_G(j)= aux_G(j)* D(j, flex_D_col)  /epsilon(aux_G(1))
            !     else
            !         aux_G(j) = aux_G(j)* D(j, flex_D_col)  /D(j, non_flex_D_col)
            !     end if
            ! end do
            ! print*, "Problem is not G"
            ! Compute the elasticity Z non-constant
            ! allocate(aux_Z_coeff(size(non_flex_Z_cols)))
            ! aux_G = aux_G* D(:, flex_D_col) / D(:, non_flex_D_col)
            non_flex_out(:,i)= aux_G + aux_Z
            ! print*, "problem is not sum"
            ! non_flex_out(i)%coeff = [G_adjusted_coeffs(i,non_flex_D_cols), Z_adjusted_coeffs(i,non_flex_Z_cols)]
            deallocate(select_non_flex_D)
            deallocate(select_non_flex_Z)
            deallocate(aux_GG,aux_ZZ)
        end do

        if (verbose) call print_stats(non_flex_out,non_flex_names)

    end function get_elasticities

    subroutine estimate_flex(s,D,flex_out)
        ! use simplex
        ! use minimization
        use nlopt
        ! use matrix, only: lm => OLS
        ! use globvar, only: s_temp =>s, D_temp => D
        implicit none
        real(c_double), intent(in) :: s(:), D(:, :)
        type(flex_type(n_coef=:,n_obs=:)), allocatable, intent(out) :: flex_out
        integer(c_int) :: m, n, u, i!, count
        real(c_double), allocatable :: s_hat(:), s_hat_out(:),temp_c(:),&
            & gamma0(:), err(:), flex_elas(:), gamma1(:)
        real(c_double) :: ntemp_c, c0_corrected, bigE, f_data(size(D,1),size(D,2)+1)!, val
        logical, allocatable :: ir(:)
        type(control_type) :: control
        
        n = size(D, 1)
        m = size(D, 2)

        allocate(flex_type(m+1,n) :: flex_out)
        allocate(gamma0(m+1))
        allocate(gamma1(m+1))
        allocate(s_hat(n))
        allocate(s_hat_out(n))
        allocate(flex_elas(n))
        allocate(err(n))
        allocate(temp_c(n))
        

        if (verbose) print*, "Size D", size(D,1), size(D,2)," size s,", size(s,1)
        if (verbose) print*, "size gamma0", size(gamma0), "size D cols,", size(D,2)
        print*, "OLS in estimte-flex", ols(D,s)
        gamma0(:) = OLS(D,s)
        if (verbose) write(*,*) "DONE running OLS for initial values", gamma0
        

        s_hat = predict(gamma0,D)
        ntemp_c = minval(s_hat - gamma0(1))
        c0_corrected = -ntemp_c + 0.1d0
        ! if (verbose) write(*,*) "c0_corrected is", c0_corrected
        gamma1(1)=c0_corrected
        gamma1(2:)=gamma0(2:)

        if (verbose) write(*,*) "initial gamma is", gamma0

        ! if (verbose) write(*,*) "initial val", min_obj_fun(gamma0)

        ! call NELDER_MEADE(gamma0,1.0d-8,min_obj_fun,1,100*50)

        f_data(:,1)=s
        f_data(:,2:)=D

        control%algorithm=NLOPT_LN_SBPLX
        control%tol=1.0D-8
        control%converged=0
        control%maxtime=60*20 !seconds
        call minimize(min_obj_fun_nlopt,gamma1,f_data,control)

        ! if (control%converged==0) then
        !     gamma1(1)=c0_corrected
        !     gamma1(2:)=gamma0(2:)
        ! endif
        ! print*, "converged", control%converged


        ! control%algorithm=NLOPT_LN_SBPLX
        ! control%tol=1.0D-8
        ! control%converged=0
        ! print*, "Second minimization", control%tol, gamma1

        ! call minimize(min_obj_fun_nlopt,gamma1,f_data,control)
        ! call NELDER_MEADE(gamma0,1.0d-8,min_obj_fun,1,100*50)
        ! call BFGS(gamma0,1.0d-8,1.0d-8,min_obj_fun,0,.true.)
        if (verbose) write(*,*) "out gamma is", gamma1

        u = assert_eq(size(D,2)+1,size(gamma1),"ln_D_E")

        s_hat_out(:) = ln_D_E(gamma1,D)

        if (verbose) call print_stats(s_hat_out,"s_hat_out")
        
        ! ir=ieee_is_finite(s_hat_out)
        ! if (.not.any(ir)) error stop "s hat is not finite"
        ! if (size(s).ne.size(s_hat_out)) error stop "s and s hat out not same size"
        ! ir=ieee_is_finite(s)
        ! if (.not.any(ir)) error stop "s is not finite"
        u = assert_eq(size(s),size(err),"err")
        ! err=pack(s-s_hat_out, mask=.true.)
        if (verbose) print*, "Size s", size(s), "Size s_hat", size(s_hat_out)

        err(:) = s-s_hat_out! epsilon=-(-(s-s_hat))=s_hat-s
        ! err=[(s(i)-s_hat_out(i),i=1,size(s))]
        ! if (verbose) print*, "err are OK"
        if (verbose) call print_stats(-err,"errors in estimate flex")
        ! if (verbose) print*, "err ok, size ", size(err)

        ! lag_err=lag_s-ln_D_E(gamma0,lag_D)
        !Correcting constant separating lnE and lnD

        bigE = sum(exp(-err))/DBLE(size(err))
        flex_elas = exp(s_hat_out-log(bigE))

        flex_out%coeff=gamma1/bigE
        flex_out%predict=flex_elas
        flex_out%resid=-err
        ! flex_out%lag_resid=-lag_errors

        if (verbose) call print_stats(flex_out%predict, "flex_elas")

    end subroutine estimate_flex

    subroutine gnr(s,D,y,Z,id,time,prod,eps,elas,ci_elas)
        use stats, only: bootstrap_ci, print_stats
        ! use globvar, only: vars_names_D, vars_names_Z
        implicit none
        real(c_double), intent(in) :: s(:),D(:,:),y(:),Z(:,:),id(:),time(:)
        real(c_double), allocatable, intent(out) ::  prod(:),eps(:),elas(:),ci_elas(:,:)
        real(c_double) :: non_flex_out(size(s),2), aux_ci(2)
        type(flex_type(n_coef=:,n_obs=:)), allocatable :: flex_out, script_Y_out
        type(non_flex_type(:,:)), allocatable :: omega_out
        integer(c_int) :: i,j

        allocate(flex_type(n_coef=size(D,2)+1,n_obs=size(s)) :: flex_out)
        allocate(flex_type(n_coef=size(D,2)+1,n_obs=size(s)) :: script_Y_out)
        allocate(non_flex_type(n_coef=size(Z,2),n_obs=size(s)) :: omega_out)
        print*, "Estimating Flexible Input Elasticities"

        print*, " OLS in gnr ", ols(D,s)
        call estimate_flex(s,D,flex_out)

        print*, "Intermediate Elasticities"
        call print_stats(flex_out%predict)
        call print_stats(flex_out%resid, "Errors ")
    
        script_Y_out = compute_script_Y(y, D, 'm', flex_out)
    
        ! omega_out = estimate_productivity(y,D,Z,lag_y,lag_D,flex_out,vars_names_D,'m')
        omega_out = estimate_productivity_NLOPT(y,D,Z,id,time,flex_out)
        print*, "Productivity"
        call print_stats(omega_out%resid)
    
        non_flex_out = get_elasticities(D,Z,"m",["k","l"],flex_out,omega_out, script_Y_out)
        
        
        print*, "Labor and Capital Elasticities"
        call print_stats(non_flex_out,["k","l"])
    
        allocate(prod(size(omega_out%resid)))
        allocate(eps(size(flex_out%resid)))
        allocate(elas(3))
        allocate(ci_elas(2,3))
        ! call write_file("Data/productivity.out", reshape([ omega_out%resid, flex_out%predict ],[N_obs,2] ))
        prod=omega_out%resid
        eps=pack(flex_out%resid, mask=.true.)
        ! print*, "Problem here?"
        elas(1)=mean(flex_out%predict)
        elas(2:)=[(mean(non_flex_out(:,i)),i=1,size(non_flex_out,2))]
        
        call bootstrap_CI(flex_out%predict,aux_ci,250)
        ci_elas(:,1)=aux_ci
        do j=1,2
            aux_ci=0.0D0
            call bootstrap_CI(non_flex_out(:,j),aux_ci,250)
            ci_elas(:,j+1)=aux_ci

        enddo
    end subroutine gnr

    subroutine gnr_CD(s,y,m,Z,id,time,coeffs,productivity)
        use nlopt
        use globvar, only: script_Y
        implicit none
        
        real(c_double), intent(in) :: s(:), y(:), Z(:,:), m(:),id(:),time(:)
        real(c_double), allocatable, intent(inout) ::  coeffs(:), productivity(:)
        real(c_double) :: gamma0, err(size(s)),bigE,gamma1,&
            & alpha0(size(Z,2)+1), alpha1(size(Z,2)), g_data(size(Z,1),size(Z,2)+3),&
            & omega(size(script_Y))
        type(control_type) :: control
        integer :: u
    
        
        ! Estimate flex to a constant
        if (verbose) print*, "Estimating flex"
        gamma0=OLS(s)
        if (verbose) print*, "Done initial OLS; gamma0", gamma0
        err = gamma0-s ! -err=-(s-gamma0)=gamma0-s
        bigE=mean(exp(err))
        gamma1=exp(gamma0-log(bigE))
        if (verbose) print*, "Gamma1 saved"
        coeffs(1) = gamma1
        
        if (verbose) print*, "Coeff m:", gamma1, shape(gamma1)
        ! Compute script_Y
        if (verbose) print*, "Computing Script_Y"
        script_Y=y-err-gamma1*m !script_Y=omega+beta_k k + beta_l l
        
        ! allocate(coeffs(3))
        ! allocate(productivity(size(s)))
        ! Initial values for GMM
        if (verbose) print*, "Initial values for GMM"
        alpha0(:) = OLS(Z,script_Y) !Z=(k,l)
        alpha1(:) = alpha0(2:)

        ! GMM
        control%algorithm=NLOPT_LN_SBPLX
        control%tol=1.0D-8
        control%maxtime=60*10
        g_data(:,1) = id
        g_data(:,2) = time
        g_data(:,3) = script_Y
        g_data(:,4:) = Z
        ! restrict minimization between 0 and 0.99
        if (verbose) print*, "Minimizing GMM obj fun"

        call minimize(min_obj_fun_markov_nlopt_CD,alpha1,g_data,control)

        if (verbose) print*, "Saving coeffs", shape(alpha1)

        ! coeffs(:) = pack([gamma1, alpha1(1), alpha1(2)], mask=.true.)
        ! coeffs(1) = gamma1
        coeffs(2:) = alpha1(:)
        if (verbose) print*, "Alpha1 saved"
        ! get productivity
        u=assert_eq(size(script_Y),size(mmul(Z,alpha1)),": not same size script Y and mmul(Z,alpha1)")
        omega(:) = script_Y-mmul(Z,alpha1)
        if (verbose) print*, "Estimating productivity", shape(script_Y)!, shape(Z)
        productivity =exp(omega+err)
        if (verbose) print*, "Omega saved"

        print*, "Coeffs m, l, k:", coeffs
        call print_stats(productivity,"CD Productivity")

        script_Y=0.0D0

    end subroutine gnr_CD

    subroutine min_obj_fun_markov_nlopt_CD(val,n, x, grad, need_grad, obj_f_data) 
        use iso_c_binding
        use globvar, only: lag_data, id, time, script_Y, kl
        ! use matrix, only: matrix_inverse_Cholesky
        ! implicit none     
        integer(c_int) :: n, need_grad
        real(c_double) :: val, x(n), grad(n), obj_f_data(:,:)
        real(c_double) :: w(size(kl,1)), delta(4),  moments(n), &
            & weight_mat(n,n)!, script_Y(size(obj_f_data,1)),&
            !& Z(size(obj_f_data,1),n)!, time(size(obj_f_data,1)), id(size(obj_f_data,1))
        real(c_double), allocatable :: lag_script_Y(:,:), lag_Z(:,:), w_in(:), lag_w(:,:), eta(:), Z_in(:,:),Z(:,:)
        integer(c_int) :: i,j
        logical :: mask(size(kl,1))

        if (need_grad .ne. 0) then
            grad=0.0d0 
        end if

        if (verbose) print*, "id", size(id),"time", size(time)
        
        ! if (size(obj_f_data,1)==0) error stop "no data"

        ! id = obj_f_data(:,1)
        ! time = obj_f_data(:,2)
        ! script_Y = obj_f_data(:,3)
        ! Z = obj_f_data(:,4:)
        Z=kl

        mask = (id==eoshift(id,shift=-1) .and. time==(eoshift(time,shift=-1)+1.0d0))

        if (verbose) print*, "mask size", count(mask)
        allocate(lag_script_Y(count(mask),1))
        allocate(lag_Z(count(mask),size(Z,2)))
        allocate(lag_w(count(mask),3))
        allocate(eta(count(mask)))
        allocate(Z_in(count(mask),size(Z,2)))

        lag_script_Y= lag_data(script_Y,id,time,1)
        if (verbose) print*, "lag_script_Y", size(lag_script_Y,1), size(lag_script_Y,2), shape(lag_script_Y)
        lag_Z=lag_data(Z,id,time,1)
        if (verbose) print*, "lag_Z", shape(lag_Z)
        ! call print_stats(lag_Z)

        ! integer :: istat
        ! character(len=60) :: err=" "
        !exp(alpha) !only positive numbers
        ! Note: no constant for C(k,l) only for Markov Process of productivity
        ! w=delta_0+delta_1 w_t-1+delta_2 w_t-1^2+delta_3 w_t-1^3+eta
        w=script_Y-mmul(Z,x) ! form omega for a guess of x
        ! print*, "w size", size(w)
        ! print*, "Z size", size(Z,2); print*, "x size", size(x)
        ! print*, "lag_Z size", size(lag_Z,2)
        lag_w(:,1)=lag_script_Y(:,1)-mmul(lag_Z,x) ! form omega at t-1 for a guess of alpha
        lag_w(:,2)=lag_w(:,1)*lag_w(:,1) ! omega_t-1^2
        lag_w(:,3)=lag_w(:,2)*lag_w(:,1) ! omega_t-1^3
        ! print*, "lag_w size ", size(lag_w,1), size(lag_w,2)
        
        w_in=pack(w,mask)
        if (verbose) print*, "w_in", shape(w_in)
        if (verbose) print*, "lag_w", shape(lag_w)
        delta=OLS(lag_w,w_in) ! Productivity's Markov Process
        if (verbose) print*, "delta", delta
        ! deallocate(eta, stat=istat,errmsg=err)
        ! print *, "Status : ",istat; print *, "Errmsg : ",err !NAG compiler
        eta=w_in-predict(delta,lag_w) !getting eta, residuals of Markov Process
        ! if (verbose) print*, "eta size", size(eta)
        Z_in=reshape(pack(Z,spread(mask,2,size(Z,2))),[count(mask),size(Z,2)])
        moments=mmul(eta,Z_in)/real(size(eta))
        ! weight_mat=mmul(Z, transpose(Z))
        ! if (verbose) print*, "moments, ", moments
        ! weight_mat=matrix_inverse_Cholesky(mmul(transpose(Z),Z)/real(size(eta)))
        ! FORALL(i=1:size(Z,2),j=1:size(Z,2),i.ne.j) weight_mat(i,j)=0.0d0 !Assuming independence
        weight_mat=0.0d0
        FORALL(i=1:n,j=1:n,i==j) weight_mat(i,j)=1.0d0
        ! weight_mat=matrix_inverse_Cholesky(weight_mat)
        val=sqrt(mmul(moments,mmul(weight_mat,moments)))
        ! retval=norm2(moments)
        ! print*, "retval", retval
    end subroutine min_obj_fun_markov_nlopt_CD

    ! function estimate_productivity(y, D, Z,lag_y, lag_D, flex_result, vars_names, flex_name) result(omega_out)
    !     use globvar, only: script_Y, lag_script_Y
    !     use simplex
    !     use minimization
    !     implicit none
    !     real(c_double), intent(in) :: D(:,:), y(:), lag_D(:,:), lag_y(:), Z(:,:)
    !     character(kind=c_char,len=*), intent(in) :: vars_names(:), flex_name
    !     type(flex_type(n_coef=*, n_obs=*)), intent(in) :: flex_result
    !     type(non_flex_type(n_coef=:, n_obs=:)), allocatable :: omega_out
    !     real(c_double) :: alpha_0(size(Z,2)+1), val, alpha_1(size(Z,2)), log_omega(size(Z,1))


    !     ! Compute script_Y
    !     script_Y = compute_script_Y(y, D, vars_names, flex_name, flex_result)
    !     ! assuming lag vars are in same order than non-lagged vars
    !     lag_script_Y = compute_script_Y(lag_y, lag_D, vars_names, flex_name, flex_result, .true.) 

    !     if (verbose) call print_stats(script_Y,"Script Y")
    !     if (verbose) call print_stats(lag_script_Y, "Lag Script Y")
    !     ! set initial values
    !     if (size(Z,1).ne.size(script_Y)) error stop "Z and script_Y not same size"
    !     alpha_0 = OLS(Z,script_Y)
    !     if (verbose) write(*,*) "OLS alpha_0 is ", alpha_0
    !     if (verbose) print*, "size alphas, ", size(alpha_1), size(alpha_0)
    !     alpha_1(:) = alpha_0(2:) !No constant

    !     val=min_obj_fun_markov(alpha_0(2:))
    !     if (verbose) write(*,*) "Obj Fun Markov initial val ",val
    !     ! call NELDER_MEADE(alpha_1,1.0d-8,min_obj_fun_markov,1,100*50)
    !     call NELDER_MEADE(alpha_1,1.0d-8,min_obj_fun_markov,1,100*50)
    !     call BFGS(alpha_1,1.0d-6,1.0d-6,min_obj_fun_markov,1,verbose)
    !     call BFGS(alpha_1,1.0d-8,1.0d-8,min_obj_fun_markov,1,verbose)
    !     if (verbose) write(*,*) "OUT alpha_1 is ", alpha_1
    !     val=min_obj_fun_markov(alpha_1)
    !     if (verbose) write(*,*) "Obj Fun Markov out val ",val

    !     allocate(non_flex_type(size(alpha_1),size(script_Y)) :: omega_out)
    !     omega_out%coeff=alpha_1
    !     ! Productivity is defined as exp(omega+errors)
    !     ! omega = script_Y-C(k,l)
    !     log_omega=script_Y-mmul(Z,alpha_1)
    !     omega_out%resid=exp(log_omega+flex_result%resid)


    ! end function estimate_productivity
    
end module gnr_mod