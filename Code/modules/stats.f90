module stats
    use iso_c_binding, only: c_float, c_double, c_int, c_char
    ! use intel_random
    use globvar, only: verbose
    use omp_lib
    implicit none

    private
    public :: print_stats, operator(.div.), mean, bootstrap_CI, bootstrap_omp

    interface print_stats
        module procedure print_stats_v, print_stats_m
    end interface print_stats

    interface operator(.div.)
        procedure div
    end interface operator(.div.)

    interface operator(.times.)
    procedure times
    end interface operator(.times.)


contains
    subroutine print_stats_v(arg1, string)
        implicit none
        
        real(c_double), intent(in) :: arg1(:)
        ! integer(c_int) :: i
        real(c_double) :: se,ci(2)
        character(len=*), optional :: string
        
        ! write(*,*) "Predict OK, xb size", size(arg1)
        ! avg=sum(arg1)/real(size(arg1))
        ! se=sqrt(sum((/((arg1(i)-avg)**2,i=1,size(arg1))/))/real((size(arg1)-1)))
        se=0.0d0
        ci=0.0d0

        ! print*, "Calling bootstrap_stats in print_stats_v"
        call bootstrap_stats(arg1,se,ci,250)
        ! call bootstrap_se(arg1,se,200)
        ! call bootstrap_CI(arg1,ci,200,0.05D0)
        ! print*, "problem here?"
        if (present(string)) write(*,*) "Printing Stats of ", string
        write(*,*) "mean", mean(arg1)
        write(*,*) "(bootsrap) se", se
        write(*,*) "(bootsrap) CI=[",ci,"]"
        write(*,*) "min", minval(arg1)
        write(*,*) "max", maxval(arg1)
        write(*,*) "-----------------------------------"

    end subroutine print_stats_v

    subroutine print_stats_m(arg1, names)
        real(c_double), intent(in) :: arg1(:,:)
        integer(c_int) :: i
        character(len=*), optional :: names(:)
        if (size(arg1,2).ne.size(names)) error stop "Names are not same size as data"
        do i=1,size(arg1,2)
            if (present(names)) then
                write(*,*) "Column ",names(i)," stats: "
            else
                write(*,*) "Column ",i," stats: "
            endif
            call print_stats_v(arg1(:,i)) 
        end do

    end subroutine print_stats_m

    elemental function div(x,y)
        real(c_double), intent(in) :: x,y
        real(c_double) :: div

        if (abs(y) < tiny(y)) then
            div=x/sign(tiny(y),y)
        else
            div=x/y
        end if
    end function div

    subroutine bootstrap_se(v, val, reps)
        ! use intel_random, only: boo
        ! use random
        ! use globvar, only: seed
        real(c_double), intent(in) :: v(:)
        real(c_double), intent(out) :: val
        integer(c_int), intent(in) :: reps
        real(c_double) :: b_dist(reps)
        ! integer(c_int) :: n, i, j, index, select(size(v)), r(size(v)*reps)

        ! n=size(v)
        ! call runif_i(1,n,n*reps,r,seed)
        ! index=1
        ! do i=1,reps
        !     do j=1,n
        !         ! select(j)=floor(Sample_Uniform(0.0D0,1.0D0))
        !         select(j)=r(index)
        !         index=index+1
        !     enddo
        !     aux(i)=mean(v(select))
        ! enddo

        call bootstrap(mean,v,b_dist,reps)

        val=se(b_dist)
        
    end subroutine bootstrap_se

    subroutine bootstrap(f, v, b_dist, reps)
        use intel_random, only: runif_int, status_intel_rnd
        ! use random
        use globvar, only: seed
        ! external f
        real(c_double), intent(in) :: v(:)
        ! real(c_double), intent(out) :: val
        integer(c_int), intent(in) :: reps
        real(c_double), intent(out)  :: b_dist(reps)
        integer(c_int) :: w_id, n, i, j, index, select(size(v)), r(size(v))!*reps) ***
        interface
            pure function f(v)
                use iso_c_binding, only: c_double
                real(c_double), intent(in) :: v(:)
                real(c_double) :: f
            end function
        end interface
        n=size(v)
        ! call runif_int(1,n,n*reps,r,seed) !***
        ! !$OMP PARALLEL DO PRIVATE(v,r,index, select,i)
        !!$OMP parallel private(w_id,i,r,v)
        !w_id=omp_get_thread_num()
        !!$OMP do
        do i=1,reps
            ! index=(i-1)*n + 1  !***
            ! select = r(index:index+n-1) !***
            ! b_dist(i)=f(v(select)) !***
            call runif_int(1,n,n,r,seed,i)
            b_dist(i)=f(v(r))
        enddo
        !!$OMP end do
        !!$OMP end parallel
        ! !$OMP END PARALLEL DO

        ! call status_intel_rnd()

    end subroutine bootstrap

    subroutine bootstrap_omp(f, v, b_dist, reps)
        use intel_random, only: runif_int_i_omp_init, &
            & runif_int_i_omp_gen, runif_int_i_omp_del
        ! use random
        use globvar, only: seed
        ! external f
        real(c_double), intent(in) :: v(:)
        ! real(c_double), intent(out) :: val
        integer(c_int), intent(in) :: reps
        real(c_double), intent(out)  :: b_dist(reps)
        integer(c_int) :: n_workers, w_id, n, nskip,i, j, index, select(size(v)), r(size(v))!*reps) ***
        interface
            pure function f(v)
                use iso_c_binding, only: c_double
                real(c_double), intent(in) :: v(:)
                real(c_double) :: f
            end function
        end interface
        n=size(v)

        !!$OMP PARALLEL DO PRIVATE(v,r,index, select,i)
        !$OMP parallel private(w_id,r)
        w_id=omp_get_thread_num()
        n_workers=omp_get_num_threads()
        nskip=(n*reps)/n_workers
        call runif_int_i_omp_init(seed,nskip,w_id,n_workers)
        !$OMP do
        do i=1,reps
            call runif_int_i_omp_gen(1,n,n,r,w_id)
            b_dist(i)=f(v(r))
        enddo
        !$OMP end do
        !$OMP barrier
        call runif_int_i_omp_del(w_id)
        !$OMP end parallel
        ! !$OMP END PARALLEL DO

        ! call status_intel_rnd()

    end subroutine bootstrap_omp

    subroutine bootstrap_omp_m(f, v, b_dist, reps)
        use intel_random, only: runif_int_i_omp_init, &
            & runif_int_i_omp_gen, runif_int_i_omp_del
        ! use random
        use globvar, only: seed
        ! external f
        real(c_double), intent(in) :: v(:,:)
        ! real(c_double), intent(out) :: val
        integer(c_int), intent(in) :: reps
        real(c_double), intent(out)  :: b_dist(reps,size(v,2))
        integer(c_int) :: n_workers, w_id, n, nskip,i, j, index, select(size(v)), r(size(v))!*reps) ***
        interface
            pure function f(v)
                use iso_c_binding, only: c_double
                real(c_double), intent(in) :: v(:,:)
                real(c_double) :: f(size(v,2))
            end function
        end interface
        n=size(v)

        !!$OMP PARALLEL DO PRIVATE(v,r,index, select,i)
        !$OMP parallel private(w_id,r)
        w_id=omp_get_thread_num()
        n_workers=omp_get_num_threads()
        nskip=(n*reps)/n_workers
        call runif_int_i_omp_init(seed,nskip,w_id,n_workers)
        !$OMP do
        do i=1,reps
            call runif_int_i_omp_gen(1,n,n,r,w_id)
            b_dist(i,:)=f(v(r,:))
        enddo
        !$OMP end do
        !$OMP barrier
        call runif_int_i_omp_del(w_id)
        !$OMP end parallel
        ! !$OMP END PARALLEL DO

        ! call status_intel_rnd()

    end subroutine bootstrap_omp_m

    subroutine bootstrap_CI(v, ci, reps, alpha)
        use intel_random, only: percentiles
        real(c_double), intent(in) :: v(:)
        real(c_double), intent(out) :: ci(2)
        integer(c_int), intent(in) :: reps
        real(c_double) :: b_dist(reps), quants(2), p(2)
        real(c_double), optional :: alpha
        ! external percentiles

        if (present(alpha)) then
            p=[alpha/2.0D0, 1.0D0-alpha/2.0D0]
        else
            p=[0.025D0, 0.975D0]
        endif 

        ! print*, "Here>>>"
        ! p=[alpha/2.0D0, 1.0D0-alpha/2.0D0]
    
        call bootstrap(mean,v,b_dist,reps)
        call percentiles(b_dist,p,quants) ! 1-\alpha=5
        ci(1)=2.0D0*mean(v)-quants(2) !Lower CI 2*\hat{\theta}-\hat{\theta}^*_(1-\alpha/2)
        ci(2)=2.0D0*mean(v)-quants(1) !Upper CI 2*\hat{\theta}-\hat{\theta}^*_(\alpha/2) 
        
    end subroutine bootstrap_CI

    subroutine bootstrap_stats(v, se_out, ci, reps, alpha)
        use intel_random, only: percentiles
        real(c_double), intent(in) :: v(:)
        real(c_double), intent(out) :: ci(2), se_out
        integer(c_int), intent(in) :: reps
        real(c_double) :: b_dist(reps), quants(2), p(2)
        real(c_double), optional :: alpha

        if (present(alpha)) then
            p=[alpha/2.0D0, 1.0D0-alpha/2.0D0]
        else
            p=[0.025D0, 0.975D0]
        endif 
        ! print*, "Here?"
        
        ! print*, "Here?"
        call bootstrap(mean,v,b_dist,reps)
        ! print*, "Here?"
        se_out=se(b_dist)
        ! print*, "Here?"
        call percentiles(b_dist,p,quants) ! 1-\alpha=5
        ! print*, "Here?"
        ci(1)=2.0D0*mean(v)-quants(2) !Lower CI 2*\hat{\theta}-\hat{\theta}^*_(1-\alpha/2)
        ci(2)=2.0D0*mean(v)-quants(1) !Upper CI 2*\hat{\theta}-\hat{\theta}^*_(\alpha/2) 
        
    end subroutine bootstrap_stats

    pure function mean(arg1)! result(mean)
        ! use iso_c_binding, only: c_double
        real(c_double), intent(in) :: arg1(:)
        real(c_double) :: mean

        mean=sum(arg1)/DBLE(size(arg1))
    end function

    pure function se(arg1)! result(se)
        use matrix, only: mmul
        real(c_double), intent(in) :: arg1(:)
        real(c_double) :: mu, se, sum_sq_diffs, diffs(size(arg1))
        integer(c_int) ::i

        mu=mean(arg1)
        diffs = (arg1-mu)! .times. (arg1-mu)
        sum_sq_diffs=sum(diffs**2)
        se=sqrt(sum_sq_diffs/DBLE((size(arg1)-1)))
    end function

    elemental function times(x,y)
        use ieee_arithmetic, only: ieee_is_finite, ieee_overflow
        use ieee_exceptions
        real(c_double), intent(in) :: x,y
        real(c_double) :: times

        call ieee_set_halting_mode(ieee_overflow,.false.)

        if (.not.ieee_is_finite(x*y)) then
            times=sign(tiny(y),min(x,y))
        else
            times=x*y
        end if

    end function times

end module stats