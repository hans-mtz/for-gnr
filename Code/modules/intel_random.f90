include 'mkl_vsl.f90'
include "errcheck.inc" 
module intel_random
    USE MKL_VSL_TYPE
    USE MKL_VSL
    use iso_c_binding, only: c_int, c_double
    implicit none
    private
    public :: runif_int, percentiles, status_intel_rnd, &
        & runif_int_i_omp_init, runif_int_i_omp_gen, &
        & runif_int_i_omp_del

    logical :: initialized=.false.
    TYPE (VSL_STREAM_STATE):: stream(12)

    interface runif_int
        module procedure runif_int, runif_int_i
    end interface
contains
    subroutine runif_int(a,b,n,r,seed)
        integer(c_int), intent(in) :: a,b,n
        integer(c_int), intent(out) :: r(n)
        TYPE (VSL_STREAM_STATE), save :: stream
        integer(c_int) :: errcode
        integer brng,method,seed
        
        brng=VSL_BRNG_MT2203
        method=VSL_RNG_METHOD_UNIFORM_STD


        ! if (.not.initialized) then
        !     ***** Initialize *****
        errcode=vslnewstream( stream, brng,  seed )
        call CheckVslError(errcode)
        !     initialized=.true.
        ! end if

    !     ***** Call RNG *****
        errcode=virnguniform( method, stream, n, r, a, b )
        call CheckVslError(errcode)

    !     ***** Deinitialize *****
        errcode=vsldeletestream( stream )
        call CheckVslError(errcode)
        
    end subroutine runif_int

    subroutine runif_int_i(a,b,n,r,seed,i)
        integer(c_int), intent(in) :: a,b,n,i
        integer(c_int), intent(out) :: r(n)
        TYPE (VSL_STREAM_STATE) :: stream
        integer(c_int) :: errcode, nskip
        integer brng,method,seed!,i
        
        brng=VSL_BRNG_WH
        method=VSL_RNG_METHOD_UNIFORM_STD


        errcode=vslnewstream( stream, brng,  seed )
        call CheckVslError(errcode)

        errcode=vslskipaheadstream( stream, 250*i)
        call CheckVslError(errcode)

        if (.not.initialized) then
            !     ***** Initialize *****
            ! errcode=vslnewstream( stream, brng+i,  seed )
            ! call CheckVslError(errcode)
            initialized=.true.
        end if

    !     ***** Call RNG *****
        errcode=virnguniform( method, stream, n, r, a, b )
        call CheckVslError(errcode)

    
    !     ***** Deinitialize *****
        errcode=vsldeletestream( stream )
        call CheckVslError(errcode)
        
    end subroutine runif_int_i

    subroutine runif_int_i_omp_init(seed,nskip,worker_id,n_workers)
        integer(c_int) :: errcode, nskip, worker_id
        integer brng,seed,n_workers
        
        brng=VSL_BRNG_WH
        
        ! allocate(stream(n_workers))

        errcode=vslnewstream( stream(worker_id+1), brng,  seed )
        call CheckVslError(errcode)

        if (worker_id/=0) then
            errcode=vslskipaheadstream( stream(worker_id+1), nskip*worker_id)
            call CheckVslError(errcode)
        end if
        
    end subroutine runif_int_i_omp_init

    subroutine runif_int_i_omp_gen(a,b,n,r,worker_id)
        integer(c_int), intent(in) :: a,b,n, worker_id
        integer(c_int), intent(out) :: r(n)
        integer(c_int) :: errcode
        integer method
        
        method=VSL_RNG_METHOD_UNIFORM_STD

    !     ***** Call RNG *****
        errcode=virnguniform( method, stream(worker_id+1), n, r, a, b )
        call CheckVslError(errcode)

        
    end subroutine runif_int_i_omp_gen

    subroutine runif_int_i_omp_del(worker_id)
        integer(c_int), intent(in) :: worker_id
        integer(c_int) :: errcode

    !     ***** Deinitialize *****
        errcode=vsldeletestream( stream(worker_id+1) )
        call CheckVslError(errcode)
        
    end subroutine runif_int_i_omp_del

    subroutine percentiles(vector, quantiles, values)
        real(c_double), intent(in) :: vector(:), quantiles(:)
        real(c_double), intent(out) :: values(size(quantiles))
        TYPE(VSL_SS_TASK) task
        integer(c_int) :: p, n, x_storage, m, o_storage, task_method, errcode
        integer(8) :: task_params
        real(c_double) :: o_stat(size(vector),1), q_out(size(quantiles),1), v_in(size(vector),1)

    !     ***** Initialize parameters of Summary Statistics task *****
        x_storage   = VSL_SS_MATRIX_STORAGE_ROWS
        o_storage   = VSL_SS_MATRIX_STORAGE_ROWS
        task_params = ior( VSL_SS_QUANTS, VSL_SS_ORDER_STATS )
        task_method = VSL_SS_METHOD_FAST
        p=1 !Task dimension
        n=size(vector) ! Number of observations
        m=size(quantiles) ! Number of quantiles
        errcode=0
        v_in(:,1)=vector

    !     ***** Create Summary Statistics task *****
        errcode = vsldssnewtask( task, p, n, x_storage, v_in )
        call CheckVslError( errcode )
        ! print*, "made it here?"
        errcode = vsldsseditquantiles( task, m, quantiles, q_out, o_stat, o_storage )
        call CheckVslError( errcode )

    !     ***** Compute quantiles and order statistics using FAST method *****
        errcode = vsldsscompute( task, task_params, task_method )
        call CheckVslError( errcode )
        ! print*, "made it here?"
        values=q_out(:,1)
        ! print*, "q_out", q_out
        ! print*, "quantiles", quantiles
        ! print*, "o_stat,", o_stat
    !     ***** Delete Summary Statistics task *****
        errcode = vslssdeletetask( task )
        call CheckVslError( errcode )
  
        call MKL_FREE_BUFFERS()
        
    end subroutine percentiles

    subroutine status_intel_rnd()
        ! import stream
        integer(c_int) :: errcode

        print*, "Initialized: ", initialized
        ! print*, "Errorcode: ", errcode
        ! print*, "VSL Stream: ", 
        
        ! if (initialized) then
        !     !     ***** Deinitialize *****
        !     errcode=vsldeletestream( stream )
        !     call CheckVslError(errcode)
        !     initialized=.false.
        ! endif 
    
        
    end subroutine status_intel_rnd
    
end module intel_random