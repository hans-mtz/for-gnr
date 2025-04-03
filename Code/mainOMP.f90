program main
    use iso_c_binding, only: c_double, c_int, c_char
    use omp_lib
    use intel_random!, only: runif_int, status_intel_rnd
    use stats!, only: print_stats, mean

    implicit none

    integer(c_int) :: w_id, n_workers,nskip, n, reps, i
    integer(c_int) :: r(10), seed!, v(10)
    integer, dimension(10) :: v=[ ( i*1, i=1,10)]
    real(c_double) :: b_dist(250)
    
    n=10
    reps=250
    seed=777
    ! v=[(j, j=1,10)]
    print*, v

    !$OMP parallel private(w_id,r)
    !default(private) shared(b_dist,i) 
    write(*,*) v(1:10), DBLE(v(1:10))
    w_id=omp_get_thread_num()
    n_workers=omp_get_num_threads()
    nskip=(n*reps)/n_workers
    call runif_int_i_omp_init(seed,nskip,w_id,n_workers)
    !$OMP do
    do i=1,reps
        call runif_int_i_omp_gen(1,n,n,r,w_id)
        b_dist(i)=mean(DBLE(v(r)))
    enddo
    !$OMP end do
    !$OMP barrier
    call runif_int_i_omp_del(w_id)
    !$OMP end parallel
    call print_stats(b_dist)
    call bootstrap_omp(mean,DBLE(v),b_dist,reps)
    call print_stats(b_dist)



end program main