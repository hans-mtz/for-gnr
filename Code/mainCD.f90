program main
    use iso_c_binding, only: c_double, c_int, c_char
    use globvar!, only: s, D, N_obs, lag_s, lag_D, y, Z, lag_y, lag_D, vars_names_D, vars_names_Z
    ! use init, only: READ_DATA
    use gnr_mod, only: gnr, gnr_CD
    use io
    use omp_lib
    ! use matrix, only: OLS
    use intel_random, only: runif_int
    use stats, only: print_stats

    implicit none

    real(c_double), allocatable ::  prod(:), eps(:),elas(:),ci_elas(:,:),productivity(:),coeffs(:)
    integer(c_int) :: r(10), worker_id, i
    logical :: save_results=.true.

    ! R Data
    ! integer(c_int), parameter :: NN(*)=[6171, 2895, 4470, 969, 3722, 2099, 1129, 817]
    ! character(kind=c_char,len=*), parameter :: inds(*)=["311", "321", "322", "331", "381", "342", "313", "351"]
    
    integer(c_int), parameter :: NN(*)=[6171, 1129, 2249, 817, 2895]
    character(kind=c_char,len=*), parameter :: inds(*)=["311", "313", "352", "351", "321"]
    

    ! integer(c_int), parameter :: NN(*)=[4470, 2099, 1129, 817, 969]
    ! character(kind=c_char,len=*), parameter :: inds(*)=["322","342","313","351","331"]
    

    ! Stata Data
    ! integer(c_int), parameter :: NN(*)=[6187, 2857, 4478, 968, 3699, 1070, 2071, 1117, 816]
    ! character(kind=c_char,len=*), parameter :: inds(*)=["311", "321", "322", "331", "381", "324", "342", "313", "351"]

    ! real(c_double) :: coeffs(3)
    ! REAL(c_double), allocatable, DIMENSION(:,:) :: D!, Z
    ! REAL(c_double), allocatable, DIMENSION(:) :: s,y!,id,time
    
    ! !$OMP PARALLEL private(worker_id,r,i)
    ! worker_id=omp_get_thread_num()
    ! !$OMP DO
    ! do i=1,5
    !     call runif_i(1,10,5,r,seed+worker_id*i)
    !     print*, "random numbers +", r, "from worker:", worker_id
    !     call runif_i(1,10,5,r,seed*worker_id+110*i)
    !     print*, "random numbers *", r, "from worker:", worker_id
    ! enddo 
    ! !$OMP END DO
    ! if (worker_id==0) call print_stats(DBLE(r))
    ! !$OMP END PARALLEL

    ! call runif_int(1,10,10,r,seed)
    ! print*, r
    ! call print_stats(DBLE(r))
    ! call status_intel_rnd()
    ! $OMP Parallel default(private)
    !   $OMP parallel do private(D,Z,s,y,script_Y,id,time,worker_id,prod,eps,elas,ci_elas)
    ! $OMP do


    do i=1,size(inds)
        ! worker_id=omp_get_thread_num()

        ! print*, "worker ",worker_id," doing industy", inds(i)
        ! if (i/=3) cycle !Select only one industry
        ! if (i==6) cycle !Leave out one industry

        allocate(D(NN(i),size(select_D)))
        allocate(s(NN(i)))
        allocate(Z(NN(i),size(select_Z)))
        allocate(y(NN(i)),script_Y(NN(i)),id(NN(i)),time(NN(i)))
        allocate(m(NN(i)),kl(NN(i),2), productivity(NN(i)),coeffs(3))

        call READ_DATA(inds(i),NN(i))

        ! print*, "OLS:", OLS(D,s)
        call gnr_CD(s,y,m,kl,id,time,coeffs,productivity)

        if (save_results) then 
            ! call write_file("Data/CD_bootstrap_ci_"//trim(inds(i))//".out",ci_elas,["m","l","k"]) ! Add se

            call write_file("Data/CD_coeffs_R_"//trim(inds(i))//".out",reshape(coeffs,[1,3]),["m","k","l"])
            call write_file("Data/CD_productivity_R_"//trim(inds(i))//".out", reshape([id,time,productivity],[size(id),3]),["plant","year","CD-productivity"])

        end if

        deallocate(Z,y,script_Y,id,time,m,kl,productivity,coeffs)
        deallocate(D,s)

    enddo

    ! DO i=1,size(inds)

    !     allocate(D(NN(i),size(select_D)))
    !     allocate(Z(NN(i),size(select_Z)))
    !     allocate(s(NN(i)),y(NN(i)),script_Y(NN(i)),id(NN(i)),time(NN(i)))
    !     allocate(m(NN(i)),kl(NN(i),2), productivity(NN(i)),coeffs(3))

    !     call READ_DATA(inds(i),nn(i))

    !     call gnr(s,D,y,Z,id,time,prod,eps,elas,ci_elas)

    !     if (.not.save_results) then

    !         call write_file("Data/bootstrap_ci_"//trim(inds(i))//".out",ci_elas,["m","l","k"])
    !         call write_file("Data/coeffs_"//trim(inds(i))//".out",reshape(elas,[1,3]),["m","l","k"])
    !         call write_file("Data/productivity_"//trim(inds(i))//".out", reshape([id,time,prod],[size(id),3]),["plant","year","productivity"])

    !     end if

    !     deallocate(D,Z,s,y,script_Y,id,time,m,kl,productivity,coeffs)
    !     ! nullify(D,Z,s,y,script_Y,id,time)
        
    ! enddo
    ! $OMP end do
    ! $OMP end parallel

end program main