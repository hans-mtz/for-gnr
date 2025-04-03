MODULE GLOBVAR
  use iso_c_binding, only: c_char, c_int, c_double
  IMPLICIT NONE
  private
  public :: verbose, seed, vars_names_D, vars_names_Z, Z, D, kl, &
   & script_Y, id, time, s, y, m, select_columns, lag_data, str_contains, &
   & select_D, select_s, select_Z, select_y, N_variables, READ_DATA
  ! This modlue declares the variables that are going to be used throughout the program
  ! higher level languages do not require you to declare your variables ahead of time
  
  INTEGER(c_int), PARAMETER :: N_variables=14!, N_obs=6187 ! Number of variables and obs in the dataset	
  logical, parameter :: verbose=.true.
  integer(c_int) :: i, N_obs
  integer(c_int), PARAMETER :: seed=777, select_s=4, select_y=3,&
    & select_D(*)=(/(i,i=5,14)/), select_Z(*)=(/6,7,8,12,13/)!, &
    ! & select_lag_Z(*)=[14,16,17,21,22], &
    ! & select_lag_s=24, select_lag_D(*)=(/15,14,(i,i=16,22)/), select_lag_y=23
  character(kind=c_char, len=10) :: vars_names_D(size(select_D)+1), &
    & vars_names_Z(size(select_Z))!,& !Integral constant does not have a constant
    ! & vars_names_lag_D(size(select_lag_D)+1), vars_names_lag_Z(size(select_lag_Z))
  ! real(c_double) :: gamma(size(select_D)+1)
  REAL(c_double), allocatable, DIMENSION(:,:) :: Z, kl!(N_obs, size(select_D)), Z(N_obs,size(select_Z))!,lag_Z(N_obs,size(select_lag_Z)),&
  
  REAL(c_double), allocatable :: D(:,:), s(:)!   ! & lag_D(N_obs,size(select_lag_D)) ! Array to store X
  ! REAL(c_double), DIMENSION(N_obs) :: s,y,script_Y,id,time!,lag_script_Y,y,lag_y,lag_s,id, time	! Array to store Y values
  REAL(c_double), allocatable, DIMENSION(:) :: id, time,y, m, script_Y

  interface select_columns
    module procedure select_columns_v, select_column_i
  end interface select_columns

  interface lag_data
    module procedure lag_data_m, lag_data_v
  end interface lag_data

  contains

    SUBROUTINE READ_DATA(inds,nn)
      ! use globvar
      USE io, only: read_file
      use iso_c_binding, only: c_double, c_int, c_char
      IMPLICIT NONE	

      integer(c_int) :: nn,j
      REAL(c_double):: dataset(nn,N_variables)	! Array where we will read in the data
      character(kind=c_char, len=10) :: vars_names_data(N_variables)
      character(kind=c_char, len=*) :: inds

      ! R Data
      call read_file("Data/R_data_"//trim(inds)//".raw", dataset)
      call read_file("Data/R_variables_"//trim(inds)//".raw", vars_names_data)
      
      ! GNR Stata Data
      ! call read_file("Data/stata_data_"//trim(inds)//".raw", dataset)
      ! call read_file("Data/stata_variables_"//trim(inds)//".raw", vars_names_data)
      
      !** Old code**
      ! call read_file('Data/gnr_stata_data.txt', dataset)
      ! OPEN(1,file='Data/colombia.raw')	                ! opening the file dataset.txt where the dataset is saved in 
      ! DO i = 1, N_obs	        ! ascii format and asigning it to handle 1
      !    READ(1,fmt=*)	dataset(i,:)	        ! reading from handle 1 into the array I created
      ! END DO					! That is, loading the dataset into memory
      ! CLOSE(1)					! Closing the file since now I have its contents in the dataset array

      WRITE(*,*) "DONE reading data for industry "//trim(inds)
      ! write(*,*) "Data", dataset(1:10,5)
      if (.true.) then
        write(*,*) "Variable names"
        write(*,'(*(a,:,","))') (trim(vars_names_data(j)),j=1,size(vars_names_data))
        write(*,*) " Data rows:", size(dataset,1)
      end if

      ! allocate(D(nn, size(select_D)), s(nn))
      D(:,:)=dataset(:,select_D)	                ! assigning X
      s(:)=dataset(:,select_s)	                        ! assigning Y
      Z(:,:)=dataset(:,select_Z)
      ! lag_Z=dataset(:,select_lag_Z)
      y(:)=dataset(:,select_y)
      ! lag_y=dataset(:,select_lag_y)
      ! lag_D=dataset(:,select_lag_D)
      kl(:,:)=dataset(:,[6,7])
      m(:) = dataset(:,5)
      id(:)=dataset(:,1)	                        ! assigning id
      time(:)=dataset(:,2)	                        ! assigning time
      ! Getting variable names
      vars_names_D(1)="Const."
      vars_names_D(2:)=vars_names_data(select_D)
      vars_names_Z=vars_names_data(select_Z)
      ! vars_names_lag_D(1)="Const."
      ! vars_names_lag_D(2:)=vars_names_data(select_lag_D) 
      ! vars_names_lag_Z=vars_names_data(select_lag_Z)
      
      if (verbose) then
        write(*,*) "Variable names D "!, vars_names_D
        write(*,'(*(a,:,","))') (trim(vars_names_D(j)),j=1,size(vars_names_D))
        write(*,*) "Variable names Z "!, vars_names_Z
        write(*,'(*(a,:,","))') (trim(vars_names_Z(j)),j=1,size(vars_names_Z))
        ! write(*,*) "Variable names lag D ", vars_names_lag_D
        ! write(*,*) "Variable names lag Z ", vars_names_lag_Z
      endif
      
      ! Done reading my data, now moving to the main program
      
    END SUBROUTINE READ_DATA

    pure function select_columns_v(names_list,dataset_names) result(columns)
      ! this function returns a vector of integers with the location of
      ! the variables in names_list in dataset_names
      character(kind=c_char,len=*), intent(in) :: names_list(:), dataset_names(:)
      integer(c_int) :: columns(size(names_list))
      integer(c_int) :: i,j

      do i=1,size(names_list)
        do j=1,size(dataset_names)
          if (names_list(i)==dataset_names(j)) columns(i)=j
        end do
      end do
    end function select_columns_v

    pure function select_column_i(name,dataset_names) result(column)
      ! this function returns a vector of integers with the location of
      ! the variables in names_list in dataset_names
      character(kind=c_char,len=*), intent(in) :: name, dataset_names(:)
      integer(c_int) :: column
      integer(c_int) :: j

      ! do i=1,size(names_list)
      do j=1,size(dataset_names)
        if (name==dataset_names(j)) column=j !Dataset no contains constant variable names do
      end do
      ! end do
    end function select_column_i

    pure function select_columns_logic(names_list,dataset_names) result(columns)
      ! this function returns a vector of integers with the location of
      ! the variables in names_list in dataset_names
      character(kind=c_char,len=*), intent(in) :: names_list(:), dataset_names(:)
      logical :: columns(size(names_list))
      integer(c_int) :: i,j

      do i=1,size(names_list)
        do j=1,size(dataset_names)
          if (names_list(i)==dataset_names(j)) columns(i)=.true.
        end do
      end do
    end function select_columns_logic

    subroutine select_columns_m(names_list,dataset_names,dataset,data_out)! result(columns)
      ! this subroutine returns a vector of integers with the location of
      ! the variables in names_list in dataset_names
      character(kind=c_char,len=*), intent(in) :: names_list(:), dataset_names(:)
      real(c_double), intent(in) :: dataset(:,:)
      real(c_double), intent(InOut) :: data_out(:,:)
      ! integer(c_int) :: columns(size(names_list))
      integer(c_int) :: i,j

      if (size(names_list).ne.size(data_out,2)) error stop "Name list is not same size as dim 2 data_out"
      if (size(dataset_names).ne.size(dataset,2)+1) error stop "Dataset names are not same size as dim 2 dataset"
      do i=1,size(names_list)
        do j=1,size(dataset_names)
          if (names_list(i)==dataset_names(j)) data_out(:,i)=dataset(:,j)
        end do
      end do
    end subroutine select_columns_m

    pure function str_contains(name,dataset_names) result(name_list)
      ! use iso_varying_string
      ! this function returns a vector of strings. The vector of strings are the
      ! elements of 'dataset_names' that contain the 'name' substring
      character(kind=c_char,len=*), intent(in) :: name, dataset_names(:)
      character(kind=c_char,len=:), allocatable :: name_list(:)
      integer(c_int) :: i,j, count
      logical :: aux(size(dataset_names))

      aux=.false.
      count=0

      do j=1,size(dataset_names)
        if (index(dataset_names(j),name) .ne. 0) then
          aux(j)=.true.
          count=count+1
        end if
      end do
      allocate(name_list(count), mold=dataset_names)
      name_list=pack(dataset_names, mask=aux)

    end function str_contains
    

    pure function lag_data_m(dataset,id,time,lag, return_index)
      real(c_double), intent(in) :: id(:),time(:),dataset(:,:)
      integer(c_int), intent(in) :: lag
      logical, optional, intent(in) :: return_index
      real(c_double), allocatable :: lag_data_m(:,:), aux(:,:)
      integer(c_int) :: j, istat
      logical :: mask(size(id))
      ! dataset is a matrix with the data
      ! id is a vector with the id of each observation
      ! time is a vector with the time of each observation
      ! columns is a vector with the columns to be lagged
      ! This function takes a panel dataset indexed by id and time
      ! and lags the columns in columns and returns the dataset with
      ! added lagged columns

      mask=id==eoshift(id,shift=-lag) .and. time==(eoshift(time,shift=-lag)+1.0d0)
    
      if (present(return_index) .and. return_index) then
        ! print*, "returning index"
        allocate(lag_data_m(count(mask),size(dataset,2)+2),stat=istat)
        if (istat .ne. 0)  error stop "Error: Predict could not allocate memory: Return Index"
        
        allocate(aux(count(mask),3), stat=istat)
        if (istat .ne. 0)  error stop "Error: Predict could not allocate memory: Return Index aux"
        aux=lag_data_v(dataset(:,1),id,time,lag,.true.)
        ! print*, "aux OK"
        lag_data_m(:,1:3)=aux
        ! print*, "Lag 1:3 OK"
        deallocate(aux)
        allocate(aux(count(mask),1))
        do j=2,size(dataset,2)
          aux=lag_data_v(dataset(:,j),id,time,lag,.false.)
          ! print*, "aux OK",j
          lag_data_m(:,j+2) =aux(:,1)
          ! print*, "Lag OK",j
        end do
      else
        allocate(lag_data_m(count(mask),size(dataset,2)),stat=istat)
        if (istat .ne. 0)  error stop "Error: Predict could not allocate memory: No Return Index"
        allocate(aux(count(mask),1))
        do j=1,size(dataset,2)
          aux=lag_data_v(dataset(:,j),id,time,lag,.false.)  
          lag_data_m(:,j) =aux(:,1)
        end do
      end if

    end function lag_data_m

  pure function lag_data_v(dataset,id,time,lag, return_index)
    real(c_double), intent(in) :: id(:),time(:),dataset(:)
    integer(c_int), intent(in) :: lag
    logical, optional, intent(in) :: return_index
    real(c_double), allocatable :: lag_data_v(:,:)
    logical :: mask(size(id))
    integer(c_int) :: istat
    ! dataset is a vecter with the data to be lagged
    ! id is a vector with the id of each observation
    ! time is a vector with the time of each observation
    ! Vector is assumed to be orderd by id and time
    ! columns is a vector with the columns to be lagged
    ! This function takes a panel dataset indexed by id and time
    ! and lags the columns in columns and returns the dataset with
    ! added lagged columns

    mask=id==eoshift(id,shift=-lag) .and. time==(eoshift(time,shift=-lag)+1.0D0)
    if (present(return_index) .and. return_index) then 
      allocate(lag_data_v(count(mask),3),stat=istat)
      if (istat .ne. 0)  error stop "Error: Predict could not allocate memory: Return Index"
      lag_data_v(:,1)=pack(eoshift(id,shift=-lag), mask=mask)
      lag_data_v(:,2)=pack(eoshift(time,shift=-lag), mask=mask)
      lag_data_v(:,3)=pack(eoshift(dataset,shift=-lag), mask=mask)
    else
      allocate(lag_data_v(count(mask),1),stat=istat)
      if (istat .ne. 0)  error stop "Error: Predict could not allocate memory: No Return Index V"
      lag_data_v(:,1)=pack(eoshift(dataset,shift=-lag), mask=mask)
    end if

  end function lag_data_v


END MODULE GLOBVAR

