MODULE INIT
  private
  PUBLIC :: READ_DATA
  ! IMPLICIT NONE
CONTAINS
  
  ! SUBROUTINE READ_DATA(inds,nn)
  !   use globvar
  !   USE io, only: read_file
  !   use iso_c_binding, only: c_double, c_int, c_char
  !   IMPLICIT NONE	

  !   integer(c_int) :: nn,j
  !   REAL(c_double):: dataset(nn,N_variables)	! Array where we will read in the data
  !   character(kind=c_char, len=10) :: vars_names_data(N_variables)
  !   character(kind=c_char, len=*) :: inds


  !   call read_file("Data/data_"//trim(inds)//".raw", dataset)
  !   call read_file("Data/variables_R.raw", vars_names_data)
  !   ! OPEN(1,file='Data/colombia.raw')	                ! opening the file dataset.txt where the dataset is saved in 
  !   ! DO i = 1, N_obs	        ! ascii format and asigning it to handle 1
  !   !    READ(1,fmt=*)	dataset(i,:)	        ! reading from handle 1 into the array I created
  !   ! END DO					! That is, loading the dataset into memory
  !   ! CLOSE(1)					! Closing the file since now I have its contents in the dataset array

  !   WRITE(*,*) "DONE reading data for industry "//trim(inds)
  !   ! write(*,*) "Data", dataset(1:10,5)
  !   if (.true.) then
  !     write(*,*) "Variable names"
  !     write(*,'(*(a,:,","))') (trim(vars_names_data(j)),j=1,size(vars_names_data))
  !     write(*,*) " Data rows:", size(dataset,1)
  !   end if

  !   D(:,:)=dataset(:,select_D)	                ! assigning X
  !   s(:)=dataset(:,select_s)	                        ! assigning Y
  !   Z=dataset(:,select_Z)
  !   ! lag_Z=dataset(:,select_lag_Z)
  !   y=dataset(:,select_y)
  !   ! lag_y=dataset(:,select_lag_y)
  !   ! lag_D=dataset(:,select_lag_D)
  !   id=dataset(:,1)
  !   time=dataset(:,2)
  !   m=dataset(:,5)
  !   lk=dataset(:,[7,6])
  !   ! Getting variable names
  !   vars_names_D(1)="Const."
  !   vars_names_D(2:)=vars_names_data(select_D)
  !   vars_names_Z=vars_names_data(select_Z)
  !   ! vars_names_lag_D(1)="Const."
  !   ! vars_names_lag_D(2:)=vars_names_data(select_lag_D) 
  !   ! vars_names_lag_Z=vars_names_data(select_lag_Z)
    
  !   if (verbose) then
  !     write(*,*) "Variable names D "!, vars_names_D
  !     write(*,'(*(a,:,","))') (trim(vars_names_D(j)),j=1,size(vars_names_D))
  !     write(*,*) "Variable names Z "!, vars_names_Z
  !     write(*,'(*(a,:,","))') (trim(vars_names_Z(j)),j=1,size(vars_names_Z))
  !     ! write(*,*) "Variable names lag D ", vars_names_lag_D
  !     ! write(*,*) "Variable names lag Z ", vars_names_lag_Z
  !   endif
    
  !   ! Done reading my data, now moving to the main program
    
  ! END SUBROUTINE READ_DATA
  
END MODULE INIT
