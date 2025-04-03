! Module to handle input/output to/from data files
module io
  implicit none
  public :: write_file, read_file
  private

  interface write_file
    module procedure write_file_m,write_file_v
  end interface write_file

  interface read_file
    module procedure read_filem,read_filev,read_filev_char
  end interface read_file

contains
  !write to data file
  subroutine write_file_v(filename,vars,var_name)
    character(len=*), intent(in) :: filename
    character(len=*), optional :: var_name
    real(8), intent(in) :: vars(:)
    integer :: i, iu

    open(newunit=iu,file=filename,status='replace',action='write')
    print "(a)", 'writing to '//trim(filename)
    if (present(var_name)) write(iu,'(a)') var_name
    do i=1,size(vars)
      write(iu,*) vars(i)
    enddo
    close(iu)
  end subroutine write_file_v

  subroutine write_file_m(filename,vars, vars_names)
    character(len=*), intent(in) :: filename
    character(len=*), optional :: vars_names(:)
    real(8), intent(in) :: vars(:,:)
    integer :: i, j, iu, n, m

    n=size(vars,1)
    m=size(vars,2)
    open(newunit=iu,file=filename,status='replace',action='write')
    print "(a)", 'writing to '//trim(filename)
    if (present(vars_names)) write(iu,'(*(a,:,","))') (vars_names(j),j=1,m)
    do i=1,n
      write(iu,'(*(G0.8,:,","))') (vars(i,j),j=1,m)
    enddo
    close(iu)
  end subroutine write_file_m

  subroutine write_res(filename,res,miu,sigma,a)
    character(len=*), intent(in) :: filename
    real(8), intent(in) :: res(:,:,:,:), miu(:),sigma(:),a(:,:,:)
    integer :: i,j,k,h,iu

    open(newunit=iu,file=filename,status='replace',action='write')
    print "(a)", 'writing to '//trim(filename)
    do i=1,size(res(1,:,1,1))
      do j=1,size(res(1,1,:,1))
        do k=1,size(res(:,1,1,1))
          write(iu,*) "(miu,sigma,a),Gauss-Legendre,Gauss-Chebyshev,Gauss-Hermite"
          write(iu,*) "(",miu(i),",",sigma(j),",",a(k,i,j),"),",res(k,i,j,1),",",&
            res(k,i,j,2),",",res(k,i,j,3)
          enddo
        enddo
    enddo
    close(iu)
  end subroutine write_res

  !read from data file
  subroutine read_filev(filename, vars)
    character(len=*), intent(in) :: filename
    real(8), intent(out) :: vars(:)
    integer :: i, iu

    open(newunit=iu,file=filename,status='old',action='read')
    print "(a)", 'reading from '//trim(filename)
    do i=1,size(vars)
      read(iu,*) vars(i)
    enddo
    close(iu)

  end subroutine read_filev

  subroutine read_filem(filename, vars)
    character(len=*), intent(in) :: filename
    real(8), intent(out) :: vars(:,:)
    integer :: i, iu

    open(newunit=iu,file=filename,status='old',action='read')
    print "(a)", 'reading from '//trim(filename)
    do i=1,size(vars(:,1))
      read(iu,*) vars(i,:)
    enddo
    close(iu)

  end subroutine read_filem

  subroutine read_filev_char(filename, vars)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: vars(:)
    integer :: i, iu

    open(newunit=iu,file=filename,status='old',action='read')
    print "(a)", 'reading from '//trim(filename)
    do i=1,size(vars)
      read(iu,*) vars(i)
    enddo
    close(iu)

  end subroutine read_filev_char

end module io
