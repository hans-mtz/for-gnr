! Module to handle input/output to/from data files
module io
  implicit none
  public :: write_file, read_file
  private

  interface write_file
    module procedure write_file_m,write_file_v
  end interface write_file

  interface read_file
    module procedure read_filem,read_filev
  end interface read_file

contains
  !write to data file
  subroutine write_file_v(filename,vars)
    character(len=*), intent(in) :: filename
    real(8), intent(in) :: vars(:)
    integer :: i, iu

    open(newunit=iu,file=filename,status='replace',action='write')
    print "(a)", 'writing to '//trim(filename)
    do i=1,size(vars)
      write(iu,*) vars(i)
    enddo
    close(iu)
  end subroutine write_file_v

  subroutine write_file_m(filename,vars)
    character(len=*), intent(in) :: filename
    real(8), intent(in) :: vars(:,:)
    integer :: i, j, iu

    open(newunit=iu,file=filename,status='replace',action='write')
    print "(a)", 'writing to '//trim(filename)
    do i=1,size(vars(:,1))
      write(iu,*) (vars(i,j),j=1,size(vars(1,:)))
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

end module io
