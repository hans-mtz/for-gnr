! mod_csv
! 
! A module for reading and writing fortran arrays to csv files.
! The csv files can then be opened in Excel or imported into Matlab 
! via the corresponding matlab code.
!
! The default directory for the csv files to be placed is './results/'.
! If you need a different directory, use "usedir([dir])". You can determine the current
! directory via "getdir()".
! 
! To save an array A with filename "A.csv", call csvwrite('A',A).
! To read an array A with filename "A.csv", call csvread('A',A).
!
! Currently, very little error checking is done, so this may crash if used improperly. 
! E.g., in reading in an array, no attempt is made to ensure that the array in the csv file
! has the same shape or properties as the one being requested. 
! 
! Grey Gordon, 2014
! Soli Deo Gloria
module mod_csv
    implicit none

    character(len=255) :: dir = ''
    character(len=255) :: scratchdir ! temporary directory that can be reverted to

    interface csvwrite
        module procedure w0d,w1d,w2d,w3d,w4d,w5d,w6d,&
                         w0i,w1i,w2i,w3i,w4i,w5i,w6i,&
                         w0l,w1l,w2l,w3l,w4l,w5l,w6l
    end interface
    interface csvread
        module procedure r0d,r1d,r2d,r3d,r4d,r5d,r6d,&
                         r0i,r1i,r2i,r3i,r4i,r5i,r6i,&
                         r0l,r1l,r2l,r3l,r4l,r5l,r6l
    end interface

    character(len=*), parameter :: fmt_d = '(e25.17e3)'
    character(len=*), parameter :: fmt_i = '(i12)'
    character(len=*), parameter :: fmt_l = '(i1)'

    private 
    public :: csvwrite,csvread,usedir,getdir,test_csv

contains

    ! Setting or getting the directory
    function getdir() result(dirout)
        implicit none
        character(len=255) :: dirout
        dirout = dir
    end function

    subroutine usedir(dirin)
        implicit none
        character(len=*), intent(in) :: dirin
        scratchdir = dir
        dir = dirin
    end subroutine

    !!! READING ROUTINES
    subroutine r0d(str,var)
        real(8), intent(out) :: var
        character(len=*) :: str
        real(8) :: tmp(1)
        call rdcore(str,tmp,[1])
        var = tmp(1)
    end subroutine

    subroutine r1d(str,var)
        real(8), intent(out) :: var(:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r2d(str,var)
        real(8), intent(out) :: var(:,:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r3d(str,var)
        real(8), intent(out) :: var(:,:,:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r4d(str,var)
        real(8), intent(out) :: var(:,:,:,:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r5d(str,var)
        real(8), intent(out) :: var(:,:,:,:,:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r6d(str,var)
        real(8), intent(out) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call rdcore(str,var,shape(var))
    end subroutine

    subroutine r0i(str,var)
        integer, intent(out) :: var
        character(len=*) :: str
        integer :: tmp(1)
        call ricore(str,tmp,[1])
        var = tmp(1)
    end subroutine

    subroutine r1i(str,var)
        integer, intent(out) :: var(:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r2i(str,var)
        integer, intent(out) :: var(:,:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r3i(str,var)
        integer, intent(out) :: var(:,:,:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r4i(str,var)
        integer, intent(out) :: var(:,:,:,:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r5i(str,var)
        integer, intent(out) :: var(:,:,:,:,:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r6i(str,var)
        integer, intent(out) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call ricore(str,var,shape(var))
    end subroutine

    subroutine r0l(str,var)
        logical, intent(out) :: var
        character(len=*) :: str
        logical :: tmp(1)
        call rlcore(str,tmp,[1])
        var = tmp(1)
    end subroutine

    subroutine r1l(str,var)
        logical, intent(out) :: var(:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine

    subroutine r2l(str,var)
        logical, intent(out) :: var(:,:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine

    subroutine r3l(str,var)
        logical, intent(out) :: var(:,:,:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine

    subroutine r4l(str,var)
        logical, intent(out) :: var(:,:,:,:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine

    subroutine r5l(str,var)
        logical, intent(out) :: var(:,:,:,:,:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine

    subroutine r6l(str,var)
        logical, intent(out) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call rlcore(str,var,shape(var))
    end subroutine


    !!! WRITING ROUTINES
    subroutine w0d(str,var)
        real(8), intent(in) :: var
        character(len=*) :: str
        real(8) :: tmp(1); tmp(1) = var
        call wdcore(str,tmp,shape(var))
    end subroutine

    subroutine w1d(str,var)
        real(8), intent(in) :: var(:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w2d(str,var)
        real(8), intent(in) :: var(:,:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w3d(str,var)
        real(8), intent(in) :: var(:,:,:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w4d(str,var)
        real(8), intent(in) :: var(:,:,:,:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w5d(str,var)
        real(8), intent(in) :: var(:,:,:,:,:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w6d(str,var)
        real(8), intent(in) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call wdcore(str,var,shape(var))
    end subroutine

    subroutine w0i(str,var)
        integer, intent(in) :: var
        character(len=*) :: str
        integer :: tmp(1); tmp(1) = var
        call wicore(str,tmp,shape(var))
    end subroutine

    subroutine w1i(str,var)
        integer, intent(in) :: var(:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w2i(str,var)
        integer, intent(in) :: var(:,:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w3i(str,var)
        integer, intent(in) :: var(:,:,:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w4i(str,var)
        integer, intent(in) :: var(:,:,:,:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w5i(str,var)
        integer, intent(in) :: var(:,:,:,:,:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w6i(str,var)
        integer, intent(in) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call wicore(str,var,shape(var))
    end subroutine

    subroutine w0l(str,var)
        logical, intent(in) :: var
        character(len=*) :: str
        logical :: tmp(1); tmp(1) = var
        call wlcore(str,tmp,shape(var))
    end subroutine

    subroutine w1l(str,var)
        logical, intent(in) :: var(:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    subroutine w2l(str,var)
        logical, intent(in) :: var(:,:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    subroutine w3l(str,var)
        logical, intent(in) :: var(:,:,:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    subroutine w4l(str,var)
        logical, intent(in) :: var(:,:,:,:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    subroutine w5l(str,var)
        logical, intent(in) :: var(:,:,:,:,:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    subroutine w6l(str,var)
        logical, intent(in) :: var(:,:,:,:,:,:)
        character(len=*) :: str
        call wlcore(str,var,shape(var))
    end subroutine

    !!! CORE ROUTINES FOR READING AND WRITING
    subroutine makeheader(unitno,vartype,str,dims)
        implicit none
        character(len=*), intent(in) :: str
        integer, intent(in) :: unitno,vartype,dims(:)
        integer :: i

        ! header info
        
        ! row 1: variable name | additional, reserved
        ! row 2: variable type | integer specifying type: 0 <=> real(8); 1 <=> integer; 2 <=> logical | variable format (string) | additional, reserved
        ! row 3: variable rank | additional, reserved
        ! row 4: variable dimensions
        ! row 5: reserved
        ! row 6: reserved
        ! row 7: reserved
        ! row 8: reserved
        ! row 9: reserved

        write(unitno,'(A,A,A)') 'Variable name:',',',trim(str) ! row 1
        select case (vartype)
            case (0)
                write(unitno,'(A,A,A,A,i3,A,A)') 'Variable type:',',','real(8)',',',0,',',fmt_d ! row 2
            case (1) 
                write(unitno,'(A,A,A,A,i3,A,A)') 'Variable type:',',','integer',',',1,',',fmt_i ! row 2
            case (2) 
                write(unitno,'(A,A,A,A,i3,A,A)') 'Variable type:',',','logical',',',2,',',fmt_l ! row 2
        end select
        write(unitno,'(A,A,i3)') 'Variable rank:',',',size(dims)
        write(unitno,'(A,A)',advance='no') 'Variable dims:',','
        do i = 1,size(dims)
            write(unitno,'(i10)',advance='no') dims(i)
            if (i<size(dims)) write(unitno,'(A)',advance='no') ','
        end do
        if (size(dims)==0) then
            write(unitno,'(i10)',advance='no') 1
        end if
        write(unitno,'(A)') ''  ! row 4 (dimensions)
        write(unitno,'(A)') ''  ! row 5
        write(unitno,'(A)') ''  ! row 6
        write(unitno,'(A)') ''  ! row 7
        write(unitno,'(A)') ''  ! row 8
        write(unitno,'(A)') ''  ! row 9

    end subroutine

    subroutine getninjnk(ni,nj,nk,dims)
        implicit none
        integer, intent(in) :: dims(:)
        integer, intent(out) :: ni,nj,nk

        nk = 1
        nj = 1
        ni = 1
        if (size(dims)>=3) nk = product(dims(3:size(dims)))
        if (size(dims)>=2) nj = dims(2)
        if (size(dims)>=1) ni = dims(1)
        
    end subroutine 
    
    subroutine wdcore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        real(8), intent(in) :: var(*)
        integer, intent(in) :: dims(:)
        integer :: unitno,i,ni,j,nj,k,nk

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='replace')

        ! create the header
        call makeheader(unitno,0,str,dims)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    write(unitno,fmt_d,advance='no') var(i + ((j-1) + (k-1)*nj)*ni)
                    if (j<nj) write(unitno,'(A1)',advance='no')','
                end do
                write(unitno,'(A)') ''
            end do
        end do

        close(unitno)

    end subroutine

    subroutine wicore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        integer, intent(in) :: var(*)
        integer, intent(in) :: dims(:)
        integer :: unitno,i,ni,j,nj,k,nk

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='replace')

        ! create the header
        call makeheader(unitno,1,str,dims)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    write(unitno,fmt_i,advance='no') var(i + ((j-1) + (k-1)*nj)*ni)
                    if (j<nj) write(unitno,'(A1)',advance='no')','
                end do
                write(unitno,'(A)') ''
            end do
        end do

        close(unitno)

    end subroutine

    subroutine wlcore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        logical, intent(in) :: var(*)
        integer, intent(in) :: dims(:)
        integer :: unitno,i,ni,j,nj,k,nk

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='replace')

        ! create the header
        call makeheader(unitno,2,str,dims)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    write(unitno,fmt_l,advance='no') merge(1,0,var(i + ((j-1) + (k-1)*nj)*ni))
                    if (j<nj) write(unitno,'(A1)',advance='no')','
                end do
                write(unitno,'(A)') ''
            end do
        end do

        close(unitno)

    end subroutine

    subroutine processheader(readfmt,unitno)
        implicit none
        character(len=25), intent(out) :: readfmt
        integer, intent(in) :: unitno
        ! local
        character(len=255) :: tmp
        integer :: i,cnt,a,b

        read(unitno,*)
        ! Go to second row, third entry to string format
        read(unitno,'(A)') tmp ! read in the entire second row
        ! Process the second row, finding the fourth entry which gives the format
        cnt = 0
        b = len(tmp) ! in case there are only four fields (and so only three commas), set b to lentmp
        do i = 1,len(tmp)
            if (tmp(i:i)==',') then
                cnt = cnt + 1
                if (cnt==3) a = i+1
                if (cnt==4) then
                    b = i-1
                    exit
                end if
            end if
        end do
        readfmt = tmp(a:b)

        do i = 3,9
            read(unitno,*)
        end do

    end subroutine
    
    subroutine rdcore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        real(8), intent(out) :: var(*)
        integer, intent(in) :: dims(:)
        character(len=25) :: readfmt
        integer :: unitno,i,ni,j,nj,k,nk

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='old')

        ! process the header 
        call processheader(readfmt,unitno)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    read(unitno,trim(readfmt),advance='no') var(i + ((j-1) + (k-1)*nj)*ni)
                    if (j<nj) read(unitno,'(1X)',advance='no')
                end do
                read(unitno,*) 
            end do
        end do

        close(unitno)

    end subroutine

    subroutine ricore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        integer, intent(out) :: var(*)
        integer, intent(in) :: dims(:)
        character(len=25) :: readfmt
        integer :: unitno,i,ni,j,nj,k,nk

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='old')

        ! process the header 
        call processheader(readfmt,unitno)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    read(unitno,trim(readfmt),advance='no') var(i + ((j-1) + (k-1)*nj)*ni)
                    if (j<nj) read(unitno,'(1X)',advance='no')
                end do
                read(unitno,*) 
            end do
        end do

        close(unitno)

    end subroutine

    subroutine rlcore(str,var,dims)
        implicit none
        character(len=*), intent(in) :: str
        logical, intent(out) :: var(*)
        integer, intent(in) :: dims(:)
        character(len=25) :: readfmt
        integer :: unitno,i,ni,j,nj,k,nk,tmp

        open(newunit=unitno,file=trim(dir)//str//'.csv',status='old')

        ! process the header 
        call processheader(readfmt,unitno)

        ! data (begins at row 10)
        call getninjnk(ni,nj,nk,dims)

        do k = 1,nk
            do i = 1,ni
                do j = 1,nj
                    read(unitno,trim(readfmt),advance='no') tmp
                    var(i + ((j-1) + (k-1)*nj)*ni) = tmp==1 ! recover logical value from {0,1}
                    if (j<nj) read(unitno,'(1X)',advance='no')
                end do
                read(unitno,*) 
            end do
        end do

        close(unitno)

    end subroutine

    subroutine test_csv()
        implicit none
        real(8) :: d0,d0_
        real(8), allocatable :: d1(:),d2(:,:),d3(:,:,:),d4(:,:,:,:)
        real(8), allocatable :: d1_(:),d2_(:,:),d3_(:,:,:),d4_(:,:,:,:)
        integer :: i0,i0_
        integer, allocatable :: i1(:),i2(:,:),i3(:,:,:),i4(:,:,:,:)
        integer, allocatable :: i1_(:),i2_(:,:),i3_(:,:,:),i4_(:,:,:,:)
        integer, parameter :: n1=7,n2=8,n3=9,n4=10
        allocate(d1(n1))
        allocate(d2(n1,n2))
        allocate(d3(n1,n2,n3))
        allocate(d4(n1,n2,n3,n4))
        allocate(i1(n1))
        allocate(i2(n1,n2))
        allocate(i3(n1,n2,n3))
        allocate(i4(n1,n2,n3,n4))
        allocate(d1_(n1))
        allocate(d2_(n1,n2))
        allocate(d3_(n1,n2,n3))
        allocate(d4_(n1,n2,n3,n4))
        allocate(i1_(n1))
        allocate(i2_(n1,n2))
        allocate(i3_(n1,n2,n3))
        allocate(i4_(n1,n2,n3,n4))

        call random_number(d0)
        call random_number(d1)
        call random_number(d2)
        call random_number(d3)
        call random_number(d4)
        i0 = (2d0*d0-1d0)*huge(1)
        i1 = (2d0*d1-1d0)*huge(1)
        i2 = (2d0*d2-1d0)*huge(1)
        i3 = (2d0*d3-1d0)*huge(1)
        i4 = (2d0*d4-1d0)*huge(1)
        d0 = (2d0*d0-1d0)*huge(0d0)
        d1 = (2d0*d1-1d0)*huge(0d0)
        d2 = (2d0*d2-1d0)*huge(0d0)
        d3 = (2d0*d3-1d0)*huge(0d0)
        d4 = (2d0*d4-1d0)*huge(0d0)


        call usedir('/tmp/')
        call csvwrite('d0',d0)
        call csvwrite('d1',d1)
        call csvwrite('d2',d2)
        call csvwrite('d3',d3)
        call csvwrite('d4',d4)
        call csvwrite('i0',i0)
        call csvwrite('i1',i1)
        call csvwrite('i2',i2)
        call csvwrite('i3',i3)
        call csvwrite('i4',i4)

        d0_ = d0
        d1_ = d1
        d2_ = d2
        d3_ = d3
        d4_ = d4
        i0_ = i0
        i1_ = i1
        i2_ = i2
        i3_ = i3
        i4_ = i4

        d0 = 0d0
        d1 = 0d0
        d2 = 0d0
        d3 = 0d0
        d4 = 0d0
        i0 = 0
        i1 = 0
        i2 = 0
        i3 = 0
        i4 = 0

        call csvread('d0',d0)
        call csvread('d1',d1)
        call csvread('d2',d2)
        call csvread('d3',d3)
        call csvread('d4',d4)
        call csvread('i0',i0)
        call csvread('i1',i1)
        call csvread('i2',i2)
        call csvread('i3',i3)
        call csvread('i4',i4)

        if (d0 /= d0_) STOP 'test_csv: failed test d0'
        if (any(d1 /= d1_)) STOP 'test_csv: failed test d1'
        if (any(d2 /= d2_)) STOP 'test_csv: failed test d2'
        if (any(d3 /= d3_)) STOP 'test_csv: failed test d3'
        if (any(d4 /= d4_)) STOP 'test_csv: failed test d4'
        if (i0 /= i0_) STOP 'test_csv: failed test i0'
        if (any(i1 /= i1_)) STOP 'test_csv: failed test i1'
        if (any(i2 /= i2_)) STOP 'test_csv: failed test i2'
        if (any(i3 /= i3_)) STOP 'test_csv: failed test i3'
        if (any(i4 /= i4_)) STOP 'test_csv: failed test i4'

        print*,'test_csv: passed'

    end subroutine 

end module mod_csv