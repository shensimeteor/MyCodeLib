program test_interp
use interp
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="test_interp"
    real :: x1(50),v1(50), x2(5)=(/-3., -0.5, 3., 49.5, 52./),v2(5)
    integer :: i
    x1=(/(i,i=1,50)/)
    open(10,file='v1.txt')
    read(10,*) v1
    close(10)
    call interp_1d_linear_reg2sct(0., 50, 1., .false., v1, x2, v2)
    print*,x2
    print*,v2
end program test_interp
