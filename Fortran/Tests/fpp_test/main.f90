#define CHECK(m) if(.not. (m)) then; write(*,*) #m ; end if
#define MESSAGE(m) print*,#m,m
program test_fpp
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="test_fpp"
    CHECK(1==2)
    MESSAGE(abc%ax)
end program test_fpp
