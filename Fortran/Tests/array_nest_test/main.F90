!! results: (/a,b,c../) is still a 1-dim array (a,b,c..is any-dim array)
!! a=(/1,2,3,4,5/), b=(/11,12,13,14/)
!! c= [ 21  23 ]
!!    [ 22  24 ]
!! (/a,b,c/)= (/a(1)..a(5),b(1)..b(4),c(1,1),c(2,1),c(1,2),c(2,2)/)

program main
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="main"
    integer :: i
    integer :: a(5)=(/(i,i=1,5)/), b(4)=(/(10+i,i=1,4)/), c(2,2)=(/(/21,22/),(/23,24/)/)
    print*, shape((/a,b,c/))
    print*,(/a,b,c/)
    print*,c(:,1)
end program main
