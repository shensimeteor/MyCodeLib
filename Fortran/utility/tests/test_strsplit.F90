program main
use utility
    character(len=*),parameter :: input="abcd;efg;hi;jklmopq;rstu;vw;xy;z"
    character(len=10), allocatable :: output(:)
    integer :: n, i

    call str_split(trim(input), ';', n, output)
    print*, trim(input)
    print*, n
    do i=1, n
        print*, trim(output(i))
    end do
end

