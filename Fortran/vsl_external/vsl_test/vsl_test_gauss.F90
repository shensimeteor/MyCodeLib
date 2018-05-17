#include "inc_common.h"
program vsl_test_gauss
use utility
use mkl_vsl_type
use mkl_vsl
use vsl_external
implicit none
    character(len=*), parameter :: PROCEDURE_NAME="vsl_test_gauss"
    integer,parameter :: n=2000
    real :: r(n)
    type(vsl_stream_state) :: stream
    integer :: seed, i
    integer :: brng=VSL_BRNG_MCG31
    integer :: method=VSL_METHOD_SGAUSSIAN_BOXMULLER2
    real :: a = 5.0, sigma = 2.0
    
    call system_clock(count = seed)
    print*,seed
    VSL_CHECK(vslnewstream(stream, brng, seed))
    VSL_CHECK(vsrnggaussian( method, stream, n, r, a, sigma ))
    
    print*, get_average(r)
    print*, sqrt(get_cov(r,r))
    open(11,file="gauss_random.txt")
    do i=1, n
         write(11, *) r(i)
    end do
    close(11)
    
end program vsl_test_gauss
