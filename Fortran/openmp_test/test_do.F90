program test_do
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="test_do"
    integer :: i, N, id,nproc, omp_get_num_threads, omp_get_thread_num
    N=10000
    !$OMP PARALLEL DO PRIVATE(id)
    do i=1,N
        id=omp_get_thread_num()
        print*, "print ",i," from thread ", id
    !    if(id==0) then
    !        nproc=omp_get_num_threads()
    !        print*, "total threads:", nproc
    !    end if
    end do
    !$OMP END PARALLEL DO
end program test_do
