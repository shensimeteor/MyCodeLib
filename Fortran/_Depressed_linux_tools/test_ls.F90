program main
use utility 
use linuxtools
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="main"
    integer :: nfile
    character(len=100),allocatable :: files(:)

    call get_dir_files(".", nfile, files)
    print*, nfile
    print*, files
end program main
