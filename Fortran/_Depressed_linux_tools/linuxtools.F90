#include "inc_common.fpp"
module linuxtools
use utility
implicit none

    ! variables

contains

    ! methods
    !!files must be allocatable and un-allocated
    subroutine get_dir_files(dir, nfile, files)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="get_dir_files"
        character(len=*) :: dir
        integer :: nfile
        character(len=*), allocatable :: files(:)
        character(len=20000) :: temp
        call system("/bin/ls "//trim(dir)//" > .temp_get_dir_files")
        open(11, file=".temp_get_dir_files")
        read(11, '(A)') temp
        close(11)
!!        call str_split(trim(temp), ' ', nfile, files)
        call allocread_line(".temp_get_dir_files", nfile, files)
        call system("/bin/rm .temp_get_dir_files")
    end subroutine get_dir_files  !!}}}

end module linuxtools
