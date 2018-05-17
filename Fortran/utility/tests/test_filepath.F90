program test
use utility
use ArgsManager
implicit none

    character(len=200) :: filepath
    call args_readargs()
    call args_getarg(1, filepath)

    print*,trim(filepath)
    print*,trim(kickout_filepath(filepath))
    print*,trim(replace_filesuffix(filepath,'.txt'))
    
end program test
