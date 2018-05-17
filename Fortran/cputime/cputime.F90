module cputime
implicit none

    ! variables
    type :: type_cpuclock
        character(len=20) :: title
        real :: tbegin
        real :: tlast
        real :: tcur
        real :: dt_total, dt_event
    end type

    type :: type_cputime
        integer,parameter :: MAX_CLK=20
        integer :: iclock=0
        type(type_cpuclock) :: clocks(MAX_CLK)
    end type

    type(type_cputime) :: cputime

contains

    subroutine cputime_start(clock_name)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="cputime_start"
        character(len=*) :: clock_name
        cputime%iclock=cputime%iclock+1
        cputime%clocks(cputime%iclock)%title=trim(clock_name)
        call cpu_time(cputime%clocks(cputime%iclock)%tbegin)
    end subroutine cputime_start  !!}}}

    ! methods
    subroutine cputime_start(clock_name)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="cputime_start"
        cputime_index=1
        call cpu_time(cputime_arrays(cputime_index)%tstart)
        cputime_arrays(cputime_index)%tlast=cputime_arrays(cputime_index)%tstart
    end subroutine cputime_start  !!}}}

    subroutine cputime_click()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="cputime_click"
        call cpu_time(cputime_arrays(cputime_index)%tcur)
        cputime_arrays(cputime_index)%dt_total = &
              cputime_arrays(cputime_index)%tcur - cputime_arrays(cputime_index)%tstart
        cputime_arrays(cputime_index)%dt_event = &
              cputime_arrays(cputime_index)%tcur - cputime_arrays(cputime_index)%tlast
        cputime_arrays(cputime_index)%tlast = cputime_arrays(cputime_index)%tcur
    end subroutine cputime_click  !!}}}

    subroutine cpu_branch()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="cpu_branch"
    end subroutine cpu_branch  !!}}}
end module cputime
