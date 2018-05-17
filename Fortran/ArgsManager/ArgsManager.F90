#include "inc_common.fpp"
module ArgsManager
! --------------------------------------------------------------------------
! Description:
!
!   Manage to retrieve Arguments
!
! History:
!
!   2014-06-23:
!
!       [Shen Si]: First creation.
! --------------------------------------------------------------------------
use utility
private

    integer,parameter :: STRLEN=300
    integer :: n_arg
    character(len=STRLEN),allocatable :: args(:)
    character(len=STRLEN) :: strtemp

    public :: args_readargs, args_getarg, args_end, args_get_narg, args_get_paraidx
    interface args_getarg
        module procedure args_getarg_int_byidx
        module procedure args_getarg_int_bytitle
        module procedure args_getarg_flt_byidx
        module procedure args_getarg_flt_bytitle
        module procedure args_getarg_dbl_bytitle
        module procedure args_getarg_dbl_byidx
        module procedure args_getarg_str_byidx
        module procedure args_getarg_str_bytitle
        module procedure args_getarg_lgc_byidx
    end interface 

contains

    subroutine args_readargs()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_readargs"
        integer :: i, stat
        n_arg=command_argument_count()
        allocate(args(n_arg))
        do i=1,n_arg
            call get_command_argument(i, args(i), status=stat)
            if(stat /= 0) RP_ERROR("Unable to get arg #"//trim(tostring(ival=i)))
        end do
    end subroutine args_readargs  !!}}}
    
    function args_get_narg()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_get_narg"
        integer :: args_get_narg
        args_get_narg=n_arg
    end function args_get_narg  !!}}}

    !!return -1 if not find
    function args_get_paraidx(para, ignore_case)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_get_paraidx"
        character(len=*),intent(in) :: para
        logical :: ignore_case
        integer :: i, args_get_paraidx
        if(ignore_case) then
            do i=1, n_arg
                 if(biggercase(trim(para)) == biggercase(trim(args(i)))) then
                     args_get_paraidx=i
                     return
                 end if
            end do
        else
            do i=1, n_arg
                 if(trim(para) == trim(args(i))) then
                     args_get_paraidx=i
                     return
                 end if
            end do
        end if
        args_get_paraidx=-1
    end function args_get_paraidx  !!}}}

    subroutine args_end()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_end"
        deallocate(args)
        n_arg=0
    end subroutine args_end  !!}}}

!! getarg clusters
    subroutine args_getarg_int_byidx(idx, para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_int_byidx"
        integer,intent(in) :: idx
        integer,intent(out) :: para
        if(idx > n_arg) RP_ERROR("idx you pass in = "//trim(tostring(ival=idx))//" while n_arg = "//trim(tostring(ival=n_arg)))
        read(args(idx), "(I)") para
    end subroutine args_getarg_int_byidx  !!}}}
    subroutine args_getarg_str_byidx(idx, para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_str_byidx"
        integer,intent(in) :: idx
        character(len=*),intent(out) :: para
        if(idx > n_arg) RP_ERROR("idx you pass in = "//trim(tostring(ival=idx))//" while n_arg = "//trim(tostring(ival=n_arg)))
        if(len(para) < len_trim(args(idx))) RP_ERROR("para you pass is too short to hold the argument")
        para=trim(args(idx))
    end subroutine args_getarg_str_byidx  !!}}}
    subroutine args_getarg_flt_byidx(idx, para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_flt_byidx"
        integer,intent(in) :: idx
        real(kind=4),intent(out) :: para
        if(idx > n_arg) RP_ERROR("idx you pass in = "//trim(tostring(ival=idx))//" while n_arg = "//trim(tostring(ival=n_arg)))
        read(args(idx), "(F)") para
    end subroutine args_getarg_flt_byidx  !!}}}
    subroutine args_getarg_dbl_byidx(idx, para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_dbl_byidx"
        integer,intent(in) :: idx
        real(kind=8),intent(out) :: para
        if(idx > n_arg) RP_ERROR("idx you pass in = "//trim(tostring(ival=idx))//" while n_arg = "//trim(tostring(ival=n_arg)))
        read(args(idx), "(F)") para
    end subroutine args_getarg_dbl_byidx  !!}}}
    subroutine args_getarg_lgc_byidx(idx, para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_lgc_byidx"
        integer,intent(in) :: idx
        logical,intent(out) :: para
        if(idx > n_arg) RP_ERROR("idx you pass in = "//trim(tostring(ival=idx))//" while n_arg = "//trim(tostring(ival=n_arg)))
        read(args(idx), *) para
    end subroutine args_getarg_lgc_byidx  !!}}}
    !!e.g.  -o output, get output by title = -o
    subroutine args_getarg_int_bytitle(title, para, default_para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_int_bytitle"
        character,intent(in) :: title
        integer,intent(in) :: default_para
        integer,intent(out) :: para
        integer :: i
        do while(trim(args(i)) /= trim(title))
            i=i+1
        end do
        if(i< n_arg) then 
            read(args(i+1), "(I)") para
        else
            para=default_para
        end if
    end subroutine args_getarg_int_bytitle  !!}}}
    subroutine args_getarg_str_bytitle(title, para, default_para)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_str_bytitle"
        character,intent(in) :: title
        character(len=*),intent(in) :: default_para
        character(len=*),intent(out) :: para
        integer :: i
        do while(trim(args(i)) /= trim(title))
            i=i+1
        end do
        if(i< n_arg) then 
            if(len(para) < len_trim(args(i+1))) RP_ERROR("para you pass is too short to hold the argument")
            para=trim(args(i+1))
        else
            para=default_para
        end if
    end subroutine args_getarg_str_bytitle  !!}}}
    subroutine args_getarg_flt_bytitle(title, para, default_para )  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_flt_bytitle"
        character,intent(in) :: title
        real(kind=4),intent(in) :: default_para
        real(kind=4),intent(out) :: para
        integer :: i
        do while(trim(args(i)) /= trim(title))
            i=i+1
        end do
        if(i< n_arg) then 
            read(args(i+1), "(F)") para
        else
            para=default_para
        end if
    end subroutine args_getarg_flt_bytitle  !!}}}
    subroutine args_getarg_dbl_bytitle(title, para, default_para )  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="args_getarg_dbl_bytitle"
        character,intent(in) :: title
        real(kind=8),intent(in) :: default_para
        real(kind=8),intent(out) :: para
        integer :: i
        do while(trim(args(i)) /= trim(title))
            i=i+1
        end do
        if(i< n_arg) then 
            read(args(i+1), "(F)") para
        else
            para=default_para
        end if
    end subroutine args_getarg_dbl_bytitle  !!}}}



 end module
