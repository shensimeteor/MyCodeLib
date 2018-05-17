module sort
use utility
implicit none
private

    public :: sorti
    interface sorti
        module procedure sorti_insert_float
        module procedure sorti_insert_integer
    end interface

contains

!!order = "ascend" or "descend"
!!NOT sort in_array itself, get out_indx, make in_array(out_indx(i)) <= in_array(out_indx(i+1)), for ascend
!! in_array(out_indx(i)) >= in_array(out_indx(i+1)) , for descend
subroutine sorti_insert_float(in_array, out_indx, order)
    real :: in_array(:)
    integer :: out_indx(:)
    integer :: n,i,j,t
    character(len=*) :: order
    integer :: i_order
    n=size(in_array)
    if(trim(lowercase(order)) == "ascend") then
        i_order = 1
    else if(trim(lowercase(order)) == "descend" )  then
        i_order = -1
    else 
        call print_message("error", "in SORTI_INSERT_FLOAT: the order you pass in can not be RECOGNIZED", __FILE__,__LINE__)
    end if
    
    out_indx=(/1:n:1/)
    do i=1,n
        do j=i+1,n
            if(  (in_array(out_indx(j)) - in_array(out_indx(i))) * i_order < 0 ) then
                t=out_indx(j)
                out_indx(j)=out_indx(i)
                out_indx(i)=t
            end if
        end do
    end do
end subroutine


subroutine sorti_insert_integer(in_array, out_indx, order)
    integer :: in_array(:)
    integer :: out_indx(:)
    integer :: n,i,j,t
    character(len=*) :: order
    integer :: i_order
    n=size(in_array)
    if(trim(lowercase(order)) == "ascend") then
        i_order = 1
    else if(trim(lowercase(order)) == "descend" )  then
        i_order = -1
    else 
        call print_message("error", "in SORTI_INSERT_FLOAT: the order you pass in can not be RECOGNIZED", __FILE__,__LINE__)
    end if
    
    out_indx=(/1:n:1/)
    do i=1,n
        do j=i+1,n
            if(  (in_array(out_indx(j)) - in_array(out_indx(i))) * i_order < 0 ) then
                t=out_indx(j)
                out_indx(j)=out_indx(i)
                out_indx(i)=t
            end if
        end do
    end do
end subroutine

end module

