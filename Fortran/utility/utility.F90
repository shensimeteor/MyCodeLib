! --------------------------------------------------------------------------
! Description:
!
!   some useful procedures
!
! History:
!
!   2014-06-27:
!
!       [shensi]: First creation.
! --------------------------------------------------------------------------
module utility
implicit none
    integer,parameter :: UTILITY_UNIT_SCREEN=6 !! in read\write , *
    
contains

!!1. STRING
function lowercase(string)  !!{{{
    character(len=*)::string
    character(len=len(string))::lowercase
    integer :: i,lenstr
    lenstr=len(string)
    do i=1,lenstr
        if (ichar(string(i:i)) >= ichar('A')  .and. ichar(string(i:i)) <= ichar('Z'))  then
            lowercase(i:i)=char(ichar(string(i:i))+ichar('a')-ichar('A'))
        else
            lowercase(i:i)=string(i:i)
        end if
    end do 
end function  !!}}}

function biggercase(string) !!{{{
    character(len=*)::string
    character(len=len(string))::biggercase
    integer :: i,lenstr
    lenstr=len(string)
    do i=1,lenstr
        if (ichar(string(i:i)) >= ichar('a')  .and. ichar(string(i:i)) <= ichar('z'))  then
            biggercase(i:i)=char(ichar(string(i:i))+ichar('A')-ichar('a'))
        else
            biggercase(i:i)=string(i:i)
        end if
    end do
end function  !!}}}

!!fmat is the # in write( ,#) , it must be enclosed by ( )
!!ival: integer, fval: real(same as compile-setting,default 4), dval:double, sval: float
function tostring(ival,fval,sval,dval,fmat)  !!{{{
    integer,optional :: ival
    real,optional :: fval
    real(kind=4),optional :: sval
    real(kind=8),optional :: dval
    character(len=*),optional :: fmat
    logical :: stat(4)
    character(len=30)::tostring
    stat(1)=present(ival)
    stat(2)=present(fval)
    stat(3)=present(sval)
    stat(4)=present(dval)
    if(count(stat) /= 1) then
        call print_message("warning","no input is supplied to function: tostring")
        tostring=" "
        return
    end if
    if(present(fmat)) then
        if(present(ival)) then
            write(tostring,trim(fmat))  ival
        else if(present(fval)) then
            write(tostring, trim(fmat)) fval
        else if(present(sval)) then
            write(tostring, trim(fmat)) sval
        else
            write(tostring,trim(fmat))  dval
        end if
    else
        if(present(ival)) then
            write(tostring,*)  ival
        else if(present(fval)) then
            write(tostring, *) fval
        else if(present(sval)) then
            write(tostring, *) sval
        else
            write(tostring,*)  dval
        end if
    end if
    tostring=trim(adjustl(tostring))
end function  !!}}}


function array2string(iarray, separator)
implicit none
    integer :: iarray(:)
    character(len=*):: separator
    character(len=200) :: array2string, fmtstr
    integer i, n
    n=size(iarray,1)
    !fmtstr = '(' // trim(tostring(ival=n-1)) // "(I,'" // separator //"'), I)" 
    !write(array2string, fmtstr) iarray
    !array2string=trim(adjustl(array2string))
    array2string=""
    do i=1,n-1
        array2string=trim(array2string)//trim(tostring(ival=iarray(i)))//separator
    end do
    array2string=trim(array2string)//trim(tostring(ival=iarray(n)))
end function

    !! split string into nsub subs (subs must be unallocated allocatable string), separated by ch
    !! if unfound ch, return nsub=1, subs(1)=string
    subroutine str_split(string, ch, nsub, subs)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="str_split"
        character(len=*) :: string
        character(len=1) :: ch
        integer :: nsub !!out
        character(len=*), allocatable :: subs(:) !!out
        integer :: i, lenstr, d1, d2
        lenstr=len_trim(string)
        nsub=1
        do i=1, lenstr
            if(string(i:i) == ch) then
                nsub=nsub+1
            end if
        end do
        if(allocated(subs)) then
            deallocate(subs)
        end if
        allocate(subs(nsub))
        d1=1
        do i=1, nsub-1
            d2=scan(string(d1:lenstr), ch)
            subs(i)=string(d1:d2+d1-2)
            d1=d1+d2
        end do
        subs(nsub)=string(d1:lenstr)
    end subroutine str_split  !!}}}



!!2. PROGRAM PRINTOUT
!!msgtype: log(normal); warning; pause(to pause); error(to stop);log: [msgstring] <[ms>
subroutine print_message(msgtype,msgstring,msgfile,msgline,msgproc) !!{{{
    character(len=*) :: msgtype,msgstring
    character(len=*),optional ::msgfile
    integer(kind=8),optional :: msgline
    character(len=*),optional :: msgproc
    character(len=1024) :: printstr1
    character(len=30) :: strline
    integer,parameter :: ERROR_RETURN_CODE=2
    printstr1=biggercase(trim(msgtype))
    if(present(msgfile)) then
        printstr1=trim(printstr1)//":@"//trim(msgfile)
    end if
    if(present(msgproc)) then
        printstr1=trim(printstr1)//"("//trim(msgproc)//")"
    end if
    if(present(msgline)) then
        write(strline,*)msgline
        printstr1=trim(printstr1)//" #"//trim(adjustl(strline))
    end if
    select case(lowercase(trim(msgtype)))
    case ("log")
       print*,"[LOG]:",trim(msgstring)
    case ( "warning" )
       print*,"[WARNING]:",trim(msgstring)
    case( "pause" )
       print*,trim(printstr1)
       print*,"---",trim(msgstring)
       pause
    case( "error" ) 
       print*,trim(printstr1)
       print*,"---",trim(msgstring)
       stop ERROR_RETURN_CODE
    end select

end subroutine print_message  !!}}}



!!3. INPUT & OUTPUT
!! allocate & read lines from file:filename
subroutine allocread_line(filename,nline,lines) !!{{{
    character(len=*) :: filename
    integer :: nline
    character(len=*),allocatable :: lines(:)
    integer :: i,stat
    open(199,file=trim(filename),status="OLD",iostat=stat)
    if(stat /= 0) then
        call  print_message("error","can not open "//trim(filename), __FILE__,__LINE__)
    end if
    i=0
    do 
        read(199,*,iostat=stat)
        if(stat /= 0) exit
        i=i+1
    end do
    nline=i
    if(allocated(lines)) then
        deallocate(lines)
    end if
    allocate(lines(nline))
    lines=" " 
    rewind(199)
    do i=1,nline
        read(199,"(A)",iostat=stat) lines(i)
        if(stat /= 0) then
            call print_message("error","fail in reading "//trim(filename),__FILE__,__LINE__)
        end if
    end do
    close(199)
end subroutine allocread_line !!}}}
    
!! write A in line & rows into funit or the fname file(will open it) or screen (both funit & fname not supplied)
!! element_fmat is e.g. "F8.3" , the format string for every single element in the matrix
subroutine write_float_matrix2(A,funit,fname,element_fmat)  !!{{{
    real :: A(:,:)
    integer :: n1,n2,i
    integer,optional :: funit
    character(len=*) ,optional :: fname,element_fmat
    character(len=50) :: fmat
    integer :: fileunit
    n1=size(A,1)
    n2=size(A,2)
    if(present(element_fmat)) then
        write(fmat,*) '(',n2,'(',trim(element_fmat),'))'
    else
        write(fmat,*) '(',n2,'F)'
    end if
    if(present(fname)) then
        fileunit=get_funit(90)
        open(fileunit,file=trim(fname)) 
    else if(present(funit)) then
        fileunit=funit
    else
        fileunit=UTILITY_UNIT_SCREEN
    end if
    do i=1,n1
        write(fileunit,fmat) A(i,:)
    end do
    if(present(fname)) then
        close(fileunit)
    end if
end subroutine  !!}}}

subroutine write_integer_matrix2(A,funit,fname,element_fmat)  !!{{{
    integer,intent(in) :: A(:,:)
    integer :: n1,n2,i
    integer,optional :: funit
    character(len=*) ,optional :: fname,element_fmat
    character(len=50) :: fmat
    integer :: fileunit
    n1=size(A,1)
    n2=size(A,2)
    if(present(element_fmat)) then
        write(fmat,*) '(',n2,'(',trim(element_fmat),'))'
    else
        write(fmat,*) '(',n2,'I)'
    end if
    if(present(fname)) then
        fileunit=get_funit(90)
        open(fileunit,file=trim(fname)) 
    else if(present(funit)) then
        fileunit=funit
    else
        fileunit=UTILITY_UNIT_SCREEN
    end if
    do i=1,n1
        write(fileunit,fmat) A(i,:)
    end do
    if(present(fname)) then
        close(fileunit)
    end if
end subroutine  !!}}}

!!return -1 if not find
function get_funit(begin_search_unit) !!{{{
implicit none
    integer :: begin_search_unit,get_funit
    logical :: unit_used
    integer,parameter :: MAX_UNIT=999
    get_funit = begin_search_unit
    do while(get_funit <= MAX_UNIT)
       inquire(get_funit, opened=unit_used)
       if(.not. unit_used) return
       get_funit=get_funit + 1 
    end do
    get_funit=-1
end function !!}}} 

!!/usr/local/bin/yes.dll -> yes.dll, assume no / in file name
function kickout_filepath(filepath)  !!{{{
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="kickout_filepath"
    character(len=*) :: filepath
    character(len=300) :: kickout_filepath
    integer :: idx, lenstr
    idx = index(filepath, '/', .true.)
    lenstr=len_trim(filepath)
    kickout_filepath=filepath(idx+1:lenstr)
end function kickout_filepath  !!}}}

!! add/replace filesuffix
!!yes.xxx -> yes.<new> ; yes -> yes.<new>
function replace_filesuffix(filename, new)  !!{{{
implicit none
    character(len=*),parameter :: PROCEDURE_NAME="replace_filesuffix"
    character(len=*) :: filename,new
    character(len=300) :: replace_filesuffix
    integer :: idx
    idx=index(trim(filename), '.', .true.)
    if(idx>0) then
        replace_filesuffix=filename(1:idx)//trim(new)
    else
        replace_filesuffix=trim(filename)//"."//trim(new)
    end if
end function replace_filesuffix  !!}}}


!!4. METEOROLOGY
!! for periodic lon, calculate lon1-lon2 along the shorter circle arc, e.g. 30-350=40,340-50=-70
!! valid for all kinds of lon
function lon_neighbor_diff(lon1,lon2) !!{{{
    real :: lon_neighbor_diff
    real :: lon1,lon2
    real :: dist1
    dist1=lon1-lon2
    do while(dist1<0)
        dist1=dist1+360.0
    end do
    do while(dist1>180)
        dist1=dist1-360
    end do
    lon_neighbor_diff=dist1
end function  !!}}}


!!5. Math
function get_average(x) !!{{{
implicit none
    real :: x(:)
    real :: get_average
    get_average=sum(x)/size(x)
end function  !!}}}

!!get covariance of(x1/x2) or variance of (x1=x2)
function get_cova_anomaly(x1, x2, op_isbias) !!{{{
implicit none
    real :: x1(:), x2(:)
    real :: get_cova_anomaly, x1avg, x2avg
    logical,optional :: op_isbias
    integer :: nx
    if(size(x1) /= size(x2)) then
        call print_message("error","x1 size /= x2 size",__FILE__,__LINE__)
    end if
    nx=size(x1)
    x1avg=sum(x1)/nx
    x2avg=sum(x2)/nx
    if(present(op_isbias) .and. op_isbias) then
        get_cova_anomaly=sum((x1-x1avg)*(x2-x2avg))/(nx)
    else
        get_cova_anomaly=sum((x1-x1avg)*(x2-x2avg))/(nx-1)
    end if
end function !!}}}

function get_cova_origin(x1, x2, op_isbias) !!{{{
implicit none
    real :: x1(:), x2(:)
    real :: get_cova_origin
    logical,optional :: op_isbias
    integer :: nx
    if(size(x1) /= size(x2)) then
        call print_message("error","x1 size /= x2 size",__FILE__,__LINE__)
    end if
    nx=size(x1)
    if(present(op_isbias) .and. op_isbias) then
        get_cova_origin=sum(x1*x2)/(nx)
    else
        get_cova_origin=sum(x1*x2)/(nx-1)
    end if
end function !!}}}

function get_corr(x1, x2)  !!{{{
implicit none
    real :: x1(:), x2(:)
    integer :: nx
    real :: get_corr, cov, var1, var2, x1avg, x2avg
    if(size(x1) /= size(x2)) then
        call print_message("error","x1 size /= x2 size",__FILE__,__LINE__)
    end if
    nx=size(x1)
    x1avg=sum(x1)/nx
    x2avg=sum(x2)/nx
    cov=sum((x1-x1avg)*(x2-x2avg))
    var1=sum((x1-x1avg)*(x1-x1avg))
    var2=sum((x2-x2avg)*(x2-x2avg))
    get_corr=cov/sqrt(var1*var2)
end function get_corr  !!}}}


!!6. Array
    subroutine fspan(start,finish,npts,array) !!{{{
    implicit none
        real :: start, finish, deta
        integer :: npts,i
        real :: array(npts)
        deta=(finish-start)/(npts-1)
        do i=1,npts
            array(i)=start+deta*(i-1)
        end do
     end subroutine fspan  !!}}}

    subroutine ispan(start, increment, npts, array) !!{{{
    implicit none
        integer :: start, increment, npts
        integer :: array(npts), i 
        do i=1,npts
            array(i) = start + increment * (i-1)
        end do
    end subroutine ispan !!}}}


    !!output array ind must have enough space
    subroutine getind_int(array, cmp, ind, nind)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="getind_int"
        integer :: array(:), cmp, nin
        integer :: ind(:), nind
        integer :: i, cnt, nout
        nin=size(array)
        nout=size(ind)
        cnt=0
        do i=1, nin
            if(array(i) == cmp) then
                cnt=cnt+1
                if(cnt > nout) then
                    call print_message("warning","out-parameter: ind is too small to hold results, skip the rest")
                    nind=nout
                    return
                end if
                ind(cnt)=i
            end if
        end do
        nind=cnt
    end subroutine getind_int  !!}}}

    subroutine getind_str(array, cmp, ind, nind)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="getind_str"
        character(len=*) :: array(:), cmp
        integer :: ind(:), nind, cnt, i, nout, nin
        nin=size(array)
        nout=size(ind)
        cnt=0
        do i=1, nin
            if(trim(array(i)) == trim(cmp)) then
                cnt=cnt+1
                if(cnt > nout) then
                    call print_message("warning","out-parameter: ind is too small to hold results, skip the rest")
                    nind=nout
                    return
                end if
                ind(cnt)=i
            end if
        end do
        nind=cnt
    end subroutine getind_str  !!}}}

    subroutine getind_flt(array, cmp, ind, nind, tolerance)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="getind_flt"
        real :: array(:), cmp, tolerance
        integer :: ind(:), nind, cnt, i, nout, nin
        nin=size(array)
        nout=size(ind)
        cnt=0
        do i=1, nin
            if( array(i) < cmp+tolerance .and. array(i) > cmp-tolerance) then
                cnt=cnt+1
                if(cnt > nout) then
                    call print_message("warning","out-parameter: ind is too small to hold results, skip the rest")
                    nind=nout
                    return
                end if
                ind(cnt)=i
            end if
        end do
        nind=cnt
    end subroutine getind_flt  !!}}}
    
    
!!7. System
    function str_time(t_pre)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="str_time"
        real :: t, t_pre
        character(len=10) :: str_time
        call cpu_time(t)
        write(str_time, '(F10.4)') t-t_pre
    end function str_time  !!}}}

end module
