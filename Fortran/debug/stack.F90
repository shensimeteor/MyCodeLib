!! stack container. tested
module stack
   type :: s_data

      character(len=256) :: funcname
      character(len=256) :: filename
      integer :: callline
      integer :: calltime

   end type s_data

   type :: s_item
      type(s_data) :: itemdata
      type(s_item),pointer :: next
   end type s_item

   type :: type_stack
      type(s_item),pointer :: top
      integer :: length
   end type type_stack

   contains

   !! s_init ,initialize a type(stack) object
   subroutine s_init(s)
      implicit none
      type(type_stack),intent(out) :: s
      nullify(s%top);
      s%length=0
   end subroutine s_init

   !! s_push, ierr=0 success
   subroutine s_push(s,sdata,ierr)
      implicit none
      type(type_stack),intent(inout) :: s
      type(s_data),intent(in) :: sdata
      integer,intent(out) :: ierr
      type(s_item),pointer :: sitem
      allocate(sitem,STAT=ierr)
      if(ierr /= 0) return
      sitem%itemdata = sdata
      sitem%next => s%top
      s%top => sitem
      s%length = s%length+1
   end subroutine s_push

   !! s_isemtpy,flag=1 empty
   subroutine s_isempty(s,flag)
      implicit none
      type(type_stack),intent(in) :: s
      integer ,intent(out) :: flag
      if( associated(s%top) == .true.) then
         flag=0
      else
         flag=1
      end if
   end subroutine s_isempty

   !! s_pop,ierr=0 success,=1,fail because empty
   subroutine s_pop(s,topdata,ierr)
      implicit none
      type(type_stack),intent(inout) :: s
      type(s_data),intent(out) :: topdata
      integer ,intent(out) :: ierr
      integer :: flag
      type(s_item),pointer :: topitem
      call s_isempty(s,flag)
      if(flag==1) then
         ierr=1
         return
      end if
      ierr=0
      topdata=s%top%itemdata
      topitem => s%top
      s%top => s%top%next
      deallocate(topitem)
      s%length=s%length-1
   end subroutine s_pop

   !! s_top,return top data
   subroutine s_top(s,topdata,ierr)
      implicit none
      type(type_stack),intent(inout) :: s
      type(s_data),intent(out) :: topdata
      integer ,intent(out) :: ierr
      integer :: flag
      call s_isempty(s,flag)
      if(flag==1) then
         ierr=1
         return
      end if
      ierr=0
      topdata=s%top%itemdata
   end subroutine s_top

   !! s_destroy
   subroutine s_destroy(s)
      implicit none
      type(type_stack),intent(inout):: s
      type(s_item),pointer :: p,p2
      p => s%top
      do while(associated(p) == .true.)
         p2 => p
         p => p%next
         deallocate(p2)
      end do
      nullify(s%top)
      s%length=0
   end subroutine s_destroy
end module stack


module debug 
   use stack 
   use utility
   private

   public :: debug_init, pre_proc, after_proc, print_messagex

   integer,parameter :: MAX_CALLS=1000  !! >=number of calls(subroutines &  functions) need to be counted in all files
   type(s_data) :: proc_array(MAX_CALLS) !! every call have an element on array
   integer :: arr_len !! proc_array_length till now
   type(stack) :: proc_stack
   character(len=500) :: dbg_printstr,dbg_printstr2

   contains
   
   !! call one time before all other funcs(subrs) involkes
   subroutine debug_init() !!{{{
      implicit none
      call s_init(proc_stack)
      arr_len=0
   end subroutine debug_init  !!}}}

   !!iscount==1, count how many times call  to  proc  in this line in this func
   !of this file happens ; 0, no count is needed
   !! should be involked only compiled with DEBUG
   subroutine pre_proc(funcname,filename,line,iscount) !!{{{
      implicit none
      character(len=*),intent(in) :: funcname,filename
      integer,intent(in) :: line
      integer,intent(in) :: iscount
      integer :: i,j,cnt,ierr
      type(s_data) :: funcdata
      
      cnt=0
      funcdata%funcname=trim(funcname)
      funcdata%filename=trim(filename)
      funcdata%callline=line
      if(iscount==1) then
         do i=arr_len,1,-1  !! descend,  may find the match quicker
            if(trim(proc_array(i)%funcname) == trim(funcdata%funcname) .and. &
               trim(proc_array(i)%filename) == trim(funcdata%filename) .and. &
               proc_array(i)%callline == funcdata%callline )  then
               cnt=proc_array(i)%calltime+1
               proc_array(i)%calltime=cnt
            end if
         end do
         if(cnt == 0) then
            arr_len=arr_len+1
            proc_array(arr_len)=funcdata
            proc_array(arr_len)%calltime=1
         end if
      else
         cnt = -1 !! means no count the calltime
      end if
      funcdata%calltime=cnt
      call s_push(proc_stack,funcdata,ierr)
      if(ierr /= 0) then
        call print_messagex("error", "pre_proc->s_push, allocate fails", &
                 trim(funcdata%filename), funcdata%callline, trim(funcdata%funcname))
      end if
   end subroutine pre_proc !!}}}

   !!should only be involked when compiled with DEBUG
   subroutine after_proc()  !!{{{
      implicit none
      integer :: ierr
      type(s_data) :: funcdata
      call s_pop(proc_stack,funcdata,ierr)
      if(ierr /= 0) then
        call print_messagex("error", "after_proc->s_pop, stack is empty", &
                 trim(funcdata%filename), funcdata%callline, trim(funcdata%funcname))
      end if
   end subroutine after_proc  !!}}}

  !!msgtype: error: stop & print file/line messages(in stacks if -DDEBUG)
  !!         pause: pause (just pause & print msgstring)
  !!         warning: warning & print file/line message(not stack)
  !!         log: just print msgstring
   subroutine print_messagex(msgtype, msgstring, msgfile, msgline, msgfunc) !!{{{
   implicit none
      character(len=*),intent(in) :: msgtype,msgstring,msgfile,msgfunc
      integer,intent(in) :: msgline
      integer :: isempty
      type(s_data) :: funcdata
      character(len=5) :: strtime

      funcdata%filename=trim(msgfile)
      funcdata%funcname=trim(msgfunc)
      funcdata%callline=msgline !! here actually not a line meet call, but a line meet error,i.e. a line call this func
      funcdata%calltime=-1
      isempty=0
      dbg_printstr=biggercase(trim(msgtype)) //": "//trim(msgstring)
      select case(lowercase(trim(msgtype)))
      case("log")
        print*,trim(dbg_printstr)
      case("warning")
        print*,trim(dbg_printstr)
        write(dbg_printstr2,'(A30,A3,A30,A3,I5)') trim(funcdata%filename),'  @',trim(funcdata%funcname),'  #', funcdata%callline
        print*,trim(dbg_printstr2)
      case("pause")
        print*,trim(dbg_printstr)
        pause
      case("error")
        do while(isempty == 0)
          if(funcdata%calltime < 0 ) then
             strtime='     *'
          else
             read(strtime,'(I)') funcdata%calltime
          end if
          write(*,'(A30,A3,A30,A3,I5,A3,A5)') trim(funcdata%filename),'  @',trim(funcdata%funcname), '  #', funcdata%callline, '   ',strtime
          call s_pop(proc_stack,funcdata,isempty)
        end do
        stop
      end select
   end subroutine print_messagex !!}}}

end module debug
