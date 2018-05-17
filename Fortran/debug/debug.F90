module module_debug 
   use module_stack 
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

end module module_debug
