!! ctlfile : zdef must have one value in one line after the line :zdef
subroutine rectl(ctlfile, max_level, nlevel, levels,  max_var, nvar, varnames, varlevels)  !!{{{
!subroutine resolve_gradsctl(ctlfile, max_level, nlevel, levels,  max_var, nvar, varnames, varlevels)  !!{{{
implicit none
    character(len=*) :: ctlfile
    integer :: max_level, nlevel, max_var, nvar
    real :: levels(max_level)
    integer :: varlevels(max_var)
    character(len=20) :: varnames(max_var)
    character(len=200) :: line, firstitem
    logical :: inzdef, invars
    integer :: count_level,count_var,stat

    open(17,file=trim(ctlfile))
    inzdef=.false.
    invars=.false.
    count_level=0
    count_var=0
    do while(1==1)
        read(17,'(A)',iostat=stat) line
        if(stat < 0) exit
        read(line,*) firstitem
        if(trim(firstitem) == "zdef") then
            read(line,*) firstitem, nlevel
            print*,'z level=', nlevel
            inzdef=.true.
            cycle
        end if
        if(trim(firstitem) == "tdef") then
            inzdef=.false.
            cycle
        endif
        if(inzdef) then
            count_level=count_level+1
            read(line,*) levels(count_level)
            print*,count_level, levels(count_level)
            cycle
        end if
        if(trim(firstitem) == "vars") then
            read(line,*) firstitem, nvar
            print*,"vars ", nvar
            invars=.true.
            cycle
        end if
        if(trim(firstitem) == "endvars") then
            invars=.false.
            cycle
        end if
        if(invars) then
            count_var=count_var+1
            read(line,*) varnames(count_var),varlevels(count_var)
            if(varlevels(count_var) <= 0) then
                varlevels(count_var)=1
            end if
            print*,count_var, varnames(count_var), varlevels(count_var)
            cycle
        end if
    end do
    close(17)
!!end subroutine resolve_gradsctl  !!}}}
end subroutine rectl  !!}}}

program main
implicit none
    integer :: nlev,nvar,i
    real :: levels(50)
    integer :: varlevels(30)
    character(len=15) :: var(30)
    call rectl("/disk1/home/shensi/GRAPES/GRAPES_GFS/climate_profile/run/2/post.ctl_2013050106", &
                            50, nlev, levels, 30, nvar, var, varlevels)
    print*,levels(1:nlev)
    print*,"--------------------"
    do i=1,nvar
        print*,var(i)
    end do
    print*,"--------------------"
    print*,varlevels(1:nvar)


end program main
