subroutine cquad(a0,b,c,nq,x,quad) !!{{{
    !!!
    implicit none
    integer, intent(in)  ::nq
    real,    intent(in)  ::a0,b,c,x(nq)
    real,    intent(out) ::quad(nq)
    integer              ::i

    quad = a0*x**2+b*x+c

    return
    !!yes
end subroutine cquad !!}}}
