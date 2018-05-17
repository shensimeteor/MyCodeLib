#include "inc_common.fpp"
module interp
use utility
implicit none

    ! variables
!!terminology define
! REgular Grid (reg) : uniformly distributed axis, defined by sx(start), nx(number), dx(step, must>0), like lon/lat
! NorMal Grid (nmg): like z or pressure, monotonous axis but steps are not uniformly distributed
! SCaTter (sct): a array of any order, containing positions of scatters
! reg (belongs to) nmg (belongs to) sct
contains

    ! methods
    !! regular grids to scatter points
    !! regular-grid defined by : sx(start_x), nx(number of points), dx(distance between points), iscycle
    !! if no iscycle, point outside domain(sx, sx+(nx-1)*dx) is fill with x1(1) .or. x1(nx)
    subroutine interp_1d_linear_reg2sct(sx, nx, dx, iscycle, v1, x2, v2)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_1d_linear_reg2sct"
        integer :: nx, nx2, i, idx1, idx2
        real :: dx, v1(:), x2(:), v2(:), sx, dis, wgt1, wgt2
        logical :: iscycle
        if(size(v1)/=nx) then
            RP_ERROR(" size(v1) /= nx ")
        end if
        if(size(x2)/=size(v2)) then
            RP_ERROR(" size(v2) /= size(x2) ")
        end if
        nx2=size(x2)
        do i=1, nx2
            dis=(x2(i)-sx)/dx
            idx1=floor(dis)+1
            idx2=idx1+1
            wgt1=1-(dis-floor(dis))
            wgt2=dis-floor(dis)
            if(iscycle) then
                if(idx1<1) idx1=nx+idx1
                if(idx2<1) idx2=nx+idx2
                if(idx1>nx) idx1=idx1-nx
                if(idx2>nx) idx2=idx2-nx
            else
                if(idx1<1) then
                    idx1=1
                    idx2=1
                    wgt1=0.5
                    wgt2=0.5
                end if
                if(idx1>=nx) then
                    idx1=nx
                    idx2=nx
                    wgt1=0.5
                    wgt2=0.5
                end if
            end if
            !!TRACE4(idx1, idx2, wgt1, wgt2)
            v2(i)=v1(idx1)*wgt1+v1(idx2)*wgt2
        end do
    end subroutine interp_1d_linear_reg2sct  !!}}}

    subroutine interp_1d_linear_nmg2sct(x1, v1, x2, v2)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_1d_linear_nmg2sct"
        real :: x1(:), x2(:), v1(:), v2(:)
        integer :: nx, nx2, i, j, idx1, idx2
        real :: dis, wgt1, wgt2
        logical :: found
        ASSUREX(size(x1) == size(v1))
        ASSUREX(size(x2) == size(v2))
        nx=size(x1)
        nx2=size(x2)
        do i=1, nx2
            found=.false.
            do j=1, nx-1
                if( (x2(i)-x1(j))*(x2(i)-x1(j+1)) <= 0 ) then
                    idx1=j
                    idx2=j+1
                    wgt1=(x1(j+1)-x2(i))/(x1(j+1)-x1(j))
                    wgt2=(x2(i)-x1(j))/(x1(j+1)-x1(j))
                    found=.true.
                    exit
                end if
            end do
            if(.not. found) then
                if( (x2(i)-x1(1))*(x1(1)-x1(2)) > 0) then !! outside of xcoord(1)
                    idx1=1
                    idx2=1
                    wgt1=0.5
                    wgt2=0.5
                else if( (x2(i)-x1(nx))*(x1(nx)-x1(nx-1)) > 0) then !!
                    idx1=nx
                    idx2=nx
                    wgt1=0.5
                    wgt2=0.5
                end if
            end if
            v2(i)=v1(idx1)*wgt1+v1(idx2)*wgt2
        end do
    end subroutine interp_1d_linear_nmg2sct  !!}}}

    subroutine interp_1d_linear_reg2sct_getWeight(sx, nx, dx, iscycle, x2, neighbor, weight)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_1d_linear_reg2sct_getWeight"
        integer :: nx, neighbor(:,:), nx2 !!
        real :: sx, dx, x2(:), weight(:,:) !!
        logical :: iscycle
        integer :: i,j, idx1, idx2
        real :: wgt1, wgt2, dis
        nx2=size(x2)
        ASSUREX(size(neighbor(:,:),1) == nx2)
        ASSUREX(size(weight(:,:),1) == nx2)
        ASSUREX(size(neighbor(:,:),2) == 2)
        ASSUREX(size(weight(:,:),2) == 2)
        do i=1, nx2
            dis=(x2(i)-sx)/dx
            idx1=floor(dis)+1
            idx2=idx1+1
            wgt1=1-(dis-floor(dis))
            wgt2=(dis-floor(dis))
            if(iscycle) then
                if(idx1<1) idx1=nx+idx1
                if(idx2<1) idx2=nx+idx2
                if(idx1>nx) idx1=idx1-nx
                if(idx2>nx) idx2=idx2-nx
            else
                if(idx1<1) then
                    idx1=1
                    idx2=1
                    wgt1=0.5
                    wgt2=0.5
                end if
                if(idx1>=nx) then
                    idx1=nx
                    idx2=nx
                    wgt1=0.5
                    wgt2=0.5
                end if
            end if
            neighbor(i,1)=idx1
            neighbor(i,2)=idx2
            weight(i,1)=wgt1
            weight(i,2)=wgt2
        end do
    end subroutine interp_1d_linear_reg2sct_getWeight  !!}}}
   
    subroutine interp_1d_linear_nmg2sct_getWeight(xcoord, x2, neighbor, weight)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_1d_linear_nmg2sct_getWeight"
        real :: xcoord(:), x2(:), weight(:,:)
        integer :: neighbor(:,:)
        integer :: nx, nx2, i, j, idx1, idx2
        real :: dis, wgt1, wgt2
        logical :: found
        nx=size(xcoord)
        nx2=size(x2)
        ASSUREX(size(neighbor(:,:),1) == nx2)
        ASSUREX(size(weight(:,:),1) == nx2)
        ASSUREX(size(neighbor(:,:),2) == 2)
        ASSUREX(size(weight(:,:),2) == 2)
        if(size(xcoord) == 1) then
            neighbor(:,:)=1
            weight(:,1)=1
            weight(:,2)=0
            return
        end if
        do i=1, nx2
            found=.false.
            do j=1, nx-1
                if( (x2(i)-xcoord(j))*(x2(i)-xcoord(j+1)) <= 0 ) then
                    idx1=j
                    idx2=j+1
                    wgt1=(xcoord(j+1)-x2(i))/(xcoord(j+1)-xcoord(j))
                    wgt2=(x2(i)-xcoord(j))/(xcoord(j+1)-xcoord(j))
                    found=.true.
                    exit
                end if
            end do
            if(.not. found) then
                if( (x2(i)-xcoord(1))*(xcoord(1)-xcoord(2)) > 0) then !! outside of xcoord(1)
                    idx1=1
                    idx2=1
                    wgt1=0.5
                    wgt2=0.5
                else if( (x2(i)-xcoord(nx))*(xcoord(nx)-xcoord(nx-1)) > 0) then !!
                    idx1=nx
                    idx2=nx
                    wgt1=0.5
                    wgt2=0.5
                end if
            end if
            neighbor(i,1)=idx1
            neighbor(i,2)=idx2
            weight(i,1)=wgt1
            weight(i,2)=wgt2
        end do
    end subroutine interp_1d_linear_nmg2sct_getWeight  !!}}}
    
    subroutine interp_2d_withWeight_arr2sct(v1, v2, xc2_neighbor, xc2_weight, yc2_neighbor, yc2_weight)
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_2d_withWeight_arr2sct"
        integer :: nxc1, nyc1, n2,i, idx1, idy1, idx2, idy2
        real :: wgx1, wgx2, wgy1, wgy2
        real :: v1(:,:),v2(:)
        real :: xc2_weight(:,:), yc2_weight(:,:)
        integer :: xc2_neighbor(:,:), yc2_neighbor(:,:)
        nxc1=size(v1,1)
        nyc1=size(v1,2)
        n2=size(v2)
        if(.not. all (n2 == (/size(xc2_neighbor,1), size(xc2_weight,1), size(yc2_neighbor,1), size(yc2_weight,1)/))) then
            RP_ERROR("v2 & neighbor/weight size conflict!")
        end if
        if(.not. all (2 == (/size(xc2_neighbor,2), size(xc2_weight,2), size(yc2_neighbor,2), size(yc2_weight,2)/))) then
            RP_ERROR("neighbor/weight size2 should be two!")
        end if
        do i=1,n2
            idy1=yc2_neighbor(i,1)
            idy2=yc2_neighbor(i,2)
            wgy1=yc2_weight(i,1)
            wgy2=yc2_weight(i,2)
            idx1=xc2_neighbor(i,1)
            idx2=xc2_neighbor(i,2)
            wgx1=xc2_weight(i,1)
            wgx2=xc2_weight(i,2)
            v2(i) = v1(idx1, idy1) * wgx1 * wgy1 + v1(idx1, idy2) * wgx1 * wgy2 + &
                    v1(idx2, idy1) * wgx2 * wgy1 + v1(idx2, idy2) * wgx2 * wgy2
        enddo
    end subroutine interp_2d_withWeight_arr2sct        



    subroutine interp_3d_withWeight(v1, v2, xc2_neighbor, xc2_weight ,&
                                    yc2_neighbor, yc2_weight, zc2_neighbor, zc2_weight)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_3d_withWeight"
        integer :: nxc1, nyc1, nzc1, nxc2, nyc2, nzc2
        real :: v1(:,:,:), v2(:,:,:), xc2_weight(:,:), yc2_weight(:,:), zc2_weight(:,:)
        integer :: xc2_neighbor(:,:), yc2_neighbor(:,:), zc2_neighbor(:,:)
        integer :: i, j, k, idx1, idx2, idy1, idy2, idz1, idz2
        real :: wgx1, wgx2, wgy1, wgy2, wgz1, wgz2
        if(.not. all(size(v2,1) == (/size(xc2_weight,1), size(xc2_neighbor,1)/))) then
            RP_ERROR("xcoord size for v2 conflicts!")
        end if
        if(.not. all(size(v2,2) == (/size(yc2_weight,1), size(yc2_neighbor,1)/))) then
            RP_ERROR("ycoord size for v2 conflicts!")
        end if
        if(.not. all(size(v2,3) == (/size(zc2_weight,1), size(zc2_neighbor,1)/))) then
            RP_ERROR("zcoord size for v2 conflicts!")
        end if
        if(.not. all((/size(xc2_neighbor,2), size(xc2_weight,2), size(yc2_neighbor,2), size(yc2_weight,2), &
                       size(zc2_neighbor,2), size(zc2_weight,2)/) == 2)) then
            RP_ERROR("xc2/yc2/zc2 should has the 2st dimension size == 2")
        end if
        nxc1=size(v1, 1)
        nyc1=size(v1, 2)
        nzc1=size(v1, 3)
        nxc2=size(v2, 1)
        nyc2=size(v2, 2)
        nzc2=size(v2, 3)
        do k=1, nzc2
            idz1=zc2_neighbor(k,1)
            idz2=zc2_neighbor(k,2)
            wgz1=zc2_weight(k,1)
            wgz2=zc2_weight(k,2)
            do j=1, nyc2
                idy1=yc2_neighbor(j,1)
                idy2=yc2_neighbor(j,2)
                wgy1=yc2_weight(j,1)
                wgy2=yc2_weight(j,2)
                do i=1, nxc2
                    idx1=xc2_neighbor(i,1)
                    idx2=xc2_neighbor(i,2)
                    wgx1=xc2_weight(i,1)
                    wgx2=xc2_weight(i,2)
                    v2(i,j,k)=v1(idx1, idy1, idz1) * wgx1 * wgy1 * wgz1 + &
                              v1(idx2, idy1, idz1) * wgx2 * wgy1 * wgz1 + &
                              v1(idx1, idy2, idz1) * wgx1 * wgy2 * wgz1 + &
                              v1(idx2, idy2, idz1) * wgx2 * wgy2 * wgz1 + &
                              v1(idx1, idy1, idz2) * wgx1 * wgy1 * wgz2 + &
                              v1(idx2, idy1, idz2) * wgx2 * wgy1 * wgz2 + &
                              v1(idx1, idy2, idz2) * wgx1 * wgy2 * wgz2 + &
                              v1(idx2, idy2, idz2) * wgx2 * wgy2 * wgz2 
                end do
            end do
        end do
    end subroutine interp_3d_withWeight  !!}}}
    
    subroutine interp_3dnmg2sct_withWeight(v1, sct, xneighbor, xweight, &
                 yneighbor, yweight, zneighbor, zweight)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="interp_3dnmg2sct_withWeight"
        real :: v1(:,:,:), sct(:)
        integer :: xneighbor(:,:), yneighbor(:,:), zneighbor(:,:)
        real :: xweight(:,:), yweight(:,:), zweight(:,:)
        integer :: nx, ny, nz, nsct, idx1, idx2, idy1, idy2, idz1, idz2, isct
        real :: wgx1, wgx2, wgy1, wgy2, wgz1, wgz2
        nx=size(v1,1)
        ny=size(v1,2)
        nz=size(v1,3)
        nsct=size(sct)
        if(.not. all((/size(xneighbor,1), size(xweight,1), size(yneighbor,1), size(yweight,1), &
                 size(zneighbor,1), size(zweight,1)/) == nsct)) then
            RP_ERROR("x/y/z's neighbor/weight should have 1st dimension size same with sct!")
        end if
        if(.not. all((/size(xneighbor,2), size(xweight,2), size(yneighbor,2), size(yweight,2), &
                size(zneighbor,2), size(zweight,2)/) == 2)) then
            RP_ERROR("x/y/z's neighbor/weight should have 2nd dimension size == 2!")
        end if
        do isct=1,  nsct
            idx1=xneighbor(isct,1)
            idx2=xneighbor(isct,2)
            idy1=yneighbor(isct,1)
            idy2=yneighbor(isct,2)
            idz1=zneighbor(isct,1)
            idz2=zneighbor(isct,2)
            wgx1=xweight(isct,1)
            wgx2=xweight(isct,2)
            wgy1=yweight(isct,1)
            wgy2=yweight(isct,2)
            wgz1=zweight(isct,1)
            wgz2=zweight(isct,2)
            sct(isct)=v1(idx1, idy1, idz1) * wgx1 * wgy1 * wgz1 + & 
                      v1(idx2, idy1, idz1) * wgx2 * wgy1 * wgz1 + &
                      v1(idx1, idy2, idz1) * wgx1 * wgy2 * wgz1 + &
                      v1(idx2, idy2, idz1) * wgx2 * wgy2 * wgz1 + &
                      v1(idx1, idy1, idz2) * wgx1 * wgy1 * wgz2 + &
                      v1(idx2, idy1, idz2) * wgx2 * wgy1 * wgz2 + &
                      v1(idx1, idy2, idz2) * wgx1 * wgy2 * wgz2 + &
                      v1(idx2, idy2, idz2) * wgx2 * wgy2 * wgz2 
        end do
    end subroutine interp_3dnmg2sct_withWeight  !!}}}

end module interp
