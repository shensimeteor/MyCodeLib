#define LPCK_PERR(info) if(info /= 0 ) then; call print_message("error","LAPACK95 meets error, output info="//trim(adjustl(tostring(ival=info))), __FILE__,__LINE__); endif

module module_probability
use utility
use sft_sort
use lapack95
use f95_precision

contains

!!gen array of scalar randoms ~ N(mean,stdv^2)
subroutine genGaussRandom(mean,stdv,n,random_array)
implicit none
    real :: mean,stdv
    integer :: n,i
    real::random_array(n)
    real,parameter :: PI = 3.1415926535898 
    real :: random01(2)
    do i=1,floor(n/2.0+0.01)
        call random_number(random01(1))
        call random_number(random01(2))
        random_array(2*i-1) = stdv* sqrt(-2*log(random01(1))) * cos(2*PI*random01(2)) + mean
        random_array(2*i) = stdv * sqrt(-2*log(random01(1))) * sin(2*PI*random01(2)) + mean
    end do
    if(2*i < n) then
        call random_number(random01(1))
        call random_number(random01(2))
        random_array(n) = stdv* sqrt(-2*log(random01(1))) * cos(2*PI*random01(2)) + mean
    end if
end subroutine


!!return random_mtrx(1:m,1:n), m is the order of Cov (or variance)
!!supply Cov if the random vector have correlationship with each other; otherwise supply variance is OK
!!if do_filter supplied and == true, filter the negative eigenvalue's mode(to make the Cov positive-finite)
subroutine genVGaussRandom(mean,Cov,variance,n,random_mtrx, do_filter)
implicit none
    real :: mean(:)
    integer :: m,n,i,j
    real,optional :: Cov(:,:), variance(:)
    logical,optional :: do_filter
    real :: random_mtrx(:,:)
    real:: random_gs01(size(mean))
    integer :: info,ct
    real :: Lmtrx(size(mean),size(mean)), Deigv(size(mean))
    integer :: indx(size(mean))
    real,parameter :: EIGRATIO_MIN=1e-6    
    m=size(mean)
    if(present(Cov)) then
        if(size(Cov,1) /= m .or. size(Cov,2) /= m) then
            call print_message("Error","genVGaussRandom's input para. Cov has a wrong size",__FILE__,__LINE__)
        end if
        Lmtrx=Cov
        if(present(do_filter) .and. do_filter ) then
!!            call print_message("log", "filtering (rid of trivial & negative eigen value's mode) is done in {genVGaussRandom}")
            call syevd(Lmtrx, Deigv, jobz='V', uplo='L', info=info)
            call sorti_insert_float(Deigv, indx, "descend" )
            ct=0
            do i=1,m
                if(Deigv(indx(i)) < 0 .or. Deigv(indx(i))/Deigv(indx(1)) < EIGRATIO_MIN ) then
                    exit
                else
                    ct=i
                end if
            end do
            if(ct == 0) then
                call print_message("error","none non-negative eigenvalue exists in Cov {genVGaussRandom}",__FILE__,__LINE__)
            end if
            do i=1,ct
                Lmtrx(:,i)=Lmtrx(:,indx(i))*sqrt(Deigv(indx(i)))
            end do
        else
            call potrf(Lmtrx,uplo='L',info=info)
            LPCK_PERR(info)
            ct=m
        end if
        do i=1,n
            call genGaussRandom(0.,1.,ct,random_gs01)
            random_mtrx(:,i)=matmul(Lmtrx(:,1:ct),random_gs01)+mean
        end do
    else if(present(variance)) then
        if(size(variance) /= m ) then
            call print_message("Error","genVGaussRandom's input para. variance has a wrong size",__FILE__,__LINE__)
        end if
        do i=1,m
            call genGaussRandom(mean(i),sqrt(variance(i)),n,random_mtrx(i,:))
        end do
   else
       call print_message("Error","for genVGaussRandom, one of these input para. variance or Cov must be supplied",__FILE__,__LINE__)
   end if

end subroutine

        
end module
