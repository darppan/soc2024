program code6
    implicit none
    integer :: n,i
    real :: s
    s=0
    print*,"Enter n value"
    read*,n
    do i=0,n-1
        s=s+(4.00*((-1)**i))/(2.00*i+1)
    end do
    print*,s
end program code6