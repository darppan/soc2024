program code7
    implicit none
    integer :: n,i
    real :: s,f,x
    s=1
    f=1.00
    print*,"Enter n value"
    read*,n
    print*,"Enter x value"
    read*,x
    do i=0,n
        if(i==0)then
            f=1.00
        else
            f=f/i
        print*,"fvalue",f
        print*,s
        s=s+((x**i)*f)
        end if
    end do
    print*,s
end program code7