program code4
    implicit none
    real :: x,f
    print*,"Enter x value"
    read*,x
    if(x<2)then
        f=5*x*x+3*x+2
    else if(x>2)then
        f=5*x*x-3*x+1
    else if(x==2)then
        f=0
    endif
    print*,f
end program code4