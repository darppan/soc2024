program code2
implicit none

    real :: x,y,a,b
    x=1
    y=2
    a=1
    b=log10(x)+COS(a)+ABS(x**2+y**2)+(2*(x*y)**0.5)
    print*,b
end program code2