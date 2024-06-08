program secant
    implicit none
    real ::x0,x1,x2,fx0,fx1,fx2,epsilon
    x0=2.00
    x1=1.90
    fx0=exp(2*x0)+x0-5
    fx1=exp(2*x1)+x1-5
    epsilon =1
    do while(epsilon>0.001)
        x2=x1-fx1*((x0-x1)/(fx0-fx1))
        fx2=exp(2*x2)+x2-5
        epsilon=abs(x1-x2)
        x0=x1
        x1=x2
        fx0=fx1
        fx1=fx2
    end do
    print*,x1
end program secant

