program newton_raphson
    implicit none
    real :: x2, x1,f,df,epsilon
    epsilon=1
    x1=7.00

    do while(epsilon>0.0001)
        f=(cos(x1*0.2)-0.226*x1*0.2)
        df=(x1*sin(x1*0.2)+0.226*0.2)
        x2=x1+(f/df)
        epsilon=abs(x1-x2)
        print*,epsilon
        x1=x2
    end do
    print*,x1
end program newton_raphson
