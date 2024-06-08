program euler
    implicit none
    real :: t,h
    real,dimension(2) :: yn1,yn2
    real, dimension(100) :: y,ye
    integer :: i
    y(1)=0
    ye(1)=0
    yn1(1)=0
    yn1(2)=5
    t=0
    h=0.01
    do i=2,100
        yn2(1)=yn1(1)+h*yn1(2)
        yn2(2)=yn1(2)+h*(10*sin(t)-5*yn1(2)-6*yn1(1))
        t=t+h
        y(i)=yn2(1)
        yn1(1)=yn2(1)
        yn1(2)=yn2(2)
        ye(i)=-6*exp(-3*t)+7*exp(-2*t)+sin(t)-cos(t)
    end do
    t=0
    open(1, file = 'eulerdata.dat')  
    do i=1,100  
      write(1,*) t,y(i) 
      t=t+h 
    end do
    close(1)
    t=0
    open(2, file = 'eulerdataexact.dat')  
    do i=1,100  
      write(2,*) t,ye(i) 
      t=t+h 
    end do
    close(2)
end program



