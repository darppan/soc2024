program rk4
    implicit none
    real :: xi,yi,zi,yi1,zi1,k0,k1,k2,k3,l0,l1,l2,l3,h
    integer :: i
    real, dimension(100) :: y,ye
    y(1)=0
    ye(1)=0
    xi=0
    yi=0
    zi=5
    h=0.01
    k0=h*zi
    l0=h*(10*sin(xi)-6*yi-5*zi)
    k1=h*(zi+(l0/2))
    l1=h*(10*sin(xi+(h/2))-6*(yi+(k0/2))-5*(zi+(l0/2)))
    k2=h*(zi+(l1/2))
    l2=h*(10*sin(xi+(h/2))-6*(yi+(k1/2))-5*(zi+(l1/2)))
    k3=h*(zi+(l2/2))
    l3=h*(10*sin(xi+(h/2))-6*(yi+(k2/2))-5*(zi+(l2/2)))
    do i=2,100
        yi1=yi+(k0+2*k1+2*k2+k3)/6
        zi1=zi+(l0+2*l1+2*l2+l3)/6
        yi=yi1
        zi=zi1
        y(i)=yi
        xi=xi+h
        k0=h*zi
        l0=h*(10*sin(xi)-6*yi-5*zi)
        k1=h*(zi+(l0/2))
        l1=h*(10*sin(xi+(h/2))-6*(yi+(k0/2))-5*(zi+(l0/2)))
        k2=h*(zi+(l1/2))
        l2=h*(10*sin(xi+(h/2))-6*(yi+(k1/2))-5*(zi+(l1/2)))
        k3=h*(zi+(l2/2))
        l3=h*(10*sin(xi+(h/2))-6*(yi+(k2/2))-5*(zi+(l2/2)))
        ye(i)=-6*exp(-3*xi)+7*exp(-2*xi)+sin(xi)-cos(xi)
    end do
    xi=0
    open(1, file = 'rk4data.dat')  
    do i=1,100  
      write(1,*) xi,y(i) 
      xi=xi+h 
    end do
    close(1)
    xi=0
    open(2, file = 'rk4dataexact.dat')  
    do i=1,100  
      write(2,*) xi,ye(i) 
      xi=xi+h 
    end do
    close(2)
end program rk4

