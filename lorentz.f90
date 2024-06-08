program lorentz
    implicit none
    real :: xi, yi, zi,  xi1, yi1, zi1, s, r, b, h
    real, dimension(10000) :: x,y,z
    integer :: i
    xi=0.00
    yi=1.00
    zi=1.05
    x(1)=0.00
    y(1)=1.00
    z(1)=1.05
    s=10.00
    r=28.00
    b=(8.00/3.00)
    h=0.01
    do i=2,10000
        xi1=xi+h*s*(yi-xi)
        yi1=yi+h*(xi*(r-zi)-yi)
        zi1=zi+h*(xi*yi-b*zi)
        xi=xi1
        yi=yi1
        zi=zi1
        x(i)=xi
        y(i)=yi
        z(i)=zi
    end do
    open(1, file = 'lorentzdata.dat')  
    do i=1,10000  
      write(1,*) x(i),y(i),z(i)  
    end do
    close(1)
end program lorentz

