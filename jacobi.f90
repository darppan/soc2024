program euler
    implicit none
    real :: xi,yi,zi,xi1,yi1,zi1,epsilon
    integer :: i
    xi=0.0
    yi=0.0
    zi=0.0
    epsilon=0.1
    i=0
    do while(epsilon>0.00001)
        xi1=(3+yi+zi)/4
        yi1=(9+2*xi-zi)/6
        zi1=(xi-yi-6)/7
        epsilon=((xi1-xi)**2+(yi1-yi)**2+(zi1-zi)**2)**0.5
        xi=xi1
        yi=yi1
        zi=zi1
        i=i+1
    end do
    print*, "iterations:",i
    print*, xi," ", yi," ", zi
end program euler