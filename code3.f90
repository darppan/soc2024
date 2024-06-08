program code3
    implicit none
    real x,m,a,b
    x=2
    m=2
    a=2
    b=(1/(a*(2*3.1415)**0.5))*exp((2*a*((x-m)**3))**0.5)
    print*,b
end program code3
