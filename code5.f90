program code5
    implicit none
    integer :: n,f,df,i,j
    print*,"Enter N Value"
    read*,n
    f=1
    df=1
    do i=n,1,-1
        f=f*i
    end do
    print*,f
    if(mod(n,2) == 0)then
        print*,"N is even. Double factorial cannot be calculated"
    else
        do j=n,1,-2
            df=df*j
        end do
        print*,df
    end if
end program code5 
