program make_laplacian_operator
    !input: operator, a N**2 x N**2 matrix with all it's elements zero
    !output: a N**2 x N**2 matrix with its element values such that
    !the U_flat vector, when matrix multiplied by the operator,
    !gives us the left hand sides of the set of equations
    !(one for each value of i, j) in the discrete Poisson equation
    real, dimension(11,11) :: operator
    integer :: i,j,ind,n
    n=9
    operator(:,:) = 0.0
    do i=1,n
        operator(i,i)=-4.0
    end do
    j=1
    do while(j+3<=n)
        operator(j,j+3)=1.0
        operator(j+3,j)=1.0
        j=j+1
    end do
    j=1
    do while(j+1<=n)
        if(ind == 2) then
            ind = 0
            j=j+1
        else
            operator(j,j+1)=1.0
            operator(j+1,j)=1.0
            j=j+1
            ind=ind+1
        endif
    end do
    open (unit=1, file = 'operator.txt', status="unknown")
    do i = 1,n
        write(1,*) (operator(i, j), j = 1, n)
    end do
    close(unit=1)
end program make_laplacian_operator