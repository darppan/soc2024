subroutine grid2flat(grid, flat)
    !convert the n x n grid to n**2 x 1 array
    real, dimension(49,49) :: grid
    real, dimension(49**2) :: flat
    integer :: i,j,k
    k=1
    do i=1,49
        do j=1,49
            print*,"grid2flat",i,j
            flat(k)=grid(i,j)
            k=k+1
        end do
    end do
end subroutine grid2flat

subroutine flat2grid(flat, grid)
    !convert the n**2 x 1 array to n x n grid
    real, dimension(49,49) :: grid
    real, dimension(49**2) :: flat
    integer :: i,j,k
    k=1
    do i=1,49
        do j=1,49
            print*,"flat2grid",i,j
            grid(i,j) = flat(k)
            k=k+1
        end do
    end do
end subroutine flat2grid

subroutine make_laplacian_operator(operator)
    !input: operator, a N**2 x N**2 matrix with all it's elements zero
    !output: a N**2 x N**2 matrix with its element values such that
    !the U_flat vector, when matrix multiplied by the operator,
    !gives us the left hand sides of the set of equations
    !(one for each value of i, j) in the discrete Poisson equation
    real, dimension(49**2,49**2) :: operator
    integer :: i,j,ind
    operator(:,:) = 0.0
    do i=1,49**2
        operator(i,i)=-4.0
    end do
    j=1
    do while(j+3<=49**2)
        operator(j,j+3)=1.0
        operator(j+3,j)=1.0
        j=j+1
    end do
    j=1
    do while(j+1<=49**2)
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
end subroutine make_laplacian_operator

subroutine solve_system_lineq(operator,cd_grid,xk)
    implicit none
    real, dimension(49**2,49**2) :: operator,lu,diainv
    real, dimension(49,49) :: cd_grid
    real, dimension(49**2) :: flat,xk1,xk,lux
    integer :: i,j,k
    real :: epsilon
    do i=1,49**2
        do j=1,49**2
            if(i==j) then
                diainv(i,i)=1.0/operator(i,i)
            else
                lu(i,j)=operator(i,j)
            endif
        end do
    end do
    call grid2flat(cd_grid,flat)
    epsilon=1.0
    do while(epsilon>0.0001)
        lux(:)=0.0
        epsilon=0.0
        do j=1,49**2
            do k=1,49**2
                lux(j)=lux(j)+lu(j,k)*xk(k)
            end do
        end do
        do i=1,49**2
            xk1(i)=diainv(i,i)*((-1*flat(i))-lux(i))
            epsilon=epsilon+(xk1(i)-xk(i))**2
            xk(i)=xk1(i)
        end do
        epsilon=(epsilon)**0.5
        print*, "epsilon:",epsilon
    end do



end subroutine solve_system_lineq


program poisson
    implicit none
    real, dimension(49,49) :: cd_grid,U
    real, dimension(49**2,49**2) :: operator
    real, dimension(49**2) :: xk
    integer :: i,j
    cd_grid(:,:) = 0.0
    cd_grid(34, 24) = 1.0
    cd_grid(14, 24) = -1.0
    cd_grid(1,:) = 1.0
    cd_grid(49,:) = -1.0
    xk(:)=0.0
    call make_laplacian_operator(operator)
    call solve_system_lineq(operator,cd_grid,xk)
    call flat2grid(xk,U)
    open (unit=2, file = 'field.txt', status="unknown")
    do i = 1,49
        write(2,*) (U(i, j), j = 1, 49)
    end do
    close(unit=2)
    open (unit=1, file = 'operator.txt', status="unknown")
    do i = 1,49**2
        write(1,*) (operator(i, j), j = 1, 49**2)
    end do
    close(unit=1)

      
end program poisson
