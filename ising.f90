program ising_model
    implicit none
    integer, parameter :: L = 100
    integer, parameter :: n_steps = 100000
    real, parameter :: T = 2.0
    integer :: lattice(L, L)
    integer :: step
    real :: energy, magnetization

    call random_seed()

    call initialize_lattice(lattice, L)

    do step = 1, n_steps

        call monte_carlo_step(lattice, L, T)
        call printlattice(lattice,L,step)

    end do

    energy = calculate_energy(lattice, L)
    magnetization = calculate_magnetization(lattice, L)


    print *, 'Final Energy:', energy
    print *, 'Final Magnetization:', magnetization

contains
    subroutine printlattice(lattice,L,step)

        implicit none
        integer, intent(inout) :: lattice(L, L)
        integer, intent(in) :: L
        integer :: fi, fj,step
        character(len=30) :: filename

        if((mod(step,100)==0))then
            write(filename, '("datafile_",I6.6, ".txt")')step
            filename = trim(adjustl(filename))
            open (unit=step, file = filename, status="unknown")
            do fi = 1,100
                write(step,*) (lattice(fi, fj), fj = 1, 100)
            end do
            close(unit=step)
        end if

    end subroutine

    subroutine initialize_lattice(lattice, L)
        implicit none
        integer, intent(out) :: lattice(L, L)
        integer, intent(in) :: L
        integer :: i, j
        real :: rand

        do i = 1, L
            do j = 1, L
                call random_seed()
                call random_number(rand)
                if (rand < 0.5) then
                    lattice(i, j) = 1
                else
                    lattice(i, j) = -1
                end if
            end do
        end do
    end subroutine initialize_lattice

    subroutine monte_carlo_step(lattice, L, T)
        implicit none
        integer, intent(inout) :: lattice(L, L)
        integer, intent(in) :: L
        real, intent(in) :: T
        integer :: i, j, si, neighbors, dE
        real :: rand

        do si = 1, L
            call random_seed()
            call random_number(rand)
            i = int(rand * L) + 1
            call random_seed()
            call random_number(rand)
            j = int(rand * L) + 1

            neighbors = lattice(mod(i, L) + 1, j) + lattice(mod(i-2, L) + 1, j) &
                      + lattice(i, mod(j, L) + 1) + lattice(i, mod(j-2, L) + 1)
            dE = 2 * lattice(i, j) * neighbors

            if (dE <= 0) then
                lattice(i, j) = -lattice(i, j)
            else
                call random_seed()
                call random_number(rand)
                if (rand < exp(-dE / T)) then
                    lattice(i, j) = -lattice(i, j)
                end if
            end if
        end do
    end subroutine monte_carlo_step

    real function calculate_energy(lattice, L)
        implicit none
        integer, intent(in) :: lattice(L, L)
        integer, intent(in) :: L
        integer :: i, j, neighbors
        calculate_energy = 0.0

        do i = 1, L
            do j = 1, L
                neighbors = lattice(mod(i, L) + 1, j) + lattice(mod(i-2, L) + 1, j) &
                          + lattice(i, mod(j, L) + 1) + lattice(i, mod(j-2, L) + 1)
                calculate_energy = calculate_energy - lattice(i, j) * neighbors
            end do
        end do

        calculate_energy = calculate_energy / 2.0
    end function calculate_energy

    real function calculate_magnetization(lattice, L)
        implicit none
        integer, intent(in) :: lattice(L, L)
        integer, intent(in) :: L
        integer :: i, j
        calculate_magnetization = 0.0

        do i = 1, L
            do j = 1, L
                calculate_magnetization = calculate_magnetization + lattice(i, j)
            end do
        end do
    end function calculate_magnetization


end program ising_model
