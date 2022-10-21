program Leibniz
    use, intrinsic :: iso_fortran_env, only: output_unit, real64
    implicit none
    integer :: i, file_unit, rc, rounds
    real(real64), parameter :: x = -1
    real(real64) :: pi
 
    open (action='read', file="rounds.txt", iostat=rc, newunit=file_unit)
    read (file_unit, '(i20)', iostat=rc) rounds
    close (file_unit)
    
    pi = 1
    do i=1, rounds, 4
        pi = pi + x / (2 * i + 1) - x / (2 * i + 3) + x / (2 * i + 5) - x / (2 * i + 7)
    enddo
    pi = 4*pi
    write(output_unit,'(f14.12)') pi
 end program Leibniz
