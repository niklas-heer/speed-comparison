program Leibniz
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, iostat_end, int64, real64
    implicit none
    integer(int64) :: i, file_unit, rc
    integer(int64) :: rounds;
    real(real64) :: pi0, pi1, pi
 
    open (action='read', file="rounds.txt", iostat=rc, newunit=file_unit)
    read (file_unit, *, iostat=rc) rounds
    close (file_unit)
    
    pi0 = 0.0_real64
    pi1 = 0.0_real64

    do i=0, rounds, 4
        pi0 = pi0 + term(i) - term(i+1)
        pi1 = pi1 + term(i+2) - term(i+3)
    enddo
 
    pi = (pi0 + pi1) * 4
    write(output_unit,*) pi

    contains
    
    pure function term(n) result(t)
        integer(int64), intent(in) :: n
        real(real64) :: t
        t = (1.0_real64 / (2 * n + 1))
    end function term
 end program Leibniz
