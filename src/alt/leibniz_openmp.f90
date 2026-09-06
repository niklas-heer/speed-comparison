! Based on the OpenMP array-reduction contribution submitted by François.
! Standalone parallel example; excluded from the single-threaded benchmark.
! gfortran -fopenmp -Ofast -march=native -funroll-loops -flto leibniz_openmp.f90 -o leibniz_openmp
program LeibnizOpenMP
    use, intrinsic :: iso_fortran_env, only: output_unit, real64, int64
    implicit none
    integer :: file_unit, rc
    integer(int64) :: i, rounds, complete_rounds
    real(real64) :: pi
    real(real64) :: t(4) = 0.0_real64

    open (action='read', status='old', file='rounds.txt', iostat=rc, newunit=file_unit)
    if (rc /= 0) error stop 'Could not open rounds.txt'
    read (file_unit, *, iostat=rc) rounds
    close (file_unit)
    if (rc /= 0) error stop 'Could not read rounds.txt'
    if (rounds < 0_int64) error stop 'rounds must be nonnegative'
    ! Leave room for denominator arithmetic and loop termination increments.
    if (rounds > (huge(rounds) - 7_int64) / 2_int64) error stop 'rounds is too large'

    complete_rounds = rounds - modulo(rounds, 4_int64)
    !$omp parallel do default(none) shared(complete_rounds) reduction(+:t) schedule(static)
    do i = 1_int64, complete_rounds, 4_int64
        t(1) = t(1) - 1.0_real64 / (2_int64 * i + 1_int64)
        t(2) = t(2) + 1.0_real64 / (2_int64 * i + 3_int64)
        t(3) = t(3) - 1.0_real64 / (2_int64 * i + 5_int64)
        t(4) = t(4) + 1.0_real64 / (2_int64 * i + 7_int64)
    end do
    !$omp end parallel do

    pi = 1.0_real64 + sum(t)
    do i = complete_rounds + 1_int64, rounds
        if (modulo(i, 2_int64) == 1_int64) then
            pi = pi - 1.0_real64 / (2_int64 * i + 1_int64)
        else
            pi = pi + 1.0_real64 / (2_int64 * i + 1_int64)
        end if
    end do
    write (output_unit, '(f18.16)') 4.0_real64 * pi
end program LeibnizOpenMP
