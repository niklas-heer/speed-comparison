program Leibniz
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, iostat_end, int64, real64
   implicit none
   integer(int64) :: i, file_unit, rc
   integer(int64) :: rounds;
   real(real64) :: x = 1.0_real64, pi = 1.0_real64

   open (action='read', file="rounds.txt", iostat=rc, newunit=file_unit)
   read (file_unit, *, iostat=rc) rounds
   close (file_unit)
   
   rounds = rounds + 2

   do i=2,rounds-1
      x = -x
      pi = pi + (x / (2 * i - 1))
   enddo

   pi = pi*4
   write(output_unit,'(F20.18)') pi

end program Leibniz
