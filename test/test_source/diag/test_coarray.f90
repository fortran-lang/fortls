program test_coarray
  implicit none
  real :: a(10)[*], b[2, *]
  character :: s(3)[*]*10
contains
  subroutine co_routine(coarray, arr)
    real, allocatable, intent(inout) :: coarray[:]
    real, intent(in) :: arr(:)[*]
  end subroutine co_routine
end program test_coarray
