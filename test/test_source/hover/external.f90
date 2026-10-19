subroutine use_external()
  implicit none
  external solve
  real, external :: scale
  integer :: info
  real :: x(2)
  call solve(2, x, info)
  x(1) = scale(x(2))
end subroutine use_external
