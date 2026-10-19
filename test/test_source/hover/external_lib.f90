subroutine solve(n, x, info)
  integer, intent(in) :: n
  real, intent(inout) :: x(n)
  integer, intent(out) :: info
end subroutine solve

real function scale(x)
  real, intent(in) :: x
end function scale
