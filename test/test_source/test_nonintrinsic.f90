module test_nonint_mod
   private
   integer, parameter, public :: DP = kind(0.0D0)
end module test_nonint_mod

program nonint
   use, non_intrinsic :: test_nonint_mod, only : DP
   implicit none
   real(DP) :: x
   x = 0.0_DP
end program nonint
