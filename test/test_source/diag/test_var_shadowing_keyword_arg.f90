module var_shadowing_keyword_arg
  character(len=6), parameter :: TEST = "4.10.4"
  character(len=6, kind=4), parameter :: TEST2 = "4.10.4"
  real(kind=8) :: a
end module var_shadowing_keyword_arg

program program_var_shadowing_keyword_arg
  use var_shadowing_keyword_arg
  integer :: len
  integer :: kind
end program program_var_shadowing_keyword_arg
