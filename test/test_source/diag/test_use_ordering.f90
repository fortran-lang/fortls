module mod_a
   integer, parameter   :: q_a = 4
end module

module mod_b
   use mod_a
   integer, parameter   :: q_b = 8
end module

program test_use_ordering
   use mod_b,  only: q_b
   use mod_a

   real(q_a) :: r_a
   real(q_b) :: r_b
end program test_use_ordering
