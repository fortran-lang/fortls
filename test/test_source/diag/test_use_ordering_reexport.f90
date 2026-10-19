module use_reexport_inner
   integer, parameter   :: q_a = 4
   integer, parameter   :: q_b = 8
end module

module use_reexport_umbrella
   use use_reexport_inner
end module

module use_reexport_limited
   use use_reexport_umbrella, only: q_a
end module

program test_use_ordering_reexport
   use use_reexport_limited
   use use_reexport_umbrella

   real(q_a) :: r_a
   real(q_b) :: r_b
end program test_use_ordering_reexport
