module foo
   implicit none
   public :: length
   private
   integer :: len
   integer :: length
end module foo

program test_private
   use foo, only: length
   use test_vis_mod
   implicit none
   print*, some_var, length
end program test_private
