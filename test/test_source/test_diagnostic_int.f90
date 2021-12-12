module test_int

   implicit none

   contains

   subroutine foo(f, arg2)
      interface
         function f(x)
            real, intent(in) :: x
            real :: f
         end function
      end interface
      integer, intent(in) :: arg2
      real :: y
      y = 1.
      print*, f(y)
   end subroutine foo

   function foo2(f, g, h) result(arg3)
      interface
         function f(x) result(z)
            real, intent(in) :: x
            real :: z
         end function
         function g(x) result(z)
            real, intent(in) :: x
            real :: z
         end function
      end interface
      interface
         function h(x) result(z)
            real, intent(in) :: x
            real :: z
         end function h
      end interface
      real :: y
      real :: arg3
      y = 1.
      arg3 = f(g(h(y)))
   end function foo2

end module test_int
