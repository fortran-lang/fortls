module mymod
   implicit none
   private
   public mytype, mytype2
   integer, public :: int1, int2, int3, int4, int5
   type :: mytype
      integer :: comp
   end type mytype
   type :: mytype2
      integer :: comp
   end type mytype2
   interface
      subroutine sub()
         import int1
         import mytype, int2
         type(mytype) :: some
      end subroutine sub
   end interface
end module mymod
