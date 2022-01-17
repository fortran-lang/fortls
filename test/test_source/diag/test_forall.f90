program test_forall
   implicit none
   integer :: i, j, dim=3, a(10) = 2

   select case (dim)
    case(3)
      forall(i=1:10)
         a(i) = a(i) **2
         forall (j=1:i) a(j) = a(j) ** 2
      end forall
    case default
      call abort()
   end select
end program test_forall
