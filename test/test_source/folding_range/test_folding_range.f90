program test_folding_range_free_form
   dimension k(3)
   do i = 1, 5
      n = 1
      m = 2
      where (k > 2)
         ! hoge
         k = & ! hoge
   n * & ! should be ignored
   3 ! should be ignored
      elsewhere
         ! piyo
         k = n * 2
         k = m * 2
      end where
   end do

   ! block comment
   ! block comment
   ! block comment
   select case (int(sum(k)))
    case (:5)
      print *, &
         "sum is small."
    case (6:15)
      print *, "sum is moderate."
    case default
      print *, "sum is large."
   end select

contains

   subroutine dosomething()
      print *, n
      print *, m
   end subroutine dosomething
end program test_folding_range_free_form
