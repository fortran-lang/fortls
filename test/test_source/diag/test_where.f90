program test_where
   implicit none
   ! Example variables
   real:: A(5),B(5),C(5)
   A = 0.0
   B = 1.0
   C = [0.0, 4.0, 5.0, 10.0, 0.0]

   ! Oneliner
   WHERE(B .GT. 0.0)  B = SUM(A, DIM=1)

   ! Simple where construct use
   where (C/=0)
      A=B/C
   elsewhere
      A=0.0
   end where

   ! Named where construct
   named: where (C/=0)
      A=B/C
   elsewhere
      A=0.0
   end where named
end program test_where
