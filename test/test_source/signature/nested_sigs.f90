program test_nan
   use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
   implicit none

   complex(qp) :: nan_zp

   nan_zp = ieee_value(1.,ieee_quiet_nan)
   print '(A4,2X,F5.1,6X,L1,2X,Z32)','zp',real(nan_zp), ieee_is_nan(real(nan_zp)),nan_zp
end program test_nan
