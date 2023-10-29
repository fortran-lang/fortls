subroutine preprocessor_elif(var, var3, var4, var5, var6)

! This file, as used in test_preproc, checks that
! 1. the steps after the preprocessor parsing has fully finished, are only
! using content from the parts within the preprocessor if-elif-else that
! should be used. To do this, it has some regular fortran code within the
! #if and #elif.
! 2. the #endif correctly concludes the if-elif, so any new #define statements
! that come after the #endif, are picked up during the preprocessor parsing.

#if 0
integer, intent(in) :: var
#elif 1
integer, intent(inout) :: var
var = 3
#else
integer, intent(out) :: var
var = 5
#endif

#define OTHERTYPE integer

OTHERTYPE :: var2

PRINT*, var

endsubroutine preprocessor_elif
