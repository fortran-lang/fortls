subroutine preprocessor_if_elif_else()

! This file, as used in test_preproc, and together with the two similar files,
! tests that when there is an if-elif-elif-else, only the first branch that
! evaluates to true is used, and the others ignored. Also when multiple
! conditions evaluate to true.

#if 0
#define PART1 0
#elif 0
#define PART2 0
#elif 0
#define PART3 0
#else
#define PART4 1
#endif

#ifndef PART1
#define PART1 0
#endif
#ifndef PART2
#define PART2 0
#endif
#ifndef PART3
#define PART3 0
#endif
#ifndef PART4
#define PART4 0
#endif

integer, parameter :: res = PART1+PART2+PART3+PART4

endsubroutine preprocessor_if_elif_else
