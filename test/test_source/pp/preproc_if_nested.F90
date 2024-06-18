subroutine preprocessor_if_nested()

! This file, as used in test_preproc, tests that when there are nested
! if-else preprocessor blocks, only the branches are used where ALL
! statements leading to the definition evaluate to true.

#if 0
#if 1
#define PART1 1
#else
#define PART2 1
#endif
#else
#if 1
#define PART3 1
#else
#define PART4 1
#endif
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

endsubroutine preprocessor_if_nested
