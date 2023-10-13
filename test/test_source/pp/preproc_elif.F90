subroutine preprocessor_elif(var, var3, var4, var5, var6)

#if 0
#define MYTYPE character
integer, intent(in) :: var
#elif 1
#define MYTYPE logical
integer, intent(inout) :: var
var = 3
#else
#define MYTYPE integer
integer, intent(out) :: var
var = 5
#endif

MYTYPE :: var1

#define OTHERTYPE integer

OTHERTYPE :: var2

PRINT*, var

#if 1
#define PART1 1,
#elif 0
#define PART2 2,
#elif 1
#define PART3 3,
#else
#define PART4 4
#endif

#ifndef PART1
#define PART1 5,
#endif
#ifndef PART2
#define PART2 5,
#endif
#ifndef PART3
#define PART3 5,
#endif
#ifndef PART4
#define PART4 5
#endif

REAL(PART1 PART2 PART3 PART4) :: var3

#undef PART1
#undef PART2
#undef PART3
#undef PART4

#if 0
#define PART1 1,
#elif 1
#define PART2 2,
#elif 1
#define PART3 3,
#else
#define PART4 4
#endif

#ifndef PART1
#define PART1 5,
#endif
#ifndef PART2
#define PART2 5,
#endif
#ifndef PART3
#define PART3 5,
#endif
#ifndef PART4
#define PART4 5
#endif

REAL(PART1 PART2 PART3 PART4) :: var4

#undef PART1
#undef PART2
#undef PART3
#undef PART4

#if 0
#define PART1 1,
#elif 0
#define PART2 2,
#elif 0
#define PART3 3,
#else
#define PART4 4
#endif

#ifndef PART1
#define PART1 5,
#endif
#ifndef PART2
#define PART2 5,
#endif
#ifndef PART3
#define PART3 5,
#endif
#ifndef PART4
#define PART4 5
#endif

REAL(PART1 PART2 PART3 PART4) :: var5

#undef PART1
#undef PART2
#undef PART3
#undef PART4

#if 1
#define PART1 1,
#elif 1
#define PART2 2,
#elif 0
#define PART3 3,
#else
#define PART4 4
#endif

#ifndef PART1
#define PART1 5,
#endif
#ifndef PART2
#define PART2 5,
#endif
#ifndef PART3
#define PART3 5,
#endif
#ifndef PART4
#define PART4 5
#endif

REAL(PART1 PART2 PART3 PART4) :: var6

endsubroutine preprocessor_elif
