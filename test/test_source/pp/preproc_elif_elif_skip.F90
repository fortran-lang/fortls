subroutine preprocessor_elif_elif_skip()
#if 0
#define PART1 0
#elif 1
#define PART2 1
#elif 1
#define PART3 0
#else
#define PART4 0
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

#undef PART1
#undef PART2
#undef PART3
#undef PART4
end subroutine preprocessor_elif_elif_skip
