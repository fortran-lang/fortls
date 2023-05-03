program preprocessor
#if defined(DUMMY)
#define CUSTOM_MACRO 0
#else
#define CUSTOM_MACRO 1
#endif
#ifdef DUMMY_MACRO
#define SECOND_CUSTOM_MACRO 0
#elif defined DUMMY
#define SECOND_CUSTOM_MACRO 1
#else
#define SECOND_CUSTOM_MACRO 2
#endif

#ifndef DUMMY_MACRO
#define THIRD_CUSTOM_MACRO 0
#elif defined (SECOND_DUMMY_MACRO)
#define THIRD_CUSTOM_MACRO 1
#else
#define THIRD_CUSTOM_MACRO 2
#endif
    print*, CUSTOM_MACRO
    print*, SECOND_CUSTOM_MACRO
    print*, THIRD_CUSTOM_MACRO
end program preprocessor
