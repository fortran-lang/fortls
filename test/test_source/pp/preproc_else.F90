subroutine preprocessor_else(var)

#if 0
#define MYTYPE logical
#else
#define MYTYPE integer
#endif

MYTYPE :: var0

#undef MYTYPE

#if 1
#define MYTYPE real
#else
#define MYTYPE character
#endif

MYTYPE :: var1

endsubroutine preprocessor_else
