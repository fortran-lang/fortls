subroutine preprocessor_elif(var)

#if 0
#define MYTYPE character
#elif 1
#define MYTYPE logical
#else
#define MYTYPE integer
#endif

MYTYPE :: var1

#define OTHERTYPE integer

OTHERTYPE :: var2

endsubroutine preprocessor_elif