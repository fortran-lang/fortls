subroutine preprocessor_elif(var)

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

endsubroutine preprocessor_elif