!! sample code adapted from json-fortran/json_macros.inc

          # define SPACING_TEST
  #    define FILE_ENCODING ,encoding='UTF-8'

#   ifdef __GFORTRAN__
! gfortran uses cpp in old-school compatibility mode so
! the # stringify and ## concatenate operators don't work
! but we can use C/C++ style comment to ensure PROCEDURE is
! correctly tokenized and prepended with 'wrap_' when the
! macro is expanded
#     define MAYBEWRAP(PROCEDURE) PROCEDURE , wrap_/**/PROCEDURE
#   else
! Intel's fpp does support the more contemporary ## concatenation
! operator, but doesn't treat the C/C++ comments the same way.
! If you use the gfortran approach and pass the -noB switch to
! fpp, the macro will expand, but with a space between wrap_ and
! whatever PROCEDURE expands to
#   define MAYBEWRAP(PROCEDURE) PROCEDURE
# endif

#     define MACROARGS( x , y ) x + y
