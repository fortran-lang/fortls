import sys

PY3K = sys.version_info >= (3, 0)

# A string used to mark literals e.g. 10, 3.14, "words", etc.
# The description name chosen is non-ambiguous and cannot naturally
# occur in Fortran (with/out C preproc) code
# It is invalid syntax to define a type starting with numerics
# it cannot also be a comment that requires !, c, d
# and ^= (xor_eq) operator is invalid in Fortran C++ preproc
FORTRAN_LITERAL = "0^=__LITERAL_INTERNAL_DUMMY_VAR_"
