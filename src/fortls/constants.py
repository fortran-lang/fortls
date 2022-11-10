from __future__ import annotations

import logging

from fortls.regex_patterns import FortranRegularExpressions

log = logging.getLogger(__name__)

# Global variables
sort_keywords = True

# Keyword identifiers
KEYWORD_LIST = [
    "pointer",
    "allocatable",
    "optional",
    "public",
    "private",
    "nopass",
    "target",
    "save",
    "parameter",
    "contiguous",
    "deferred",
    "dimension",
    "intent",
    "pass",
    "pure",
    "impure",
    "elemental",
    "recursive",
    "abstract",
    "external",
]
KEYWORD_ID_DICT = {keyword: ind for (ind, keyword) in enumerate(KEYWORD_LIST)}

# Type identifiers
BASE_TYPE_ID = -1
MODULE_TYPE_ID = 1
SUBROUTINE_TYPE_ID = 2
FUNCTION_TYPE_ID = 3
CLASS_TYPE_ID = 4
INTERFACE_TYPE_ID = 5
VAR_TYPE_ID = 6
METH_TYPE_ID = 7
SUBMODULE_TYPE_ID = 8
BLOCK_TYPE_ID = 9
SELECT_TYPE_ID = 10
DO_TYPE_ID = 11
WHERE_TYPE_ID = 12
IF_TYPE_ID = 13
ASSOC_TYPE_ID = 14
ENUM_TYPE_ID = 15


class Severity:
    error = 1
    warn = 2
    info = 3


#: A string used to mark literals e.g. 10, 3.14, "words", etc.
#: The description name chosen is non-ambiguous and cannot naturally
#: occur in Fortran (with/out C preproc) code
#: It is invalid syntax to define a type starting with numerics
#: it cannot also be a comment that requires !, c, d
#: and ^= (xor_eq) operator is invalid in Fortran C++ preproc
FORTRAN_LITERAL = "0^=__LITERAL_INTERNAL_DUMMY_VAR_"

# Fortran Regular Expressions dataclass variable, immutable
FRegex = FortranRegularExpressions()
