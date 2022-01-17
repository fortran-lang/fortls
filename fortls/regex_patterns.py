from __future__ import annotations

import re
from typing import Pattern

USE_REGEX = re.compile(
    r"[ ]*USE([, ]+(?:INTRINSIC|NON_INTRINSIC))?[ :]+(\w*)([, ]+ONLY[ :]+)?",
    re.I,
)
IMPORT_REGEX = re.compile(r"[ ]*IMPORT[ :]+([a-z_])", re.I)
INCLUDE_REGEX = re.compile(r"[ ]*INCLUDE[ :]*[\'\"]([^\'\"]*)", re.I)
CONTAINS_REGEX = re.compile(r"[ ]*(CONTAINS)[ ]*$", re.I)
IMPLICIT_REGEX = re.compile(r"[ ]*IMPLICIT[ ]+([a-z]*)", re.I)
SUB_MOD_REGEX = re.compile(r"[ ]*(PURE|IMPURE|ELEMENTAL|RECURSIVE)+", re.I)
SUB_REGEX = re.compile(r"[ ]*SUBROUTINE[ ]+([a-z0-9_]+)", re.I)
END_SUB_REGEX = re.compile(r"SUBROUTINE", re.I)
FUN_REGEX = re.compile(r"[ ]*FUNCTION[ ]+([a-z0-9_]+)", re.I)
RESULT_REGEX = re.compile(r"RESULT[ ]*\(([a-z0-9_]*)\)", re.I)
END_FUN_REGEX = re.compile(r"FUNCTION", re.I)
MOD_REGEX = re.compile(r"[ ]*MODULE[ ]+([a-z0-9_]+)", re.I)
END_MOD_REGEX = re.compile(r"MODULE", re.I)
SUBMOD_REGEX = re.compile(r"[ ]*SUBMODULE[ ]*\(", re.I)
END_SMOD_REGEX = re.compile(r"SUBMODULE", re.I)
END_PRO_REGEX = re.compile(r"(MODULE)?[ ]*PROCEDURE", re.I)
BLOCK_REGEX = re.compile(r"[ ]*([a-z_][a-z0-9_]*[ ]*:[ ]*)?BLOCK(?![a-z0-9_])", re.I)
END_BLOCK_REGEX = re.compile(r"BLOCK", re.I)
DO_REGEX = re.compile(r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?DO([ ]+[0-9]*|$)", re.I)
END_DO_REGEX = re.compile(r"DO", re.I)
WHERE_REGEX = re.compile(r"[ ]*WHERE[ ]*\(", re.I)
END_WHERE_REGEX = re.compile(r"WHERE", re.I)
IF_REGEX = re.compile(r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?IF[ ]*\(", re.I)
THEN_REGEX = re.compile(r"\)[ ]*THEN$", re.I)
END_IF_REGEX = re.compile(r"IF", re.I)
ASSOCIATE_REGEX = re.compile(r"[ ]*ASSOCIATE[ ]*\(", re.I)
END_ASSOCIATE_REGEX = re.compile(r"ASSOCIATE", re.I)
END_FIXED_REGEX = re.compile(r"[ ]*([0-9]*)[ ]*CONTINUE", re.I)
SELECT_REGEX = re.compile(
    r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?SELECT[ ]*" r"(CASE|TYPE)[ ]*\(([a-z0-9_=> ]*)",
    re.I,
)
SELECT_TYPE_REGEX = re.compile(r"[ ]*(TYPE|CLASS)[ ]+IS[ ]*\(([a-z0-9_ ]*)", re.I)
SELECT_DEFAULT_REGEX = re.compile(r"[ ]*CLASS[ ]+DEFAULT", re.I)
END_SELECT_REGEX = re.compile(r"SELECT", re.I)
PROG_REGEX = re.compile(r"[ ]*PROGRAM[ ]+([a-z0-9_]+)", re.I)
END_PROG_REGEX = re.compile(r"PROGRAM", re.I)
INT_REGEX = re.compile(r"[ ]*(ABSTRACT)?[ ]*INTERFACE[ ]*([a-z0-9_]*)", re.I)
END_INT_REGEX = re.compile(r"INTERFACE", re.I)
END_WORD_REGEX = re.compile(
    r"[ ]*END[ ]*(DO|WHERE|IF|BLOCK|ASSOCIATE|SELECT"
    r"|TYPE|ENUM|MODULE|SUBMODULE|PROGRAM|INTERFACE"
    r"|SUBROUTINE|FUNCTION|PROCEDURE|FORALL)?([ ]+(?!\W)|$)",
    re.I,
)
TYPE_DEF_REGEX = re.compile(r"[ ]*(TYPE)[, :]+", re.I)
EXTENDS_REGEX = re.compile(r"EXTENDS[ ]*\(([a-z0-9_]*)\)", re.I)
GENERIC_PRO_REGEX = re.compile(
    r"[ ]*(GENERIC)[, ]*(PRIVATE|PUBLIC)?[ ]*::[ ]*[a-z]", re.I
)
GEN_ASSIGN_REGEX = re.compile(r"(ASSIGNMENT|OPERATOR)\(", re.I)
END_TYPED_REGEX = re.compile(r"TYPE", re.I)
ENUM_DEF_REGEX = re.compile(r"[ ]*ENUM[, ]+", re.I)
END_ENUMD_REGEX = re.compile(r"ENUM", re.I)
NAT_VAR_REGEX = re.compile(
    r"[ ]*(INTEGER|REAL|DOUBLE[ ]*PRECISION|COMPLEX"
    r"|DOUBLE[ ]*COMPLEX|CHARACTER|LOGICAL|PROCEDURE"
    r"|EXTERNAL|CLASS|TYPE)",  # external :: variable is handled by this
    re.I,
)
KIND_SPEC_REGEX = re.compile(r"[ ]*([*]?\([ ]*[a-z0-9_*:]|\*[ ]*[0-9:]*)", re.I)
KEYWORD_LIST_REGEX = re.compile(
    r"[ ]*,[ ]*(PUBLIC|PRIVATE|ALLOCATABLE|"
    r"POINTER|TARGET|DIMENSION\(|"
    r"OPTIONAL|INTENT\([inout]*\)|DEFERRED|NOPASS|"
    r"PASS\([a-z0-9_]*\)|SAVE|PARAMETER|EXTERNAL|"
    r"CONTIGUOUS)",
    re.I,
)
PARAMETER_VAL_REGEX = re.compile(r"[\w]*[\s\&]*=[\s\&]*([\w\.\*\-\+\\]*)", re.I)
TATTR_LIST_REGEX = re.compile(
    r"[ ]*,[ ]*(PUBLIC|PRIVATE|ABSTRACT|EXTENDS\([a-z0-9_]*\))", re.I
)
VIS_REGEX = re.compile(r"[ ]*(PUBLIC|PRIVATE)[ :]", re.I)
WORD_REGEX = re.compile(r"[a-z_][a-z0-9_]*", re.I)
NUMBER_REGEX = re.compile(
    r"[\+\-]?(\b\d+\.?\d*|\.\d+)(_\w+|d[\+\-]?\d+|e[\+\-]?\d+(_\w+)?)?(?![a-z_])",
    re.I,
)
LOGICAL_REGEX = re.compile(r".true.|.false.", re.I)
SUB_PAREN_MATCH = re.compile(r"\([a-z0-9_, ]*\)", re.I)
KIND_SPEC_MATCH = re.compile(r"\([a-z0-9_, =*]*\)", re.I)


SQ_STRING_REGEX = re.compile(r"\'[^\']*\'", re.I)
DQ_STRING_REGEX = re.compile(r"\"[^\"]*\"", re.I)
LINE_LABEL_REGEX = re.compile(r"[ ]*([0-9]+)[ ]+", re.I)
NON_DEF_REGEX = re.compile(r"[ ]*(CALL[ ]+[a-z_]|[a-z_][a-z0-9_%]*[ ]*=)", re.I)
# Fixed format matching rules
FIXED_COMMENT_LINE_MATCH = re.compile(r"(!|c|d|\*)", re.I)
FIXED_CONT_REGEX = re.compile(r"(     [\S])")
FIXED_DOC_MATCH = re.compile(r"(?:!|c|d|\*)(<|>|!)", re.I)
FIXED_OPENMP_MATCH = re.compile(r"[!|c|\*]\$OMP", re.I)
# Free format matching rules
FREE_COMMENT_LINE_MATCH = re.compile(r"([ ]*!)")
FREE_CONT_REGEX = re.compile(r"([ ]*&)")
FREE_DOC_MATCH = re.compile(r"[ ]*!(<|>|!)")
FREE_OPENMP_MATCH = re.compile(r"[ ]*!\$OMP", re.I)
FREE_FORMAT_TEST = re.compile(r"[ ]{1,4}[a-z]", re.I)
# Preprocessor mathching rules
DEFINED_REGEX = re.compile(r"defined[ ]*\([ ]*([a-z_][a-z0-9_]*)[ ]*\)", re.I)
PP_REGEX = re.compile(r"#(if |ifdef|ifndef|else|elif|endif)")
PP_DEF_REGEX = re.compile(r"#(define|undef)[ ]*([\w]+)(\((\w+(,[ ]*)?)+\))?", re.I)
PP_DEF_TEST_REGEX = re.compile(r"(![ ]*)?defined[ ]*\([ ]*([a-z0-9_]*)[ ]*\)$", re.I)
PP_INCLUDE_REGEX = re.compile(r"#include[ ]*([\"a-z0-9_\.]*)", re.I)
PP_ANY_REGEX = re.compile(r"(^#:?\w+)")
# Context matching rules
CALL_REGEX = re.compile(r"[ ]*CALL[ ]+[a-z0-9_%]*$", re.I)
INT_STMNT_REGEX = re.compile(r"^[ ]*[a-z]*$", re.I)
TYPE_STMNT_REGEX = re.compile(r"[ ]*(TYPE|CLASS)[ ]*(IS)?[ ]*$", re.I)
PROCEDURE_STMNT_REGEX = re.compile(r"[ ]*(PROCEDURE)[ ]*$", re.I)
PRO_LINK_REGEX = re.compile(r"[ ]*(MODULE[ ]*PROCEDURE )", re.I)
SCOPE_DEF_REGEX = re.compile(
    r"[ ]*(MODULE|PROGRAM|SUBROUTINE|FUNCTION|INTERFACE)[ ]+", re.I
)
END_REGEX = re.compile(
    r"[ ]*(END)( |MODULE|PROGRAM|SUBROUTINE|FUNCTION|PROCEDURE|TYPE|DO|IF|SELECT)?",
    re.I,
)
# Object regex patterns
CLASS_VAR_REGEX = re.compile(r"(TYPE|CLASS)[ ]*\(", re.I)
DEF_KIND_REGEX = re.compile(r"([a-z]*)[ ]*\((?:KIND|LEN)?[ =]*([a-z_]\w*)", re.I)
OBJBREAK_REGEX = re.compile(r"[\/\-(.,+*<>=$: ]", re.I)


def src_file_exts(input_exts: list[str] = []) -> Pattern[str]:
    """Create a REGEX for which file extensions the Language Server should parse
    Default extensions are
    F F03 F05 F08 F18 F77 F90 F95 FOR FPP f f03 f05 f08 f18 f77 f90 f95 for fpp

    Parameters
    ----------
    input_exts : list[str], optional
        Additional Fortran, by default []

    Returns
    -------
    Pattern[str]
        A compiled regular expression, by default
        '.(F|F03|F05|F08|F18|F77|F90|F95|FOR|FPP|f|f03|f05|f08|f18|f77|f90|f95|for|fpp)?'
    """
    EXTS = ["", "77", "90", "95", "03", "05", "08", "18", "OR", "PP"]
    FORTRAN_FILE_EXTS = []
    for e in EXTS:
        FORTRAN_FILE_EXTS.extend([f"F{e}".upper(), f"f{e}".lower()])
    # Add the custom extensions for the server to parse
    for e in input_exts:
        if e.startswith("."):
            FORTRAN_FILE_EXTS.append(e.replace(".", ""))
    # Cast into a set to ensure uniqueness of extensions & sort for consistency
    # Create a regular expression from this
    return re.compile(fr"\.({'|'.join(sorted(set(FORTRAN_FILE_EXTS)))})?$")
