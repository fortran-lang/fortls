from __future__ import annotations

from dataclasses import dataclass
from re import I, compile
from typing import Pattern


@dataclass(frozen=True)
class FortranRegularExpressions:
    USE: Pattern = compile(
        r"[ ]*USE([, ]+(?:INTRINSIC|NON_INTRINSIC))?[ :]+(\w*)([, ]+ONLY[ :]+)?",
        I,
    )
    IMPORT: Pattern = compile(r"[ ]*IMPORT[ :]+([a-z_])", I)
    INCLUDE: Pattern = compile(r"[ ]*INCLUDE[ :]*[\'\"]([^\'\"]*)", I)
    CONTAINS: Pattern = compile(r"[ ]*(CONTAINS)[ ]*$", I)
    IMPLICIT: Pattern = compile(r"[ ]*IMPLICIT[ ]+([a-z]*)", I)
    SUB_MOD: Pattern = compile(r"[ ]*\b(PURE|IMPURE|ELEMENTAL|RECURSIVE)\b", I)
    SUB: Pattern = compile(r"[ ]*SUBROUTINE[ ]+([a-z0-9_]+)", I)
    END_SUB: Pattern = compile(r"SUBROUTINE", I)
    FUN: Pattern = compile(r"[ ]*FUNCTION[ ]+([a-z0-9_]+)", I)
    RESULT: Pattern = compile(r"RESULT[ ]*\(([a-z0-9_]*)\)", I)
    END_FUN: Pattern = compile(r"FUNCTION", I)
    MOD: Pattern = compile(r"[ ]*MODULE[ ]+([a-z0-9_]+)", I)
    END_MOD: Pattern = compile(r"MODULE", I)
    SUBMOD: Pattern = compile(r"[ ]*SUBMODULE[ ]*\(", I)
    END_SMOD: Pattern = compile(r"SUBMODULE", I)
    END_PRO: Pattern = compile(r"(MODULE)?[ ]*PROCEDURE", I)
    BLOCK: Pattern = compile(r"[ ]*([a-z_][a-z0-9_]*[ ]*:[ ]*)?BLOCK(?![a-z0-9_])", I)
    END_BLOCK: Pattern = compile(r"BLOCK", I)
    DO: Pattern = compile(r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?DO([ ]+[0-9]*|$)", I)
    END_DO: Pattern = compile(r"DO", I)
    WHERE: Pattern = compile(r"[ ]*WHERE[ ]*\(", I)
    END_WHERE: Pattern = compile(r"WHERE", I)
    IF: Pattern = compile(r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?IF[ ]*\(", I)
    THEN: Pattern = compile(r"\)[ ]*THEN$", I)
    END_IF: Pattern = compile(r"IF", I)
    ASSOCIATE: Pattern = compile(r"[ ]*ASSOCIATE[ ]*\(", I)
    END_ASSOCIATE: Pattern = compile(r"ASSOCIATE", I)
    END_FIXED: Pattern = compile(r"[ ]*([0-9]*)[ ]*CONTINUE", I)
    SELECT: Pattern = compile(
        r"[ ]*(?:[a-z_][a-z0-9_]*[ ]*:[ ]*)?SELECT[ ]*"
        r"(CASE|TYPE)[ ]*\(([a-z0-9_=> ]*)",
        I,
    )
    SELECT_TYPE: Pattern = compile(r"[ ]*(TYPE|CLASS)[ ]+IS[ ]*\(([a-z0-9_ ]*)", I)
    SELECT_DEFAULT: Pattern = compile(r"[ ]*CLASS[ ]+DEFAULT", I)
    END_SELECT: Pattern = compile(r"SELECT", I)
    PROG: Pattern = compile(r"[ ]*PROGRAM[ ]+([a-z0-9_]+)", I)
    END_PROG: Pattern = compile(r"PROGRAM", I)
    INT: Pattern = compile(r"[ ]*(ABSTRACT)?[ ]*INTERFACE[ ]*([a-z0-9_]*)", I)
    END_INT: Pattern = compile(r"INTERFACE", I)
    END_WORD: Pattern = compile(
        r"[ ]*END[ ]*(DO|WHERE|IF|BLOCK|ASSOCIATE|SELECT"
        r"|TYPE|ENUM|MODULE|SUBMODULE|PROGRAM|INTERFACE"
        r"|SUBROUTINE|FUNCTION|PROCEDURE|FORALL)?([ ]+(?!\W)|$)",
        I,
    )
    TYPE_DEF: Pattern = compile(r"[ ]*(TYPE)[, :]+", I)
    EXTENDS: Pattern = compile(r"EXTENDS[ ]*\(([a-z0-9_]*)\)", I)
    GENERIC_PRO: Pattern = compile(
        r"[ ]*(GENERIC)[, ]*(PRIVATE|PUBLIC)?[ ]*::[ ]*[a-z]", I
    )
    GEN_ASSIGN: Pattern = compile(r"(ASSIGNMENT|OPERATOR)\(", I)
    END_TYPED: Pattern = compile(r"TYPE", I)
    ENUM_DEF: Pattern = compile(r"[ ]*ENUM[, ]+", I)
    END_ENUMD: Pattern = compile(r"ENUM", I)
    VAR: Pattern = compile(
        r"[ ]*(INTEGER|REAL|DOUBLE[ ]*PRECISION|COMPLEX"
        r"|DOUBLE[ ]*COMPLEX|CHARACTER|LOGICAL|PROCEDURE"
        r"|EXTERNAL|CLASS|TYPE)",  # external :: variable is handled by this
        I,
    )
    KIND_SPEC: Pattern = compile(r"[ ]*([*]?\([ ]*[a-z0-9_*:]|\*[ ]*[0-9:]*)", I)
    KEYWORD_LIST: Pattern = compile(
        r"[ ]*,[ ]*(PUBLIC|PRIVATE|ALLOCATABLE|"
        r"POINTER|TARGET|DIMENSION[ ]*\(|"
        r"OPTIONAL|INTENT[ ]*\([ ]*(?:IN|OUT|INOUT)[ ]*\)|DEFERRED|NOPASS|"
        r"PASS[ ]*\(\w*\)|SAVE|PARAMETER|EXTERNAL|"
        r"CONTIGUOUS)",
        I,
    )
    PARAMETER_VAL: Pattern = compile(r"\w*[\s\&]*=[\s\&]*([\w\.\*\-\+\\\'\"]*)", I)
    TATTR_LIST: Pattern = compile(
        r"[ ]*,[ ]*(PUBLIC|PRIVATE|ABSTRACT|EXTENDS\([a-z0-9_]*\))", I
    )
    VIS: Pattern = compile(r"[ ]*\b(PUBLIC|PRIVATE)\b", I)
    WORD: Pattern = compile(r"[a-z_][a-z0-9_]*", I)
    NUMBER: Pattern = compile(
        r"[\+\-]?(\b\d+\.?\d*|\.\d+)(_\w+|d[\+\-]?\d+|e[\+\-]?\d+(_\w+)?)?(?!\w)",
        I,
    )
    LOGICAL: Pattern = compile(r".true.|.false.", I)
    SUB_PAREN: Pattern = compile(r"\([a-z0-9_, ]*\)", I)
    # KIND_SPEC_MATCH: Pattern = compile(r"\([a-z0-9_, =*]*\)", I)

    SQ_STRING: Pattern = compile(r"\'[^\']*\'", I)
    DQ_STRING: Pattern = compile(r"\"[^\"]*\"", I)
    LINE_LABEL: Pattern = compile(r"[ ]*([0-9]+)[ ]+", I)
    NON_DEF: Pattern = compile(r"[ ]*(CALL[ ]+[a-z_]|[a-z_][a-z0-9_%]*[ ]*=)", I)
    # Fixed format matching rules
    FIXED_COMMENT: Pattern = compile(r"([!cd*])", I)
    FIXED_CONT: Pattern = compile(r"( {5}[\S])")
    FIXED_DOC: Pattern = compile(r"(?:[!cd\*])([<>!])", I)
    FIXED_OPENMP: Pattern = compile(r"[!c\*]\$OMP", I)
    # Free format matching rules
    FREE_COMMENT: Pattern = compile(r"([ ]*!)")
    FREE_CONT: Pattern = compile(r"([ ]*&)")
    FREE_DOC: Pattern = compile(r"[ ]*!([<>!])")
    FREE_OPENMP: Pattern = compile(r"[ ]*!\$OMP", I)
    FREE_FORMAT_TEST: Pattern = compile(r"[ ]{1,4}[a-z]", I)
    # Preprocessor matching rules
    DEFINED: Pattern = compile(r"defined[ ]*\(?[ ]*([a-z_][a-z0-9_]*)[ ]*\)?", I)
    PP_REGEX: Pattern = compile(r"#(if |ifdef|ifndef|else|elif|endif)")
    PP_DEF: Pattern = compile(r"#(define|undef)[ ]*([\w]+)(\((\w+(,[ ]*)?)+\))?", I)
    PP_DEF_TEST: Pattern = compile(r"(![ ]*)?defined[ ]*\([ ]*([a-z0-9_]*)[ ]*\)$", I)
    PP_INCLUDE: Pattern = compile(r"#include[ ]*([\"a-z0-9_\.]*)", I)
    PP_ANY: Pattern = compile(r"(^#:?\w+)")
    # Context matching rules
    CALL: Pattern = compile(r"[ ]*CALL[ ]+[a-z0-9_%]*$", I)
    INT_STMNT: Pattern = compile(r"^[ ]*[a-z]*$", I)
    TYPE_STMNT: Pattern = compile(r"[ ]*(TYPE|CLASS)[ ]*(IS)?[ ]*$", I)
    PROCEDURE_STMNT: Pattern = compile(r"[ ]*(PROCEDURE)[ ]*$", I)
    PRO_LINK: Pattern = compile(r"[ ]*(MODULE[ ]*PROCEDURE )", I)
    SCOPE_DEF: Pattern = compile(
        r"[ ]*(MODULE|PROGRAM|SUBROUTINE|FUNCTION|INTERFACE)[ ]+", I
    )
    END: Pattern = compile(
        r"[ ]*(END)("
        r" |MODULE|PROGRAM|SUBROUTINE|FUNCTION|PROCEDURE|TYPE|DO|IF|SELECT)?",
        I,
    )
    # Object regex patterns
    CLASS_VAR: Pattern = compile(r"(TYPE|CLASS)[ ]*\(", I)
    DEF_KIND: Pattern = compile(r"([a-z]*)[ ]*\((?:KIND|LEN)?[ =]*([a-z_]\w*)", I)
    OBJBREAK: Pattern = compile(r"[\/\-(.,+*<>=$: ]", I)


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
    return compile(rf"\.({'|'.join(sorted(set(FORTRAN_FILE_EXTS)))})?$")
