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
    IMPORT: Pattern = compile(
        r"[ ]*IMPORT"
        r"(?:"
        r"[ ]*,[ ]*(?P<spec>ALL|NONE)"  # import, [all | none]
        r"|"  # or
        r"[ ]*,[ ]*(?P<only>ONLY)[ ]*:[ ]*(?P<start1>[\w_])"  # import, only: name-list
        r"|"  # or
        r"[ ]+(?:::[ ]*)?(?P<start2>[\w_])"  # import [[::] name-list]
        r")?",  # standalone import
        I,
    )
    INCLUDE: Pattern = compile(r"[ ]*INCLUDE[ :]*[\'\"]([^\'\"]*)", I)
    CONTAINS: Pattern = compile(r"[ ]*(CONTAINS)[ ]*$", I)
    IMPLICIT: Pattern = compile(r"[ ]*IMPLICIT[ ]+([a-z]*)", I)
    #: Parse procedure keywords but not if they start with , or ( or end with , or )
    #: This is to avoid parsing as keywords variables named pure, impure, etc.
    SUB_MOD: Pattern = compile(
        r"[ ]*(?!<[,\()][ ]*)\b(PURE|IMPURE|ELEMENTAL|RECURSIVE)\b(?![,\)][ ]*)", I
    )
    SUB: Pattern = compile(r"[ ]*SUBROUTINE[ ]+(\w+)", I)
    END_SUB: Pattern = compile(r"SUBROUTINE", I)
    FUN: Pattern = compile(r"[ ]*FUNCTION[ ]+(\w+)", I)
    RESULT: Pattern = compile(r"RESULT[ ]*\((\w*)\)", I)
    END_FUN: Pattern = compile(r"FUNCTION", I)
    MOD: Pattern = compile(r"[ ]*MODULE[ ]+(\w+)", I)
    END_MOD: Pattern = compile(r"MODULE", I)
    SUBMOD: Pattern = compile(r"[ ]*SUBMODULE[ ]*\(", I)
    END_SMOD: Pattern = compile(r"SUBMODULE", I)
    END_PRO: Pattern = compile(r"(MODULE)?[ ]*PROCEDURE", I)
    BLOCK: Pattern = compile(r"[ ]*([a-z_]\w*[ ]*:[ ]*)?BLOCK|CRITICAL(?!\w)", I)
    END_BLOCK: Pattern = compile(r"BLOCK|CRITICAL", I)
    DO: Pattern = compile(r"[ ]*(?:[a-z_]\w*[ ]*:[ ]*)?DO([ ]+[0-9]*|$)", I)
    END_DO: Pattern = compile(r"DO", I)
    WHERE: Pattern = compile(r"[ ]*WHERE[ ]*\(", I)
    END_WHERE: Pattern = compile(r"WHERE", I)
    IF: Pattern = compile(r"[ ]*(?:[a-z_]\w*[ ]*:[ ]*)?IF[ ]*\(", I)
    THEN: Pattern = compile(r"\)[ ]*THEN$", I)
    END_IF: Pattern = compile(r"IF", I)
    ASSOCIATE: Pattern = compile(r"[ ]*ASSOCIATE[ ]*\(", I)
    END_ASSOCIATE: Pattern = compile(r"ASSOCIATE", I)
    END_FIXED: Pattern = compile(r"[ ]*([0-9]*)[ ]*CONTINUE", I)
    SELECT: Pattern = compile(
        r"[ ]*(?:[a-z_]\w*[ ]*:[ ]*)?SELECT[ ]*" r"(CASE|TYPE)[ ]*\(([\w=> ]*)",
        I,
    )
    SELECT_TYPE: Pattern = compile(r"[ ]*(TYPE|CLASS)[ ]+IS[ ]*\(([\w ]*)", I)
    SELECT_DEFAULT: Pattern = compile(r"[ ]*CLASS[ ]+DEFAULT", I)
    END_SELECT: Pattern = compile(r"SELECT", I)
    PROG: Pattern = compile(r"[ ]*PROGRAM[ ]+(\w+)", I)
    END_PROG: Pattern = compile(r"PROGRAM", I)
    INT: Pattern = compile(r"[ ]*(ABSTRACT)?[ ]*INTERFACE[ ]*(\w*)", I)
    END_INT: Pattern = compile(r"INTERFACE", I)
    END_WORD: Pattern = compile(
        r"[ ]*END[ ]*(DO|WHERE|IF|BLOCK|CRITICAL|ASSOCIATE|SELECT"
        r"|TYPE|ENUM|MODULE|SUBMODULE|PROGRAM|INTERFACE"
        r"|SUBROUTINE|FUNCTION|PROCEDURE|FORALL)?([ ]+(?!\W)|$)",
        I,
    )
    TYPE_DEF: Pattern = compile(r"[ ]*(TYPE)[, :]+", I)
    EXTENDS: Pattern = compile(r"EXTENDS[ ]*\((\w*)\)", I)
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
    KIND_SPEC: Pattern = compile(r"[ ]*([*]?\([ ]*[\w*:]|\*[ ]*[0-9:]*)", I)
    KEYWORD_LIST: Pattern = compile(
        r"[ ]*,[ ]*(PUBLIC|PRIVATE|ALLOCATABLE|"
        r"POINTER|TARGET|DIMENSION[ ]*\(|"
        r"OPTIONAL|INTENT[ ]*\([ ]*(?:IN|OUT|IN[ ]*OUT)[ ]*\)|DEFERRED|NOPASS|"
        r"PASS[ ]*\(\w*\)|SAVE|PARAMETER|EXTERNAL|"
        r"CONTIGUOUS)",
        I,
    )
    PARAMETER_VAL: Pattern = compile(r"\w*[\s\&]*=(([\s\&]*[\w\.\-\+\*\/\'\"])*)", I)
    TATTR_LIST: Pattern = compile(
        r"[ ]*,[ ]*(PUBLIC|PRIVATE|ABSTRACT|EXTENDS\(\w*\))", I
    )
    VIS: Pattern = compile(r"[ ]*\b(PUBLIC|PRIVATE)\b", I)
    WORD: Pattern = compile(r"[a-z_][\w\$]*", I)
    NUMBER: Pattern = compile(
        r"[\+\-]?(\b\d+\.?\d*|\.\d+)(_\w+|d[\+\-]?\d+|e[\+\-]?\d+(_\w+)?)?(?!\w)",
        I,
    )
    LOGICAL: Pattern = compile(r".true.|.false.", I)
    SUB_PAREN: Pattern = compile(r"\([\w, ]*\)", I)
    # KIND_SPEC_MATCH: Pattern = compile(r"\([\w, =*]*\)", I)

    SQ_STRING: Pattern = compile(r"\'[^\']*\'", I)
    DQ_STRING: Pattern = compile(r"\"[^\"]*\"", I)
    LINE_LABEL: Pattern = compile(r"[ ]*([0-9]+)[ ]+", I)
    NON_DEF: Pattern = compile(r"[ ]*(CALL[ ]+[a-z_]|[a-z_][\w%]*[ ]*=)", I)
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
    DEFINED: Pattern = compile(r"defined[ ]*\(?[ ]*([a-z_]\w*)[ ]*\)?", I)
    PP_REGEX: Pattern = compile(r"[ ]*#[ ]*(if |ifdef|ifndef|else|elif|endif)", I)
    PP_DEF: Pattern = compile(
        r"[ ]*#[ ]*(define|undef|undefined)[ ]*(\w+)(\([ ]*([ \w,]*?)[ ]*\))?",
        I,
    )
    PP_DEF_TEST: Pattern = compile(r"(![ ]*)?defined[ ]*\([ ]*(\w*)[ ]*\)$", I)
    PP_INCLUDE: Pattern = compile(r"[ ]*#[ ]*include[ ]*([\"\w\.]*)", I)
    PP_ANY: Pattern = compile(r"^[ ]*#:?[ ]*(\w+)")
    # Context matching rules
    CALL: Pattern = compile(r"[ ]*CALL[ ]+[\w%]*$", I)
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
    DEF_KIND: Pattern = compile(r"(\w*)[ ]*\((?:KIND|LEN)?[ =]*(\w*)", I)
    OBJBREAK: Pattern = compile(r"[\/\-(.,+*<>=: ]", I)


# TODO: use this in the main code
def create_src_file_exts_regex(input_exts: list[str] = []) -> Pattern[str]:
    r"""Create a REGEX for which sources the Language Server should parse.

    Default extensions are (case insensitive):
    F F03 F05 F08 F18 F77 F90 F95 FOR FPP

    Parameters
    ----------
    input_exts : list[str], optional
        Additional list of file extensions to parse, in Python REGEX format
        that means special characters must be escaped
        , by default []

    Examples
    --------
    >>> regex = create_src_file_exts_regex([r"\.fypp", r"\.inc"])
    >>> regex.search("test.fypp")
    <re.Match object; span=(4, 9), match='.fypp'>
    >>> regex.search("test.inc")
    <re.Match object; span=(4, 8), match='.inc'>

    >>> regex = create_src_file_exts_regex([r"\.inc.*"])
    >>> regex.search("test.inc.1")
    <re.Match object; span=(4, 10), match='.inc.1'>

    Invalid regex expressions will cause the function to revert to the default
    extensions

    >>> regex = create_src_file_exts_regex(["*.inc"])
    >>> regex.search("test.inc") is None
    True

    Returns
    -------
    Pattern[str]
        A compiled regular expression for matching file extensions
    """
    import re

    DEFAULT = r"\.[fF](77|90|95|03|05|08|18|[oO][rR]|[pP]{2})?"
    EXPRESSIONS = [DEFAULT]
    try:
        EXPRESSIONS.extend(input_exts)
        # Add its expression as an OR and force they match the end of the string
        return re.compile(rf"(({'$)|('.join(EXPRESSIONS)}$))")
    except re.error:
        # TODO: Add a warning to the logger
        return re.compile(rf"({DEFAULT}$)")


def create_src_file_exts_str(input_exts: list[str] = []) -> Pattern[str]:
    """This is a version of create_src_file_exts_regex that takes a list
    sanitises the list of input_exts before compiling the regex.
    For more info see create_src_file_exts_regex
    """
    import re

    input_exts = [re.escape(ext) for ext in input_exts]
    return create_src_file_exts_regex(input_exts)
