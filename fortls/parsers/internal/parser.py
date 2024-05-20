from __future__ import annotations

import hashlib
import logging
import os
import re
import sys
from collections import Counter, deque

# Python < 3.8 does not have typing.Literals
try:
    from typing import Literal
except ImportError:
    from typing_extensions import Literal

from re import Match, Pattern

from fortls.constants import (
    DO_TYPE_ID,
    INTERFACE_TYPE_ID,
    SELECT_TYPE_ID,
    SUBMODULE_TYPE_ID,
    FRegex,
    Severity,
    log,
)
from fortls.ftypes import (
    ClassInfo,
    FunSig,
    GenProcDefInfo,
    InterInfo,
    Range,
    ResultSig,
    SelectInfo,
    SmodInfo,
    SubInfo,
    VarInfo,
    VisInfo,
)
from fortls.helper_functions import (
    detect_fixed_format,
    find_paren_match,
    find_word_in_line,
    get_paren_level,
    get_paren_substring,
    map_keywords,
    separate_def_list,
    strip_line_label,
    strip_strings,
)

from .associate import Associate
from .ast import FortranAST
from .block import Block
from .do import Do
from .enum import Enum
from .function import Function
from .if_block import If
from .imports import Import, ImportTypes
from .interface import Interface
from .method import Method
from .module import Module
from .program import Program
from .scope import Scope
from .select import Select
from .submodule import Submodule
from .subroutine import Subroutine
from .type import Type
from .use import Use
from .variable import Variable
from .where import Where


def get_line_context(line: str) -> tuple[str, None] | tuple[str, str]:
    """Get context of ending position in line (for completion)

    Parameters
    ----------
    line : str
        file line

    Returns
    -------
    tuple[str, None]
        Possible string values:
        `var_key`, `pro_line`, `var_only`, `mod_mems`, `mod_only`, `pro_link`,
        `skip`, `import`, `vis`, `call`, `type_only`, `int_only`, `first`, `default`
    """
    last_level, sections = get_paren_level(line)
    lev1_end = sections[-1].end
    # Test if variable definition statement
    test_match = read_var_def(line)
    if test_match is not None:
        if test_match[0] == "var":
            if (test_match[1].var_names is None) and (lev1_end == len(line)):
                return "var_key", None
            # Procedure link?
            if (test_match[1].var_type == "PROCEDURE") and (line.find("=>") > 0):
                return "pro_link", None
            return "var_only", None
    # Test if in USE statement
    test_match = read_use_stmt(line)
    if test_match is not None:
        if len(test_match[1].only_list) > 0:
            return "mod_mems", test_match[1].mod_name
        else:
            return "mod_only", None
    # Test for interface procedure link
    if FRegex.PRO_LINK.match(line):
        return "pro_link", None
    # Test if scope declaration or end statement (no completion provided)
    if FRegex.SCOPE_DEF.match(line) or FRegex.END.match(line):
        return "skip", None
    # Test if import statement
    if FRegex.IMPORT.match(line):
        return "import", None
    # Test if visibility statement
    if FRegex.VIS.match(line):
        return "vis", None
    # In type-def
    type_def = False
    if FRegex.TYPE_DEF.match(line):
        type_def = True
    # Test if in call statement
    if (lev1_end == len(line)) and FRegex.CALL.match(last_level):
        return "call", None
    # Test if variable definition using type/class or procedure
    if (len(sections) == 1) and (sections[0].start >= 1):
        # Get string one level up
        test_str, _ = get_paren_level(line[: sections[0].start - 1])
        if FRegex.TYPE_STMNT.match(test_str) or (
            type_def and FRegex.EXTENDS.search(test_str)
        ):
            return "type_only", None
        if FRegex.PROCEDURE_STMNT.match(test_str):
            return "int_only", None
    # Only thing on line?
    if FRegex.INT_STMNT.match(line):
        return "first", None
    # Default or skip context
    if type_def:
        return "skip", None
    else:
        return "default", None


def parse_var_keywords(test_str: str) -> tuple[list[str], str]:
    """Parse Fortran variable declaration keywords"""
    # Needs to be this way and not simply call finditer because no regex can
    # capture nested parenthesis
    keyword_match = FRegex.KEYWORD_LIST.match(test_str)
    keywords = []
    while keyword_match:
        tmp_str = re.sub(r"^[, ]*", "", keyword_match.group(0))
        test_str = test_str[keyword_match.end(0) :]
        if tmp_str.lower().startswith("dimension"):
            match_char = find_paren_match(test_str)
            if match_char < 0:
                break  # Incomplete dimension statement
            else:
                tmp_str += test_str[: match_char + 1]
                test_str = test_str[match_char + 1 :]
        tmp_str = re.sub(r"^[, ]*", "", tmp_str)
        keywords.append(tmp_str.strip().upper())
        keyword_match = FRegex.KEYWORD_LIST.match(test_str)
    return keywords, test_str


def read_var_def(line: str, var_type: str | None = None, fun_only: bool = False):
    """Attempt to read variable definition line"""

    def parse_kind(line: str):
        match = FRegex.KIND_SPEC.match(line)
        if not match:
            return None, line
        kind_str = match.group(1).replace(" ", "")
        line = line[match.end(0) :]
        if kind_str.find("(") >= 0:
            match_char = find_paren_match(line)
            if match_char < 0:  # this triggers while typing with autocomplete
                raise ValueError("Incomplete kind specification")
            kind_str += line[: match_char + 1].strip()
            line = line[match_char + 1 :]
        return kind_str, line

    if var_type is None:
        type_match = FRegex.VAR.match(line)
        if type_match is None:
            return None
        var_type = type_match.group(0).strip()
        trailing_line = line[type_match.end(0) :]
    else:
        trailing_line = line[len(var_type) :]
    var_type = var_type.upper()
    trailing_line = trailing_line.split("!")[0]
    if len(trailing_line) == 0:
        return None

    # Parse the global kind, if any, for the current line definition
    # The global kind in some cases, like characters can be overriden by a locally
    # defined kind
    try:
        kind_str, trailing_line = parse_kind(trailing_line)
    except ValueError:
        return None

    # Class and Type statements need a kind spec
    if not kind_str and var_type in ("TYPE", "CLASS"):
        return None
    # Make sure next character is space or comma or colon
    if not kind_str and not trailing_line[0] in (" ", ",", ":"):
        return None

    keywords, trailing_line = parse_var_keywords(trailing_line)
    # Check if this is a function definition
    fun_def = read_fun_def(
        trailing_line,
        ResultSig(type=var_type, keywords=keywords, kind=kind_str),
    )
    if fun_def or fun_only:
        return fun_def
    # Split the type and variable name
    line_split = trailing_line.split("::")
    if len(line_split) == 1:
        if len(keywords) > 0:
            var_words = None
        else:
            trailing_line = line_split[0]
            var_words = separate_def_list(trailing_line.strip())
    else:
        trailing_line = line_split[1]
        var_words = separate_def_list(trailing_line.strip())
        if var_words is None:
            var_words = []

    return "var", VarInfo(
        var_type=var_type,
        keywords=keywords,
        var_names=var_words,
        var_kind=kind_str,
    )


def get_procedure_modifiers(
    line: str, regex: Pattern
) -> tuple[str, str, str] | tuple[None, None, None]:
    """Attempt to match procedure modifiers for FUNCTIONS and SUBROUTINES

    Parameters
    ----------
    line : str
        document line
    regex : Pattern
        regular expression to use e.g. Function or Subroutine sig

    Returns
    -------
    tuple[str, str, str] | tuple[None, None, None]
        procedure name, arguments, trailing line
    """
    match = regex.match(line)
    if match is None:
        return None, None, None

    name: str = match.group(1)
    trailing_line = line[match.end(0) :].split("!")[0]
    trailing_line = trailing_line.strip()

    paren_match = FRegex.SUB_PAREN.match(trailing_line)
    args = ""
    if paren_match is not None:
        word_match = FRegex.WORD.findall(paren_match.group(0))
        if word_match is not None:
            word_match = [word for word in word_match]
            args = ",".join(word_match)
        trailing_line = trailing_line[paren_match.end(0) :]

    return name, args, trailing_line


def read_fun_def(
    line: str, result: ResultSig = None, mod_flag: bool = False
) -> tuple[Literal["fun"], FunSig] | None:
    """Attempt to read FUNCTION definition line

    To infer the `result` `type` and `name` the variable definition is called
    with the function only flag

    Parameters
    ----------
    line : str
        file line
    result : RESULT_sig, optional
        a dataclass containing the result signature of the function
    mod_flag : bool, optional
        flag for module and module procedure parsing, by default False

    Returns
    -------
    tuple[Literal["fun"], FUN_sig] | None
        a named tuple
    """
    # Get all the keyword modifier mathces
    keywords = re.findall(FRegex.SUB_MOD, line)
    # remove modifiers from line
    line = re.sub(FRegex.SUB_MOD, "", line)

    # Try and get the result type
    # Recursively will call read_var_def which will then call read_fun_def
    # with the variable result having been populated
    if keywords:
        tmp_var = read_var_def(line, fun_only=True)
        if tmp_var is not None:
            # Update keywords for function into dataclass
            tmp_var[1].keywords = keywords
            return tmp_var

    name, args, trailing_line = get_procedure_modifiers(line, FRegex.FUN)
    if name is None:
        return None

    # Extract if possible the variable name of the result()
    trailing_line = trailing_line.strip()
    results_match = FRegex.RESULT.match(trailing_line)
    if result is None:
        result = ResultSig()
    if results_match:
        result.name = results_match.group(1).strip().lower()
    return "fun", FunSig(name, args, keywords, mod_flag, result)


def read_sub_def(
    line: str, mod_flag: bool = False
) -> tuple[Literal["sub"], SubInfo] | None:
    """Attempt to read a SUBROUTINE definition line

    Parameters
    ----------
    line : str
        document line
    mod_flag : bool, optional
        flag for module and module procedure parsing, by default False

    Returns
    -------
    tuple[Literal["sub"], SUB_info] | None
        a SUB_info dataclass object
    """
    # Get all the keyword modifier matches
    keywords = re.findall(FRegex.SUB_MOD, line)
    # remove modifiers from line
    line = re.sub(FRegex.SUB_MOD, "", line)
    name, args, _ = get_procedure_modifiers(line, FRegex.SUB)
    if name is None:
        return None

    return "sub", SubInfo(name, args, keywords, mod_flag)


def read_block_def(line: str) -> tuple[Literal["block"], str] | None:
    """Attempt to read BLOCK definition line"""
    block_match = FRegex.BLOCK.match(line)
    if block_match:
        name: str = block_match.group(1)
        if name:
            name = name.replace(":", " ").strip()
        return "block", name
    return None


def read_do_def(line: str) -> tuple[Literal["do"], str] | None:
    """Attempt to read a DO loop

    Returns
    -------
    tuple[Literal["do"], str] | None
        Tuple with "do" and a fixed format tag if present
    """
    line_stripped = strip_strings(line, maintain_len=True)
    line_no_comment = line_stripped.split("!")[0].rstrip()
    do_match = FRegex.DO.match(line_no_comment)
    if do_match:
        return "do", do_match.group(1).strip()
    return None


def read_where_def(line: str) -> tuple[Literal["where"], bool] | None:
    """Attempt to read a WHERE block

    Returns
    -------
    tuple[Literal["where"], bool] | None
        Tuple with "where" and a boolean indicating if labelled on unlabelled
    """
    line_stripped = strip_strings(line, maintain_len=True)
    line_no_comment = line_stripped.split("!")[0].rstrip()
    # Match WHERE blocks
    where_match = FRegex.WHERE.match(line_no_comment)
    if where_match:
        trailing_line = line[where_match.end(0) :]
        close_paren = find_paren_match(trailing_line)
        if close_paren < 0:
            return "where", True
        if FRegex.WORD.match(trailing_line[close_paren + 1 :].strip()):
            return "where", True
        else:
            return "where", False
    return None


def read_if_def(line: str) -> tuple[Literal["if"], None] | None:
    """Attempt to read an IF conditional

    Returns
    -------
    tuple[Literal["if"], None] | None
        A Literal "if" and None tuple
    """
    line_stripped = strip_strings(line, maintain_len=True)
    line_no_comment = line_stripped.split("!")[0].rstrip()
    if FRegex.IF.match(line_no_comment) and FRegex.THEN.search(line_no_comment):
        return "if", None
    return None


def read_associate_def(line: str):
    assoc_match = FRegex.ASSOCIATE.match(line)
    if assoc_match is not None:
        trailing_line = line[assoc_match.end(0) :]
        match_char = find_paren_match(trailing_line)
        if match_char < 0:
            return "assoc", []
        var_words = separate_def_list(trailing_line[:match_char].strip())
        return "assoc", var_words


def read_select_def(line: str):
    """Attempt to read SELECT definition line"""
    select_match = FRegex.SELECT.match(line)
    select_desc = None
    select_binding = None
    if select_match is None:
        select_type_match = FRegex.SELECT_TYPE.match(line)
        if select_type_match is None:
            select_default_match = FRegex.SELECT_DEFAULT.match(line)
            if select_default_match is None:
                return None
            else:
                return "select", SelectInfo(4, None, None)
        select_type = 3
        select_desc = select_type_match.group(1).upper()
        select_binding = select_type_match.group(2)
    else:
        select_word = select_match.group(1)
        select_type = -1
        if select_word.lower().startswith("case"):
            select_type = 1
        elif select_word.lower().startswith("type"):
            select_type = 2
        select_binding = select_match.group(2)
    return "select", SelectInfo(select_type, select_binding, select_desc)


def read_type_def(line: str):
    """Attempt to read TYPE definition line"""
    type_match = FRegex.TYPE_DEF.match(line)
    if type_match is None:
        return None
    trailing_line = line[type_match.end(1) :].split("!")[0]
    trailing_line = trailing_line.strip()
    # Parse keywords
    keyword_match = FRegex.TATTR_LIST.match(trailing_line)
    keywords: list[str] = []
    parent = None
    while keyword_match:
        keyword_strip = keyword_match.group(0).replace(",", " ").strip().upper()
        extend_match = FRegex.EXTENDS.match(keyword_strip)
        if extend_match:
            parent = extend_match.group(1).lower()
        else:
            keywords.append(keyword_strip)
        # Get visibility and/or extends/abstract modifiers
        trailing_line = trailing_line[keyword_match.end(0) :]
        keyword_match = FRegex.TATTR_LIST.match(trailing_line)
    # Get name
    line_split = trailing_line.split("::")
    if len(line_split) == 1:
        if len(keywords) > 0 and parent is None:
            return None
        else:
            if trailing_line.split("(")[0].strip().lower() == "is":
                return None
            trailing_line = line_split[0]
    else:
        trailing_line = line_split[1]
    #
    word_match = FRegex.WORD.match(trailing_line.strip())
    if word_match:
        name: str = word_match.group(0)
    else:
        return None
    #
    return "typ", ClassInfo(name, parent, keywords)


def read_enum_def(line: str):
    """Attempt to read ENUM definition line"""
    if FRegex.ENUM_DEF.match(line):
        return "enum", None
    return None


def read_generic_def(line: str):
    """Attempt to read generic procedure definition line"""
    generic_match = FRegex.GENERIC_PRO.match(line)
    if generic_match is None:
        return None
    #
    trailing_line = line[generic_match.end(0) - 1 :].split("!")[0].strip()
    if len(trailing_line) == 0:
        return None
    # Set visibility
    if generic_match.group(2) is None:
        vis_flag = 0
    else:
        if generic_match.group(2).lower() == "private":
            vis_flag = -1
        else:
            vis_flag = 1
    #
    i1 = trailing_line.find("=>")
    if i1 < 0:
        return None
    bound_name: str = trailing_line[:i1].strip()
    if FRegex.GEN_ASSIGN.match(bound_name):
        return None
    pro_list = trailing_line[i1 + 2 :].split(",")
    #
    pro_out: list[str] = []
    for bound_pro in pro_list:
        if len(bound_pro.strip()) > 0:
            pro_out.append(bound_pro.strip())
    if len(pro_out) == 0:
        return None
    #
    return "gen", GenProcDefInfo(bound_name, pro_out, vis_flag)


def read_mod_def(line: str):
    """Attempt to read MODULE and MODULE PROCEDURE, MODULE FUNCTION definition lines"""
    # Get all the keyword modifier mathces
    keywords = re.findall(FRegex.SUB_MOD, line)
    # remove modifiers from line
    line = re.sub(FRegex.SUB_MOD, "", line)

    mod_match = FRegex.MOD.match(line)
    if mod_match is None:
        return None

    name = mod_match.group(1)
    if name.lower() == "procedure":
        trailing_line = line[mod_match.end(1) :]
        pro_names = []
        line_split = trailing_line.split(",")
        for name in line_split:
            pro_names.append(name.strip().lower())
        return "int_pro", pro_names
    # Check for submodule definition
    trailing_line = line[mod_match.start(1) :]
    # module procedure
    sub_res = read_sub_def(trailing_line, mod_flag=True)
    if sub_res is not None:
        return sub_res
    # module function
    fun_res = read_var_def(trailing_line, fun_only=True)
    if fun_res is not None:
        fun_res[1].mod_flag = True
        fun_res[1].keywords = keywords
        return fun_res
    fun_res = read_fun_def(trailing_line, mod_flag=True)
    if fun_res is not None:
        fun_res[1].keywords = keywords
        return fun_res
    return "mod", name


def read_submod_def(line: str):
    """Attempt to read SUBMODULE definition line"""
    submod_match = FRegex.SUBMOD.match(line)
    if submod_match is None:
        return None

    parent_name: str = ""
    name: str = ""
    trailing_line = line[submod_match.end(0) :].split("!")[0]
    trailing_line = trailing_line.strip()
    parent_match = FRegex.WORD.match(trailing_line)
    if parent_match:
        parent_name = parent_match.group(0).lower()
        if len(trailing_line) > parent_match.end(0) + 1:
            trailing_line = trailing_line[parent_match.end(0) + 1 :].strip()
        else:
            trailing_line = ""

    name_match = FRegex.WORD.search(trailing_line)
    if name_match:
        name = name_match.group(0).lower()
    return "smod", SmodInfo(name, parent_name)


def read_prog_def(line: str) -> tuple[Literal["prog"], str] | None:
    """Attempt to read PROGRAM definition line"""
    prog_match = FRegex.PROG.match(line)
    if prog_match is None:
        return None
    return "prog", prog_match.group(1)


def read_int_def(line: str) -> tuple[Literal["int"], InterInfo] | None:
    """Attempt to read INTERFACE definition line"""
    int_match = FRegex.INT.match(line)
    if int_match is None:
        return None

    int_name = int_match.group(2).lower()
    is_abstract = int_match.group(1) is not None
    if int_name == "":
        return "int", InterInfo(None, is_abstract)
    if int_name == "assignment" or int_name == "operator":
        return "int", InterInfo(None, False)
    return "int", InterInfo(int_match.group(2), is_abstract)


def read_use_stmt(line: str) -> tuple[Literal["use"], Use] | None:
    """Attempt to read USE statement"""
    use_match = FRegex.USE.match(line)
    if use_match is None:
        return None

    trailing_line = line[use_match.end(0) :].lower()
    use_mod = use_match.group(2)
    only_list: set[str] = set()
    rename_map: dict[str, str] = {}
    if use_match.group(3):
        for only_stmt in trailing_line.split(","):
            only_split = only_stmt.split("=>")
            only_name = only_split[0].strip()
            only_list.add(only_name)
            if len(only_split) == 2:
                rename_map[only_name] = only_split[1].strip()
    return "use", Use(use_mod, only_list, rename_map)


def read_imp_stmt(line: str) -> tuple[Literal["import"], Import] | None:
    """Attempt to read IMPORT statement"""
    import_match = FRegex.IMPORT.match(line)
    if import_match is None:
        return None

    import_type = import_match.groupdict()
    is_empty = all(value is None for value in import_type.values())
    # import
    # import, all
    if is_empty or (import_type["spec"] and import_type["spec"].lower() == "all"):
        return "import", Import("#import", ImportTypes.ALL)
    # import, none
    elif import_type["spec"] and import_type["spec"].lower() == "none":
        return "import", Import("#import", ImportTypes.NONE)
    # import, only: a, b, c
    # import :: a, b, c
    # import a, b, c
    trailing_line = line[import_match.end(0) - 1 :].lower()
    import_list = {import_obj.strip() for import_obj in trailing_line.split(",")}
    return "import", Import("#import", ImportTypes.ONLY, import_list)


def read_inc_stmt(line: str) -> tuple[Literal["inc"], str] | None:
    """Attempt to read INCLUDE statement"""
    inc_match = FRegex.INCLUDE.match(line)
    if inc_match is None:
        return None

    inc_path: str = inc_match.group(1)
    return "inc", inc_path


def read_vis_stmnt(line: str) -> tuple[Literal["vis"], VisInfo] | None:
    """Attempt to read PUBLIC/PRIVATE statement"""
    vis_match = FRegex.VIS.match(line)
    if vis_match is None:
        return None

    vis_type = 0
    if vis_match.group(1).lower() == "private":
        vis_type = 1
    trailing_line = line[vis_match.end(0) :].split("!")[0]
    mod_words = FRegex.WORD.findall(trailing_line)
    return "vis", VisInfo(vis_type, mod_words)


def_tests = [
    read_var_def,
    read_sub_def,
    read_fun_def,
    read_block_def,
    read_where_def,
    read_do_def,
    read_if_def,
    read_associate_def,
    read_select_def,
    read_type_def,
    read_enum_def,
    read_use_stmt,
    read_imp_stmt,
    read_int_def,
    read_generic_def,
    read_mod_def,
    read_prog_def,
    read_submod_def,
    read_inc_stmt,
    read_vis_stmnt,
]


def find_external_type(file_ast: FortranAST, desc_string: str, name: str) -> bool:
    """Encountered a variable with EXTERNAL as its type
    Try and find an already defined variable with a
    NORMAL Fortran Type"""
    if not desc_string.upper() == "EXTERNAL":
        return False
    counter = 0
    # Definition without EXTERNAL has already been parsed
    for v in file_ast.variable_list:
        if name == v.name:
            # If variable is already in external objs it has
            # been parsed correctly so exit
            if v in file_ast.external_objs:
                return False

            v.set_external_attr()
            file_ast.external_objs.append(v)
            counter += 1
            # TODO: do I need to update AST any more?
    if counter == 1:
        return True
    else:
        return False


def find_external_attr(file_ast: FortranAST, name: str, new_var: Variable) -> bool:
    """Check if this NORMAL Fortran variable is in the external_objs with only
    ``EXTERNAL`` as its type. Used to detect seperated ``EXTERNAL`` declarations.

    Parameters
    ----------
    file_ast : fortran_ast
        AST file
    name : str
        Variable name, stripped
    new_var : fortran_var
        Fortran variable to check against

    Returns
    -------
    bool
        True if only a single ``EXTERNAL`` definition is encountered False
        for everything else, which will cause a diagnostic error to be raised
    """
    counter = 0
    for v in file_ast.external_objs:
        if v.name != name:
            continue
        if v.desc.upper() != "EXTERNAL":
            continue
        # We do this once
        if counter == 0:
            v.desc = new_var.desc
            v.set_external_attr()
        # TODO: do i need to update AST any more?
        counter += 1

    # Only one definition encountered
    if counter == 1:
        return True
    # If no variable or multiple variables add to AST.
    # Multiple defs will throw diagnostic error as it should
    else:
        return False


def find_external(
    file_ast: FortranAST,
    desc_string: str,
    name: str,
    new_var: Variable,
) -> bool:
    """Find a procedure, function, subroutine, etc. that has been defined as
    ``EXTERNAL``. ``EXTERNAL``s are parsed as ``fortran_var``, since there is no
    way of knowing if ``real, external :: val`` is a function or a subroutine.

    This method exists solely for ``EXTERNAL`` s that are defined across multiple
    lines e.g.

    .. code-block:: fortran

            EXTERNAL VAR
            REAL VAR

    or

    .. code-block:: fortran

            REAL VAR
            EXTERNAL VAR


    Parameters
    ----------
    file_ast : fortran_ast
        AST
    desc_string : str
        Variable type e.g. ``REAL``, ``INTEGER``, ``EXTERNAL``
    name : str
        Variable name
    new_var : fortran_var
        The line variable that we are attempting to match with an ``EXTERNAL``
        definition

    Returns
    -------
    bool
        True if the variable is ``EXTERNAL`` and we manage to link it to the
        rest of its components, else False
    """
    if find_external_type(file_ast, desc_string, name):
        return True
    elif desc_string.upper() != "EXTERNAL":
        if find_external_attr(file_ast, name, new_var):
            return True
    return False


class FortranFile:
    def __init__(self, path: str = None, pp_suffixes: list = None):
        self.path: str = path
        self.contents_split: list[str] = []
        self.contents_pp: list[str] = []
        self.pp_defs: dict = {}
        self.nLines: int = 0
        self.fixed: bool = False
        self.preproc: bool = False
        self.ast: FortranAST = None
        self.hash: str = None
        if path:
            _, file_ext = os.path.splitext(os.path.basename(path))
            if pp_suffixes:
                self.preproc = file_ext in pp_suffixes
            else:
                self.preproc = file_ext == file_ext.upper()
        self.COMMENT_LINE_MATCH, self.DOC_COMMENT_MATCH = self.get_comment_regexs()

    def copy(self) -> FortranFile:
        """Copy content to new file object (does not copy objects)"""
        copy_obj = FortranFile(self.path)
        copy_obj.preproc = self.preproc
        copy_obj.fixed = self.fixed
        copy_obj.contents_pp = self.contents_pp
        copy_obj.contents_split = self.contents_split
        copy_obj.pp_defs = self.pp_defs
        copy_obj.set_contents(self.contents_split)
        return copy_obj

    def load_from_disk(self) -> tuple[str | None, bool | None]:
        """Read file from disk or update file contents only if they have changed
        A MD5 hash is used to determine that

        Returns
        -------
        tuple[str|None, bool|None]
            ``str`` : string containing IO error message else None
            ``bool``: boolean indicating if the file has changed
        """
        contents: str
        try:
            with open(self.path, encoding="utf-8", errors="replace") as f:
                contents = re.sub(r"\t", r" ", f.read())
        except OSError:
            return "Could not read/decode file", None
        else:
            # Check if files are the same
            try:
                hash = hashlib.md5(
                    contents.encode("utf-8"), usedforsecurity=False
                ).hexdigest()
            # Python <=3.8 does not have the `usedforsecurity` option
            except TypeError:
                hash = hashlib.md5(contents.encode("utf-8")).hexdigest()

            if hash == self.hash:
                return None, False

            self.hash = hash
            self.contents_split = contents.splitlines()
            self.fixed = detect_fixed_format(self.contents_split)
            self.contents_pp = self.contents_split
            self.nLines = len(self.contents_split)
            return None, True

    def apply_change(self, change: dict) -> bool:
        """Apply a change to the file."""

        def check_change_reparse(line_no: int) -> bool:
            if (line_no < 0) or (line_no > self.nLines - 1):
                return True
            pre_lines, curr_line, _ = self.get_code_line(line_no, forward=False)
            # Skip comment lines
            if self.fixed:
                if FRegex.FIXED_COMMENT.match(curr_line):
                    return False
            else:
                if FRegex.FREE_COMMENT.match(curr_line):
                    return False
            # Check for line labels and semicolons
            full_line = "".join(pre_lines) + curr_line
            full_line, line_label = strip_line_label(full_line)
            if line_label is not None:
                return True
            line_stripped = strip_strings(full_line, maintain_len=True)
            if line_stripped.find(";") >= 0:
                return True
            # Find trailing comments
            comm_ind = line_stripped.find("!")
            if comm_ind >= 0:
                line_no_comment = full_line[:comm_ind]
            else:
                line_no_comment = full_line
            # Various single line tests
            if FRegex.END_WORD.match(line_no_comment):
                return True
            if FRegex.IMPLICIT.match(line_no_comment):
                return True
            if FRegex.CONTAINS.match(line_no_comment):
                return True
            # Generic "non-definition" line
            if FRegex.NON_DEF.match(line_no_comment):
                return False
            # Loop through tests
            for test in def_tests:
                if test(line_no_comment):
                    return True
            return False

        self.hash = None
        text = change.get("text", "")
        change_range = change.get("range")
        if len(text) == 0:
            text_split = [""]
        else:
            text_split = text.splitlines()
            # Check for ending newline
            if (text[-1] == "\n") or (text[-1] == "\r"):
                text_split.append("")

        if change_range is None:
            # The whole file has changed
            self.set_contents(text_split)
            return True

        start_line = change_range["start"]["line"]
        start_col = change_range["start"]["character"]
        end_line = change_range["end"]["line"]
        end_col = change_range["end"]["character"]

        # Check for an edit occurring at the very end of the file
        if start_line == self.nLines:
            self.set_contents(self.contents_split + text_split)
            return True

        # Check for single line edit
        if (start_line == end_line) and (len(text_split) == 1):
            prev_line = self.contents_split[start_line]
            self.contents_split[start_line] = (
                prev_line[:start_col] + text + prev_line[end_col:]
            )
            self.contents_pp[start_line] = self.contents_split[start_line]
            return check_change_reparse(start_line)

        # Apply standard change to document
        new_contents = []
        for i, line in enumerate(self.contents_split):
            if (i < start_line) or (i > end_line):
                new_contents.append(line)
                continue

            if i == start_line:
                for j, change_line in enumerate(text_split):
                    if j == 0:
                        new_contents.append(line[:start_col] + change_line)
                    else:
                        new_contents.append(change_line)

            if i == end_line:
                new_contents[-1] += line[end_col:]
        self.set_contents(new_contents)
        return True

    def set_contents(self, contents_split: list, detect_format: bool = True):
        """Set file contents"""
        self.contents_split = contents_split
        self.contents_pp = self.contents_split
        self.nLines = len(self.contents_split)
        if detect_format:
            self.fixed = detect_fixed_format(self.contents_split)

    def get_line(self, line_no: int, pp_content: bool = False) -> str:
        """Get single line from file"""
        try:
            if pp_content:
                return self.contents_pp[line_no]
            return self.contents_split[line_no]
        except (TypeError, IndexError):
            return None

    def get_code_line(
        self,
        line_no: int,
        forward: bool = True,
        backward: bool = True,
        pp_content: bool = False,
        strip_comment: bool = False,
    ) -> tuple[list[str], str, list[str]]:
        """Get full code line from file including any adjacent continuations"""
        curr_line = self.get_line(line_no, pp_content)
        if curr_line is None:
            return [], None, []
        # Search backward for prefix lines
        line_ind = line_no - 1
        pre_lines = []
        if backward:
            if self.fixed:  # Fixed format file
                tmp_line = curr_line
                while line_ind > 0:
                    if FRegex.FIXED_CONT.match(tmp_line):
                        prev_line = tmp_line
                        tmp_line = self.get_line(line_ind, pp_content)
                        if line_ind == line_no - 1:
                            curr_line = " " * 6 + curr_line[6:]
                        else:
                            pre_lines[-1] = " " * 6 + prev_line[6:]
                        pre_lines.append(tmp_line)
                    else:
                        break
                    line_ind -= 1
            else:  # Free format file
                opt_cont_match = FRegex.FREE_CONT.match(curr_line)
                if opt_cont_match:
                    curr_line = (
                        " " * opt_cont_match.end(0) + curr_line[opt_cont_match.end(0) :]
                    )
                while line_ind > 0:
                    tmp_line = strip_strings(
                        self.get_line(line_ind, pp_content), maintain_len=True
                    )
                    tmp_no_comm = tmp_line.split("!")[0]
                    cont_ind = tmp_no_comm.rfind("&")
                    opt_cont_match = FRegex.FREE_CONT.match(tmp_no_comm)
                    if opt_cont_match:
                        if cont_ind == opt_cont_match.end(0) - 1:
                            break
                        tmp_no_comm = (
                            " " * opt_cont_match.end(0)
                            + tmp_no_comm[opt_cont_match.end(0) :]
                        )
                    if cont_ind >= 0:
                        pre_lines.append(tmp_no_comm[:cont_ind])
                    else:
                        break
                    line_ind -= 1
        # Search forward for trailing lines with continuations
        line_ind = line_no + 1
        post_lines = []
        if forward:
            if self.fixed:
                if line_ind < self.nLines:
                    next_line = self.get_line(line_ind, pp_content)
                    line_ind += 1
                    cont_match = FRegex.FIXED_CONT.match(next_line)
                    while (cont_match is not None) and (line_ind < self.nLines):
                        post_lines.append(" " * 6 + next_line[6:])
                        next_line = self.get_line(line_ind, pp_content)
                        line_ind += 1
                        cont_match = FRegex.FIXED_CONT.match(next_line)
            else:
                line_stripped = strip_strings(curr_line, maintain_len=True)
                iAmper = line_stripped.find("&")
                iComm = line_stripped.find("!")
                if iComm < 0:
                    iComm = iAmper + 1
                next_line = ""
                # Read the next line if needed
                while (iAmper >= 0) and (iAmper < iComm):
                    if line_ind == line_no + 1:
                        curr_line = curr_line[:iAmper]
                    elif next_line != "":
                        post_lines[-1] = next_line[:iAmper]
                    next_line = self.get_line(line_ind, pp_content)
                    if next_line is None:
                        break

                    line_ind += 1
                    # Skip any preprocessor statements when seeking the next line
                    if FRegex.PP_ANY.match(next_line):
                        next_line = ""
                        post_lines.append("")
                        continue
                    # Skip empty or comment lines
                    match = FRegex.FREE_COMMENT.match(next_line)
                    if next_line.rstrip() == "" or match:
                        next_line = ""
                        post_lines.append("")
                        continue
                    opt_cont_match = FRegex.FREE_CONT.match(next_line)
                    if opt_cont_match:
                        next_line = next_line[opt_cont_match.end(0) :]
                    post_lines.append(next_line)
                    line_stripped = strip_strings(next_line, maintain_len=True)
                    iAmper = line_stripped.find("&")
                    iComm = line_stripped.find("!")
                    if iComm < 0:
                        iComm = iAmper + 1
        # Detect start of comment in current line
        if strip_comment:
            curr_line = self.strip_comment(curr_line)
        pre_lines.reverse()
        return pre_lines, curr_line, post_lines

    def strip_comment(self, line: str) -> str:
        """Strip comment from line"""
        if self.fixed:
            if FRegex.FIXED_COMMENT.match(line) and FRegex.FIXED_OPENMP.match(line):
                return ""
        else:
            if FRegex.FREE_OPENMP.match(line) is None:
                line = line.split("!")[0]
        return line

    def find_word_in_code_line(
        self,
        line_no: int,
        word: str,
        forward: bool = True,
        backward: bool = False,
        pp_content: bool = False,
    ) -> tuple[int, Range]:
        back_lines, curr_line, forward_lines = self.get_code_line(
            line_no, forward=forward, backward=backward, pp_content=pp_content
        )
        word_range = Range(-1, -1)
        if curr_line is not None:
            find_word_lower = word.lower()
            word_range = find_word_in_line(curr_line.lower(), find_word_lower)
        if backward and (word_range.start < 0):
            back_lines.reverse()
            for i, line in enumerate(back_lines):
                word_range = find_word_in_line(line.lower(), find_word_lower)
                if word_range.start >= 0:
                    line_no -= i + 1
                    return line_no, word_range
        if forward and (word_range.start < 0):
            for i, line in enumerate(forward_lines):
                word_range = find_word_in_line(line.lower(), find_word_lower)
                if word_range.start >= 0:
                    line_no += i + 1
                    return line_no, word_range
        return line_no, word_range

    def preprocess(
        self, pp_defs: dict = None, include_dirs: set = None, debug: bool = False
    ) -> tuple[list, list]:
        if pp_defs is None:
            pp_defs = {}
        if include_dirs is None:
            include_dirs = set()

        self.contents_pp, pp_skips, pp_defines, self.pp_defs = preprocess_file(
            self.contents_split,
            self.path,
            pp_defs=pp_defs,
            include_dirs=include_dirs,
            debug=debug,
        )
        return pp_skips, pp_defines

    def check_file(self, obj_tree, max_line_length=-1, max_comment_line_length=-1):
        diagnostics = []
        if (max_line_length > 0) or (max_comment_line_length > 0):
            msg_line = f'Line length exceeds "max_line_length" ({max_line_length})'
            msg_comment = (
                'Comment line length exceeds "max_comment_line_length"'
                f" ({max_comment_line_length})"
            )

            if self.fixed:
                COMMENT_LINE_MATCH = FRegex.FIXED_COMMENT
            else:
                COMMENT_LINE_MATCH = FRegex.FREE_COMMENT
            for i, line in enumerate(self.contents_split):
                if COMMENT_LINE_MATCH.match(line) is None:
                    if 0 < max_line_length < len(line):
                        self.ast.add_error(
                            msg_line, Severity.warn, i + 1, max_line_length, len(line)
                        )
                else:
                    if 0 < max_comment_line_length < len(line):
                        self.ast.add_error(
                            msg_comment,
                            Severity.warn,
                            i + 1,
                            max_comment_line_length,
                            len(line),
                        )
        errors, diags_ast = self.ast.check_file(obj_tree)
        diagnostics += diags_ast
        for error in errors:
            diagnostics.append(error.build(self))
        return diagnostics

    def parse(
        self,
        debug: bool = False,
        pp_defs: dict = None,
        include_dirs: set = None,
    ) -> FortranAST:
        """Parse Fortran file contents of a fortran_file object and build an
        Abstract Syntax Tree (AST)

        Parameters
        ----------
        debug : bool, optional
            Set to true to enable debugging, by default False
        pp_defs : dict, optional
            Preprocessor definitions and their values, by default None
        include_dirs : set, optional
            Preprocessor include directories, by default None

        Returns
        -------
        fortran_ast
            An Abstract Syntax Tree
        """

        if pp_defs is None:
            pp_defs = {}
        if include_dirs is None:
            include_dirs = set()
        # Configure the parser logger
        if debug:
            logging.basicConfig(
                level=logging.DEBUG, stream=sys.stdout, format="%(message)s"
            )

        # This is not necessarily the same as self.ast
        file_ast = FortranAST(self)
        if self.preproc:
            log.debug("=== PreProc Pass ===\n")
            pp_skips, pp_defines = self.preprocess(
                pp_defs=pp_defs, include_dirs=include_dirs, debug=debug
            )
            for pp_reg in pp_skips:
                file_ast.start_ppif(pp_reg[0])
                file_ast.end_ppif(pp_reg[1])
            log.debug("\n=== Parsing Pass ===\n")
        else:
            log.debug("=== No PreProc ===\n")
            pp_skips = []
            pp_defines = []

        line_no = 0
        line_no_end = 0
        block_id_stack = []
        docs: list[str] = []  # list used to temporarily store docstrings
        counters = Counter(
            do=0,
            ifs=0,
            block=0,
            select=0,
            imports=0,
            interface=0,
        )
        multi_lines = deque()
        self.COMMENT_LINE_MATCH, self.DOC_COMMENT_MATCH = self.get_comment_regexs()
        while (line_no_end < self.nLines) or multi_lines:
            # Get next line
            # Get a normal line, i.e. the stack is empty
            if not multi_lines:
                # Check if we need to advance the line number due to `&` continuation
                line_no = line_no_end if line_no_end > line_no else line_no
                # get_line has a 0-based index
                line = self.get_line(line_no, pp_content=True)
                line_no += 1  # Move to next line
                line_no_end = line_no
                get_full = True
            # Line is part of a multi-line construct, i.e. contained ';'
            else:
                line = multi_lines.pop()
                get_full = False

            if line == "":
                continue  # Skip empty lines

            # Parse documentation strings to AST nodes, this implicitly operates
            # on docs, i.e. appends or nullifies it
            idx = self.parse_docs(line, line_no, file_ast, docs)
            if idx:
                line_no = idx
                line_no_end = line_no
                continue
            # Handle preprocessing regions
            do_skip = False
            for pp_reg in pp_skips:
                if (line_no >= pp_reg[0]) and (line_no <= pp_reg[1]):
                    do_skip = True
                    break
            if line_no in pp_defines:
                do_skip = True
            if do_skip:
                continue
            # Get full line, seek forward for code lines
            # @note line_no-1 refers to the array index for the current line
            if get_full:
                _, line, post_lines = self.get_code_line(
                    line_no - 1, backward=False, pp_content=True
                )
                # Save the end of the line for the next iteration.
                # Need to keep the line number for registering start of Scopes
                line_no_end += len(post_lines)
                line = "".join([line] + post_lines)
            line, line_label = strip_line_label(line)
            line_stripped = strip_strings(line, maintain_len=True)
            # Find trailing comments
            comm_ind = line_stripped.find("!")
            if comm_ind >= 0:
                line_no_comment = line[:comm_ind]
                line_stripped = line_stripped[:comm_ind]
                docs = self.get_single_line_docstring(line[comm_ind:])
            else:
                line_no_comment = line
            # Split lines with semicolons, place the multiple lines into a stack
            if line_stripped.find(";") >= 0:
                multi_lines.extendleft(line_stripped.split(";"))
                line = multi_lines.pop()
                line_stripped = line
                line_no_comment = line
            # Test for scope end
            if file_ast.end_scope_regex is not None:
                match = FRegex.END_WORD.match(line_no_comment)
                # Handle end statement
                if self.parse_end_scope_word(line_no_comment, line_no, file_ast, match):
                    continue
                # Look for old-style end of DO loops with line labels
                if self.parse_do_fixed_format(
                    line, line_no, file_ast, line_label, block_id_stack
                ):
                    continue

            # Skip if known generic code line
            if FRegex.NON_DEF.match(line_no_comment):
                continue
            # Mark implicit statement
            if self.parse_implicit(line_no_comment, line_no, file_ast):
                continue
            # Mark contains statement
            if self.parse_contains(line_no_comment, line_no, file_ast):
                continue
            # Loop through tests
            obj_read = self.get_fortran_definition(line)
            # Move to next line if nothing in the definition tests matches
            if obj_read is None:
                continue

            obj_type: str = obj_read[0]
            obj_info = obj_read[1]
            if obj_type == "var":
                if obj_info.var_names is None:
                    continue
                link_name: str = None
                procedure_def = False
                if obj_info.var_type[:3] == "PRO":
                    if file_ast.current_scope.get_type() == INTERFACE_TYPE_ID:
                        for var_name in obj_info.var_names:
                            file_ast.add_int_member(var_name)
                        log.debug("%s !!! INTERFACE-PRO - Ln:%d", line.strip(), line_no)
                        continue
                    procedure_def = True
                    link_name = get_paren_substring(obj_info.var_type)
                for var_name in obj_info.var_names:
                    desc = obj_info.var_type
                    link_name: str = None
                    if var_name.find("=>") > -1:
                        name_split = var_name.split("=>")
                        name = name_split[0]
                        link_name = name_split[1].split("(")[0].strip()
                        if link_name.lower() == "null":
                            link_name = None
                    else:
                        name = var_name.split("=")[0]
                    # Add dimension if specified
                    # TODO: turn into function and add support for co-arrays i.e. [*]
                    # Copy global keywords to the individual variable
                    var_keywords: list[str] = obj_info.keywords[:]
                    # The name starts with (
                    if name.find("(") == 0:
                        continue
                    name, dims = self.parse_imp_dim(name)
                    name, char_len = self.parse_imp_char(name)
                    if dims:
                        var_keywords.append(dims)
                    if char_len:
                        desc += char_len

                    name = name.strip()
                    keywords, keyword_info = map_keywords(var_keywords)

                    if procedure_def:
                        new_var = Method(
                            file_ast,
                            line_no,
                            name,
                            desc,
                            keywords,
                            keyword_info=keyword_info,
                            proc_ptr=obj_info.var_kind,
                            link_obj=link_name,
                        )
                    else:
                        new_var = Variable(
                            file_ast,
                            line_no,
                            name,
                            desc,
                            keywords,
                            keyword_info=keyword_info,
                            kind=obj_info.var_kind,
                            link_obj=link_name,
                        )
                        # If the object is fortran_var and a parameter include
                        #  the value in hover
                        if new_var.is_parameter():
                            _, col = find_word_in_line(line, name)
                            match = FRegex.PARAMETER_VAL.match(line[col:])
                            if match:
                                var = " ".join(match.group(1).strip().split())
                                new_var.set_parameter_val(var)

                        # Check if the "variable" is external and if so cycle
                        if find_external(file_ast, desc, name, new_var):
                            continue

                    # if not merge_external:
                    file_ast.add_variable(new_var)
                log.debug("%s !!! VARIABLE - Ln:%d", line, line_no)

            elif obj_type == "mod":
                new_mod = Module(file_ast, line_no, obj_info)
                file_ast.add_scope(new_mod, FRegex.END_MOD)
                log.debug("%s !!! MODULE - Ln:%d", line, line_no)

            elif obj_type == "smod":
                new_smod = Submodule(
                    file_ast, line_no, obj_info.name, ancestor_name=obj_info.parent
                )
                file_ast.add_scope(new_smod, FRegex.END_SMOD)
                log.debug("%s !!! SUBMODULE - Ln:%d", line, line_no)

            elif obj_type == "prog":
                new_prog = Program(file_ast, line_no, obj_info)
                file_ast.add_scope(new_prog, FRegex.END_PROG)
                log.debug("%s !!! PROGRAM - Ln:%d", line, line_no)

            elif obj_type == "sub":
                keywords, _ = map_keywords(obj_info.keywords)
                new_sub = Subroutine(
                    file_ast,
                    line_no,
                    obj_info.name,
                    args=obj_info.args,
                    mod_flag=obj_info.mod_flag,
                    keywords=keywords,
                )
                file_ast.add_scope(new_sub, FRegex.END_SUB)
                log.debug("%s !!! SUBROUTINE - Ln:%d", line, line_no)

            elif obj_type == "fun":
                keywords, keyword_info = map_keywords(obj_info.keywords)
                new_fun = Function(
                    file_ast,
                    line_no,
                    obj_info.name,
                    args=obj_info.args,
                    mod_flag=obj_info.mod_flag,
                    keywords=keywords,
                    keyword_info=keyword_info,
                    result_type=obj_info.result.type,
                    result_name=obj_info.result.name,
                )
                file_ast.add_scope(new_fun, FRegex.END_FUN)
                # function type is present without result(), register the automatic
                # result() variable that is the function name
                if obj_info.result.type:
                    keywords, keyword_info = map_keywords(obj_info.result.keywords)
                    new_obj = Variable(
                        file_ast,
                        line_no,
                        name=obj_info.result.name,
                        var_desc=obj_info.result.type,
                        keywords=keywords,
                        keyword_info=keyword_info,
                        kind=obj_info.result.kind,
                    )
                    file_ast.add_variable(new_obj)
                log.debug("%s !!! FUNCTION - Ln:%d", line, line_no)

            elif obj_type == "block":
                name = obj_info
                if name is None:
                    counters["block"] += 1
                    name = f"#BLOCK{counters['block']}"
                new_block = Block(file_ast, line_no, name)
                file_ast.add_scope(new_block, FRegex.END_BLOCK, req_container=True)
                log.debug("%s !!! BLOCK - Ln:%d", line, line_no)

            elif obj_type == "do":
                counters["do"] += 1
                name = f"#DO{counters['do']}"
                if obj_info != "":
                    block_id_stack.append(obj_info)
                new_do = Do(file_ast, line_no, name)
                file_ast.add_scope(new_do, FRegex.END_DO, req_container=True)
                log.debug("%s !!! DO - Ln:%d", line, line_no)

            elif obj_type == "where":
                # Add block if WHERE is not single line
                if not obj_info:
                    counters["do"] += 1
                    name = f"#WHERE{counters['do']}"
                    new_do = Where(file_ast, line_no, name)
                    file_ast.add_scope(new_do, FRegex.END_WHERE, req_container=True)
                log.debug("%s !!! WHERE - Ln:%d", line, line_no)

            elif obj_type == "assoc":
                counters["block"] += 1
                name = f"#ASSOC{counters['block']}"
                new_assoc = Associate(file_ast, line_no, name)
                file_ast.add_scope(new_assoc, FRegex.END_ASSOCIATE, req_container=True)
                for bound_var in obj_info:
                    try:
                        bind_name, link_name = bound_var.split("=>")
                        file_ast.add_variable(
                            new_assoc.create_binding_variable(
                                file_ast,
                                line_no,
                                bind_name.strip(),
                                link_name.strip(),
                            )
                        )
                    except ValueError:
                        pass
                log.debug("%s !!! ASSOCIATE - Ln:%d", line, line_no)

            elif obj_type == "if":
                counters["if"] += 1
                name = f"#IF{counters['if']}"
                new_if = If(file_ast, line_no, name)
                file_ast.add_scope(new_if, FRegex.END_IF, req_container=True)
                log.debug("%s !!! IF - Ln:%d", line, line_no)

            elif obj_type == "select":
                counters["select"] += 1
                name = f"#SELECT{counters['select']}"
                new_select = Select(file_ast, line_no, name, obj_info)
                file_ast.add_scope(new_select, FRegex.END_SELECT, req_container=True)
                new_var = new_select.create_binding_variable(
                    file_ast,
                    line_no,
                    f"{obj_info.desc}({obj_info.binding})",
                    obj_info.type,
                )
                if new_var is not None:
                    file_ast.add_variable(new_var)
                log.debug("%s !!! SELECT - Ln:%d", line, line_no)

            elif obj_type == "typ":
                keywords, _ = map_keywords(obj_info.keywords)
                new_type = Type(file_ast, line_no, obj_info.name, keywords)
                if obj_info.parent is not None:
                    new_type.set_inherit(obj_info.parent)
                file_ast.add_scope(new_type, FRegex.END_TYPED, req_container=True)
                log.debug("%s !!! TYPE - Ln:%d", line, line_no)

            elif obj_type == "enum":
                counters["block"] += 1
                name = f"#ENUM{counters['block']}"
                new_enum = Enum(file_ast, line_no, name)
                file_ast.add_scope(new_enum, FRegex.END_ENUMD, req_container=True)
                log.debug("%s !!! ENUM - Ln:%d", line, line_no)

            elif obj_type == "int":
                name = obj_info.name
                if name is None:
                    counters["interface"] += 1
                    name = f"#GEN_INT{counters['interface']}"
                new_int = Interface(file_ast, line_no, name, abstract=obj_info.abstract)
                file_ast.add_scope(new_int, FRegex.END_INT, req_container=True)
                log.debug("%s !!! INTERFACE - Ln:%d", line, line_no)

            elif obj_type == "gen":
                new_int = Interface(
                    file_ast, line_no, obj_info.bound_name, abstract=False
                )
                new_int.set_visibility(obj_info.vis_flag)
                file_ast.add_scope(new_int, FRegex.END_INT, req_container=True)
                for pro_link in obj_info.pro_links:
                    file_ast.add_int_member(pro_link)
                file_ast.end_scope(line_no)
                log.debug("%s !!! GENERIC - Ln:%d", line, line_no)

            elif obj_type == "int_pro":
                if file_ast.current_scope is not None:
                    if file_ast.current_scope.get_type() == INTERFACE_TYPE_ID:
                        for name in obj_info:
                            file_ast.add_int_member(name)
                        log.debug("%s !!! INTERFACE-PRO - Ln:%d", line, line_no)

                    elif file_ast.current_scope.get_type() == SUBMODULE_TYPE_ID:
                        new_impl = Scope(file_ast, line_no, obj_info[0])
                        file_ast.add_scope(new_impl, FRegex.END_PRO)
                        log.debug("%s !!! INTERFACE-IMPL - Ln:%d", line, line_no)

            elif obj_type == "use":
                obj_info.line_number = line_no
                file_ast.add_use(obj_info)
                log.debug("%s !!! USE - Ln:%d", line, line_no)

            elif obj_type == "import":
                obj_info.line_number = line_no
                obj_info.mod_name += str(counters["import"])
                file_ast.add_use(obj_info)
                counters["imports"] += 1
                log.debug("%s !!! IMPORT - Ln:%d", line, line_no)

            elif obj_type == "inc":
                file_ast.add_include(obj_info, line_no)
                log.debug("%s !!! INCLUDE - Ln:%d", line, line_no)

            elif obj_type == "vis":
                if file_ast.current_scope is None:
                    msg = "Visibility statement without enclosing scope"
                    file_ast.add_error(msg, Severity.error, line_no, 0)
                else:
                    if len(obj_info.obj_names) == 0 and obj_info.type == 1:  # private
                        file_ast.current_scope.set_default_vis(-1)
                    else:
                        if obj_info.type == 1:  # private
                            for word in obj_info.obj_names:
                                file_ast.add_private(word)
                        else:
                            for word in obj_info.obj_names:
                                file_ast.add_public(word)
                log.debug("%s !!! VISIBILITY - Ln:%d", line, line_no)

        file_ast.close_file(line_no)
        if debug:
            if len(file_ast.end_errors) > 0:
                log.debug("\n=== Scope Errors ===\n")
                for error in file_ast.end_errors:
                    if error[0] >= 0:
                        message = f"Unexpected end of scope at line {error[0]}"
                    else:
                        message = "Unexpected end statement: No open scopes"
                    log.debug("%s: %s", error[1], message)
            if len(file_ast.parse_errors) > 0:
                log.debug("\n=== Parsing Errors ===\n")
                for error in file_ast.parse_errors:
                    log.debug("%s: %s", error["range"], error["message"])
        return file_ast

    def parse_imp_dim(self, line: str):
        """Parse the implicit dimension of an array e.g.
        var(3,4), var_name(size(val,1)*10)

        Parameters
        ----------
        line : str
            line containing variable name

        Returns
        -------
        tuple[str, str]
            truncated line, dimension string
        """
        m = re.compile(r"[ ]*\w+[ ]*(\()", re.I).match(line)
        if not m:
            return line, None
        i = find_paren_match(line[m.end(1) :])
        if i < 0:
            return line, None  # triggers for autocomplete
        dims = line[m.start(1) : m.end(1) + i + 1]
        line = line[: m.start(1)] + line[m.end(1) + i + 1 :]
        return line, f"dimension{dims}"

    def parse_imp_char(self, line: str):
        """Parse the implicit character length from a variable e.g.
        var_name*10 or var_name*(10), var_name*(size(val, 1))

        Parameters
        ----------
        line : str
            line containing potential variable

        Returns
        -------
        tuple[str, str]
            truncated line, character length
        """
        match = re.compile(r"(\w+)[ ]*\*[ ]*(\d+|\()", re.I).match(line)
        if not match:
            return line, None
        if match.group(2) == "(":
            i = find_paren_match(line[match.end(2) :])
            if i < 0:
                return line, None  # triggers for autocomplete
            char_len = line[match.start(2) : match.end(2) + i + 1]
        elif match.group(2).isdigit():
            char_len = match.group(2)
        return match.group(1), f"*{char_len}"

    def parse_end_scope_word(
        self, line: str, ln: int, file_ast: FortranAST, match: re.Match
    ) -> bool:
        """Parses END keyword marking the end of scopes

        Parameters
        ----------
        line : str
            Document line
        ln : int
            Line number
        file_ast : fortran_ast
            AST object
        match : re.Match
            END word regular expression match

        Returns
        -------
        bool
            True if a AST scope is closed, False otherwise
        """
        if match is None:
            return False

        end_scope_word: str = None
        if match.group(1) is None:
            end_scope_word = ""
            if file_ast.current_scope.req_named_end() and (
                file_ast.current_scope is not file_ast.none_scope
            ):
                file_ast.end_errors.append([ln, file_ast.current_scope.sline])
        else:
            scope_match = file_ast.end_scope_regex.match(line[match.start(1) :])
            if scope_match is not None:
                end_scope_word = scope_match.group(0)
        if end_scope_word is not None:
            if (file_ast.current_scope.get_type() == SELECT_TYPE_ID) and (
                file_ast.current_scope.is_type_region()
            ):
                file_ast.end_scope(ln)
            file_ast.end_scope(ln)
            log.debug("%s !!! END %s Scope - Ln:%d", line, end_scope_word.upper(), ln)
            return True
        return False

    def parse_do_fixed_format(
        self,
        line: str,
        ln: int,
        file_ast: FortranAST,
        line_label: str,
        block_id_stack: list[str],
    ):
        if (file_ast.current_scope.get_type() == DO_TYPE_ID) and (
            line_label is not None
        ):
            # TODO: try and move to end_scope pattern
            did_close = False
            while (len(block_id_stack) > 0) and (line_label == block_id_stack[-1]):
                file_ast.end_scope(ln)
                block_id_stack.pop()
                did_close = True
                log.debug("%s !!! END DO-LABELLED - Ln:%d", line, ln)
            if did_close:
                return True
        return False

    def parse_implicit(self, line: str, ln: int, file_ast: FortranAST) -> bool:
        """Parse implicit statements from a line

        Parameters
        ----------
        line : str
            Document line
        ln : int
            Line number
        file_ast : fortran_ast
            AST object

        Returns
        -------
        bool
            True if an IMPLICIT statements present, False otherwise
        """
        match = FRegex.IMPLICIT.match(line)
        if match is None:
            return False
        if file_ast.current_scope is None:
            msg = "IMPLICIT statement without enclosing scope"
            file_ast.add_error(msg, Severity.error, ln, match.start(1), match.end(1))
        else:
            if match.group(1).lower() == "none":
                file_ast.current_scope.set_implicit(False, ln)
            else:
                file_ast.current_scope.set_implicit(True, ln)

        log.debug("%s !!! IMPLICIT - Ln:%d", line, ln)
        return True

    def parse_contains(self, line: str, ln: int, file_ast: FortranAST) -> bool:
        """Parse contain statements

        Parameters
        ----------
        line : str
            Document line
        ln : int
            Line number
        file_ast : fortran_ast
            AST object

        Returns
        -------
        bool
            True if a contains is present, False otherwise
        """
        match = FRegex.CONTAINS.match(line)
        if match is None:
            return False
        msg: str = None
        try:
            if file_ast.current_scope is None:
                msg = "CONTAINS statement without enclosing scope"
            else:
                file_ast.current_scope.mark_contains(ln)
        except ValueError:
            msg = "Multiple CONTAINS statements in scope"
        if msg:
            file_ast.add_error(msg, Severity.error, ln, match.start(1), match.end(1))
        log.debug("%s !!! CONTAINS - Ln:%d", line, ln)
        return True

    def parse_docs(self, line: str, ln: int, file_ast: FortranAST, docs: list[str]):
        """Parse documentation stings of style Doxygen or FORD.
        Multiline docstrings are detected if the first comment starts with `!>`
        docstring continuations are detected with either `!>`, `!<` or `!!`

        Parameters
        ----------
        line : str
            Document line
        ln : int
            Line number
        file_ast : fortran_ast
            AST object
        docs : list[str]
            Docstrings that are pending processing e.g. single line docstrings
        """

        def format(docs: list[str]) -> str:
            """Format docstrings and parse for Doxygen tags"""
            if len(docs) == 1:
                return f"{docs[0]}"
            docstr = ""
            has_args = True
            idx_args = -1
            for i, line in enumerate(docs):
                if line.startswith("@brief"):
                    docstr += line.replace("@brief", "", 1).strip() + "\n"
                elif line.startswith("@param"):
                    if has_args:
                        docstr += "\n**Parameters:**  \n"
                        has_args = False
                        idx_args = len(docstr)
                    docstr += re.sub(
                        r"[@\\]param(?:[\[\(]\s*[\w,]+\s*[\]\)])?\s+(.*?)\s+",
                        r"  \n`\1` - ",
                        line + " ",
                    )
                elif line.startswith("@return"):
                    docstr += "\n**Returns:**  \n"
                    docstr += line.replace("@return", "", 1).strip() + "\n"
                else:
                    docstr += line.strip() + "\n"
            # Remove new line characters from 1st @param line
            if idx_args > 0:
                docstr = docstr[: idx_args - 3] + docstr[idx_args:].replace(
                    "  \n ", "", 1
                )
            return docstr

        def add_line_comment(file_ast: FortranAST, docs: list[str]):
            # Handle dangling comments from previous line
            if docs:
                file_ast.add_doc(format(docs))
                log.debug("%s !!! Doc string - Line:%d", format(docs), ln)
                docs[:] = []  # empty the documentation stack

        # Check for comments in line
        if not self.COMMENT_LINE_MATCH.match(line):
            add_line_comment(file_ast, docs)
            return False
        # Check for documentation
        doc_match = self.DOC_COMMENT_MATCH.match(line)
        if not doc_match:
            add_line_comment(file_ast, docs)
            return False

        _ln = ln
        ln, docs[:], predocmark = self.get_docstring(ln, line, doc_match, docs)

        # Count the total length of all the stings in docs
        # most efficient implementation, see: shorturl.at/dfmyV
        if len("".join(docs)) > 0:
            file_ast.add_doc(format(docs), forward=predocmark)
        for i, doc_line in enumerate(docs):
            log.debug("%s !!! Doc string - Line:%d", doc_line, _ln + i)
        docs[:] = []
        return ln

    def get_docstring(
        self, ln: int, line: str, match: Match[str], docs: list[str]
    ) -> tuple[int, list[str], bool]:
        """Extract entire documentation strings from the current file position

        Parameters
        ----------
        ln : int
            Line number
        line : str
            Document line, not necessarily produced by `get_line()`
        match : Match[str]
            Regular expression DOC match
        docs : list[str]
            Docstrings that are pending processing e.g. single line docstrings

        Returns
        -------
        tuple[int, list[str], bool]
            The new line number at the end of the docstring, the docstring and
            a boolean flag indicating whether the docstring precedes the AST node
            (Doxygen style) or succeeds it (traditional FORD style)
        """
        docstring: list[str] = docs
        docstring.append(line[match.end(0) :].strip())
        predocmark = True if match.group(1) == ">" else False

        if ln >= self.nLines:
            return ln, docstring, predocmark

        # @note line index is 0-based
        # Start from the current line until EOF and check for docs
        for i in range(ln, self.nLines):
            next_line = self.get_line(i, pp_content=True)
            match = self.DOC_COMMENT_MATCH.match(next_line)
            if not match:
                ln = i
                break
            docstring.append(next_line[match.end(0) :].strip())
        return ln, docstring, predocmark

    def get_single_line_docstring(self, line: str) -> list[str]:
        """Get a docstring of a single line. This is the same for both Legacy
        and Modern Fortran

        Parameters
        ----------
        line : str
            Line of code

        Returns
        -------
        list[str]
            A list containing the docstring. List will be empty if there is no
            match or the match is an empty string itself
        """
        match = FRegex.FREE_DOC.match(line)
        if not match:
            return []
        # if the string is empty return an empty list instead
        doc = line[match.end(0) :].strip()
        return [doc] if doc else []

    def get_comment_regexs(self) -> tuple[Pattern[str], Pattern[str]]:
        if self.fixed:
            return FRegex.FIXED_COMMENT, FRegex.FIXED_DOC
        return FRegex.FREE_COMMENT, FRegex.FREE_DOC

    def get_fortran_definition(self, line: str):
        for fortran_def in def_tests:
            obj = fortran_def(line)
            if obj is not None:
                return obj
        return None


def preprocess_file(
    contents_split: list,
    file_path: str = None,
    pp_defs: dict = None,
    include_dirs: set = None,
    debug: bool = False,
):
    # Look for and mark excluded preprocessor paths in file
    # Initial implementation only looks for "if" and "ifndef" statements.
    # For "if" statements all blocks are excluded except the "else" block if present
    # For "ifndef" statements all blocks excluding the first block are excluded
    def eval_pp_if(text, defs: dict = None):
        def replace_ops(expr: str):
            expr = expr.replace("&&", " and ")
            expr = expr.replace("||", " or ")
            expr = expr.replace("!=", " <> ")
            expr = expr.replace("!", " not ")
            expr = expr.replace(" <> ", " != ")
            return expr

        def replace_defined(line: str):
            i0 = 0
            out_line = ""
            for match in FRegex.DEFINED.finditer(line):
                if match.group(1) in defs:
                    out_line += line[i0 : match.start(0)] + "(@$@)"
                else:
                    out_line += line[i0 : match.start(0)] + "(%$%)"
                i0 = match.end(0)
            if i0 < len(line):
                out_line += line[i0:]
            return out_line

        def replace_vars(line: str):
            i0 = 0
            out_line = ""
            for match in FRegex.WORD.finditer(line):
                if match.group(0) in defs:
                    out_line += line[i0 : match.start(0)] + defs[match.group(0)]
                else:
                    out_line += line[i0 : match.start(0)] + "False"
                i0 = match.end(0)
            if i0 < len(line):
                out_line += line[i0:]
            out_line = out_line.replace("@$@", "True")
            out_line = out_line.replace("%$%", "False")
            return out_line

        if defs is None:
            defs = {}
        out_line = replace_defined(text)
        out_line = replace_vars(out_line)
        try:
            line_res = eval(replace_ops(out_line))
        except:
            return False
        else:
            return line_res

    def expand_func_macro(def_name: str, def_value: tuple[str, str]):
        def_args, sub = def_value
        def_args = def_args.split(",")
        regex = re.compile(rf"\b{def_name}\s*\({','.join(['(.*)']*len(def_args))}\)")

        for i, arg in enumerate(def_args, start=1):
            sub = re.sub(rf"\b({arg.strip()})\b", rf"\\{i}", sub)

        return regex, sub

    def append_multiline_macro(def_value: str | tuple, line: str):
        if isinstance(def_value, tuple):
            def_args, def_value = def_value
            def_value += line
            return (def_args, def_value)
        return def_value + line

    if pp_defs is None:
        pp_defs = {}
    if include_dirs is None:
        include_dirs = set()
    if file_path is not None:
        include_dirs.add(os.path.abspath(os.path.dirname(file_path)))
    pp_skips = []
    pp_defines = []
    pp_stack = []
    pp_stack_group = []
    defs_tmp = pp_defs.copy()
    def_regexes = {}
    output_file = []
    def_cont_name = None
    for i, line in enumerate(contents_split):
        # Handle multiline macro continuation
        if def_cont_name is not None:
            output_file.append("")
            is_multiline = line.strip()[-1] != "\\"
            line_to_append = line.strip() if is_multiline else line[0:-1].strip()
            defs_tmp[def_cont_name] = append_multiline_macro(
                defs_tmp[def_cont_name], line_to_append
            )
            if is_multiline:
                def_cont_name = None
            continue
        # Handle conditional statements
        match = FRegex.PP_REGEX.match(line)
        if match:
            output_file.append(line)
            def_name = None
            if_start = False
            # Opening conditional statements
            if match.group(1).lower() == "if ":
                is_path = eval_pp_if(line[match.end(1) :], defs_tmp)
                if_start = True
            elif match.group(1).lower() == "ifdef":
                if_start = True
                def_name = line[match.end(0) :].strip()
                is_path = def_name in defs_tmp
            elif match.group(1).lower() == "ifndef":
                if_start = True
                def_name = line[match.end(0) :].strip()
                is_path = not (def_name in defs_tmp)
            if if_start:
                if is_path:
                    pp_stack.append([-1, -1])
                    log.debug("%s !!! Conditional TRUE(%d)", line.strip(), i + 1)
                else:
                    pp_stack.append([i + 1, -1])
                    log.debug("%s !!! Conditional FALSE(%d)", line.strip(), i + 1)
                continue
            if len(pp_stack) == 0:
                continue
            # Closing/middle conditional statements
            inc_start = False
            exc_start = False
            exc_continue = False
            if match.group(1).lower() == "elif":
                if (not pp_stack_group) or (pp_stack_group[-1][0] != len(pp_stack)):
                    # First elif statement for this elif group
                    if pp_stack[-1][0] < 0:
                        pp_stack_group.append([len(pp_stack), True])
                    else:
                        pp_stack_group.append([len(pp_stack), False])
                if pp_stack_group[-1][1]:
                    # An earlier if or elif in this group has been true
                    exc_continue = True
                    if pp_stack[-1][0] < 0:
                        pp_stack[-1][0] = i + 1
                elif eval_pp_if(line[match.end(1) :], defs_tmp):
                    pp_stack[-1][1] = i + 1
                    pp_skips.append(pp_stack.pop())
                    pp_stack_group[-1][1] = True
                    pp_stack.append([-1, -1])
                    inc_start = True
                else:
                    exc_start = True
            elif match.group(1).lower() == "else":
                if pp_stack[-1][0] < 0:
                    pp_stack[-1][0] = i + 1
                    exc_start = True
                elif (
                    pp_stack_group
                    and (pp_stack_group[-1][0] == len(pp_stack))
                    and (pp_stack_group[-1][1])
                ):
                    # An earlier if or elif in this group has been true
                    exc_continue = True
                else:
                    pp_stack[-1][1] = i + 1
                    pp_skips.append(pp_stack.pop())
                    pp_stack.append([-1, -1])
                    inc_start = True
            elif match.group(1).lower() == "endif":
                if pp_stack_group and (pp_stack_group[-1][0] == len(pp_stack)):
                    pp_stack_group.pop()
                if pp_stack[-1][0] < 0:
                    pp_stack.pop()
                    log.debug("%s !!! Conditional TRUE/END(%d)", line.strip(), i + 1)
                    continue
                if pp_stack[-1][1] < 0:
                    pp_stack[-1][1] = i + 1
                    log.debug("%s !!! Conditional FALSE/END(%d)", line.strip(), i + 1)
                pp_skips.append(pp_stack.pop())
            if debug:
                if inc_start:
                    log.debug("%s !!! Conditional TRUE(%d)", line.strip(), i + 1)
                elif exc_start:
                    log.debug("%s !!! Conditional FALSE(%d)", line.strip(), i + 1)
                elif exc_continue:
                    log.debug("%s !!! Conditional EXCLUDED(%d)", line.strip(), i + 1)
            continue
        # Handle variable/macro definitions files
        match = FRegex.PP_DEF.match(line)
        if (match is not None) and ((len(pp_stack) == 0) or (pp_stack[-1][0] < 0)):
            output_file.append(line)
            pp_defines.append(i + 1)
            def_name = match.group(2)
            # If this is an argument list of a function add them to the name
            # get_definition will only return the function name upon hover
            # hence if the argument list is appended in the def_name then
            # querying the dictionary will not yield a result.
            # Need to properly parse the preprocessor files instead of this.
            # This also does not allow for multiline argument list definitions.
            # if match.group(3):
            #     def_name += match.group(3)
            if (match.group(1) == "define") and (def_name not in defs_tmp):
                eq_ind = line[match.end(0) :].find(" ")
                if eq_ind >= 0:
                    # Handle multiline macros
                    if line.rstrip()[-1] == "\\":
                        def_value = line[match.end(0) + eq_ind : -1].strip()
                        def_cont_name = def_name
                    else:
                        def_value = line[match.end(0) + eq_ind :].strip()
                else:
                    def_value = "True"

                # are there arguments to parse?
                if match.group(3):
                    def_value = (match.group(4), def_value)

                defs_tmp[def_name] = def_value
            elif (match.group(1) == "undef") and (def_name in defs_tmp):
                defs_tmp.pop(def_name, None)
            log.debug("%s !!! Define statement(%d)", line.strip(), i + 1)
            continue
        # Handle include files
        match = FRegex.PP_INCLUDE.match(line)
        if (match is not None) and ((len(pp_stack) == 0) or (pp_stack[-1][0] < 0)):
            log.debug("%s !!! Include statement(%d)", line.strip(), i + 1)
            include_filename = match.group(1).replace('"', "")
            include_path = None
            # Intentionally keep this as a list and not a set. There are cases
            # where projects play tricks with the include order of their headers
            # to get their codes to compile. Using a set would not permit that.
            for include_dir in include_dirs:
                include_path_tmp = os.path.join(include_dir, include_filename)
                if os.path.isfile(include_path_tmp):
                    include_path = os.path.abspath(include_path_tmp)
                    break
            if include_path is not None:
                try:
                    include_file = FortranFile(include_path)
                    err_string, _ = include_file.load_from_disk()
                    if err_string is None:
                        log.debug("\n!!! Parsing include file '%s'", include_path)
                        _, _, _, defs_tmp = preprocess_file(
                            include_file.contents_split,
                            file_path=include_path,
                            pp_defs=defs_tmp,
                            include_dirs=include_dirs,
                            debug=debug,
                        )
                        log.debug("!!! Completed parsing include file\n")

                    else:
                        log.debug("!!! Failed to parse include file: %s", err_string)

                except:
                    log.debug("!!! Failed to parse include file: exception")

            else:
                log.debug(
                    "%s !!! Could not locate include file (%d)", line.strip(), i + 1
                )

        # Substitute (if any) read in preprocessor macros
        for def_tmp, value in defs_tmp.items():
            # Skip if the line does not contain the macro at all. This is supposed to
            # spare the expensive regex-substitution in case we do not need it at all
            if def_tmp not in line:
                continue
            def_regex = def_regexes.get(def_tmp)
            if def_regex is None:
                if isinstance(value, tuple):
                    def_regex = expand_func_macro(def_tmp, value)
                else:
                    def_regex = re.compile(rf"\b{def_tmp}\b")
                def_regexes[def_tmp] = def_regex

            if isinstance(def_regex, tuple):
                def_regex, value = def_regex

            line_new, nsubs = def_regex.subn(value, line)
            if nsubs > 0:
                log.debug(
                    "%s !!! Macro sub(%d) '%s' -> '%s'",
                    line.strip(),
                    i + 1,
                    def_tmp,
                    value,
                )
                line = line_new
        output_file.append(line)
    return output_file, pp_skips, pp_defines, defs_tmp
