from __future__ import annotations

import logging
import os
from pathlib import Path

from fortls.constants import KEYWORD_ID_DICT, KEYWORD_LIST, sort_keywords
from fortls.regex_patterns import (
    DQ_STRING_REGEX,
    FIXED_COMMENT_LINE_MATCH,
    FREE_FORMAT_TEST,
    LINE_LABEL_REGEX,
    LOGICAL_REGEX,
    NAT_VAR_REGEX,
    NUMBER_REGEX,
    OBJBREAK_REGEX,
    SQ_STRING_REGEX,
    WORD_REGEX,
)

log = logging.getLogger(__name__)


def expand_name(line, char_poss):
    """Get full word containing given cursor position"""
    # The order here is important.
    # WORD will capture substrings in logical and strings
    regexs = [LOGICAL_REGEX, SQ_STRING_REGEX, DQ_STRING_REGEX, WORD_REGEX, NUMBER_REGEX]
    for r in regexs:
        for num_match in r.finditer(line):
            if num_match.start(0) <= char_poss and num_match.end(0) >= char_poss:
                return num_match.group(0)
    return ""


def detect_fixed_format(file_lines):
    """Detect fixed/free format by looking for characters in label columns
    and variable declarations before column 6. Treat intersection format
    files as free format."""
    for line in file_lines:
        if FREE_FORMAT_TEST.match(line):
            return False
        tmp_match = NAT_VAR_REGEX.match(line)
        if tmp_match and tmp_match.start(1) < 6:
            return False
        # Trailing ampersand indicates free or intersection format
        if not FIXED_COMMENT_LINE_MATCH.match(line):
            line_end = line.split("!")[0].strip()
            if len(line_end) > 0 and line_end[-1] == "&":
                return False
    return True


def strip_line_label(line):
    """Strip leading numeric line label"""
    match = LINE_LABEL_REGEX.match(line)
    if match is None:
        return line, None
    else:
        line_label = match.group(1)
        out_str = line[: match.start(1)] + " " * len(line_label) + line[match.end(1) :]
        return out_str, line_label


def strip_strings(in_line, maintain_len=False):
    """String string literals from code line"""

    def repl_sq(m):
        return "'{0}'".format(" " * (len(m.group()) - 2))

    def repl_dq(m):
        return '"{0}"'.format(" " * (len(m.group()) - 2))

    if maintain_len:
        out_line = SQ_STRING_REGEX.sub(repl_sq, in_line)
        out_line = DQ_STRING_REGEX.sub(repl_dq, out_line)
    else:
        out_line = SQ_STRING_REGEX.sub("", in_line)
        out_line = DQ_STRING_REGEX.sub("", out_line)
    return out_line


def separate_def_list(test_str):
    """Separate definition lists, skipping parenthesis and bracket groups

    Examples:
      "var1, var2, var3" -> ["var1", "var2", "var3"]
      "var, init_var(3) = [1,2,3], array(3,3)" -> ["var", "init_var", "array"]
    """
    stripped_str = strip_strings(test_str)
    paren_count = 0
    def_list = []
    curr_str = ""
    for char in stripped_str:
        if (char == "(") or (char == "["):
            paren_count += 1
        elif (char == ")") or (char == "]"):
            paren_count -= 1
        elif (char == ",") and (paren_count == 0):
            curr_str = curr_str.strip()
            if curr_str != "":
                def_list.append(curr_str)
                curr_str = ""
            elif (curr_str == "") and (len(def_list) == 0):
                return None
            continue
        curr_str += char
    curr_str = curr_str.strip()
    if curr_str != "":
        def_list.append(curr_str)
    return def_list


def find_word_in_line(line, word):
    """Find Fortran word in line"""
    i0 = -1
    for poss_name in WORD_REGEX.finditer(line):
        if poss_name.group() == word:
            i0 = poss_name.start()
            break
    return i0, i0 + len(word)


def find_paren_match(test_str):
    """Find matching closing parenthesis by searching forward,
    returns -1 if no match is found"""
    paren_count = 1
    ind = -1
    for (i, char) in enumerate(test_str):
        if char == "(":
            paren_count += 1
        elif char == ")":
            paren_count -= 1
        if paren_count == 0:
            return i
    return ind


def get_line_prefix(pre_lines: list, curr_line: str, col: int, qs: bool = True) -> str:
    """Get code line prefix from current line and preceding continuation lines

    Parameters
    ----------
    pre_lines : list
        for multiline cases get all the previous, relevant lines
    curr_line : str
        the current line
    col : int
        column index of the current line
    qs : bool, optional
        strip quotes i.e. string literals from `curr_line` and `pre_lines`.
        Need this disable when hovering over string literals, by default True

    Returns
    -------
    str
        part of the line including any relevant line continuations before `col`
    """
    if (curr_line is None) or (col > len(curr_line)) or (curr_line.startswith("#")):
        return None
    prepend_string = "".join(pre_lines)
    curr_line = prepend_string + curr_line
    col += len(prepend_string)
    line_prefix = curr_line[:col].lower()
    # Ignore string literals
    if qs:
        if (line_prefix.find("'") > -1) or (line_prefix.find('"') > -1):
            sq_count = 0
            dq_count = 0
            for char in line_prefix:
                if (char == "'") and (dq_count % 2 == 0):
                    sq_count += 1
                elif (char == '"') and (sq_count % 2 == 0):
                    dq_count += 1
            if (dq_count % 2 == 1) or (sq_count % 2 == 1):
                return None
    return line_prefix


def resolve_globs(glob_path: str, root_path: str = None) -> list[str]:
    """Resolve glob patterns

    Parameters
    ----------
    glob_path : str
        Path containing the glob pattern follows
        `fnmatch` glob pattern, can include relative paths, etc.
        see fnmatch: https://docs.python.org/3/library/fnmatch.html#module-fnmatch

    root_path : str, optional
        root path to start glob search. If left empty the root_path will be
        extracted from the glob_path, by default None

    Returns
    -------
    list[str]
        Expanded glob patterns with absolute paths.
        Absolute paths are used to resolve any potential ambiguity
    """
    # Path.glob returns a generator, we then cast the Path obj to a str
    # alternatively use p.as_posix()
    if root_path:
        return [str(p) for p in Path(root_path).resolve().glob(glob_path)]
    # Attempt to extract the root and glob pattern from the glob_path
    # This is substantially less robust that then above
    else:
        p = Path(glob_path).expanduser()
        parts = p.parts[p.is_absolute() :]
        return [str(i) for i in Path(p.root).resolve().glob(str(Path(*parts)))]


def only_dirs(paths: list[str], err_msg: list = []) -> list[str]:
    dirs: list[str] = []
    for p in paths:
        if os.path.isdir(p):
            dirs.append(p)
        elif os.path.isfile(p):
            continue
        else:
            msg: str = (
                f"Directory '{p}' specified in Configuration settings file does not"
                " exist"
            )
            if err_msg:
                err_msg.append([2, msg])
            else:
                log.warning(msg)
    return dirs


def set_keyword_ordering(sorted):
    global sort_keywords
    sort_keywords = sorted


def map_keywords(keywords):
    mapped_keywords = []
    keyword_info = {}
    for keyword in keywords:
        keyword_prefix = keyword.split("(")[0].lower()
        keyword_ind = KEYWORD_ID_DICT.get(keyword_prefix)
        if keyword_ind is not None:
            mapped_keywords.append(keyword_ind)
            if keyword_prefix in ("intent", "dimension", "pass"):
                keyword_substring = get_paren_substring(keyword)
                if keyword_substring is not None:
                    keyword_info[keyword_prefix] = keyword_substring
    if sort_keywords:
        mapped_keywords.sort()
    return mapped_keywords, keyword_info


def get_keywords(keywords, keyword_info={}):
    keyword_strings = []
    for keyword_id in keywords:
        string_rep = KEYWORD_LIST[keyword_id]
        addl_info = keyword_info.get(string_rep)
        string_rep = string_rep.upper()
        if addl_info is not None:
            string_rep += "({0})".format(addl_info)
        keyword_strings.append(string_rep)
    return keyword_strings


def get_paren_substring(test_str):
    i1 = test_str.find("(")
    i2 = test_str.rfind(")")
    if i1 > -1 and i2 > i1:
        return test_str[i1 + 1 : i2]
    else:
        return None


def get_paren_level(line):
    """Get sub-string corresponding to a single parenthesis level,
    via backward search up through the line.

    Examples:
      "CALL sub1(arg1,arg2" -> ("arg1,arg2", [[10, 19]])
      "CALL sub1(arg1(i),arg2" -> ("arg1,arg2", [[10, 14], [17, 22]])
    """
    if line == "":
        return "", [[0, 0]]
    level = 0
    in_string = False
    string_char = ""
    i1 = len(line)
    sections = []
    for i in range(len(line) - 1, -1, -1):
        char = line[i]
        if in_string:
            if char == string_char:
                in_string = False
            continue
        if (char == "(") or (char == "["):
            level -= 1
            if level == 0:
                i1 = i
            elif level < 0:
                sections.append([i + 1, i1])
                break
        elif (char == ")") or (char == "]"):
            level += 1
            if level == 1:
                sections.append([i + 1, i1])
        elif (char == "'") or (char == '"'):
            in_string = True
            string_char = char
    if level == 0:
        sections.append([i, i1])
    sections.reverse()
    out_string = ""
    for section in sections:
        out_string += line[section[0] : section[1]]
    return out_string, sections


def get_var_stack(line):
    """Get user-defined type field sequence terminating the given line

    Examples:
      "myvar%foo%bar" -> ["myvar", "foo", "bar"]
      "myarray(i)%foo%bar" -> ["myarray", "foo", "bar"]
      "CALL self%method(this%foo" -> ["this", "foo"]
    """
    if len(line) == 0:
        return [""]
    final_var, sections = get_paren_level(line)
    if final_var == "":
        return [""]
    # Continuation of variable after paren requires '%' character
    iLast = 0
    for (i, section) in enumerate(sections):
        if not line[section[0] : section[1]].startswith("%"):
            iLast = i
    final_var = ""
    for section in sections[iLast:]:
        final_var += line[section[0] : section[1]]
    #
    if final_var is not None:
        final_op_split = OBJBREAK_REGEX.split(final_var)
        return final_op_split[-1].split("%")
    else:
        return None
