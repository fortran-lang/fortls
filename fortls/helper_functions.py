from __future__ import annotations

import os
from pathlib import Path

from fortls.constants import KEYWORD_ID_DICT, KEYWORD_LIST, FRegex, log, sort_keywords
from fortls.ftypes import Range


def expand_name(line: str, char_pos: int) -> str:
    """Get full word containing given cursor position

    Parameters
    ----------
    line : str
        Text line
    char_pos : int
        Column position along the line

    Returns
    -------
    str
        Word regex match for the input column
    """
    # The order here is important.
    # WORD will capture substrings in logical and strings
    regexs = [
        FRegex.LOGICAL,
        FRegex.SQ_STRING,
        FRegex.DQ_STRING,
        FRegex.WORD,
        FRegex.NUMBER,
    ]
    for r in regexs:
        for num_match in r.finditer(line):
            if num_match.start(0) <= char_pos <= num_match.end(0):
                return num_match.group(0)
    return ""


def detect_fixed_format(file_lines: list[str]) -> bool:
    """Detect fixed/free format by looking for characters in label columns
    and variable declarations before column 6. Treat intersection format
    files as free format.

    Parameters
    ----------
    file_lines : list[str]
        List of consecutive file lines

    Returns
    -------
    bool
        True if file_lines are of Fixed Fortran style
    """
    for line in file_lines:
        if FRegex.FREE_FORMAT_TEST.match(line):
            return False
        tmp_match = FRegex.VAR.match(line)
        if tmp_match and tmp_match.start(1) < 6:
            return False
        # Trailing ampersand indicates free or intersection format
        if not FRegex.FIXED_COMMENT.match(line):
            line_end = line.split("!")[0].strip()
            if len(line_end) > 0 and line_end[-1] == "&":
                return False
    return True


def strip_line_label(line: str) -> tuple[str, str | None]:
    """Strip leading numeric line label

    Parameters
    ----------
    line : str
        Text line

    Returns
    -------
    tuple[str, str | None]
        Output string, Line label returns None if no line label present
    """

    match = FRegex.LINE_LABEL.match(line)
    if match is None:
        return line, None
    else:
        line_label = match.group(1)
        out_str = line[: match.start(1)] + " " * len(line_label) + line[match.end(1) :]
        return out_str, line_label


def strip_strings(in_line: str, maintain_len: bool = False) -> str:
    """Strips string literals from code line

    Parameters
    ----------
    in_line : str
        Text string
    maintain_len : bool, optional
        Maintain the len(in_line) in the output string, by default False

    Returns
    -------
    str
        Stripped string
    """

    def repl_sq(m):
        return "'{0}'".format(" " * (len(m.group()) - 2))

    def repl_dq(m):
        return '"{0}"'.format(" " * (len(m.group()) - 2))

    if maintain_len:
        out_line = FRegex.SQ_STRING.sub(repl_sq, in_line)
        out_line = FRegex.DQ_STRING.sub(repl_dq, out_line)
    else:
        out_line = FRegex.SQ_STRING.sub("", in_line)
        out_line = FRegex.DQ_STRING.sub("", out_line)
    return out_line


def separate_def_list(test_str: str) -> list[str] | None:
    """Separate definition lists, skipping parenthesis and bracket groups

    Parameters
    ----------
    test_str : str
        Text string

    Returns
    -------
    list[str] | None
        [description]

    Examples
    --------
    >>> separate_def_list("var1, var2, var3")
    ["var1", "var2", "var3"]


    >>> separate_def_list("var, init_var(3) = [1,2,3], array(3,3)")
    ["var", "init_var", "array"]
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


def find_word_in_line(line: str, word: str) -> Range:
    """Find Fortran word in line

    Parameters
    ----------
    line : str
        Text line
    word : str
        word to find in line

    Returns
    -------
    Range
        start and end positions (indices) of the word if not found it returns
        -1, len(word) -1
    """
    i = -1
    for poss_name in FRegex.WORD.finditer(line):
        if poss_name.group() == word:
            i = poss_name.start()
            break
    # TODO: if i == -1: return None makes more sense
    return Range(i, i + len(word))


def find_paren_match(string: str) -> int:
    """Find matching closing parenthesis **from an already open parenthesis scope**
    by forward search of the string, returns -1 if no match is found

    Parameters
    ----------
    string : str
        Input string

    Returns
    -------
    int
        The index of the matching ``)`` character in the string

    Examples
    --------
    >>> find_paren_match("a, b)")
    4

    Multiple parenthesis that are closed

    >>> find_paren_match("a, (b, c), d)")
    12

    If the outermost parenthesis is not closed function returns -1

    >>> find_paren_match("a, (b, (c, d)")
    -1
    """
    paren_count = 1
    ind = -1
    for (i, char) in enumerate(string):
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
        strip quotes i.e. string literals from ``curr_line`` and ``pre_lines``.
        Need this disable when hovering over string literals, by default True

    Returns
    -------
    str
        part of the line including any relevant line continuations before ``col``
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
    """Resolve paths (absolute and relative) and glob patterns

    Parameters
    ----------
    glob_path : str
        Path containing the glob pattern follows
        ``fnmatch`` glob pattern, can include relative paths, etc.
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
    # Resolve absolute paths i.e. not in our root_path
    if os.path.isabs(glob_path) or not root_path:
        p = Path(glob_path).resolve()
        root = p.root
        rel = str(p.relative_to(root))  # contains glob pattern
        return [str(p.resolve()) for p in Path(root).glob(rel)]
    else:
        return [str(p.resolve()) for p in Path(root_path).resolve().glob(glob_path)]


def only_dirs(paths: list[str], err_msg: list = []) -> list[str]:
    """From a list of strings returns only paths that are directories

    Parameters
    ----------
    paths : list[str]
        A list containing the files and directories
    err_msg : list, optional
        A list to append error messages if any, else use log channel, by default []

    Returns
    -------
    list[str]
        A list containing only valid directories
    """
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


def map_keywords(keywords: list[str]):
    mapped_keywords = []
    keyword_info = {}
    for keyword in keywords:
        keyword_prefix = keyword.split("(")[0].lower().strip()
        keyword_ind = KEYWORD_ID_DICT.get(keyword_prefix)
        # keyword_ind can be 0 which if 0: evaluates to False
        if keyword_ind is not None:
            mapped_keywords.append(keyword_ind)
            if keyword_prefix in ("intent", "dimension", "pass"):
                keyword_substring = get_paren_substring(keyword)
                if keyword_substring is not None:
                    keyword_info[keyword_prefix] = keyword_substring
    if sort_keywords:
        mapped_keywords.sort()
    return mapped_keywords, keyword_info


def get_keywords(keywords: list, keyword_info: dict = {}):
    keyword_strings = []
    for keyword_id in keywords:
        string_rep = KEYWORD_LIST[keyword_id]
        addl_info = keyword_info.get(string_rep)
        string_rep = string_rep.upper()
        if addl_info is not None:
            string_rep += f"({addl_info})"
        keyword_strings.append(string_rep)
    return keyword_strings


def get_paren_substring(string: str) -> str | None:
    """Get the contents enclosed by the first pair of parenthesis

    Parameters
    ----------
    string : str
        A string

    Returns
    -------
    str | None
        The part of the string enclosed in parenthesis e.g.  or None


    Examples
    --------
    >>> get_paren_substring("some line(a, b, (c, d))")
    "a, b, (c, d)"

    If the line has incomplete parenthesis however, ``None`` is returned
    >>> get_paren_substring("some line(a, b")
    None
    """
    i1 = string.find("(")
    i2 = string.rfind(")")
    if -1 < i1 < i2:
        return string[i1 + 1 : i2]
    else:
        return None


def get_paren_level(line: str) -> tuple[str, list[Range]]:
    """Get sub-string corresponding to a single parenthesis level,
    via backward search up through the line.

    Parameters
    ----------
    line : str
        Document line

    Returns
    -------
    tuple[str, list[Range]]
        Arguments as a string and a list of Ranges for the arguments against ``line``

    Examples
    --------
    >>> get_paren_level("CALL sub1(arg1,arg2")
    ('arg1,arg2', [Range(start=10, end=19)])

    If the range is interrupted by parenthesis, another Range variable is used
    to mark the ``start`` and ``end`` of the argument

    >>> get_paren_level("CALL sub1(arg1(i),arg2")
    ('arg1,arg2', [Range(start=10, end=14), Range(start=17, end=22)])

    """
    if line == "":
        return "", [Range(0, 0)]
    level = 0
    in_string = False
    string_char = ""
    i1 = len(line)
    sections: list[Range] = []
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
                sections.append(Range(i + 1, i1))
                break
        elif (char == ")") or (char == "]"):
            level += 1
            if level == 1:
                sections.append(Range(i + 1, i1))
        elif (char == "'") or (char == '"'):
            in_string = True
            string_char = char
    if level == 0:
        sections.append(Range(i, i1))
    sections.reverse()
    out_string = ""
    for section in sections:
        out_string += line[section.start : section.end]
    return out_string, sections


def get_var_stack(line: str) -> list[str]:
    """Get user-defined type field sequence terminating the given line

    Parameters
    ----------
    line : str
        Document line

    Returns
    -------
    list[str]
        list of objects split by ``%``

    Examples
    --------
    >>> get_var_stack("myvar%foo%bar")
    ["myvar", "foo", "bar"]

    >>> get_var_stack("myarray(i)%foo%bar")
    ["myarray", "foo", "bar"]

    In this case it will operate at the end of the string i.e. ``"this%foo"``

    >>> get_var_stack("CALL self%method(this%foo")
    ["this", "foo"]
    """
    if len(line) == 0:
        return [""]
    final_var, sections = get_paren_level(line)
    if final_var == "":
        return [""]
    # Continuation of variable after paren requires '%' character
    iLast = 0
    for (i, section) in enumerate(sections):
        if not line[section.start : section.end].startswith("%"):
            iLast = i
    final_var = ""
    for section in sections[iLast:]:
        final_var += line[section.start : section.end]

    if final_var is not None:
        final_op_split: list[str] = FRegex.OBJBREAK.split(final_var)
        return final_op_split[-1].split("%")
    else:
        return None
