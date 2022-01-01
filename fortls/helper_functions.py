from fortls.regex_patterns import (
    DQ_STRING_REGEX,
    FIXED_COMMENT_LINE_MATCH,
    FREE_FORMAT_TEST,
    LINE_LABEL_REGEX,
    LOGICAL_REGEX,
    NAT_VAR_REGEX,
    NUMBER_REGEX,
    SQ_STRING_REGEX,
    WORD_REGEX,
)


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
