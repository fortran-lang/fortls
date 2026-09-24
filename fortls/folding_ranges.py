import re
from typing import Literal, TypedDict
from fortls.constants import (
    FUNCTION_TYPE_ID,
    IF_TYPE_ID,
    MODULE_TYPE_ID,
    SELECT_TYPE_ID,
    SUBMODULE_TYPE_ID,
    SUBROUTINE_TYPE_ID,
    WHERE_TYPE_ID,
    FRegex,
)
from fortls.parsers.internal.parser import FortranFile
from fortls.parsers.internal.scope import Scope


class FoldingRange(TypedDict, total=False):
    startLine: int
    endLine: int
    kind: Literal["comment", "imports", "region"]


def get_folding_ranges_by_block_comment(file_obj: FortranFile, min_block_size: int):
    """
    Get the folding ranges in the given file based on the block comment

    Returns
    -------
    list[FoldingRange]
        List of folding ranges as defined in LSP including `startLine` and `endLine`,
        with `kind` of `comment`.
    """
    tracker = BlockCommentTracker(min_block_size)
    comment_regex = file_obj.get_comment_regexs()[0]

    for index, line in enumerate(file_obj.contents_split):
        if comment_regex.match(line):
            tracker.process(index)

    tracker.finalize(file_obj.nLines)

    return [
        {
            "startLine": r[0],
            "endLine": r[1],
            "kind": "comment",
        }
        for r in tracker.result
    ]


class BlockCommentTracker:
    """Track the comment lines and generates ranges for block comments in `result`"""

    def __init__(self, min_block_size: int):
        self.min_block_size = min_block_size
        """Minimum number of consecutive comment lines to make a block"""
        self.result: list[tuple[int, int]] = []
        """List or ranges for comment blocks"""

        self._start_index = -99
        self._last_index = -99

    def process(self, index: int):
        """Process comment lines one by one"""
        assert index > self._last_index
        if index - self._last_index == 1:
            # Consecutive, just keep tracking
            self._last_index = index
        else:
            # Sequence just broken, add to the result if the size is large enough
            if self._last_index - self._start_index + 1 >= self.min_block_size:
                self.result.append((self._start_index, self._last_index))
            # Start a new block
            self._start_index = index
            self._last_index = index

    def finalize(self, max_line_index: int):
        """Process all unfinished blocks"""
        self.process(max_line_index + 99)


def get_folding_ranges_by_indent(file_obj: FortranFile) -> list[FoldingRange]:
    """
    Get the folding ranges in the given file based on the indent

    Returns
    -------
    list[FoldingRange]
        List of folding ranges as defined in LSP including `startLine` and `endLine`.
    """

    tracker = IndentTracker()

    index = 0
    while index < file_obj.nLines:
        # Extract the code from the line (and following concatenated lines)
        [_, curr_line, post_lines] = file_obj.get_code_line(
            index, backward=False, strip_comment=True
        )
        if file_obj.fixed:
            curr_line = curr_line[6:]
        code_line = curr_line + "".join(post_lines)

        # Process the indent if the line is not empty
        indent = IndentTracker.count_indent(code_line)
        if indent < len(code_line):
            tracker.process(index, indent)

        # Increment the line index skipping the concatenated lines
        index += 1 + len(post_lines)

    tracker.finalize(file_obj.nLines)

    return [
        {
            "startLine": r[0],
            "endLine": r[1],
        }
        for r in tracker.result
    ]


class IndentTracker:
    """Track the indent changes and generates ranges in `result`."""

    INDENT_PATTERN = re.compile(r"[ ]*")

    @classmethod
    def count_indent(self, s: str) -> int:
        return len(self.INDENT_PATTERN.match(s).group(0))

    def __init__(self):
        self.result: list[tuple[int, int]] = []
        """List or ranges based on indent changes"""

        self._indent_stack: list[tuple[int, int]] = []  # start index and indent
        self._last_indent = -1
        self._last_index = -1

    def process(self, index: int, indent: int):
        """Process indented lines one by one"""
        assert index > self._last_index

        if indent > self._last_indent:
            # At indent in, push the start index and indent to the stack
            self._indent_stack.append((self._last_index, indent))
        elif indent < self._last_indent:
            # At indent out, create ranges for the preceding deeper blocks
            while self._indent_stack and self._indent_stack[-1][1] > indent:
                start_index = self._indent_stack.pop()[0]
                # Add to the result only if the range is valid
                if start_index >= 0 and index - start_index > 1:
                    self.result.append((start_index, index - 1))

        self._last_indent = indent
        self._last_index = index

    def finalize(self, num_lines: int):
        """Process all unfinished blocks"""
        self.process(num_lines, -1)


RANGE_CLOSE_PATTENS: dict[int, re.Pattern] = {
    IF_TYPE_ID: re.compile(r"ELSE(\s*IF)?\b", re.I),
    SELECT_TYPE_ID: re.compile(r"(CASE|TYPE|CLASS)\b", re.I),
    WHERE_TYPE_ID: re.compile(r"ELSEWHERE\b", re.I),
    MODULE_TYPE_ID: FRegex.CONTAINS,
    SUBMODULE_TYPE_ID: FRegex.CONTAINS,
    FUNCTION_TYPE_ID: FRegex.CONTAINS,
    SUBROUTINE_TYPE_ID: FRegex.CONTAINS,
}


def get_folding_ranges_by_syntax(file_obj: FortranFile) -> list[FoldingRange]:
    """
    Get the folding ranges in the given file based on the syntax

    Returns
    -------
    list[FoldingRange]
        List of folding ranges as defined in LSP including `startLine` and `endLine`.
    """

    range_by_sline: dict[int, int] = dict()
    scope_by_sline: dict[int, Scope] = dict()

    scopes: list[Scope] = file_obj.ast.scope_list
    for scope in scopes:
        # We assume different scopes should have different slines, but just in case...
        conflict_range = range_by_sline.get(scope.sline)
        if conflict_range is not None and scope.eline - 1 < conflict_range:
            continue
        # Create default ranges based on each scope,
        # which may be split later in this process
        range_by_sline[scope.sline] = scope.eline - 1
        scope_by_sline[scope.sline] = scope

    # Split the scope if necessary
    for scope in scopes:
        range_close_pattern = RANGE_CLOSE_PATTENS.get(scope.get_type())
        if range_close_pattern is None:
            continue

        range_sline = None if scope.get_type() in [SELECT_TYPE_ID] else scope.sline

        line_no = scope.sline + 1
        while line_no < scope.eline:
            # Skip child scopes
            child_scope = scope_by_sline.get(line_no)
            if child_scope:
                line_no = child_scope.eline + 1
                continue

            # Extract the code from the line (and following concatenated lines)
            [_, curr_line, post_lines] = file_obj.get_code_line(
                line_no - 1, backward=False, strip_comment=True
            )
            if file_obj.fixed:
                curr_line = curr_line[6:]
            code_line = (curr_line + "".join(post_lines)).strip()

            # If the code matches to the pattern, split the range
            if range_close_pattern.match(code_line):
                if range_sline is not None:
                    range_by_sline[range_sline] = line_no - 1
                range_sline = line_no

            line_no += 1 + len(post_lines)

        if range_sline is not None:
            range_by_sline[range_sline] = line_no - 1

    return [
        {
            "startLine": r[0] - 1,
            "endLine": r[1] - 1,
        }
        for r in range_by_sline.items()
        if r[0] < r[1]
    ]
