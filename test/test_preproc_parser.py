from __future__ import annotations

from fortls.parsers.internal.parser import preprocess_file


def test_pp_leading_spaces():
    lines = [
        " #define LEADING_SPACES_INDENT 1",
        "  #   define LEADING_SPACES_INDENT2",
        "    #    define FILE_ENCODING ,encoding='UTF-8'",
        "program pp_intentation",
        "  implicit none",
        "  print*, LEADING_SPACES_INDENT",
        "  open(unit=1,file='somefile.txt' FILE_ENCODING)",
        "end program pp_intentation",
    ]
    _, _, _, defs = preprocess_file(lines)
    ref = {
        "LEADING_SPACES_INDENT": "1",
        "LEADING_SPACES_INDENT2": "True",
        "FILE_ENCODING": ",encoding='UTF-8'",
    }
    assert defs == ref
