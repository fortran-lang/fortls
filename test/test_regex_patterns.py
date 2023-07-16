from __future__ import annotations

import pytest

from fortls.regex_patterns import create_src_file_exts_regex


@pytest.mark.parametrize(
    "input_exts, input_files, matches",
    [
        (
            [],
            [
                "test.f",
                "test.F",
                "test.f90",
                "test.F90",
                "test.f03",
                "test.F03",
                "test.f18",
                "test.F18",
                "test.f77",
                "test.F77",
                "test.f95",
                "test.F95",
                "test.for",
                "test.FOR",
                "test.fpp",
                "test.FPP",
            ],
            [True] * 16,
        ),
        ([], ["test.ff", "test.f901", "test.f90.ff"], [False, False, False]),
        ([r"\.inc"], ["test.inc", "testinc", "test.inc2"], [True, False, False]),
        (["inc.*"], ["test.inc", "testinc", "test.inc2"], [True, True, True]),
    ],
)
def test_src_file_exts(
    input_exts: list[str],
    input_files: list[str],
    matches: list[bool],
):
    regex = create_src_file_exts_regex(input_exts)
    results = [bool(regex.search(file)) for file in input_files]
    assert results == matches
