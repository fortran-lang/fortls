import pytest
from setup_tests import test_dir

from fortls.parsers.internal.parser import (
    FortranFile,
    FortranFileNotFoundError,
    preprocess_file,
)


def test_line_continuations():
    file_path = test_dir / "parse" / "line_continuations.f90"
    file = FortranFile(str(file_path))
    file.load_from_disk()
    file.parse()


def test_submodule():
    file_path = test_dir / "parse" / "submodule.f90"
    file = FortranFile(str(file_path))
    file.load_from_disk()
    ast = file.parse()
    assert ast.scope_list[0].name == "val"
    assert ast.scope_list[0].ancestor_name == "p1"
    assert ast.scope_list[1].name == ""
    assert ast.scope_list[1].ancestor_name == "p2"


def test_private_visibility_interfaces():
    file_path = test_dir / "vis" / "private.f90"
    file = FortranFile(str(file_path))
    file.load_from_disk()
    file.parse()


def test_end_scopes_semicolon():
    file_path = test_dir / "parse" / "trailing_semicolon.f90"
    file = FortranFile(str(file_path))
    file.load_from_disk()
    ast = file.parse()
    assert not ast.end_errors


def test_load_from_disk_exception():
    file = FortranFile("/path/to/nonexistent/file.f90")
    with pytest.raises(FortranFileNotFoundError):
        file.load_from_disk()


def test_preprocess_missing_includes_exception():
    preprocess_file(["#include 'nonexistent_file.f90'"])


def test_preprocess_eval_if_exception():
    preprocess_file(["#if (1=and=1)", 'print*, "1==1"', "#endif"])
