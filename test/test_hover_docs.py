"""
Focused tests for hover documentation markdown handling to increase patch coverage.
"""

import os
import shutil
import tempfile

import pytest

from fortls.parsers.internal.parser import FortranFile


def _parse_fortran(code: str, temp_dir: str):
    """Helper to parse Fortran code and return the AST"""
    source_path = os.path.join(temp_dir, "test.f90")
    with open(source_path, "w") as f:
        f.write(code)

    parser = FortranFile(source_path)
    err, _ = parser.load_from_disk()
    assert err is None

    ast = parser.parse()
    ast.resolve_links({}, 0)
    return ast


def _get_object_from_program(ast, obj_name: str):
    """Helper to get a subroutine or function by name from AST"""
    for name, obj in ast.global_dict.items():
        if hasattr(obj, "get_children"):
            for child in obj.get_children():
                if hasattr(child, "name") and child.name.lower() == obj_name.lower():
                    return child
    return None


@pytest.fixture
def temp_dir():
    """Create a temporary directory for test files"""
    temp_dir = tempfile.mkdtemp()
    yield temp_dir
    shutil.rmtree(temp_dir)


def test_empty_docstring_case(temp_dir):
    """TEST 1: Empty docstring case - covers get_docs_full when doc_str is None"""
    code = """
program test_prog
contains
subroutine empty_doc(a, b)
    integer, intent(in) :: a
    real, intent(in) :: b
end subroutine empty_doc
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "empty_doc")
    assert sub is not None

    hover, docs = sub.get_hover(long=True)
    assert "SUBROUTINE" in hover
    # Empty docstring should result in empty docs
    assert docs == ""


def test_markdown_list_with_blank_line(temp_dir):
    """TEST 2: Markdown list with blank line - covers markdown formatting with blank lines"""
    code = """
program test_prog
contains
!> @param a Description of a
!!
!! * Item 1
!!
!! * Item 2
subroutine blank_line_test(a)
    integer, intent(in) :: a !! Description of a
end subroutine blank_line_test
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "blank_line_test")
    assert sub is not None

    md = sub.get_hover_md(long=True)
    # Should handle blank lines in markdown without issues
    assert "**Parameters:**" in md
    assert "`a`" in md
    # No wrong indentation
    assert "\n  -" not in md


def test_multiple_params_one_missing_doc(temp_dir):
    """TEST 3: Multiple params (one missing doc) - covers mixed arg documentation handling"""
    code = """
program test_prog
contains
subroutine mixed_docs(a, b, c, d)
    integer, intent(in) :: a !! First has doc
    real, intent(in) :: b
    character(len=*) :: c !! Third has doc
    logical :: d
end subroutine mixed_docs
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "mixed_docs")
    assert sub is not None

    hover, docs = sub.get_hover(long=True)
    # Parameters section appears when any arg has docs
    assert "**Parameters:**" in docs
    # Only documented args appear in params
    assert "`a`" in docs
    assert "`c`" in docs


def test_trailing_spaces_handling(temp_dir):
    """TEST 4: Trailing spaces handling - covers docstring parsing with whitespace issues"""
    code = """
program test_prog
contains
!> Description with trailing spaces
!! Another line with spaces
subroutine trailing_spaces(a)
    integer, intent(in) :: a !! Arg a
end subroutine trailing_spaces
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "trailing_spaces")
    assert sub is not None

    # Should handle trailing spaces without crashing
    hover, docs = sub.get_hover(long=True)
    assert hover is not None
    md = sub.get_hover_md(long=True)
    assert md is not None


def test_multiple_markdown_blocks(temp_dir):
    """TEST 5: Multiple markdown blocks - covers branch coverage for multiple doc blocks"""
    code = """
program test_prog
contains
!> First block
!! Second block
!> Third block (after !!)
subroutine multi_block(a, b)
    integer, intent(in) :: a !! Arg a doc
    real, intent(in) :: b !! Arg b doc
end subroutine multi_block
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "multi_block")
    assert sub is not None

    hover, docs = sub.get_hover(long=True)
    # Should combine multiple blocks
    assert "**Parameters:**" in docs
    assert "`a`" in docs
    assert "`b`" in docs


def test_empty_markdown_line_before_list(temp_dir):
    """TEST 6: Empty markdown line before list - covers edge case in doc parsing"""
    code = """
program test_prog
contains
!> Description here

!! * List item 1
!! * List item 2
subroutine before_list(a, b)
    integer, intent(in) :: a !! First arg
    character(len=*) :: b !! Second arg
end subroutine before_list
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "before_list")
    assert sub is not None

    md = sub.get_hover_md(long=True)
    assert "**Parameters:**" in md
    assert "`a`" in md
    assert "`b`" in md
    # No wrong indentation in output
    assert "\n  -" not in md


def test_hover_complex_spacing_case(temp_dir):
    """Test hover with complex spacing in docstring"""
    code = """
program test_prog
contains
!> Doc with    multiple   spaces
!! And   more   complex   spacing
!! * List   with   spaced   items
subroutine complex_spacing(a)
    integer, intent(in) :: a !! Arg with  spacing
end subroutine complex_spacing
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "complex_spacing")
    assert sub is not None

    md = sub.get_hover_md(long=True)
    assert "**Parameters:**" in md
    assert "`a`" in md


def test_hover_only_blank_lines_between_docs(temp_dir):
    """Test hover with only blank lines between docstring parts"""
    code = """
program test_prog
contains
!> First doc section


!! Second doc section


!> Third doc section
subroutine blank_between(a, b)
    integer, intent(in) :: a !! First arg
    real, intent(in) :: b !! Second arg
end subroutine blank_between
end program test_prog
"""
    ast = _parse_fortran(code, temp_dir)
    sub = _get_object_from_program(ast, "blank_between")
    assert sub is not None

    md = sub.get_hover_md(long=True)
    assert "**Parameters:**" in md
    assert "`a`" in md
    assert "`b`" in md
    # No wrong indentation
    assert "\n  -" not in md
