import os
import shutil
import tempfile

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


def _get_function_from_program(ast, func_name: str):
    """Helper to get a function by name from AST"""
    for name, obj in ast.global_dict.items():
        if hasattr(obj, "get_children"):
            for child in obj.get_children():
                if hasattr(child, "name") and child.name.lower() == func_name.lower():
                    if hasattr(child, "result_type"):
                        return child
    return None


def test_function_docstring_indentation():
    """Test Function.get_hover() produces correct markdown without extra indentation.

    Regression test for bug where documentation list items had extra leading
    whitespace due to docs being joined with "  \\n" instead of "\\n".
    Before fix: "\\n  - item" (wrong - 2 extra spaces)
    After fix: "\\n- item" (correct - no extra spaces)
    """
    temp_dir = tempfile.mkdtemp()
    try:
        code = """
program test_prog
    implicit none
contains
    !> @brief Test function with markdown list
    !!
    !! This function has a parameter with a markdown list:
    !! * item1 - first item
    !! * item2 - second item
    !!
    !! @param[in] x The input parameter
    function my_func(x) result(y)
        integer, intent(in) :: x
        integer :: y
    end function my_func
end program test_prog
"""
        ast = _parse_fortran(code, temp_dir)
        func = _get_function_from_program(ast, "my_func")
        assert func is not None, "Could not find function 'my_func'"

        # Get hover with long=True to include docs
        hover, docs = func.get_hover(long=True)

        # Verify docs are not incorrectly indented
        # The bug was: "  \n".join(docs) caused "\n  -" instead of "\n-"
        assert "\n  -" not in docs, f"Found wrong indentation in docs: {docs}"
        assert "\n  *" not in docs, f"Found wrong indentation in docs: {docs}"

        # Verify markdown list items are present without wrong indentation
        assert "item1" in docs
        assert "item2" in docs

        # Also verify the markdown version
        md = func.get_hover_md(long=True)
        assert "\n  -" not in md, f"Found wrong indentation in markdown: {md}"
    finally:
        shutil.rmtree(temp_dir)


def test_function_with_result_variable_docs():
    """Test Function.get_hover() with result variable documentation."""
    temp_dir = tempfile.mkdtemp()
    try:
        code = """
module test_mod
contains
    !> @brief Function that returns a value
    !!
    !! @param[in] a First argument
    !! @param[in] b Second argument
    !! * option1 - first option
    !! * option2 - second option
    !!
    !! @return The computed result
    pure integer function add(a, b)
        integer, intent(in) :: a, b
    end function add
end module test_mod
"""
        ast = _parse_fortran(code, temp_dir)
        func = _get_function_from_program(ast, "add")
        assert func is not None, "Could not find function 'add'"

        # Get hover with long=True
        hover, docs = func.get_hover(long=True)

        # Verify no wrong indentation
        assert "\n  -" not in docs, f"Found wrong indentation in docs: {docs}"

        # Verify markdown list items are present
        assert "option1" in docs
        assert "option2" in docs
        assert "**Parameters:**" in docs or "Parameters" in docs
    finally:
        shutil.rmtree(temp_dir)


def test_docstring_indentation():
    root_dir = os.path.dirname(os.path.abspath(__file__))
    source_path = os.path.join(root_dir, "test_source", "docs", "test_param_list.f90")

    parser = FortranFile(source_path)
    err, _ = parser.load_from_disk()
    assert err is None

    ast = parser.parse()
    ast.resolve_links({}, 0)

    # Locate program 'bug'
    prog = ast.global_dict.get("bug")
    assert prog is not None

    # Locate function 'foo'
    foo = None
    for child in prog.get_children():
        if child.name == "foo":
            foo = child
            break
    assert foo is not None

    # Verify docstrings are parsed (parameter docs are stored in subroutine's doc_str)
    assert foo.doc_str is not None

    # Verify bar docstring contains markdown list (stored in foo.doc_str)
    assert "* b" in foo.doc_str
    assert "* a" in foo.doc_str

    # Verify baz docstring is simple (stored in foo.doc_str)
    assert "baz" in foo.doc_str.lower()

    # Get hover markdown and ensure no indentation issues (need long=True for args)
    hover = foo.get_hover_md(long=True)

    # Ensure markdown is correct
    assert "**Parameters:**" in hover

    # Ensure no wrong indentation (the bug was that list items caused wrong indentation)
    assert "\n  -" not in hover
