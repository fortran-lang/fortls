import os

from fortls.parsers.internal.parser import FortranFile


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
