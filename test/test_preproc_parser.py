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


def test_pp_macro_expansion():
    lines = [
        "# define WRAP(PROCEDURE) PROCEDURE , wrap_/**/PROCEDURE",
        "generic, public :: set => WRAP(abc)",
        "procedure :: WRAP(abc)",
    ]
    ref = [
        "# define WRAP(PROCEDURE) PROCEDURE , wrap_/**/PROCEDURE",
        "generic, public :: set => abc , wrap_/**/abc",
        "procedure :: abc , wrap_/**/abc",
    ]
    output, _, _, _ = preprocess_file(lines)
    assert output == ref


def test_pp_zero_argument_function_macro():
    """A function-like macro with no arguments expands without crashing.

    Regression test for #486. `"".split(",")` is `[""]`, not `[]`, so a macro
    such as `ok()` was treated as having one unnamed argument. The argument
    substitution then ran `\b()\b`, which matches at every word boundary, and
    injected group references throughout the replacement text -- `ie/=0` became
    `...\10`, and `re` raised "invalid group reference 10".
    """
    lines = [
        "#define ok() if(ie/=0) then; return; end if;",
        "subroutine b",
        "integer :: ie",
        "ie = 1",
        "ok()",
        "end subroutine",
    ]
    ref = [
        "#define ok() if(ie/=0) then; return; end if;",
        "subroutine b",
        "integer :: ie",
        "ie = 1",
        "if(ie/=0) then; return; end if;",
        "end subroutine",
    ]
    output, _, _, _ = preprocess_file(lines)
    assert output == ref


def test_pp_function_macro_arities():
    """Zero, one and two argument macros all expand correctly."""
    lines = [
        "#define NOARG() 42",
        "#define SQUARE(x) ((x)*(x))",
        "#define ADD(a, b) ((a) + (b))",
        "i = NOARG()",
        "j = SQUARE(3)",
        "k = ADD(1, 2)",
    ]
    output, _, _, _ = preprocess_file(lines)
    # The leading space in "( 2)" is pre-existing behaviour: argument capture
    # does not strip whitespace. Asserted as-is so this test stays about arity.
    assert output[3:] == ["i = 42", "j = ((3)*(3))", "k = ((1) + ( 2))"]
