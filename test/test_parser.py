import pytest
from setup_tests import test_dir

from fortls.parsers.internal.parser import FortranFile


def test_line_continuations():
    file_path = test_dir / "parse" / "line_continuations.f90"
    file = FortranFile(str(file_path))
    err_str, _ = file.load_from_disk()
    assert err_str is None
    try:
        file.parse()
        assert True
    except Exception as e:
        print(e)
        assert False


def test_submodule():
    file_path = test_dir / "parse" / "submodule.f90"
    file = FortranFile(str(file_path))
    err_str, _ = file.load_from_disk()
    assert err_str is None
    try:
        ast = file.parse()
        assert True
        assert ast.scope_list[0].name == "val"
        assert ast.scope_list[0].ancestor_name == "p1"
        assert ast.scope_list[1].name == ""
        assert ast.scope_list[1].ancestor_name == "p2"
    except Exception as e:
        print(e)
        assert False


def test_private_visibility_interfaces():
    file_path = test_dir / "vis" / "private.f90"
    file = FortranFile(str(file_path))
    err_str, _ = file.load_from_disk()
    file.parse()
    assert err_str is None


def test_end_scopes_semicolon():
    file_path = test_dir / "parse" / "trailing_semicolon.f90"
    file = FortranFile(str(file_path))
    err_str, _ = file.load_from_disk()
    ast = file.parse()
    assert err_str is None
    assert not ast.end_errors


def test_weird_parser_bug():
    file_path = test_dir / "parse" / "mixed" / "preproc_and_normal_syntax.F90"
    file = FortranFile(str(file_path))
    err_str, _ = file.load_from_disk()
    ast = file.parse()
    assert err_str is None
    assert not ast.end_errors


@pytest.mark.parametrize(
    "ln_no, pp_defs, reference",
    [
        (6, {}, 6),
        (7, {}, 6),
        (8, {}, 6),
        (11, {"TEST": True}, 60),  # not entirely correct ref vals
        (23, {"MULT": True}, 90),  # not entirely correct ref vals
        (32, {"TEST": True, "MULT": True}, 130),  # not entirely correct ref vals
        (39, {"TEST": True, "MULT": True}, 2400),  # not entirely correct ref vals
    ],
)
def test_get_code_line_multilines(ln_no: int, pp_defs: dict, reference: int):
    """Tests how the get_code_line performs with multi-line and preprocessor

    Not all the results are correct, since get_code_line is not aware of the
    preprocessor skips. Instead what it does is it evaluates all the line
    continuations and appends them in post.
    """

    def calc_result(res: tuple):
        pre, cur, post = res
        res = "".join(pre + [cur] + post).replace(" ", "")
        assert "result" in res, "Fortran variable `result` not found in results"
        loc = {}
        exec(res, None, loc)
        return loc["result"]

    file_path = test_dir / "parse" / "mixed" / "multilines.F90"
    file = FortranFile(str(file_path))
    file.load_from_disk()
    file.preprocess(pp_defs=pp_defs)
    pp = bool(pp_defs)
    res = file.get_code_line(line_no=ln_no, pp_content=pp)
    result = calc_result(res)
    assert result == reference
