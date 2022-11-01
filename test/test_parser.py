from setup_tests import test_dir

from fortls.parse_fortran import FortranFile


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
