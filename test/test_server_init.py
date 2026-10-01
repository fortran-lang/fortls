import os
import sys
import tempfile

import pytest
from setup_tests import Path, run_request, write_rpc_request

from fortls.constants import Severity


@pytest.fixture()
def setup_tmp_file():
    levels = 2000
    fd, filename = tempfile.mkstemp(suffix=".f90")
    try:
        with os.fdopen(fd, "w") as tmp:
            tmp.write(
                "program nested_if\n"
                + str("if (.true.) then\n" * levels)
                + str("end if\n" * levels)
                + "end program nested_if"
            )
        yield filename
    finally:
        os.remove(filename)


# The error comes from pickling the deeply nested AST in a worker process.
# From Python 3.14 the C recursion check uses the stack size, pickle succeeds
# and there is no error to report.
@pytest.mark.skipif(
    sys.version_info >= (3, 14), reason="Python 3.14+ pickles the nested AST"
)
def test_recursion_error_handling(setup_tmp_file):
    root = Path(setup_tmp_file).parent
    request_string = write_rpc_request(1, "initialize", {"rootPath": str(root)})
    errcode, results = run_request(request_string)
    assert errcode == 0
    assert results[0]["type"] == Severity.error


# Before Python 3.13 "**" matches only directories, "*" also matches symlinks
@pytest.mark.parametrize("glob", ["./**", "./*"])
def test_dangling_symlink_in_source_dirs(tmp_path, glob):
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "bar.f90").write_text("module bar_mod\nend module bar_mod\n")
    try:
        os.symlink(tmp_path / "nonexistent", tmp_path / "dead_link")
        os.symlink(tmp_path / "loop_link", tmp_path / "loop_link")
    except OSError:
        pytest.skip("Creating symlinks is not supported on this system")
    request_string = write_rpc_request(1, "initialize", {"rootPath": str(tmp_path)})
    request_string += write_rpc_request(2, "workspace/symbol", {"query": "bar_mod"})
    errcode, results = run_request(request_string, [f"--source_dirs {glob}"])
    assert errcode == 0
    assert results[1][0]["name"] == "bar_mod"
