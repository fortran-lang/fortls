import os
import stat
import tempfile

import pytest
from setup_tests import Path, run_request, write_rpc_request

from fortls.constants import Severity


@pytest.fixture()
def setup_unreadable_file():
    """Create a Fortran file that cannot be read (no read permissions)."""
    fd, filename = tempfile.mkstemp(suffix=".f90")
    try:
        with os.fdopen(fd, "w") as tmp:
            tmp.write("program test\nend program\n")
        # Make file unreadable
        os.chmod(filename, 0o000)
        yield filename
    finally:
        # Restore permissions before cleanup
        os.chmod(filename, stat.S_IRUSR | stat.S_IWUSR)
        os.remove(filename)


def test_file_read_error_handling(setup_unreadable_file):
    """Test that workspace_init properly handles and reports file read errors."""
    root = Path(setup_unreadable_file).parent
    request_string = write_rpc_request(1, "initialize", {"rootPath": str(root)})
    errcode, results = run_request(request_string)
    assert errcode == 0
    assert results[0]["type"] == Severity.error
