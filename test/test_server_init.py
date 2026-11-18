import os

import pytest
from setup_tests import Path, run_request, write_rpc_request


@pytest.fixture()
def setup_tmp_file(tmp_path):
    # In its own directory: the workspace root is the file's parent
    levels = 2000
    filename = tmp_path / "nested_if.f90"
    filename.write_text(
        "program nested_if\n"
        + str("if (.true.) then\n" * levels)
        + str("end if\n" * levels)
        + "end program nested_if"
    )
    return str(filename)


def test_deeply_nested_blocks(setup_tmp_file):
    # Workspace files are summarized without their executable blocks, so the
    # nesting depth no longer exceeds the recursion limit when they are sent
    # back from the workers
    root = Path(setup_tmp_file).parent
    request_string = write_rpc_request(1, "initialize", {"rootPath": str(root)})
    request_string += write_rpc_request(2, "workspace/symbol", {"query": "nested"})
    # The file is parsed in full for requests on it, blocks included
    request_string += hover_request(setup_tmp_file, 0, 10)
    request_string += hover_request(setup_tmp_file, 1, 6)
    errcode, results = run_request(request_string)
    assert errcode == 0
    assert results[1][0]["name"] == "nested_if"
    assert "PROGRAM nested_if" in results[2]["contents"]["value"]
    assert "LOGICAL" in results[3]["contents"]["value"]


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


def hover_request(file_path: str, line: int, character: int) -> str:
    return write_rpc_request(
        1,
        "textDocument/hover",
        {
            "textDocument": {"uri": str(file_path)},
            "position": {"line": line, "character": character},
        },
    )
