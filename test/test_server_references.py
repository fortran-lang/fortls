from pathlib import Path

from setup_tests import path_to_uri, run_request, test_dir, write_rpc_request


def validate_refs(result_array, checks):
    def find_in_results(uri, sline):
        for i, result in enumerate(result_array):
            if (result["uri"] == uri) and (result["range"]["start"]["line"] == sline):
                del result_array[i]
                return result
        return None

    assert len(result_array) == len(checks)
    for check in checks:
        result = find_in_results(path_to_uri(check[0]), check[1])
        assert result is not None
        assert result["range"]["start"]["character"] == check[2]
        assert result["range"]["end"]["character"] == check[3]


def ref_req(uri: Path, ln: int, ch: int):
    return write_rpc_request(
        2,
        "textDocument/references",
        {
            "textDocument": {"uri": str(uri)},
            "position": {"line": ln - 1, "character": ch - 1},
        },
    )


def test_references():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += ref_req(file_path, 10, 9)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    free_path = str(test_dir / "subdir" / "test_free.f90")
    validate_refs(
        results[1],
        (
            [str(test_dir / "test_prog.f08"), 2, 21, 27],
            [str(test_dir / "test_prog.f08"), 9, 5, 11],
            [free_path, 8, 8, 14],
            [free_path, 16, 9, 15],
            [free_path, 18, 14, 20],
            [free_path, 36, 6, 12],
            [free_path, 44, 6, 12],
            [free_path, 78, 6, 12],
        ),
    )
