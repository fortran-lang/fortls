from setup_tests import Path, run_request, test_dir, write_rpc_request


def folding_req(file_path: Path) -> str:
    return write_rpc_request(
        1,
        "textDocument/foldingRange",
        {"textDocument": {"uri": str(file_path)}},
    )


def validate_folding(results: list, ref: list):
    assert len(results) == len(ref)
    for i in range(0, len(results)):
        assert results[i] == ref[i]


def test_if_folding():
    """Test the ranges for several blocks are correct"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_if_folding.f90"
    string += folding_req(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = [
        {"startLine": 2, "endLine": 5},
        {"startLine": 13, "endLine": 14},
        {"startLine": 11, "endLine": 15},
        {"startLine": 10, "endLine": 16},
        {"startLine": 20, "endLine": 22},
        {"startLine": 19, "endLine": 23},
        {"startLine": 24, "endLine": 27},
        {"startLine": 28, "endLine": 29},
        {"startLine": 0, "endLine": 31},
    ]
    validate_folding(results[1], ref)
