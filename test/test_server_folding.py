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
        {"startLine": 0, "endLine": 42},
        {"startLine": 10, "endLine": 20},
        {"startLine": 21, "endLine": 22},
        {"startLine": 11, "endLine": 19},
        {"startLine": 13, "endLine": 14},
        {"startLine": 15, "endLine": 16},
        {"startLine": 17, "endLine": 18},
        {"startLine": 30, "endLine": 34},
        {"startLine": 35, "endLine": 38},
        {"startLine": 39, "endLine": 40},
        {"startLine": 2, "endLine": 5},
        {"startLine": 25, "endLine": 27},
        {"startLine": 31, "endLine": 33},
    ]
    validate_folding(results[1], ref)


def test_mline_sub_folding():
    """Test the ranges for several blocks are correct"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_mline_sub_folding.f90"
    string += folding_req(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = [
        {"startLine": 0, "endLine": 3},
        {"startLine": 4, "endLine": 14},
        {"startLine": 0, "endLine": 4},
        {"startLine": 6, "endLine": 9},
        {"startLine": 12, "endLine": 13},
    ]
    validate_folding(results[1], ref)
