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


def test_folding_if():
    """Test the ranges for several blocks are correct"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_folding_if.f90"
    string += folding_req(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = [
        {"startLine": 0, "endLine": 43},
        {"startLine": 11, "endLine": 21},
        {"startLine": 22, "endLine": 23},
        {"startLine": 12, "endLine": 20},
        {"startLine": 14, "endLine": 15},
        {"startLine": 16, "endLine": 17},
        {"startLine": 18, "endLine": 19},
        {"startLine": 31, "endLine": 35},
        {"startLine": 36, "endLine": 39},
        {"startLine": 40, "endLine": 41},
        {"startLine": 2, "endLine": 6},
        {"startLine": 26, "endLine": 28},
        {"startLine": 32, "endLine": 34},
    ]
    validate_folding(results[1], ref)


def test_folding_select_case():
    """Test the ranges for several blocks are correct"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_folding_select_case.f90"
    string += folding_req(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = [
        {"startLine": 0, "endLine": 14},
        {"startLine": 4, "endLine": 4},
        {"startLine": 5, "endLine": 6},
        {"startLine": 7, "endLine": 8},
        {"startLine": 9, "endLine": 10},
        {"startLine": 11, "endLine": 12},
    ]
    validate_folding(results[1], ref)


def test_folding_subroutine():
    """Test the ranges for several blocks are correct"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_folding_subroutine.f90"
    string += folding_req(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    print(results[1])
    ref = [
        {"startLine": 1, "endLine": 4},
        {"startLine": 5, "endLine": 17},
        {"startLine": 1, "endLine": 5},
        {"startLine": 8, "endLine": 11},
        {"startLine": 15, "endLine": 16},
    ]
    validate_folding(results[1], ref)
