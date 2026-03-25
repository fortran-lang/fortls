from pathlib import Path

from setup_tests import path_to_uri, run_request, test_dir, write_rpc_request


def validate_ranges(result_ranges, ref_ranges):
    actual_ranges = [
        (r["startLine"], r["endLine"], r.get("kind")) for r in result_ranges
    ]
    actual_ranges.sort()
    assert actual_ranges == ref_ranges


def folding_range_request(uri: Path):
    return write_rpc_request(
        1,
        "textDocument/foldingRange",
        {
            "textDocument": {"uri": str(uri)},
        },
    )


def test_folding_range_fixed_form():
    """Test several patterns of folding range in fixed form"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "folding_range" / "test_folding_range.f"
    string += folding_range_request(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_ranges = [
        (1, 23, None),
        (2, 22, None),
        (3, 7, None),
        (5, 6, None),
        (10, 11, None),
        (12, 19, None),
        (16, 18, "comment"),
        (20, 21, None),
    ]
    validate_ranges(results[1], ref_ranges)


def test_folding_range_free_form():
    """Test several patterns of folding range in free form"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "folding_range" / "test_folding_range.f90"
    string += folding_range_request(file_path)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_ranges = [
        (0, 29, None),
        (2, 14, None),
        (5, 9, None),
        (10, 13, None),
        (17, 19, "comment"),
        (20, 27, None),
        (21, 23, None),
        (24, 25, None),
        (26, 27, None),
        (30, 35, None),
        (32, 34, None),
    ]
    validate_ranges(results[1], ref_ranges)
