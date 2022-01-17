from __future__ import annotations

import os
from setup_tests import (
    run_request,
    # path_to_uri,
    # write_rpc_notification,
    write_rpc_request,
    test_dir,
)


def test_hover():
    def hover_req(file_path: str, ln: int, col: int) -> str:
        return write_rpc_request(
            1,
            "textDocument/hover",
            {
                "textDocument": {"uri": file_path},
                "position": {"line": ln, "character": col},
            },
        )

    def check_return(result_array, checks):
        assert len(result_array) == len(checks)
        for (i, check) in enumerate(checks):
            assert result_array[i]["contents"][0]["value"] == check

    root_dir = os.path.join(test_dir, "pp")
    string = write_rpc_request(1, "initialize", {"rootPath": root_dir})
    file_path = os.path.join(test_dir, "pp", "preproc.F90")
    string += hover_req(file_path, 5, 8)  # user defined type
    string += hover_req(file_path, 7, 30)  # variable
    string += hover_req(file_path, 7, 40)  # multi-lin variable
    string += hover_req(file_path, 8, 7)  # function with if conditional
    string += hover_req(file_path, 9, 7)  # multiline function with if conditional
    file_path = os.path.join(test_dir, "pp", "preproc_keywords.F90")
    string += hover_req(file_path, 6, 2)  # ignores PP across Fortran line continuations
    errcode, results = run_request(string, f" --config={root_dir}/.pp_conf.json")
    assert errcode == 0

    # Reference solution
    ref_results = (
        "#define PCType character*(80)",
        "#define PETSC_ERR_INT_OVERFLOW 84",
        "#define varVar 55",
        "#define ewrite if (priority <= 3) write((priority), format)",
        "#define ewrite2 if (priority <= 3) write((priority), format)",
        "REAL, CONTIGUOUS, POINTER, DIMENSION(:)",
    )
    assert len(ref_results) == len(results) - 1
    check_return(results[1:], ref_results)
