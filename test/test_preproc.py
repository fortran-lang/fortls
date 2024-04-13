from __future__ import annotations

from setup_tests import run_request, test_dir, write_rpc_request


def test_hover():
    def hover_req(file_path: str, ln: int, col: int) -> str:
        return write_rpc_request(
            1,
            "textDocument/hover",
            {
                "textDocument": {"uri": str(file_path)},
                "position": {"line": ln, "character": col},
            },
        )

    def check_return(result_array, checks):
        assert len(result_array) == len(checks)
        for i, check in enumerate(checks):
            assert result_array[i]["contents"]["value"] == check

    root_dir = test_dir / "pp"
    string = write_rpc_request(1, "initialize", {"rootPath": str(root_dir)})
    file_path = root_dir / "preproc.F90"
    string += hover_req(file_path, 5, 8)  # user defined type
    string += hover_req(file_path, 7, 30)  # variable
    string += hover_req(file_path, 7, 40)  # multi-lin variable
    string += hover_req(file_path, 8, 7)  # function with if conditional
    string += hover_req(file_path, 9, 7)  # multiline function with if conditional
    string += hover_req(file_path, 10, 15)  # defined without ()
    file_path = root_dir / "preproc_keywords.F90"
    string += hover_req(file_path, 6, 2)  # ignores PP across Fortran line continuations
    file_path = root_dir / "preproc_else.F90"
    string += hover_req(file_path, 8, 12)
    string += hover_req(file_path, 18, 12)
    file_path = root_dir / "preproc_elif.F90"
    string += hover_req(file_path, 22, 15)
    string += hover_req(file_path, 24, 10)
    file_path = root_dir / "preproc_elif_elif_skip.F90"
    string += hover_req(file_path, 30, 23)
    file_path = root_dir / "preproc_if_elif_else.F90"
    string += hover_req(file_path, 30, 23)
    file_path = root_dir / "preproc_if_elif_skip.F90"
    string += hover_req(file_path, 30, 23)
    config = str(root_dir / ".pp_conf.json")
    errcode, results = run_request(string, ["--config", config])
    assert errcode == 0

    # Reference solution
    ref_results = (
        "```fortran90\n#define PCType character*(80)\n```",
        "```fortran90\n#define PETSC_ERR_INT_OVERFLOW 84\n```",
        "```fortran90\n#define varVar 55\n```",
        (
            "```fortran90\n#define ewrite(priority, format)"
            " if (priority <= 3) write((priority), format)\n```"
        ),
        (
            "```fortran90\n#define ewrite2(priority, format)"
            " if (priority <= 3) write((priority), format)\n```"
        ),
        "```fortran90\n#define SUCCESS .true.\n```",
        "```fortran90\nREAL, CONTIGUOUS, POINTER, DIMENSION(:) :: var1\n```",
        "```fortran90\nINTEGER :: var0\n```",
        "```fortran90\nREAL :: var1\n```",
        "```fortran90\nINTEGER :: var2\n```",
        "```fortran90\nINTEGER, INTENT(INOUT) :: var\n```",
        "```fortran90\nINTEGER, PARAMETER :: res = 0+1+0+0\n```",
        "```fortran90\nINTEGER, PARAMETER :: res = 0+0+0+1\n```",
        "```fortran90\nINTEGER, PARAMETER :: res = 1+0+0+0\n```",
    )
    assert len(ref_results) == len(results) - 1
    check_return(results[1:], ref_results)
