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
    string += hover_req(file_path, 7, 40)  # multi-line variable
    string += hover_req(file_path, 8, 7)  # function with if conditional
    string += hover_req(file_path, 9, 7)  # multiline function with if conditional
    string += hover_req(file_path, 10, 15)  # defined without ()
    file_path = root_dir / "preproc_keywords.F90"
    string += hover_req(file_path, 6, 2)  # ignores PP across Fortran line continuations
    file_path = root_dir / "preproc_ifdef.F90"
    string += hover_req(file_path, 21, 15)  # defined inside `if` block
    string += hover_req(file_path, 22, 15)  # defined inside `elif` block
    string += hover_req(file_path, 23, 15)  # defined inside `ifndef` block
    file_path = root_dir / "preproc_use.F90"
    string += hover_req(file_path, 7, 15)  # Correct subroutine definition from mpi_f08
    string += hover_req(file_path, 7, 30)  # Correct type of MPI_COMM_WORLD
    file_path = root_dir / "preproc_use_include.F90"
    string += hover_req(file_path, 3, 15)  # Correct subroutine definition from mpi_f08
    string += hover_req(file_path, 3, 30)  # Correct type of MPI_COMM_WORLD
    string += hover_req(
        file_path, 4, 15
    )  # Found definition of `omp_get_num_threads` function from omp_lib module
    config = str(root_dir / ".pp_conf.json")
    errcode, results = run_request(string, ["--config", config])
    assert errcode == 0

    # Reference solution
    ref_results = (
        "```fortran90\n#define PCType character*(80)\n```",
        "```fortran90\n#define PETSC_ERR_INT_OVERFLOW 84\n```",
        "```fortran90\n#define varVar 55\n```",
        (
            "```fortran90\n#define ewrite if (priority <= 3) write((priority),"
            " format)\n```"
        ),
        (
            "```fortran90\n#define ewrite2 if (priority <= 3) write((priority),"
            " format)\n```"
        ),
        "```fortran90\n#define SUCCESS .true.\n```",
        "```fortran90\nREAL, CONTIGUOUS, POINTER, DIMENSION(:) :: var1\n```",
        "```fortran90\n#define CUSTOM_MACRO 0\n```",
        "```fortran90\n#define SECOND_CUSTOM_MACRO 1\n```",
        "```fortran90\n#define THIRD_CUSTOM_MACRO 0\n```",
        (
            "```fortran90"
            "\nSUBROUTINE MPI_Comm_size(comm, size, ierror=ierror)"
            "\n TYPE(MPI_Comm), INTENT(IN) :: comm"
            "\n INTEGER(C_INT), INTENT(OUT) :: size"
            "\n INTEGER(C_INT), OPTIONAL, INTENT(OUT) :: ierror"
            "\n"
            "```"
            "\n-----\n"
            "\n**Parameters:**     "
            "\n`comm` Communicator.   "
            "\n`size` Size of communicator.   "
            "\n`ierror` Error status."
        ),
        "```fortran90\nTYPE(MPI_Comm) :: MPI_COMM_WORLD\n```",
        (
            "```fortran90"
            "\nSUBROUTINE MPI_Comm_size(comm, size, ierror=ierror)"
            "\n TYPE(MPI_Comm), INTENT(IN) :: comm"
            "\n INTEGER(C_INT), INTENT(OUT) :: size"
            "\n INTEGER(C_INT), OPTIONAL, INTENT(OUT) :: ierror"
            "\n"
            "```"
            "\n-----\n"
            "\n**Parameters:**     "
            "\n`comm` Communicator.   "
            "\n`size` Size of communicator.   "
            "\n`ierror` Error status."
        ),
        "```fortran90\nTYPE(MPI_Comm) :: MPI_COMM_WORLD\n```",
        (
            "```fortran90"
            "\nINTEGER FUNCTION omp_get_num_threads() RESULT(omp_get_num_threads)"
            "\n```"
        ),
    )
    assert len(ref_results) == len(results) - 1
    check_return(results[1:], ref_results)
