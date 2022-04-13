# from types import NoneType
from setup_tests import (
    path_to_uri,
    run_request,
    test_dir,
    write_rpc_notification,
    write_rpc_request,
)


def test_init():
    def check_return(result_dict):
        # Expected capabilities
        # {
        #     "completionProvider": {
        #         "resolveProvider": false,
        #         "triggerCharacters": ["%"]
        #     },
        #     "definitionProvider": true,
        #     "documentSymbolProvider": true,
        #     "referencesProvider": True,
        #     "hoverProvider": true,
        #     "textDocumentSync": 2
        # }
        #
        assert "capabilities" in result_dict
        assert result_dict["capabilities"]["textDocumentSync"] == 2
        assert result_dict["capabilities"]["definitionProvider"] is True
        assert result_dict["capabilities"]["documentSymbolProvider"] is True
        assert result_dict["capabilities"]["hoverProvider"] is True
        assert result_dict["capabilities"]["referencesProvider"] is True
        assert (
            result_dict["capabilities"]["completionProvider"]["resolveProvider"]
            is False
        )
        assert (
            result_dict["capabilities"]["completionProvider"]["triggerCharacters"][0]
            == "%"
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[0])


def test_logger():
    """Test the logger"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    errcode, results = run_request(string, ["--debug_log"])
    assert errcode == 0
    assert results[1]["type"] == 3
    assert results[1]["message"] == "fortls debugging enabled"


def test_open():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = str(test_dir / "subdir" / "test_free.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, fortls_args=["--disable_diagnostics"])
    #
    assert errcode == 0
    assert len(results) == 1


def test_change():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_unknown.f90"
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": str(file_path)}}
    )
    string += write_rpc_notification(
        "textDocument/didChange",
        {
            "textDocument": {"uri": str(file_path)},
            "contentChanges": [
                {
                    "text": "module test_unkown\nend module test_unknown\n",
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 0, "character": 0},
                    },
                }
            ],
        },
    )
    string += write_rpc_request(
        2, "textDocument/documentSymbol", {"textDocument": {"uri": str(file_path)}}
    )
    file_path = test_dir / "subdir" / "test_free.f90"
    string += write_rpc_notification(
        "textDocument/didChange",
        {
            "textDocument": {"uri": str(file_path)},
            "contentChanges": [
                {
                    "text": " unicode test",
                    "range": {
                        "start": {"line": 3, "character": 3},
                        "end": {"line": 3, "character": 3},
                    },
                },
                {
                    "text": "",
                    "range": {
                        "start": {"line": 6, "character": 0},
                        "end": {"line": 31, "character": 0},
                    },
                },
                {
                    "text": "",
                    "range": {
                        "start": {"line": 7, "character": 0},
                        "end": {"line": 39, "character": 0},
                    },
                },
            ],
        },
    )
    string += write_rpc_request(
        3, "textDocument/documentSymbol", {"textDocument": {"uri": str(file_path)}}
    )
    errcode, results = run_request(string, fortls_args=["--disable_diagnostics"])
    #
    assert errcode == 0
    assert len(results) == 3
    assert len(results[1]) == 1
    assert len(results[2]) == 5


def test_symbols():
    def check_return(result_array):
        # Expected objects
        objs = (
            ["test_free", 2, 0, 81],
            ["scale_type", 5, 4, 6],
            ["val", 13, 5, 5],
            ["vector", 5, 8, 16],
            ["n", 13, 9, 9],
            ["v", 13, 10, 10],
            ["bound_nopass", 6, 11, 11],
            ["create", 6, 13, 13],
            ["norm", 6, 14, 14],
            ["bound_pass", 6, 15, 15],
            ["scaled_vector", 5, 18, 23],
            ["scale", 13, 19, 19],
            ["set_scale", 6, 21, 21],
            ["norm", 6, 22, 22],
            ["fort_wrap", 11, 26, 29],
            ["vector_create", 12, 35, 41],
            ["vector_norm", 12, 43, 47],
            ["scaled_vector_set", 12, 49, 53],
            ["scaled_vector_norm", 12, 55, 59],
            ["unscaled_norm", 12, 61, 65],
            ["test_sig_Sub", 12, 67, 70],
            ["bound_pass", 12, 72, 80],
        )
        assert len(result_array) == len(objs)
        for i, obj in enumerate(objs):
            assert result_array[i]["name"] == obj[0]
            assert result_array[i]["kind"] == obj[1]
            assert result_array[i]["location"]["range"]["start"]["line"] == obj[2]
            assert result_array[i]["location"]["range"]["end"]["line"] == obj[3]

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    string += write_rpc_request(
        2, "textDocument/documentSymbol", {"textDocument": {"uri": str(file_path)}}
    )
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[1])


def test_workspace_symbols():
    def check_return(result_array):
        # Expected objects
        objs = (
            ["test", 6, 7],
            ["test_abstract", 2, 0],
            ["test_free", 2, 0],
            ["test_gen_type", 5, 1],
            ["test_generic", 2, 0],
            ["test_inherit", 2, 0],
            ["test_int", 2, 0],
            ["test_mod", 2, 0],
            ["test_nonint_mod", 2, 0],
            ["test_preproc_keywords", 2, 0],
            ["test_private", 2, 8],
            ["test_program", 2, 0],
            ["test_rename_sub", 6, 9],
            ["test_select", 2, 0],
            ["test_select_sub", 6, 16],
            ["test_sig_Sub", 6, 67],
            ["test_str1", 13, 5],
            ["test_str2", 13, 5],
            ["test_sub", 6, 8],
            ["test_vis_mod", 2, 0],
        )
        assert len(result_array) == len(objs)
        for i, obj in enumerate(objs):
            assert result_array[i]["name"] == obj[0]
            assert result_array[i]["kind"] == obj[1]
            assert result_array[i]["location"]["range"]["start"]["line"] == obj[2]

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    string += write_rpc_request(2, "workspace/symbol", {"query": "test"})
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[1])


def test_comp():
    def check_return(result_array, checks):
        assert len(result_array) == checks[0]
        if checks[0] > 0:
            assert result_array[0]["label"] == checks[1]
            assert result_array[0]["detail"] == checks[2]
            try:
                assert result_array[0]["insertText"] == checks[3]
            except KeyError:
                pass

    def comp_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/completion",
            {
                "textDocument": {"uri": str(file_path)},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += comp_request(file_path, 12, 6)
    string += comp_request(file_path, 13, 6)
    string += comp_request(file_path, 17, 24)
    string += comp_request(file_path, 18, 23)
    string += comp_request(file_path, 20, 7)
    string += comp_request(file_path, 21, 20)
    string += comp_request(file_path, 21, 42)
    string += comp_request(file_path, 23, 26)
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += comp_request(file_path, 30, 12)
    string += comp_request(file_path, 31, 8)
    string += comp_request(file_path, 31, 23)
    string += comp_request(file_path, 35, 12)
    string += comp_request(file_path, 36, 48)
    file_path = test_dir / "test_inc.f90"
    string += comp_request(file_path, 10, 2)
    file_path = test_dir / "subdir" / "test_inc2.f90"
    string += comp_request(file_path, 3, 2)
    file_path = test_dir / "subdir" / "test_abstract.f90"
    string += comp_request(file_path, 7, 12)
    file_path = test_dir / "subdir" / "test_free.f90"
    string += comp_request(file_path, 10, 22)
    string += comp_request(file_path, 14, 27)
    string += comp_request(file_path, 28, 14)
    file_path = test_dir / "subdir" / "test_fixed.f"
    string += comp_request(file_path, 15, 8)
    string += comp_request(file_path, 15, 21)
    file_path = test_dir / "subdir" / "test_select.f90"
    string += comp_request(file_path, 21, 7)
    string += comp_request(file_path, 23, 7)
    string += comp_request(file_path, 25, 7)
    string += comp_request(file_path, 30, 7)
    file_path = test_dir / "test_block.f08"
    string += comp_request(file_path, 2, 2)
    string += comp_request(file_path, 5, 4)
    string += comp_request(file_path, 8, 6)
    file_path = test_dir / "subdir" / "test_generic.f90"
    string += comp_request(file_path, 14, 10)
    file_path = test_dir / "subdir" / "test_inherit.f90"
    string += comp_request(file_path, 10, 11)
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += comp_request(file_path, 13, 5)
    string += comp_request(file_path, 14, 5)
    file_path = test_dir / "subdir" / "test_vis.f90"
    string += comp_request(file_path, 8, 10)
    file_path = test_dir / "test_import.f90"
    string += comp_request(file_path, 15, 20)
    file_path = test_dir / "completion" / "test_vis_mod_completion.f90"
    string += comp_request(file_path, 12, 16)
    string += comp_request(file_path, 12, 24)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += comp_request(file_path, 12, 6)
    errcode, res = run_request(string)
    assert errcode == 0
    results.extend(res[1:])

    #
    exp_results = (
        # test_prog.f08
        [1, "myfun", "DOUBLE PRECISION FUNCTION myfun(n, xval)", "myfun"],
        [9, "glob_sub", "SUBROUTINE glob_sub(n, xval, yval)", "glob_sub"],
        [1, "bound_nopass", "SUBROUTINE bound_nopass(a, b)", "bound_nopass"],
        [1, "bound_pass", "SUBROUTINE bound_pass(arg1)", "bound_pass"],
        [1, "stretch_vector", "TYPE(scaled_vector)"],
        [6, "scale", "TYPE(scale_type)"],
        [2, "n", "INTEGER(4)"],
        [1, "val", "REAL(8)"],
        # subdir/test_submod.F90
        [1, "point", "TYPE"],
        [1, "distance", "REAL"],
        [2, "x", "REAL"],
        [1, "point", "TYPE"],
        [2, "x", "REAL"],
        # test_inc.f90
        [2, "val1", "REAL(8)"],
        # subdir/test_inc2.f90
        [2, "val1", "REAL(8)"],
        # subdir/test_abstract.f90
        [1, "abs_interface", "SUBROUTINE"],
        # subdir/test_free.f90
        [1, "DIMENSION(:)", "KEYWORD"],
        [2, "vector_create", "SUBROUTINE"],
        [4, "INTRINSIC", "KEYWORD"],
        # subdir/test_fixed.f90
        [1, "bob", "CHARACTER*(LEN=200)"],
        [1, "dave", "CHARACTER*(20)"],
        # subdir/test_select.f90
        [2, "a", "REAL(8)"],
        [2, "a", "COMPLEX(8)"],
        [1, "n", "INTEGER(4)"],
        [2, "a", "REAL(8)"],
        # test_block.f08
        [9, "READ", "STATEMENT"],
        [10, "READ", "STATEMENT"],
        [11, "READ", "STATEMENT"],
        # subdir/test_generic.f90
        [
            4,
            "my_gen",
            "SUBROUTINE my_gen(self, a, b)",
            "my_gen(${1:self}, ${2:a}, ${3:b})",
        ],
        # subdir/test_inherit.f90
        [1, "val", "REAL(8)"],
        # subdir/test_rename.F90
        [1, "localname", "INTEGER"],
        [2, "renamed_var2", "REAL(8)"],
        # subdir/test_vis.f90
        [3, "some_type", "TYPE"],
        # test_import.f90
        # TODO: this should be 1, mytype2 should not appear in autocomplete
        # see #5 and #8 on GitHub
        [2, "mytype", "TYPE"],
        # completion/test_vis_mod_completion.f90
        [1, "some_var", "INTEGER"],
        [3, "length", "INTEGER"],
        # test_prog.f08, completion without signature_help
        # returns the entire completion as a snippet
        [
            1,
            "myfun",
            "DOUBLE PRECISION FUNCTION myfun(n, xval)",
            "myfun(${1:n}, ${2:xval})",
        ],
    )
    assert len(exp_results) + 1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i + 1], exp_results[i])


def test_sig():
    def check_return(results, checks):
        assert results.get("activeParameter", -1) == checks[0]
        signatures = results.get("signatures")
        assert signatures[0].get("label") == checks[2]
        assert len(signatures[0].get("parameters")) == checks[1]

    def sig_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/signatureHelp",
            {
                "textDocument": {"uri": str(file_path)},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += sig_request(file_path, 25, 18)
    string += sig_request(file_path, 25, 20)
    string += sig_request(file_path, 25, 22)
    string += sig_request(file_path, 25, 27)
    string += sig_request(file_path, 25, 29)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    sub_sig = "test_sig_Sub(arg1, arg2, opt1=opt1, opt2=opt2, opt3=opt3)"
    exp_results = (
        [0, 5, sub_sig],
        [1, 5, sub_sig],
        [2, 5, sub_sig],
        [3, 5, sub_sig],
        [4, 5, sub_sig],
    )
    assert len(exp_results) + 1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i + 1], exp_results[i])


def test_def():
    def check_return(result_array, checks):
        # If no definition is given result is None
        if result_array is None:
            assert not checks[0]
            return None
        assert result_array["uri"] == path_to_uri(checks[2])
        assert result_array["range"]["start"]["line"] == checks[0]
        assert result_array["range"]["start"]["line"] == checks[1]

    def def_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/definition",
            {
                "textDocument": {"uri": str(file_path)},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 12, 6)
    string += def_request(file_path, 13, 6)
    string += def_request(file_path, 20, 7)
    string += def_request(file_path, 21, 20)
    string += def_request(file_path, 21, 42)
    string += def_request(file_path, 23, 26)
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += def_request(file_path, 30, 12)
    string += def_request(file_path, 35, 12)
    file_path = test_dir / "test_inc.f90"
    string += def_request(file_path, 2, 15)
    string += def_request(file_path, 10, 2)
    string += def_request(file_path, 12, 13)
    file_path = test_dir / "subdir" / "test_inc2.f90"
    string += def_request(file_path, 3, 2)
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += def_request(file_path, 13, 5)
    string += def_request(file_path, 14, 5)
    file_path = test_dir / "hover" / "functions.f90"
    string += def_request(file_path, 3, 17)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    fixed_path = str(test_dir / "subdir" / "test_fixed.f")
    free_path = str(test_dir / "subdir" / "test_free.f90")
    exp_results = (
        # test_prog.f08
        [0, 0, fixed_path],
        [22, 22, fixed_path],
        [10, 10, str(test_dir / "test_prog.f08")],
        [21, 21, free_path],
        [14, 14, free_path],
        [5, 5, free_path],
        # subdir/test_submod.F90
        [1, 1, str(test_dir / "subdir" / "test_submod.F90")],
        [1, 1, str(test_dir / "subdir" / "test_submod.F90")],
        # test_inc.f90
        [2, 2, str(test_dir / "subdir" / "test_inc2.f90")],
        [0, 0, str(test_dir / "subdir" / "test_inc2.f90")],
        [None],
        # subdir/test_inc2.f90
        [4, 4, str(test_dir / "test_inc.f90")],
        # subdir/test_rename.F90
        [6, 6, str(test_dir / "subdir" / "test_rename.F90")],
        [1, 1, str(test_dir / "subdir" / "test_rename.F90")],
        # hover/functions.f90
        [3, 3, str(test_dir / "hover" / "functions.f90")],
    )
    assert len(exp_results) + 1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i + 1], exp_results[i])


def test_refs():
    def check_return(result_array, checks):
        def find_in_results(uri, sline):
            for (i, result) in enumerate(result_array):
                if (result["uri"] == uri) and (
                    result["range"]["start"]["line"] == sline
                ):
                    del result_array[i]
                    return result
            return None

        assert len(result_array) == len(checks)
        for check in checks:
            result = find_in_results(path_to_uri(check[0]), check[1])
            assert result is not None
            assert result["range"]["start"]["character"] == check[2]
            assert result["range"]["end"]["character"] == check[3]

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += write_rpc_request(
        2,
        "textDocument/references",
        {
            "textDocument": {"uri": str(file_path)},
            "position": {"line": 9, "character": 8},
        },
    )
    errcode, results = run_request(string)
    assert errcode == 0
    #
    free_path = str(test_dir / "subdir" / "test_free.f90")
    check_return(
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
        for (i, check) in enumerate(checks):
            assert result_array[i]["contents"][0]["value"] == check

    #
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_abstract.f90"
    string += hover_req(file_path, 7, 30)
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 2, 28)
    string += hover_req(file_path, 3, 28)
    string += hover_req(file_path, 4, 28)
    string += hover_req(file_path, 4, 41)
    string += hover_req(file_path, 6, 28)
    string += hover_req(file_path, 7, 38)
    string += hover_req(file_path, 7, 55)
    string += hover_req(file_path, 8, 37)
    string += hover_req(file_path, 8, 50)
    string += hover_req(file_path, 9, 37)
    string += hover_req(file_path, 9, 48)
    string += hover_req(file_path, 10, 37)
    string += hover_req(file_path, 10, 48)
    file_path = test_dir / "hover" / "pointers.f90"
    string += hover_req(file_path, 1, 26)
    file_path = test_dir / "hover" / "functions.f90"
    string += hover_req(file_path, 1, 11)
    string += hover_req(file_path, 7, 19)
    string += hover_req(file_path, 12, 12)
    string += hover_req(file_path, 18, 19)
    string += hover_req(file_path, 23, 34)
    string += hover_req(file_path, 28, 11)
    string += hover_req(file_path, 34, 21)
    string += hover_req(file_path, 46, 11)
    string += hover_req(file_path, 51, 11)
    string += hover_req(file_path, 55, 11)
    file_path = test_dir / "hover" / "spaced_keywords.f90"
    string += hover_req(file_path, 1, 45)
    string += hover_req(file_path, 2, 99)
    file_path = test_dir / "hover" / "recursive.f90"
    string += hover_req(file_path, 9, 40)
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += hover_req(file_path, 29, 24)
    string += hover_req(file_path, 34, 24)
    file_path = test_dir / "test_diagnostic_int.f90"
    string += hover_req(file_path, 19, 14)

    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    #
    ref_results = (
        """SUBROUTINE test(a, b)
 INTEGER(4), DIMENSION(3,6), INTENT(IN) :: a
 REAL(8), DIMENSION(4), INTENT(OUT) :: b""",
        "INTEGER, PARAMETER :: var = 1000",
        "INTEGER",
        "INTEGER, PARAMETER :: var2 = 23",
        "INTEGER, PARAMETER :: var3 = var*var2",
        "INTEGER, PARAMETER :: var4 = 123",
        "DOUBLE PRECISION, PARAMETER :: somevar = 23.12",
        "DOUBLE PRECISION, PARAMETER :: some = 1e-19",
        "LOGICAL(kind=8), PARAMETER :: long_bool = .true.",
        "LOGICAL",
        "CHARACTER(len=5), PARAMETER :: sq_str = '12345'",
        "CHARACTER(LEN=5)",
        'CHARACTER(len=5), PARAMETER :: dq_str = "12345"',
        "CHARACTER(LEN=5)",
        "INTEGER, POINTER",
        """FUNCTION fun1(arg) RESULT(fun1)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: fun1""",
        """FUNCTION fun2(arg) RESULT(fun2)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: fun2""",
        """FUNCTION fun3(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval""",
        """FUNCTION fun4(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval""",
        # Notice that the order of the modifiers does not match the source code
        # This is part of the test, ideally they would be identical but previously
        # any modifiers before the type would be discarded
        """PURE ELEMENTAL FUNCTION fun5(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval""",
        """FUNCTION fun6(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER, DIMENSION(10,10) :: retval""",
        """PURE FUNCTION outer_product(x, y) RESULT(outer_product)
 REAL, DIMENSION(:), INTENT(IN) :: x
 REAL, DIMENSION(:), INTENT(IN) :: y
 REAL, DIMENSION(SIZE(X), SIZE(Y)) :: outer_product""",
        """FUNCTION dlamch(cmach) RESULT(dlamch)
 CHARACTER :: CMACH""",
        """FUNCTION fun7() RESULT(val)
 TYPE(c_ptr) :: val""",
        """TYPE(c_ptr) FUNCTION c_loc(x) RESULT(c_loc)""",
        """REAL, DIMENSION(:, :), INTENT(IN)""",
        """REAL, DIMENSION( SIZE(ARG1, 1), MAXVAL([SIZE(ARG1, 2), """
        """SIZE(ARG1, 1)]) ), INTENT(OUT)""",
        """RECURSIVE SUBROUTINE recursive_assign_descending(node, vector, current_loc)
 TYPE(tree_inode), POINTER, INTENT(IN) :: node
 INTEGER, DIMENSION(:), INTENT(INOUT) :: vector
 INTEGER, INTENT(INOUT) :: current_loc""",
        """FUNCTION point_dist(a, b) RESULT(distance)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 REAL :: distance""",
        """FUNCTION is_point_equal_a(a, b) RESULT(is_point_equal_a)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 LOGICAL :: is_point_equal_a""",
        # Could be subject to change
        """FUNCTION foo2(f, g, h) RESULT(arg3)
 FUNCTION f(x) :: f
 FUNCTION g(x) :: g
 FUNCTION h(x) :: h
 REAL :: arg3""",
    )
    assert len(ref_results) == len(results) - 1
    check_return(results[1:], ref_results)
