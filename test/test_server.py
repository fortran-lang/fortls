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
