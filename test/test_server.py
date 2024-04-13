from setup_tests import run_request, test_dir, write_rpc_notification, write_rpc_request


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
            ["test_associate_block", 2, 0],
            ["test_free", 2, 0],
            ["test_gen_type", 5, 1],
            ["test_generic", 2, 0],
            ["test_inherit", 2, 0],
            ["test_int", 2, 0],
            ["test_mod", 2, 0],
            ["test_nan", 2, 0],
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
