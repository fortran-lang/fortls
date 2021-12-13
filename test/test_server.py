import os
import subprocess
import sys

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, root_dir)

from fortls.jsonrpc import (
    path_to_uri,
    read_rpc_messages,
    write_rpc_notification,
    write_rpc_request,
)

run_command = os.path.join(root_dir, "fortls.py --incrmental_sync --use_signature_help")
test_dir = os.path.join(root_dir, "test", "test_source")


def run_request(request, fortls_args=""):
    pid = subprocess.Popen(
        run_command + fortls_args,
        shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    results = pid.communicate(input=request.encode())
    tmp_file = StringIO(results[0].decode())
    results = read_rpc_messages(tmp_file)
    parsed_results = []
    for result in results:
        try:
            parsed_results.append(result["result"])
        except KeyError:
            try:
                # Present in `method`s
                parsed_results.append(result["params"])
            except:
                raise RuntimeError(
                    "Only 'result' and 'params' keys have been implemented for testing."
                    " Please add the new key."
                )
        except:
            raise RuntimeError(
                "Unexpected error encountered trying to extract server results"
            )
    errcode = pid.poll()
    return errcode, parsed_results


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
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[0])


def test_open():
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, fortls_args=" --disable_diagnostics")
    #
    assert errcode == 0
    assert len(results) == 1


def test_change():
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_unknown.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    string += write_rpc_notification(
        "textDocument/didChange",
        {
            "textDocument": {"uri": file_path},
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
        2, "textDocument/documentSymbol", {"textDocument": {"uri": file_path}}
    )
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_notification(
        "textDocument/didChange",
        {
            "textDocument": {"uri": file_path},
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
        3, "textDocument/documentSymbol", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, fortls_args=" --disable_diagnostics")
    #
    assert errcode == 0
    assert len(results) == 3
    assert len(results[1]) == 1
    assert len(results[2]) == 5


def test_symbols():
    def check_return(result_array):
        # Expected objects
        objs = (
            ["test_free", 2, 0, 79],
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
            ["bound_pass", 12, 72, 78],
        )
        assert len(result_array) == len(objs)
        for i, obj in enumerate(objs):
            assert result_array[i]["name"] == obj[0]
            assert result_array[i]["kind"] == obj[1]
            assert result_array[i]["location"]["range"]["start"]["line"] == obj[2]
            assert result_array[i]["location"]["range"]["end"]["line"] == obj[3]

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_request(
        2, "textDocument/documentSymbol", {"textDocument": {"uri": file_path}}
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
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
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

    def comp_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/completion",
            {
                "textDocument": {"uri": file_path},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += comp_request(file_path, 12, 6)
    string += comp_request(file_path, 13, 6)
    string += comp_request(file_path, 17, 24)
    string += comp_request(file_path, 18, 23)
    string += comp_request(file_path, 20, 7)
    string += comp_request(file_path, 21, 20)
    string += comp_request(file_path, 21, 42)
    string += comp_request(file_path, 23, 26)
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += comp_request(file_path, 30, 12)
    string += comp_request(file_path, 31, 8)
    string += comp_request(file_path, 31, 23)
    string += comp_request(file_path, 35, 12)
    string += comp_request(file_path, 36, 48)
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += comp_request(file_path, 10, 2)
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += comp_request(file_path, 3, 2)
    file_path = os.path.join(test_dir, "subdir", "test_abstract.f90")
    string += comp_request(file_path, 7, 12)
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += comp_request(file_path, 10, 22)
    string += comp_request(file_path, 28, 14)
    file_path = os.path.join(test_dir, "subdir", "test_fixed.f")
    string += comp_request(file_path, 15, 8)
    string += comp_request(file_path, 15, 21)
    file_path = os.path.join(test_dir, "subdir", "test_select.f90")
    string += comp_request(file_path, 21, 7)
    string += comp_request(file_path, 23, 7)
    string += comp_request(file_path, 25, 7)
    string += comp_request(file_path, 30, 7)
    file_path = os.path.join(test_dir, "test_block.f08")
    string += comp_request(file_path, 2, 2)
    string += comp_request(file_path, 5, 4)
    string += comp_request(file_path, 8, 6)
    file_path = os.path.join(test_dir, "subdir", "test_generic.f90")
    string += comp_request(file_path, 14, 10)
    file_path = os.path.join(test_dir, "subdir", "test_inherit.f90")
    string += comp_request(file_path, 10, 11)
    file_path = os.path.join(test_dir, "subdir", "test_rename.F90")
    string += comp_request(file_path, 13, 5)
    string += comp_request(file_path, 14, 5)
    file_path = os.path.join(test_dir, "subdir", "test_vis.f90")
    string += comp_request(file_path, 8, 10)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    exp_results = (
        # test_prog.f08
        [1, "myfun", "DOUBLE PRECISION FUNCTION myfun(n, xval)"],
        [4, "glob_sub", "SUBROUTINE glob_sub(n, xval, yval)"],
        [1, "bound_nopass", "SUBROUTINE bound_nopass(a, b)"],
        [1, "bound_pass", "SUBROUTINE bound_pass(arg1)"],
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
        [3, "INTENT(IN)", "KEYWORD"],
        # subdir/test_fixed.f90
        [1, "bob", "CHARACTER*(LEN=200)"],
        [1, "dave", "CHARACTER*(20)"],
        # subdir/test_select.f90
        [2, "a", "REAL(8)"],
        [2, "a", "COMPLEX(8)"],
        [1, "n", "INTEGER(4)"],
        [2, "a", "REAL(8)"],
        # test_block.f08
        [7, "READ", "STATEMENT"],
        [8, "READ", "STATEMENT"],
        [9, "READ", "STATEMENT"],
        # subdir/test_generic.f90
        [4, "my_gen", "SUBROUTINE my_gen(self, a, b)"],
        # subdir/test_inherit.f90
        [1, "val", "REAL(8)"],
        # subdir/test_rename.F90
        [1, "localname", "INTEGER"],
        [1, "renamed_var2", "REAL(8)"],
        # subdir/test_vis.f90
        [3, "some_type", "TYPE"],
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
                "textDocument": {"uri": file_path},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
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
        assert result_array["uri"] == path_to_uri(checks[2])
        assert result_array["range"]["start"]["line"] == checks[0]
        assert result_array["range"]["start"]["line"] == checks[1]

    def def_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/definition",
            {
                "textDocument": {"uri": file_path},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += def_request(file_path, 12, 6)
    string += def_request(file_path, 13, 6)
    string += def_request(file_path, 20, 7)
    string += def_request(file_path, 21, 20)
    string += def_request(file_path, 21, 42)
    string += def_request(file_path, 23, 26)
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += def_request(file_path, 30, 12)
    string += def_request(file_path, 35, 12)
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += def_request(file_path, 10, 2)
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += def_request(file_path, 3, 2)
    file_path = os.path.join(test_dir, "subdir", "test_rename.F90")
    string += def_request(file_path, 13, 5)
    string += def_request(file_path, 14, 5)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    fixed_path = os.path.join(test_dir, "subdir", "test_fixed.f")
    free_path = os.path.join(test_dir, "subdir", "test_free.f90")
    exp_results = (
        # test_prog.f08
        [0, 0, fixed_path],
        [22, 22, fixed_path],
        [10, 10, os.path.join(test_dir, "test_prog.f08")],
        [21, 21, free_path],
        [14, 14, free_path],
        [5, 5, free_path],
        # subdir/test_submod.F90
        [1, 1, os.path.join(test_dir, "subdir", "test_submod.F90")],
        [1, 1, os.path.join(test_dir, "subdir", "test_submod.F90")],
        # test_inc.f90
        [0, 0, os.path.join(test_dir, "subdir", "test_inc2.f90")],
        # subdir/test_inc2.f90
        [4, 4, os.path.join(test_dir, "test_inc.f90")],
        # subdir/test_rename.F90
        [6, 6, os.path.join(test_dir, "subdir", "test_rename.F90")],
        [1, 1, os.path.join(test_dir, "subdir", "test_rename.F90")],
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
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += write_rpc_request(
        2,
        "textDocument/references",
        {"textDocument": {"uri": file_path}, "position": {"line": 9, "character": 8}},
    )
    errcode, results = run_request(string)
    assert errcode == 0
    #
    free_path = os.path.join(test_dir, "subdir", "test_free.f90")
    check_return(
        results[1],
        (
            [os.path.join(test_dir, "test_prog.f08"), 2, 21, 27],
            [os.path.join(test_dir, "test_prog.f08"), 9, 5, 11],
            [free_path, 8, 8, 14],
            [free_path, 16, 9, 15],
            [free_path, 18, 14, 20],
            [free_path, 36, 6, 12],
            [free_path, 44, 6, 12],
            [free_path, 50, 6, 12],
            [free_path, 76, 6, 12],
        ),
    )


def test_hover():
    def check_return(result_array, checks):
        assert len(result_array) == len(checks)
        for (i, check) in enumerate(checks):
            assert result_array[i]["contents"][0]["value"] == check

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_abstract.f90")
    string += write_rpc_request(
        2,
        "textDocument/hover",
        {"textDocument": {"uri": file_path}, "position": {"line": 7, "character": 30}},
    )
    errcode, results = run_request(string)
    assert errcode == 0
    #
    check_return(
        results[1:],
        (
            """SUBROUTINE test(a, b)
 INTEGER(4), DIMENSION(3,6), INTENT(IN) :: a
 REAL(8), DIMENSION(4), INTENT(OUT) :: b""",
        ),
    )


def test_docs():
    def check_return(result_array, checks):
        comm_lines = []
        for (i, hover_line) in enumerate(
            result_array["contents"][0]["value"].splitlines()
        ):
            if hover_line.count("!!") > 0:
                comm_lines.append((i, hover_line))
        assert len(comm_lines) == len(checks)
        for i in range(len(checks)):
            assert comm_lines[i][0] == checks[i][0]
            assert comm_lines[i][1] == checks[i][1]

    def hover_request(file_path, line, char):
        return write_rpc_request(
            1,
            "textDocument/hover",
            {
                "textDocument": {"uri": file_path},
                "position": {"line": line, "character": char},
            },
        )

    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += hover_request(file_path, 13, 19)
    string += hover_request(file_path, 13, 31)
    string += hover_request(file_path, 14, 17)
    string += hover_request(file_path, 14, 28)
    string += hover_request(file_path, 21, 18)
    string += hover_request(file_path, 21, 37)
    string += hover_request(file_path, 22, 17)
    string += hover_request(file_path, 22, 32)
    string += hover_request(file_path, 15, 32)
    string += hover_request(file_path, 15, 47)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    check_return(results[1], ((1, "!! Doc 1"), (3, " !! Doc 5")))
    check_return(results[2], ((1, "!! Doc 4"), (4, " !! Doc 5")))
    check_return(results[3], ((1, "!! Doc 2"),))
    check_return(results[4], ((1, "!! Doc 6"),))
    check_return(results[5], ((1, "!! Doc 7"), (3, " !! Doc 8")))
    check_return(results[6], ((1, "!! Doc 7"), (4, " !! Doc 8")))
    check_return(results[7], ((1, "!! Doc 3"),))
    check_return(results[8], ())
    check_return(results[9], ())
    check_return(results[10], ((3, " !! Doc 9"), (4, " !! Doc 10")))


def test_diagnostic_interfaces():
    """
    Tests the diagnostics for subroutines and functions with interfaces
    as arguments
    """
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_diagnostic_int.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    # Example of a diagnostics message
    # string += write_rpc_notification(
    #     "textDocument/publishDiagnostics",
    #     {
    #         "uri": file_path,
    #         "diagnostics": [
    #             {
    #                 "range": {
    #                     "start": {"line": 0, "character": 0},
    #                     "end": {"line": 0, "character": 0},
    #                 },
    #                 "message": "",
    #                 "severity": 0,
    #             }
    #         ],
    #     },
    # )
    errcode, results = run_request(string)
    assert errcode == 0
    # check that the diagnostics list is empty
    assert not results[1]["diagnostics"]


if __name__ == "__main__":
    test_init()
    test_open()
    test_change()
    test_symbols()
    test_workspace_symbols()
    test_comp()
    test_sig()
    test_def()
    test_refs()
    test_hover()
    test_docs()
    test_diagnostic_interfaces()
