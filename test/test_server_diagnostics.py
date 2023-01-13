# from types import NoneType
from setup_tests import (
    path_to_uri,
    run_request,
    test_dir,
    write_rpc_notification,
    write_rpc_request,
)


def test_interface_args():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test subroutines and functions with interfaces as arguments
    file_path = str(test_dir / "test_diagnostic_int.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_nonintrinsic():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test that  use, non_intrinsic does not raise a diagnostic error
    file_path = str(test_dir / "test_nonintrinsic.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_submodules_spaced():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test that submodules with spacings in their parent's names are parsed
    file_path = str(test_dir / "test_submodule.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_end_named_variables():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Tests that variables named end do not close the scope prematurely
    file_path = str(test_dir / "diag" / "test_scope_end_name_var.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_external():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test that externals can be split between multiple lines
    # and that diagnostics for multiple definitions of externals can account
    # for that
    file_path = str(test_dir / "diag" / "test_external.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    root = path_to_uri(str((test_dir / "diag" / "test_external.f90").resolve()))
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 7, "character": 17},
                "end": {"line": 7, "character": 22},
            },
            "message": 'Variable "VAR_B" declared twice in scope',
            "severity": 1,
            "relatedInformation": [
                {
                    "location": {
                        "uri": str(root),
                        "range": {
                            "start": {"line": 5, "character": 0},
                            "end": {"line": 5, "character": 0},
                        },
                    },
                    "message": "First declaration",
                }
            ],
        },
        {
            "range": {
                "start": {"line": 8, "character": 17},
                "end": {"line": 8, "character": 22},
            },
            "message": 'Variable "VAR_A" declared twice in scope',
            "severity": 1,
            "relatedInformation": [
                {
                    "location": {
                        "uri": str(root),
                        "range": {
                            "start": {"line": 3, "character": 0},
                            "end": {"line": 3, "character": 0},
                        },
                    },
                    "message": "First declaration",
                }
            ],
        },
    ]


def test_forall():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Checks that forall with end forall inside a case select does not cause
    # unexpected end of scope.
    file_path = str(test_dir / "diag" / "test_forall.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_use_ordering():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test USE directive ordering errors
    file_path = str(test_dir / "diag" / "test_use_ordering.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_where():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test where blocks
    file_path = str(test_dir / "diag" / "test_where.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_multiline():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test where semicolon (multi-line)
    file_path = str(test_dir / "diag" / "test_semicolon.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_enum():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test ENUM block
    file_path = str(test_dir / "diag" / "test_enum.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_module_procedure():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "subdir" / "test_submod.F90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_max_line_length():
    root = test_dir / "diag"
    string = write_rpc_request(1, "initialize", {"rootPath": str(root)})
    file_path = str(root / "test_lines.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    file_path = str(root / "conf_long_lines.json")
    errcode, results = run_request(string, [f"--config {file_path}"])
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 2, "character": 80},
                "end": {"line": 2, "character": 155},
            },
            "message": 'Line length exceeds "max_line_length" (80)',
            "severity": 2,
        },
        {
            "range": {
                "start": {"line": 3, "character": 100},
                "end": {"line": 3, "character": 127},
            },
            "message": 'Comment line length exceeds "max_comment_line_length" (100)',
            "severity": 2,
        },
    ]


def test_implicit_none():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_implicit_none.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 4, "character": 9},
                "end": {"line": 4, "character": 13},
            },
            "message": "IMPLICIT statement without enclosing scope",
            "severity": 1,
        },
    ]


def test_contains():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_contains.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 3, "character": 4},
                "end": {"line": 3, "character": 12},
            },
            "message": "Multiple CONTAINS statements in scope",
            "severity": 1,
        },
        {
            "range": {
                "start": {"line": 5, "character": 0},
                "end": {"line": 5, "character": 8},
            },
            "message": "CONTAINS statement without enclosing scope",
            "severity": 1,
        },
        {
            "range": {
                "start": {"line": 8, "character": 0},
                "end": {"line": 8, "character": 0},
            },
            "message": "Subroutine/Function definition before CONTAINS statement",
            "severity": 1,
        },
    ]


def test_visibility():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_visibility.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 5, "character": 0},
                "end": {"line": 5, "character": 0},
            },
            "message": "Visibility statement without enclosing scope",
            "severity": 1,
        },
        {
            "range": {
                "start": {"line": 1, "character": 8},
                "end": {"line": 1, "character": 26},
            },
            "message": 'Module "nonexisting_module" not found in project',
            "severity": 3,
        },
        {
            "range": {
                "start": {"line": 3, "character": 8},
                "end": {"line": 3, "character": 11},
            },
            "message": 'Module "mod" not found in project',
            "severity": 3,
        },
        {
            "range": {
                "start": {"line": 2, "character": 4},
                "end": {"line": 2, "character": 12},
            },
            "message": "USE statements after IMPLICIT statement",
            "severity": 1,
        },
    ]


def test_import():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_import.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 1, "character": 0},
                "end": {"line": 1, "character": 0},
            },
            "message": "IMPORT statement outside of interface",
            "severity": 1,
        }
    ]


def test_variable():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_variable.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 4, "character": 19},
                "end": {"line": 4, "character": 22},
            },
            "message": 'Variable "val" masks variable in parent scope',
            "severity": 2,
            "relatedInformation": [
                {
                    "location": {
                        "uri": path_to_uri(str(file_path)),
                        "range": {
                            "start": {"line": 1, "character": 0},
                            "end": {"line": 1, "character": 0},
                        },
                    },
                    "message": "First declaration",
                }
            ],
        }
    ]


def test_function():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    # Test module procedure in submodules importing scopes
    file_path = str(test_dir / "diag" / "test_function.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1]["diagnostics"] == [
        {
            "range": {
                "start": {"line": 3, "character": 31},
                "end": {"line": 3, "character": 34},
            },
            "message": 'Variable "bar" with INTENT keyword not found in argument list',
            "severity": 1,
        }
    ]


def test_submodule_scopes():
    """Test that submodule procedures and functions with modifier keywords are correctly
    parsed and their scopes correctly closed."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "diag")})
    file_path = str(test_dir / "diag" / "test_scope_overreach.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_keyword_arg_list_var_names():
    """Test argument list variables named as keywords are correctly parsed."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "diag")})
    file_path = str(test_dir / "diag" / "test_function_arg_list.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_attribute_and_variable_name_collision():
    """Test variables named with attribute names do not cause a collision."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "diag")})
    file_path = str(test_dir / "diag" / "var_shadowing_keyword_arg.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    assert results[1]["diagnostics"] == []


def test_critical_scope():
    """Test that critical scopes are correctly parsed."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "diag")})
    file_path = str(test_dir / "diag" / "tst_critical.f90")
    string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": file_path}}
    )
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    assert results[1]["diagnostics"] == []
