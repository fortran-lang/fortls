from pathlib import Path

from setup_tests import path_to_uri, run_request, test_dir, write_rpc_request


def validate_def(result_array, checks):
    # If no definition is given result is None
    if result_array is None:
        assert not checks[0]
        return None
    assert result_array["uri"] == path_to_uri(checks[2])
    assert result_array["range"]["start"]["line"] == checks[0]
    assert result_array["range"]["start"]["line"] == checks[1]


def def_request(uri: Path, line, char):
    return write_rpc_request(
        1,
        "textDocument/definition",
        {
            "textDocument": {"uri": str(uri)},
            "position": {"line": line - 1, "character": char - 1},
        },
    )


def test_def_fun_sub_fixed():
    """Test that going to definition of a function or submodule works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 13, 7)
    string += def_request(file_path, 14, 7)
    errcode, results = run_request(string)
    assert errcode == 0
    fixed_path = str(test_dir / "subdir" / "test_fixed.f")
    ref_res = [[0, 0, fixed_path], [22, 22, fixed_path]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_variable():
    """Test that going to definition of a variable works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 21, 8)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[10, 10, str(test_dir / "test_prog.f08")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_type_bound_procedure1():
    """Test that going to definition of type bound procedure works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 22, 21)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[21, 21, str(test_dir / "subdir" / "test_free.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_type_bound_procedure2():
    """Test that going to definition of type bound procedure works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 22, 43)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[14, 14, str(test_dir / "subdir" / "test_free.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_type_nested_variable():
    """Test that going to definition of type bound nested variables works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += def_request(file_path, 24, 27)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[5, 5, str(test_dir / "subdir" / "test_free.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_type_in_submod_function():
    """Test that going into the definition of a type bound function in a submodule"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += def_request(file_path, 31, 13)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[1, 1, str(test_dir / "subdir" / "test_submod.F90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_type_in_submod_procedure():
    """Test that going into the definition of a type bound procedure in a submodule"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += def_request(file_path, 36, 13)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[1, 1, str(test_dir / "subdir" / "test_submod.F90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_include_file():
    """Test that going into the location of an include file works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_inc.f90"
    string += def_request(file_path, 3, 16)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[2, 2, str(test_dir / "subdir" / "test_inc2.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_include_variable1():
    """Test that going to definition of a variable in an include file works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_inc.f90"
    string += def_request(file_path, 11, 3)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[0, 0, str(test_dir / "subdir" / "test_inc2.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_include_variable2():
    """Test that going to definition of a variable in an include file works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_inc2.f90"
    string += def_request(file_path, 4, 3)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[4, 4, str(test_dir / "test_inc.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_include_file_missing():
    """Test that going to the definition of a missing file will not break fortls"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_inc.f90"
    string += def_request(file_path, 13, 14)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[None]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_rename_only_variable():
    """Test that going to definition of a renamed list variable will take you
    to the original definition.
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += def_request(file_path, 14, 6)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[6, 6, str(test_dir / "subdir" / "test_rename.F90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_rename_only_variable_nested():
    """Test that going to definition of a renamed list variable will take you
    to the original definition, tests the multiply renamed case.
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += def_request(file_path, 15, 6)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[1, 1, str(test_dir / "subdir" / "test_rename.F90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)


def test_def_function_implicit_result_variable():
    """Test that going to definition on the implicitly defined variable RESULT
    works.
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "functions.f90"
    string += def_request(file_path, 4, 18)
    errcode, results = run_request(string)
    assert errcode == 0
    ref_res = [[3, 3, str(test_dir / "hover" / "functions.f90")]]
    assert len(ref_res) == len(results) - 1
    for i, res in enumerate(ref_res):
        validate_def(results[i + 1], res)
