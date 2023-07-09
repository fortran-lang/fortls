from setup_tests import (
    check_post_msg,
    path_to_uri,
    run_request,
    test_dir,
    write_rpc_request,
)


def rename_request(new_name: str, file_path, ln: int, ch: int):
    return write_rpc_request(
        1,
        "textDocument/rename",
        {
            "newName": new_name,
            "textDocument": {"uri": str(file_path)},
            "position": {"line": ln, "character": ch},
        },
    )


def check_rename_response(response: dict, references: dict):
    # Loop over URI's if the change spans multiple files there will be more than 1
    for uri, changes in response.items():
        refs = references[uri]
        # Loop over all the changes in the current URI, instances of object
        for c, r in zip(changes, refs):
            assert c["range"] == r["range"]
            assert c["newText"] == r["newText"]


def create(new_text: str, sln: int, sch: int, eln: int, ech: int):
    return {
        "range": {
            "start": {"line": sln, "character": sch},
            "end": {"line": eln, "character": ech},
        },
        "newText": new_text,
    }


def test_rename_var():
    """ "Test simple variable rename"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += rename_request("str_rename", file_path, 5, 25)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(file_path))] = [create("str_rename", 5, 20, 5, 29)]
    check_rename_response(results[1]["changes"], ref)


def test_rename_var_across_module():
    """Test renaming objects like variables across modules works"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += rename_request("new_module_var", file_path, 26, 15)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(test_dir / "subdir" / "test_free.f90"))] = [
        create("new_module_var", 32, 11, 32, 26)
    ]
    ref[path_to_uri(str(file_path))] = [create("new_module_var", 2, 44, 2, 59)]
    ref[path_to_uri(str(file_path))].append(create("new_module_var", 26, 8, 26, 23))

    check_rename_response(results[1]["changes"], ref)


def test_rename_empty():
    """Test that renaming nothing will not error"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "rename")})
    file_path = test_dir / "rename" / "test_rename_imp_type_bound_proc.f90"
    string += rename_request("bar", file_path, 9, 0)
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    assert results[1] is None


def test_rename_member_type_ptr():
    """Test that renaming type bound pointers of procedure methods rename
    only the pointer and not the implementation, even if the pointer and the
    implementation share the same name
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += rename_request("bp_rename", file_path, 18, 25)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(file_path))] = [create("bp_rename", 18, 16, 18, 26)]
    ref[path_to_uri(str(test_dir / "subdir" / "test_free.f90"))] = [
        create("bp_rename", 15, 27, 15, 37)
    ]
    check_rename_response(results[1]["changes"], ref)


def test_rename_member_type_ptr_null():
    """Test renaming type bound pointers of procedure methods works when pointing to
    null
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += rename_request("bp_rename", file_path, 17, 25)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(file_path))] = [create("bp_rename", 17, 16, 17, 28)]
    ref[path_to_uri(str(test_dir / "subdir" / "test_free.f90"))] = [
        create("bp_rename", 11, 43, 11, 55)
    ]
    check_rename_response(results[1]["changes"], ref)


def test_rename_type_bound_proc_no_ptr():
    """Test renaming type bound pointers of procedure methods works when no pointer
    is setup. Requesting to rename the procedure should rename, the implementation
    and the Method itself i.e. call self%foo()

    Requesting to rename the implementation should also rename the procedure
    and all the locations it is called in
    """
    root = test_dir / "rename"
    string = write_rpc_request(1, "initialize", {"rootPath": str(root)})
    file_path = root / "test_rename_imp_type_bound_proc.f90"
    # Rename the procedure name and check if implementation also renames
    string += rename_request("bar", file_path, 5, 23)
    # Rename the implementation name and check if declaration, references change
    string += rename_request("bar", file_path, 10, 18)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(file_path))] = [create("bar", 5, 21, 5, 24)]
    ref[path_to_uri(str(file_path))].append(create("bar", 10, 15, 10, 18))
    ref[path_to_uri(str(file_path))].append(create("bar", 12, 18, 12, 21))
    ref[path_to_uri(str(file_path))].append(create("bar", 13, 19, 13, 22))
    check_rename_response(results[1]["changes"], ref)
    check_rename_response(results[2]["changes"], ref)


def test_rename_non_existent_file():
    """Test renaming type bound pointers of procedure methods works when pointing to
    null
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "fake.f90"
    string += rename_request("bar", file_path, 5, 23)
    errcode, results = run_request(string)
    assert errcode == 0
    assert results[1] is None


def test_rename_nested():
    """Test renaming heavily nested constructs works"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "rename")})
    file_path = test_dir / "rename" / "test_rename_nested.f90"
    string += rename_request("bar", file_path, 6, 23)
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    ref = {}
    ref[path_to_uri(str(file_path))] = [create("bar", 6, 23, 6, 26)]
    ref[path_to_uri(str(file_path))].append(create("bar", 9, 27, 9, 30))
    check_rename_response(results[1]["changes"], ref)


def test_rename_intrinsic():
    """Test renaming an intrinsic function, while no other function exists
    with the same name, will throw an error
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "rename")})
    file_path = test_dir / "rename" / "test_rename_nested.f90"
    string += rename_request("bar", file_path, 8, 27)
    errcode, results = run_request(string, ["-n", "1"])
    assert errcode == 0
    check_post_msg(results[1], "Rename failed: Cannot rename intrinsics", 2)
    assert results[2] is None


def test_rename_use_only_rename():
    """Test renaming constructs of `use mod, only: val => root_val
    are handled correctly
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "subdir")})
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += rename_request("bar", file_path, 13, 5)
    errcode, results = run_request(string, ["-n", "1"])
    # FIXME: to be implemented
    assert errcode == 0


def test_rename_skip_intrinsic():
    """Test that renaming functions named the same as intrinsic functions e.g. size()
    will only rename the user defined functions
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "rename")})
    file_path = test_dir / "rename" / "test_rename_intrinsic.f90"
    string += rename_request("bar", file_path, 22, 13)
    errcode, results = run_request(string, ["-n", "1"])
    # FIXME: to be implemented
    assert errcode == 0
