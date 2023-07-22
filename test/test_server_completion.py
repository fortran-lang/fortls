from setup_tests import run_request, test_dir, write_rpc_request


def validate_comp(result_array, checks):
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


def test_comp1():
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
    errcode, results = run_request(string, ["--use_signature_help", "-n1"])
    assert errcode == 0

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
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp2():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += comp_request(file_path, 30, 12)
    string += comp_request(file_path, 31, 8)
    string += comp_request(file_path, 31, 23)
    string += comp_request(file_path, 35, 12)
    string += comp_request(file_path, 36, 48)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_submod.F90
        [1, "point", "TYPE"],
        [1, "distance", "REAL"],
        [2, "x", "REAL"],
        [1, "point", "TYPE"],
        [2, "x", "REAL"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp3():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_inc.f90"
    string += comp_request(file_path, 10, 2)
    file_path = test_dir / "subdir" / "test_inc2.f90"
    string += comp_request(file_path, 3, 2)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # test_inc.f90
        [2, "val1", "REAL(8)"],
        # subdir/test_inc2.f90
        [2, "val1", "REAL(8)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp4():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_abstract.f90"
    string += comp_request(file_path, 7, 12)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_abstract.f90
        [1, "abs_interface", "SUBROUTINE"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp5():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    string += comp_request(file_path, 10, 22)
    string += comp_request(file_path, 14, 27)
    string += comp_request(file_path, 28, 15)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_free.f90
        [1, "DIMENSION(:)", "KEYWORD"],
        [2, "vector_create", "SUBROUTINE"],
        [3, "INTENT(IN)", "KEYWORD"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp6():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_select.f90"
    string += comp_request(file_path, 21, 7)
    string += comp_request(file_path, 23, 7)
    string += comp_request(file_path, 25, 7)
    string += comp_request(file_path, 30, 7)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_select.f90
        [2, "a", "REAL(8)"],
        [2, "a", "COMPLEX(8)"],
        [1, "n", "INTEGER(4)"],
        [2, "a", "REAL(8)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp7():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_block.f08"
    string += comp_request(file_path, 2, 2)
    string += comp_request(file_path, 5, 4)
    string += comp_request(file_path, 8, 6)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # test_block.f08
        [9, "READ", "STATEMENT"],
        [10, "READ", "STATEMENT"],
        [11, "READ", "STATEMENT"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp8():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_inherit.f90"
    string += comp_request(file_path, 10, 11)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_inherit.f90
        [1, "val", "REAL(8)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp9():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_rename.F90"
    string += comp_request(file_path, 13, 5)
    string += comp_request(file_path, 14, 5)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_rename.F90
        [1, "localname", "INTEGER"],
        [2, "renamed_var2", "REAL(8)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp10():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_vis.f90"
    string += comp_request(file_path, 8, 10)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_vis.f90
        [3, "some_type", "TYPE"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp11():
    """Indicate the derived types arguments separated with spaces and types"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += comp_request(file_path, 23, 26)
    string += comp_request(file_path, 27, 28)
    string += comp_request(file_path, 28, 30)
    string += comp_request(file_path, 29, 30)
    errcode, results = run_request(string, ["--use_signature_help", "-n1"])
    assert errcode == 0

    exp_results = (
        # test_prog.f08
        [1, "val", "REAL(8)"],
        [1, "val", "REAL(8)"],
        [1, "val", "REAL(8)"],
        [1, "val", "REAL(8)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_import_host_association():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_import.f90"
    string += comp_request(file_path, 15, 20)
    errcode, results = run_request(string, ["--use_signature_help", "-n1"])
    assert errcode == 0

    exp_results = ([1, "mytype", "TYPE"],)
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_visibility_scopes():
    """Test that PUBLIC, PRIVATE scopes are enforced in autocomplete results."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "completion" / "test_vis_mod_completion.f90"
    string += comp_request(file_path, 12, 16)
    string += comp_request(file_path, 12, 24)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # completion/test_vis_mod_completion.f90
        [1, "some_var", "INTEGER"],
        [3, "length", "INTEGER"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_interface():
    """Test that the interface signature autocompletion, with placeholders, works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_generic.f90"
    string += comp_request(file_path, 14, 10)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_generic.f90
        [
            4,
            "my_gen",
            "SUBROUTINE my_gen(self, a, b)",
            "my_gen(${1:self}, ${2:a}, ${3:b})",
        ],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_no_signature_help():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += comp_request(file_path, 12, 6)
    errcode, results = run_request(string)
    assert errcode == 0

    exp_results = (
        # test_prog.f08, completion without signature_help
        # returns the entire completion as a snippet
        [
            1,
            "myfun",
            "DOUBLE PRECISION FUNCTION myfun(n, xval)",
            "myfun(${1:n}, ${2:xval})",
        ],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_fixed():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_fixed.f"
    string += comp_request(file_path, 15, 8)
    string += comp_request(file_path, 15, 21)
    errcode, results = run_request(string, ["--use_signature_help"])
    assert errcode == 0

    exp_results = (
        # subdir/test_fixed.f90
        [1, "bob", "CHARACTER*(LEN=200)"],
        [1, "dave", "CHARACTER*(20)"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_comp_documentation():
    """Test that "documentation" is returned for autocomplete results."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    string += comp_request(file_path, 21, 37)
    errcode, results = run_request(
        string,
    )
    assert errcode == 0

    exp_results = [
        {
            "label": "scaled_vector_set",
            "kind": 3,
            "detail": "SUBROUTINE",
            "documentation": {
                "kind": "markdown",
                "value": (
                    "```fortran90\n"
                    "SUBROUTINE scaled_vector_set(self, scale)\n"
                    " CLASS(scaled_vector), INTENT(INOUT) :: self\n"
                    " REAL(8), INTENT(IN) :: scale\n"
                    "```\n"
                    "-----\n"
                    "Doc 7   \n\n"
                    "**Parameters:**     \n"
                    "`scale` Doc 8"
                ),
            },
        },
        {
            "label": "scaled_vector_norm",
            "kind": 3,
            "detail": "REAL(8) FUNCTION",
            "documentation": {
                "kind": "markdown",
                "value": (
                    "```fortran90\n"
                    "FUNCTION scaled_vector_norm(self) RESULT(norm)\n"
                    " CLASS(scaled_vector), INTENT(IN) :: self\n"
                    " REAL(8) :: norm\n"
                    "```\n"
                    "-----\n"
                    "Top level docstring  \n\n"
                    "**Parameters:**    \n"
                    "`self` self value docstring  \n\n"
                    "**Return:**  \n"
                    "`norm`return value docstring"
                ),
            },
        },
    ]
    assert len(exp_results) == len(results[1])
    assert exp_results == results[1]


def test_comp_use_only_interface():
    """Test completion of interfaces when using USE ONLY give the right signature."""
    string = write_rpc_request(
        1, "initialize", {"rootPath": str(test_dir / "completion")}
    )
    file_path = test_dir / "completion" / "use_only_interface.f90"
    string += comp_request(file_path, 21, 29)
    errcode, results = run_request(
        string,
    )
    assert errcode == 0
    exp_results = [[1, "some_sub", "INTERFACE"]]
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_import():
    """Test that import works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "imp")})
    file_path = test_dir / "imp" / "import.f90"
    string += comp_request(file_path, 13, 16)  # import type1
    string += comp_request(file_path, 17, 16)  # import, only: type2
    string += comp_request(file_path, 21, 16)  # import, none
    string += comp_request(file_path, 25, 16)  # import, all
    string += comp_request(file_path, 29, 16)  # import
    string += comp_request(file_path, 34, 16)  # import type1; import type2
    string += comp_request(file_path, 38, 16)  # import :: type1, type2
    errcode, results = run_request(string, ["--use_signature_help", "-n1"])
    assert errcode == 0
    exp_results = (
        [1, "type1", "TYPE"],
        [1, "type2", "TYPE"],
        [0],
        [2, "type1", "TYPE"],
        [2, "type1", "TYPE"],
        [2, "type1", "TYPE"],
        [2, "type1", "TYPE"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)


def test_use_multiple():
    """Test that USE multiple times works."""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "use")})
    file_path = test_dir / "use" / "use.f90"
    string += comp_request(file_path, 14, 11)
    string += comp_request(file_path, 15, 12)
    errcode, results = run_request(string, ["--use_signature_help", "-n1"])
    assert errcode == 0
    exp_results = (
        [5, "val1", "INTEGER"],
        [1, "val4", "INTEGER"],
    )
    assert len(exp_results) == len(results) - 1
    for i, ref in enumerate(exp_results):
        validate_comp(results[i + 1], ref)
