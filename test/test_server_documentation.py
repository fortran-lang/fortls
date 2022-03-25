from setup_tests import run_request, test_dir, write_rpc_request


def check_return(result_array, checks):
    comm_lines = []
    for (i, hover_line) in enumerate(result_array["contents"][0]["value"].splitlines()):
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
            "textDocument": {"uri": str(file_path)},
            "position": {"line": line, "character": char},
        },
    )


def test_doxygen():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_doxygen.f90"
    string += hover_request(file_path, 15, 17)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = (
        (1, "!! @brief inserts a value into an ordered array"),
        (2, "!! "),
        (
            3,
            '!! An array "list" consisting of n ascending ordered values. The method'
            " insert a",
        ),
        (4, '!! "new_entry" into the array.'),
        (5, "!! hint: use cshift and eo-shift"),
        (6, "!! "),
        (7, "!! @param[in,out]   list    a real array, size: max_size"),
        (8, "!! @param[in]       n       current values in the array"),
        (9, "!! @param[in]       max_size    size if the array"),
        (10, "!! @param[in]       new_entry   the value to insert"),
    )
    check_return(results[1], ref)


def test_ford():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_ford.f90"
    string += hover_request(file_path, 5, 20)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = (
        (1, "!! Feeds your cats and dogs, if enough food is available. If not enough"),
        (2, "!! food is available, some of your pets will get angry."),
        (4, " !! The number of cats to keep track of."),
        (6, " !! The number of dogs to keep track of."),
        (8, " !! The amount of pet food (in kilograms) which you have on hand."),
        (10, " !! The number of pets angry because they weren't fed."),
    )
    check_return(results[1], ref)


def test_doc_overwrite_type_bound_procedure_sub():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Test we can override method docstring e.g.
    # procedure :: name => name_imp !< Doc override
    # We want to preserve the argument list docstring
    string += hover_request(file_path, 13, 19)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 1"), (3, " !! Doc 5")))


def test_doc_type_bound_procedure_sub_implementation():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # procedure :: name => name_imp !< Doc override
    # Test that name_imp will yield the full docstring present in the implementation
    string += hover_request(file_path, 13, 31)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 4"), (4, " !! Doc 5")))


def test_doc_overwrite_type_bound_procedure_fun():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Test we can override function docstring e.g.
    # procedure :: name => name_imp !< Doc override
    # We want to preserve the argument list docstring
    string += hover_request(file_path, 14, 17)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 2"),))


def test_doc_type_bound_procedure_fun_implementation():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # procedure :: name => name_imp !< Doc override
    # Test that name_imp will yield the full docstring present in the implementation
    string += hover_request(file_path, 14, 28)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 6"),))


def test_doc_empty_overwrite_type_bound_procedure_sub():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Test we can ignore overriding method docstring  and return the original e.g.
    # procedure :: name => name_imp !<
    # We want to preserve the argument list docstring
    string += hover_request(file_path, 21, 18)
    string += hover_request(file_path, 21, 37)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 7"), (3, " !! Doc 8")))
    check_return(results[2], ((1, "!! Doc 7"), (4, " !! Doc 8")))


def test_doc_empty_overwrite_type_bound_procedure_fun():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # procedure :: name => name_imp !< Doc
    # We want to preserve the procedure docstring but also fetch the empty
    # docs for the implementation
    string += hover_request(file_path, 22, 17)
    string += hover_request(file_path, 22, 32)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((1, "!! Doc 3"),))
    check_return(results[2], ())


def test_doc_multiline_type_bound_procedure_arg_list():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Check that inline docstrings can be input and carried over in multiple lines
    # for both the procedure pointer and the implementation
    string += hover_request(file_path, 15, 32)
    string += hover_request(file_path, 15, 47)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(results[1], ((2, " !! Doc 9"), (3, " !! Doc 10")))
    check_return(
        results[2],
        ((2, " !! Doc 9"), (3, " !! Doc 10"), (5, " !! Doc 11"), (6, " !! Doc 12")),
    )
