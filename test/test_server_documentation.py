from setup_tests import run_request, test_dir, write_rpc_request


def check_return(result_array, checks, only_docs=False):
    comm_lines = []
    found_docs = False
    idx = 0
    for i, hover_line in enumerate(result_array["contents"]["value"].splitlines()):
        if hover_line == "-----":
            found_docs = True
        if found_docs and only_docs:
            comm_lines.append((idx, hover_line))
            idx += 1
        elif not only_docs:
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
    errcode, results = run_request(string, ["-n1"])
    assert errcode == 0
    ref = (
        (0, "```fortran90"),
        (1, "SUBROUTINE insert(list, n, max_size, new_entry)"),
        (2, " REAL, DIMENSION(:), INTENT(INOUT) :: list"),
        (3, " INTEGER, INTENT(IN) :: n"),
        (4, " INTEGER, INTENT(IN) :: max_size"),
        (5, " REAL, INTENT(IN) :: new_entry"),
        (6, "```"),
        (7, "-----"),
        (8, "inserts a value into an ordered array"),
        (9, ""),
        (
            10,
            (
                'An array "list" consisting of n ascending ordered values. The method'
                " insert a"
            ),
        ),
        (11, '"new_entry" into the array.'),
        (12, "hint: use cshift and eo-shift"),
        (13, ""),
        (14, ""),
        (15, "**Parameters:**  "),
        (16, "`list` - a real array, size: max_size   "),
        (17, "`n` - current values in the array   "),
        (18, "`max_size` - size if the array   "),
        (19, "`new_entry` - the value to insert "),
    )
    check_return(results[1], ref)


def test_ford():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_ford.f90"
    string += hover_request(file_path, 5, 20)
    errcode, results = run_request(string)
    assert errcode == 0
    ref = (
        (0, "```fortran90"),
        (1, "SUBROUTINE feed_pets(cats, dogs, food, angry)"),
        (2, " INTEGER, INTENT(IN) :: cats"),
        (3, " INTEGER, INTENT(IN) :: dogs"),
        (4, " REAL, INTENT(INOUT) :: food"),
        (5, " INTEGER, INTENT(OUT) :: angry"),
        (6, "```"),
        (7, "-----"),
        (8, "Feeds your cats and dogs, if enough food is available. If not enough"),
        (9, "food is available, some of your pets will get angry."),
        (10, "   "),
        (11, ""),
        (12, "**Parameters:**     "),
        (13, "`cats` The number of cats to keep track of.   "),
        (14, "`dogs` The number of dogs to keep track of.   "),
        (15, "`food` The amount of pet food (in kilograms) which you have on hand.   "),
        (16, "`angry` The number of pets angry because they weren't fed."),
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
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 1   "),
            (2, ""),
            (3, "**Parameters:**     "),
            (4, "`n` Doc 5"),
        ),
        True,
    )


def test_doc_type_bound_procedure_sub_implementation():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # procedure :: name => name_imp !< Doc override
    # Test that name_imp will yield the full docstring present in the implementation
    string += hover_request(file_path, 13, 31)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 4   "),
            (2, ""),
            (3, "**Parameters:**     "),
            (4, "`n` Doc 5"),
        ),
        True,
    )


def test_doc_variable():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # n !! Doc 5
    # Test that a variable can carry over documentation
    string += hover_request(file_path, 37, 26)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 5"),
        ),
        True,
    )


def test_doc_overwrite_type_bound_procedure_fun():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Test we can override function docstring e.g.
    # procedure :: name => name_imp !< Doc override
    # We want to preserve the argument list docstring
    string += hover_request(file_path, 14, 17)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 2"),
        ),
        True,
    )


def test_doc_type_bound_procedure_fun_implementation():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # procedure :: name => name_imp !< Doc override
    # Test that name_imp will yield the full docstring present in the implementation
    string += hover_request(file_path, 14, 28)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 6"),
        ),
        True,
    )


def test_doc_empty_overwrite_type_bound_procedure_sub():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Test we can ignore overriding method docstring  and return the original e.g.
    # procedure :: name => name_imp !<
    # We want to preserve the argument list docstring
    # the self argument in the second request is not included because it is
    # missing a doc string
    string += hover_request(file_path, 21, 18)
    string += hover_request(file_path, 21, 37)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 7   "),
            (2, ""),
            (3, "**Parameters:**     "),
            (4, "`scale` Doc 8"),
        ),
        True,
    )
    check_return(
        results[2],
        (
            (0, "-----"),
            (1, "Doc 7   "),
            (2, ""),
            (3, "**Parameters:**     "),
            (4, "`scale` Doc 8"),
        ),
        True,
    )


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
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 3  "),
            (2, ""),
            (3, "**Return:**  "),
            (4, "`norm`return value docstring"),
        ),
        True,
    )
    check_return(
        results[2],
        (
            (0, "-----"),
            (1, "Top level docstring  "),
            (2, ""),
            (3, "**Parameters:**    "),
            (4, "`self` self value docstring  "),
            (5, ""),
            (6, "**Return:**  "),
            (7, "`norm`return value docstring"),
        ),
        True,
    )


def test_doc_multiline_type_bound_procedure_arg_list():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_free.f90"
    # Check that inline docstrings can be input and carried over in multiple lines
    # for both the procedure pointer and the implementation
    string += hover_request(file_path, 15, 32)
    string += hover_request(file_path, 15, 47)
    errcode, results = run_request(string)
    assert errcode == 0
    check_return(
        results[1],
        (
            (0, "-----"),
            (1, "Doc 3  "),
            (2, ""),
            (3, "**Parameters:**     "),
            (4, "`arg1` Doc 9"),
            (5, "Doc 10"),
        ),
        True,
    )
    check_return(
        results[2],
        (
            (0, "-----"),
            (1, ""),
            (2, "**Parameters:**     "),
            (3, "`arg1` Doc 9"),
            (4, "Doc 10   "),
            (5, "`self` Doc 11"),
            (6, "Doc 12"),
        ),
        True,
    )


def test_doxygen_doc_for_module_use():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_module_and_type_doc.f90"
    string += hover_request(file_path, 24, 14)
    errcode, results = run_request(string)
    assert errcode == 0

    ref = (
        (0, "```fortran90"),
        (1, "MODULE doxygen_doc_mod"),
        (2, "```"),
        (3, "-----"),
        (4, "module doc for doxygen_doc_mod"),
        (5, ""),
        (6, "with info"),
    )
    check_return(results[1], ref)


def test_ford_doc_for_module_use():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_module_and_type_doc.f90"
    string += hover_request(file_path, 25, 14)
    errcode, results = run_request(string)
    assert errcode == 0

    ref = (
        (0, "```fortran90"),
        (1, "MODULE ford_doc_mod"),
        (2, "```"),
        (3, "-----"),
        (4, "Doc for ford_doc_mod"),
    )
    check_return(results[1], ref)


def test_doxygen_doc_for_type():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_module_and_type_doc.f90"
    string += hover_request(file_path, 27, 11)
    errcode, results = run_request(string)
    assert errcode == 0

    ref = (
        (0, "```fortran90"),
        (1, "TYPE :: a_t"),
        (2, "```"),
        (3, "-----"),
        (4, "Doc for a_t"),
    )
    check_return(results[1], ref)


def test_ford_doc_for_type():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "docs")})
    file_path = test_dir / "docs" / "test_module_and_type_doc.f90"
    string += hover_request(file_path, 28, 11)
    errcode, results = run_request(string)
    assert errcode == 0

    ref = (
        (0, "```fortran90"),
        (1, "TYPE :: b_t"),
        (2, "```"),
        (3, "-----"),
        (4, "Doc for b_t"),
    )
    check_return(results[1], ref)
