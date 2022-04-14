from setup_tests import run_request, test_dir, write_rpc_request


def hover_req(file_path: str, ln: int, col: int) -> str:
    return write_rpc_request(
        1,
        "textDocument/hover",
        {
            "textDocument": {"uri": str(file_path)},
            "position": {"line": ln, "character": col},
        },
    )


def validate_hover(result_array: list, checks: list):
    assert len(result_array) - 1 == len(checks)
    for (i, check) in enumerate(checks):
        assert result_array[i + 1]["contents"][0]["value"] == check


def test_hover_abstract_int_procedure():
    """Tests that the binding of an abstract interface is correctly resolved"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_abstract.f90"
    string += hover_req(file_path, 7, 30)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        """SUBROUTINE test(a, b)
 INTEGER(4), DIMENSION(3,6), INTENT(IN) :: a
 REAL(8), DIMENSION(4), INTENT(OUT) :: b"""
    ]
    validate_hover(results, ref_results)


def test_hover_parameter_multiline():
    """Test that hover parameters display value correctly across lines"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 2, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER, PARAMETER :: var = 1000"]
    validate_hover(results, ref_results)


def test_hover_literal_num():
    """Test that hovering over literals shows their type INTEGER"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 3, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER"]
    validate_hover(results, ref_results)


def test_hover_parameter():
    """Test that hover parameters display value correctly"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 4, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER, PARAMETER :: var2 = 23"]
    validate_hover(results, ref_results)


def test_hover_parameter_nested():
    """Test that hover parameters using other parameter values works"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 4, 41)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER, PARAMETER :: var3 = var*var2"]
    validate_hover(results, ref_results)


def test_hover_parameter_multiline_missing_type():
    """Test that hover parameters display correctly when type is split across lines"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 6, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER, PARAMETER :: var4 = 123"]
    validate_hover(results, ref_results)


def test_hover_literal_real():
    """Test that hovering over literals shows their values REAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 47)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["REAL"]
    validate_hover(results, ref_results)


def test_hover_parameter_double():
    """Test that hovering over parameters shows their type DOUBLE PRECISION"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 38)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["DOUBLE PRECISION, PARAMETER :: somevar = 23.12"]
    validate_hover(results, ref_results)


def test_hover_parameter_double_sf():
    """Test that hovering over parameters shows their type scientific notation"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 55)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["DOUBLE PRECISION, PARAMETER :: some = 1e-19"]
    validate_hover(results, ref_results)


def test_hover_parameter_bool():
    """Test that hovering over parameters shows their values LOGICAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 8, 38)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["LOGICAL(kind=8), PARAMETER :: long_bool = .true."]
    validate_hover(results, ref_results)


def test_hover_literal_bool():
    """Test that hovering over literals shows their type LOGICAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 8, 50)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["LOGICAL"]
    validate_hover(results, ref_results)


def test_hover_parameter_str_sq():
    """Test that hovering over parameters shows their value, single quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 9, 37)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["CHARACTER(len=5), PARAMETER :: sq_str = '12345'"]
    validate_hover(results, ref_results)


def test_hover_literal_string_sq():
    """Test that hovering over literals shows their values single quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 9, 48)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["CHARACTER(LEN=5)"]
    validate_hover(results, ref_results)


def test_hover_parameter_str_dq():
    """Test that hovering over parameters shows their value, double quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 10, 37)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ['CHARACTER(len=5), PARAMETER :: dq_str = "12345"']
    validate_hover(results, ref_results)


def test_hover_literal_string_dq():
    """Test that hovering over literals shows their values double quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 10, 48)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["CHARACTER(LEN=5)"]
    validate_hover(results, ref_results)


def test_hover_pointer_attr():
    """Test that hovering maintains the variable attributes e.g. POINTER"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "pointers.f90"
    string += hover_req(file_path, 1, 26)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["INTEGER, POINTER"]
    validate_hover(results, ref_results)


def test_hover_functions():
    """Test that hovering over functions provides the expected results"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
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
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0

    ref_results = [
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
    ]
    validate_hover(results, ref_results)


def test_hover_spaced_keywords():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "spaced_keywords.f90"
    string += hover_req(file_path, 1, 45)
    string += hover_req(file_path, 2, 99)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        """REAL, DIMENSION(:, :), INTENT(IN)""",
        """REAL, DIMENSION( SIZE(ARG1, 1), MAXVAL([SIZE(ARG1, 2), """
        """SIZE(ARG1, 1)]) ), INTENT(OUT)""",
    ]
    validate_hover(results, ref_results)


def test_hover_recursive():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "recursive.f90"
    string += hover_req(file_path, 9, 40)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        """RECURSIVE SUBROUTINE recursive_assign_descending(node, vector, current_loc)
 TYPE(tree_inode), POINTER, INTENT(IN) :: node
 INTEGER, DIMENSION(:), INTENT(INOUT) :: vector
 INTEGER, INTENT(INOUT) :: current_loc"""
    ]
    validate_hover(results, ref_results)


def test_hover_subroutine():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_submod.F90"
    string += hover_req(file_path, 29, 24)
    string += hover_req(file_path, 34, 24)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        """FUNCTION point_dist(a, b) RESULT(distance)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 REAL :: distance""",
        """FUNCTION is_point_equal_a(a, b) RESULT(is_point_equal_a)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 LOGICAL :: is_point_equal_a""",
    ]
    validate_hover(results, ref_results)


def test_hover_interface_as_argument():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_diagnostic_int.f90"
    string += hover_req(file_path, 19, 14)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = (
        # Could be subject to change
        """FUNCTION foo2(f, g, h) RESULT(arg3)
 FUNCTION f(x) :: f
 FUNCTION g(x) :: g
 FUNCTION h(x) :: h
 REAL :: arg3""",
    )
    validate_hover(results, ref_results)
