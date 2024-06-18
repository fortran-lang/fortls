import json

from setup_tests import Path, run_request, test_dir, write_rpc_request


def hover_req(file_path: Path, ln: int, col: int) -> str:
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
    for i, check in enumerate(checks):
        assert result_array[i + 1]["contents"]["value"] == check


def test_hover_abstract_int_procedure():
    """Tests that the binding of an abstract interface is correctly resolved"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "subdir" / "test_abstract.f90"
    string += hover_req(file_path, 7, 30)
    errcode, results = run_request(string, fortls_args=["--sort_keywords", "-n1"])
    assert errcode == 0
    ref_results = [
        """```fortran90
SUBROUTINE test(a, b)
 INTEGER(4), DIMENSION(3,6), INTENT(IN) :: a
 REAL(8), DIMENSION(4), INTENT(OUT) :: b
```"""
    ]
    validate_hover(results, ref_results)


def test_hover_parameter_multiline():
    """Test that hover parameters display value correctly across lines"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 2, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var = 1000\n```"]
    validate_hover(results, ref_results)


def test_hover_literal_num():
    """Test that hovering over literals shows their type INTEGER"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 3, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter():
    """Test that hover parameters display value correctly"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 4, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var2 = 23\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_dollar():
    """Test that hover parameters with dollar in name are recognized correctly"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 20, 31)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER(4), PARAMETER :: SIG$ERR = -1\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_eqnospace():
    """Test that hover parameters display value correctly"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 11, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_no_space = 123\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_morespace():
    """Test that hover parameters display value correctly"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 12, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_more_space = 123\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_var_sum():
    """Test that hover parameters display value correctly with sum"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 13, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_sum1 = 1 + 23\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_var_neg():
    """Test that hover parameters display value correctly with extraction"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 14, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_ex1 = 1 - 23\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_var_mul():
    """Test that hover parameters display value correctly with
    multiplication and spaces"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 15, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_mul1 = 1 * 23\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_var_div():
    """Test that hover parameters display value correctly with value of division"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 16, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var_div1 = 1/1\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_var_multiline2():
    """Test that hover parameters display value correctly with
    multiplication and spaces. Item 2"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 17, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nINTEGER, PARAMETER :: var_multi2 = 1 * 23 + 2 /1\n```"
    ]
    validate_hover(results, ref_results)


def test_hover_parameter_nested():
    """Test that hover parameters using other parameter values works"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 4, 41)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var3 = var*var2\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_multiline_missing_type():
    """Test that hover parameters display correctly when type is split across lines"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 6, 28)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, PARAMETER :: var4 = 123\n```"]
    validate_hover(results, ref_results)


def test_hover_literal_real():
    """Test that hovering over literals shows their values REAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 47)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nREAL\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_double():
    """Test that hovering over parameters shows their type DOUBLE PRECISION"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 38)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nDOUBLE PRECISION, PARAMETER :: somevar = 23.12\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_double_sf():
    """Test that hovering over parameters shows their type scientific notation"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 7, 55)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nDOUBLE PRECISION, PARAMETER :: some = 1e-19\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_bool():
    """Test that hovering over parameters shows their values LOGICAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 8, 38)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nLOGICAL(kind=8), PARAMETER :: long_bool = .true.\n```"
    ]
    validate_hover(results, ref_results)


def test_hover_literal_bool():
    """Test that hovering over literals shows their type LOGICAL"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 8, 50)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nLOGICAL\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_str_sq():
    """Test that hovering over parameters shows their value, single quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 9, 37)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nCHARACTER(len=5), PARAMETER :: sq_str = '12345'\n```"]
    validate_hover(results, ref_results)


def test_hover_literal_string_sq():
    """Test that hovering over literals shows their values single quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 9, 48)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nCHARACTER(LEN=5)\n```"]
    validate_hover(results, ref_results)


def test_hover_parameter_str_dq():
    """Test that hovering over parameters shows their value, double quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 10, 37)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ['```fortran90\nCHARACTER(len=5), PARAMETER :: dq_str = "12345"\n```']
    validate_hover(results, ref_results)


def test_hover_literal_string_dq():
    """Test that hovering over literals shows their values double quote STRING"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "parameters.f90"
    string += hover_req(file_path, 10, 48)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nCHARACTER(LEN=5)\n```"]
    validate_hover(results, ref_results)


def test_hover_pointer_attr():
    """Test that hovering maintains the variable attributes e.g. POINTER"""
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "pointers.f90"
    string += hover_req(file_path, 1, 26)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = ["```fortran90\nINTEGER, POINTER :: val1\n```"]
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
        """```fortran90
FUNCTION fun1(arg) RESULT(fun1)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: fun1
```""",
        """```fortran90
FUNCTION fun2(arg) RESULT(fun2)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: fun2
```""",
        """```fortran90
FUNCTION fun3(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval
```""",
        """```fortran90
FUNCTION fun4(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval
```""",
        # Notice that the order of the modifiers does not match the source code
        # This is part of the test, ideally they would be identical but previously
        # any modifiers before the type would be discarded
        """```fortran90
PURE ELEMENTAL FUNCTION fun5(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER :: retval
```""",
        """```fortran90
FUNCTION fun6(arg) RESULT(retval)
 INTEGER, INTENT(IN) :: arg
 INTEGER, DIMENSION(10,10) :: retval
```""",
        """```fortran90
PURE FUNCTION outer_product(x, y) RESULT(outer_product)
 REAL, DIMENSION(:), INTENT(IN) :: x
 REAL, DIMENSION(:), INTENT(IN) :: y
 REAL, DIMENSION(SIZE(X), SIZE(Y)) :: outer_product
```""",
        """```fortran90
FUNCTION dlamch(cmach) RESULT(dlamch)
 CHARACTER :: CMACH
```""",
        """```fortran90
FUNCTION fun7() RESULT(val)
 TYPE(c_ptr) :: val
```""",
        """```fortran90
TYPE(c_ptr) FUNCTION c_loc(x) RESULT(c_loc)
```""",
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
        """```fortran90\nREAL, DIMENSION(:, :), INTENT(IN) :: arg1\n```""",
        """```fortran90\nREAL, DIMENSION( SIZE(ARG1, 1), MAXVAL([SIZE(ARG1, 2), """
        """SIZE(ARG1, 1)]) ), INTENT(OUT) :: arg2\n```""",
    ]
    validate_hover(results, ref_results)


def test_hover_recursive():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "recursive.f90"
    string += hover_req(file_path, 9, 40)
    errcode, results = run_request(string, fortls_args=["--sort_keywords"])
    assert errcode == 0
    ref_results = [
        """```fortran90
RECURSIVE SUBROUTINE recursive_assign_descending(node, vector, current_loc)
 TYPE(tree_inode), POINTER, INTENT(IN) :: node
 INTEGER, DIMENSION(:), INTENT(INOUT) :: vector
 INTEGER, INTENT(INOUT) :: current_loc
```"""
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
        """```fortran90
FUNCTION point_dist(a, b) RESULT(distance)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 REAL :: distance
```""",
        """```fortran90
FUNCTION is_point_equal_a(a, b) RESULT(is_point_equal_a)
 TYPE(point), INTENT(IN) :: a
 TYPE(point), INTENT(IN) :: b
 LOGICAL :: is_point_equal_a
```""",
    ]
    validate_hover(results, ref_results)


def test_hover_interface_as_argument():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_diagnostic_int.f90"
    string += hover_req(file_path, 19, 14)
    errcode, results = run_request(string, fortls_args=["--sort_keywords", "-n1"])
    assert errcode == 0
    ref_results = [
        # Could be subject to change
        """```fortran90
FUNCTION foo2(f, g, h) RESULT(arg3)
 FUNCTION f(x)
 FUNCTION g(x)
 FUNCTION h(x)
 REAL :: arg3
```""",
    ]
    validate_hover(results, ref_results)


def test_hover_block():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "associate_block.f90"
    string += hover_req(file_path, 4, 17)
    string += hover_req(file_path, 4, 20)
    # string += hover_req(file_path, 10, 11)    # slice of array
    errcode, results = run_request(string, fortls_args=["--sort_keywords", "-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nREAL, DIMENSION(5) :: X\n```",
        "```fortran90\nREAL :: Y\n```",
    ]
    validate_hover(results, ref_results)


def test_associate_block_func_result():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "associate_block_2.f90"
    string += hover_req(file_path, 2, 14)
    string += hover_req(file_path, 3, 9)
    errorcode, results = run_request(string, fortls_args=["--sort_keywords", "-n", "1"])
    assert errorcode == 0
    ref_results = [
        "```fortran90\nLOGICAL FUNCTION :: hi\n```",
        "```fortran90\nLOGICAL FUNCTION :: hi\n```",
    ]
    validate_hover(results, ref_results)


def test_hover_submodule_procedure():
    """Test that submodule procedures and functions with modifier keywords
    are correctly displayed when hovering.
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "diag")})
    file_path = test_dir / "diag" / "test_scope_overreach.f90"
    string += hover_req(file_path, 18, 37)
    string += hover_req(file_path, 23, 37)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        """```fortran90
PURE RECURSIVE FUNCTION foo_sp(x) RESULT(fi)
 REAL(sp), INTENT(IN) :: x
 REAL(sp) :: fi
```""",
        """```fortran90
PURE RECURSIVE FUNCTION foo_dp(x) RESULT(fi)
 REAL(dp), INTENT(IN) :: x
 REAL(dp) :: fi
```""",
    ]
    validate_hover(results, ref_results)


def test_var_type_kinds():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "parse")})
    file_path = test_dir / "parse" / "test_kinds_and_dims.f90"
    string += hover_req(file_path, 2, 24)
    string += hover_req(file_path, 2, 27)
    string += hover_req(file_path, 3, 15)
    string += hover_req(file_path, 3, 19)
    string += hover_req(file_path, 4, 20)
    string += hover_req(file_path, 4, 25)
    string += hover_req(file_path, 5, 23)
    string += hover_req(file_path, 6, 25)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nINTEGER(kind=4) :: a\n```",
        "```fortran90\nINTEGER(kind=4), DIMENSION(3,4) :: b\n```",
        "```fortran90\nINTEGER*8 :: aa\n```",
        "```fortran90\nINTEGER*8, DIMENSION(3,4) :: bb\n```",
        "```fortran90\nINTEGER(8) :: aaa\n```",
        "```fortran90\nINTEGER(8), DIMENSION(3,4) :: bbb\n```",
        "```fortran90\nREAL(kind=r15) :: r\n```",
        "```fortran90\nREAL(kind(0.d0)) :: rr\n```",
    ]
    validate_hover(results, ref_results)


def test_kind_function_result():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "parse")})
    file_path = test_dir / "parse" / "test_kinds_and_dims.f90"
    string += hover_req(file_path, 9, 18)
    string += hover_req(file_path, 14, 25)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        """```fortran90
FUNCTION foo(val) RESULT(r)
 REAL(8), INTENT(IN) :: val
 REAL*8 :: r
```""",
        """```fortran90
FUNCTION phi(val) RESULT(r)
 REAL(8), INTENT(IN) :: val
 REAL(kind=8) :: r
```""",
    ]
    validate_hover(results, ref_results)


def test_var_type_asterisk():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "parse")})
    file_path = test_dir / "parse" / "test_kinds_and_dims.f90"
    string += hover_req(file_path, 2 + 19, 18)
    string += hover_req(file_path, 2 + 19, 21)
    string += hover_req(file_path, 2 + 19, 29)
    string += hover_req(file_path, 3 + 19, 21)
    string += hover_req(file_path, 4 + 19, 17)
    string += hover_req(file_path, 5 + 19, 23)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nCHARACTER*17 :: A\n```",
        "```fortran90\nCHARACTER*17, DIMENSION(3,4) :: B\n```",
        "```fortran90\nCHARACTER*17, DIMENSION(9) :: V\n```",
        "```fortran90\nCHARACTER*(6+3) :: C\n```",
        "```fortran90\nCHARACTER*10, DIMENSION(3,4) :: D\n```",
        "```fortran90\nCHARACTER*(LEN(B)), DIMENSION(3,4) :: DD\n```",
    ]
    validate_hover(results, ref_results)


def test_var_name_asterisk():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "parse")})
    file_path = test_dir / "parse" / "test_kinds_and_dims.f90"
    string += hover_req(file_path, 26, 15)
    string += hover_req(file_path, 26, 22)
    string += hover_req(file_path, 26, 34)
    string += hover_req(file_path, 27, 15)
    string += hover_req(file_path, 28, 15)
    string += hover_req(file_path, 29, 15)
    string += hover_req(file_path, 31, 24)
    string += hover_req(file_path, 32, 32)
    # string += hover_req(file_path, 33, 32)  # FIXME: this is not displayed correctly
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nCHARACTER*17 :: AA\n```",
        "```fortran90\nCHARACTER*17, DIMENSION(3,4) :: BB\n```",
        "```fortran90\nCHARACTER*17, DIMENSION(9) :: VV\n```",
        "```fortran90\nCHARACTER*(6+3) :: CC\n```",
        "```fortran90\nCHARACTER*(LEN(A)) :: AAA\n```",
        "```fortran90\nCHARACTER*10, DIMENSION(*) :: INPUT\n```",
        "```fortran90\nCHARACTER(LEN=200) :: F\n```",
        "```fortran90\nCHARACTER(KIND=4, LEN=200), DIMENSION(3,4) :: FF\n```",
        # "CHARACTER(KIND=4, LEN=100), DIMENSION(3,4)",
    ]
    validate_hover(results, ref_results)


def test_intent():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "hover" / "intent.f90"
    string += hover_req(file_path, 2, 31)
    string += hover_req(file_path, 3, 29)
    string += hover_req(file_path, 4, 34)
    string += hover_req(file_path, 5, 35)
    string += hover_req(file_path, 6, 35)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        """```fortran90\nINTEGER(4), INTENT(IN) :: arg1\n```""",
        """```fortran90\nINTEGER, INTENT(OUT) :: arg2\n```""",
        """```fortran90\nINTEGER(4), INTENT(INOUT) :: arg3\n```""",
        """```fortran90\nINTEGER(4), INTENT(IN OUT) :: arg4\n```""",
        """```fortran90\nREAL, OPTIONAL, INTENT(IN) :: arg5\n```""",
    ]
    validate_hover(results, ref_results)


def test_multiline_func_args():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "functions.f90"
    string += hover_req(file_path, 58, 22)
    string += hover_req(file_path, 59, 22)
    string += hover_req(file_path, 60, 22)

    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nINTEGER, INTENT(IN) :: val1\n```",
        "```fortran90\nINTEGER, INTENT(IN) :: val2\n```",
        "```fortran90\nREAL :: val4\n```",
    ]
    validate_hover(results, ref_results)


def test_intrinsics():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "functions.f90"
    string += hover_req(file_path, 39, 23)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    path = (
        test_dir.parent.parent
        / "fortls"
        / "parsers"
        / "internal"
        / "intrinsic.procedures.markdown.json"
    )
    with open(path, encoding="utf-8") as f:
        intrinsics = json.load(f)
    ref_results = ["\n-----\n" + intrinsics["SIZE"]]
    validate_hover(results, ref_results)


def test_types():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "types.f90"
    string += hover_req(file_path, 3, 25)
    string += hover_req(file_path, 6, 44)
    string += hover_req(file_path, 9, 35)

    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nTYPE, ABSTRACT :: base_t\n```",
        "```fortran90\nTYPE, ABSTRACT, EXTENDS(base_t) :: extends_t\n```",
        "```fortran90\nTYPE, EXTENDS(extends_t) :: a_t\n```",
    ]
    validate_hover(results, ref_results)


def test_complicated_kind_spec():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "complicated_kind_spec.f90"
    string += hover_req(file_path, 1, 40)
    string += hover_req(file_path, 2, 40)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        '```fortran90\nREAL(int(sin(0.5))+8+len("ab((c")-3) :: y\n```',
        '```fortran90\nREAL(int(sin(0.5))+8+len("ab))c")-3) :: z\n```',
    ]
    validate_hover(results, ref_results)


def test_multiline_lexical_token():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir / "hover")})
    file_path = test_dir / "hover" / "multiline_lexical_tokens.f90"
    string += hover_req(file_path, 4, 8)
    string += hover_req(file_path, 8, 16)
    errcode, results = run_request(string, fortls_args=["-n", "1"])
    assert errcode == 0
    ref_results = [
        "```fortran90\nINTEGER :: i\n```",
        '```fortran90\nREAL(int(sin(0.5))+8+len("ab))c")-3) :: Z\n```',
    ]
    validate_hover(results, ref_results)
