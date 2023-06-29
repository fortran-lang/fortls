from fortls.helper_functions import detect_fixed_format


def test_detect_fixed_format():
    assert detect_fixed_format([" free format"]) is False
    assert detect_fixed_format([" INTEGER, PARAMETER :: N = 10"]) is False
    assert detect_fixed_format(["C Fixed format"]) is True
    assert detect_fixed_format(["trailing line & ! comment"]) is False
    assert (
        detect_fixed_format(
            ["#if defined(A) && !defined(B)", "C Fixed format", "#endif"], preproc=True
        )
        is True
    )
    assert (
        detect_fixed_format(
            ["#if defined(A) && !defined(B)", " free format", "#endif"], preproc=True
        )
        is False
    )
