module test_rename_implicit_result
    implicit none
contains

    ! No RESULT() clause: the function returns through an implicit result
    ! variable that shares the function's name. Renaming the function must
    ! rename the assignment below too. See issue #322.
    real pure function sind(x)
        real, intent(in), value :: x
        sind = sin(x*((4.0*atan(1.0))/180.0))
    end function

    ! Explicit RESULT() clause: the function name is not a variable in the
    ! body, so renaming the function must NOT touch `r`.
    real function withresult(x) result(r)
        real, intent(in) :: x
        r = x*3.0
    end function

end module test_rename_implicit_result
