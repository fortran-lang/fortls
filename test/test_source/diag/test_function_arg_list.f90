program test_arg_names_as_keywords
    implicit none
    integer, parameter :: impure = 8
contains
    subroutine foo(recursive, ierr)
        integer, intent(in) :: recursive
        integer, intent(out) :: ierr
        print*, recursive
    end subroutine foo
    real(8) impure elemental function foo2(recursive, elemental) result(pure)
        integer, intent(in) :: recursive, elemental
    end function foo2
    real( kind = impure ) pure elemental function foo3(recursive) result(pure)
        integer, intent(in) :: recursive
    end function foo3
    subroutine foo4(&
        recursive, &
        ierr)
        integer, intent(in) :: recursive
        integer, intent(out) :: ierr
        print*, recursive
    end subroutine foo4
    pure real(impure) function foo5(recursive) result(val)
        integer, intent(in) :: recursive
    end function foo5
end program test_arg_names_as_keywords
