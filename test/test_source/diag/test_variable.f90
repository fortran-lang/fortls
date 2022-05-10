program test_variable
    integer :: val
    contains
    subroutine foo()
        integer :: val  ! Warn: shadows parent
    end subroutine
end program test_variable
