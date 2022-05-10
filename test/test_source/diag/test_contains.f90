program test_contains
    implicit none
    contains
    contains
end program test_contains
contains

module test_contains2
    subroutine foo()    ! Err: before contains
    end subroutine
    contains
end module test_contains2
