program bug
    implicit none
contains

    !> @brief Test subroutine
    !!
    !! @param[in] bar This is a description for bar bar.
    !! It has multiple lines:
    !! * b - this is b
    !! * a - this is a
    !!
    !! @param[in] baz baz baz is a nonsense word
    subroutine foo(bar, baz)
        character(len=*), intent(in) :: bar
        character(len=*), intent(in) :: baz
    end subroutine foo
end program bug
