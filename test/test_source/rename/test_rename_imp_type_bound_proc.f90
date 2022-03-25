module mod
    implicit none

    type :: t
    contains
        procedure :: foo
    end type t

contains

    subroutine foo(self)
        class(t), intent(in) :: self
        call self%foo()
    end subroutine foo
end module mod
