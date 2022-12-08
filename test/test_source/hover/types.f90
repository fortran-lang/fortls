module some_mod
    implicit none

    type, abstract :: base_t
    end type

    type, abstract, extends(base_t) :: extends_t
    end type

    type, extends(extends_t) :: a_t
    end type
end module
