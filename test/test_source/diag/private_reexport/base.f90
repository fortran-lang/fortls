module reexport_base
    implicit none
    integer :: hidden_var, shared_var
contains
    subroutine hidden_sub()
    end subroutine hidden_sub
end module reexport_base
