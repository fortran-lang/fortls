program reexport_main
    use reexport_middle
    implicit none
contains
    subroutine masking()
        integer :: hidden_var, hidden_sub, shared_var
    end subroutine masking
end program reexport_main
