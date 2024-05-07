module visibility
    private :: name
    private :: generic_interf
    interface name
        module procedure :: name_sp
    end interface name
    interface
        subroutine generic_interf(noop)
            integer, intent(in) :: noop
        end subroutine generic_interf
    end interface
contains
    subroutine name_sp(val)
        real(4), intent(in) :: val
        print *, 'name_sp', val
    end subroutine name_sp
end module visibility
