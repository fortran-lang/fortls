module m
    interface
        module subroutine sub(arg)
            integer :: arg
        end subroutine
    end interface
end module m

submodule (m) n

    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
    implicit none

    integer, parameter :: sp = selected_real_kind(6)
    integer, parameter :: dp = selected_real_kind(15)

contains

    pure recursive module function foo_sp(x) result(fi)
        real(sp), intent(in) :: x
        real(sp) :: fi
    end function foo_sp

    pure recursive module function foo_dp(x) result(fi)
        real(dp), intent(in) :: x
        real(dp) :: fi
    end function foo_dp
end submodule n
