subroutine intent(arg1, arg2, arg3, arg4, arg5)
    implicit none
    integer(4), intent(in) :: arg1
    integer, intent(out) :: arg2
    integer(4), intent(inout) :: arg3
    integer(4), intent(in out) :: arg4
    real, optional, intent(in) :: arg5
end subroutine intent
