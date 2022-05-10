module test_vis_mod

implicit none
private

type :: some_type
end type some_type
integer :: some_var
public some_var

contains
subroutine some_sub
end subroutine some_sub
end module test_vis_mod
