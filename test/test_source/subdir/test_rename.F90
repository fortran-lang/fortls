module rename_mod1
real(8) :: var1
end module rename_mod1
!
module rename_mod2
use rename_mod1, only: renamed_var1 => var1
integer :: originalname
end module rename_mod2
!
subroutine test_rename_sub()
use rename_mod2, only : localname => originalname, renamed_var2 => renamed_var1
implicit none
!
localname = 4
renamed_var2 = 4
end subroutine test_rename_sub
