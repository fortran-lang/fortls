module use_mod
integer :: val1, val2, val3
contains
end module use_mod
module use_mod_all
integer :: val4, val5
contains
end module use_mod_all

program use_main
use use_mod, only: val1, val2
use use_mod, only: val3_renamed => val3
use use_mod_all, only: val4
use use_mod_all, only: val4, val5
print*, val3_renamed
print*, val4
end program use_main
