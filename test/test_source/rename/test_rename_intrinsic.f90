module test_rename_intrinsic
    implicit none
    interface size
        module procedure size_comp
    end interface size
contains

    subroutine size_comp(val, ret)
        integer, intent(in) :: val(:)
        integer, intent(out) :: ret
        integer, dimension(5) :: fixed
        ret = maxval([size(val), size(fixed)])
    end subroutine size_comp

end module test_rename_intrinsic

program driver
    use test_rename_intrinsic
    implicit none
    integer, dimension(10) :: val
    integer, dimension(5) :: tmp
    integer :: sz
    call size(val, sz)  ! This is fortran_sub and should be renamed
    print*, size(val)   ! This is an intrinsic, should be skipped in renaming
end program driver
