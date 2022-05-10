module mod
    implicit none
contains

    subroutine fi()
        contains
            subroutine phi()
                integer :: a(5)
                print*, size(a) ! this is an intrinsic
            end subroutine phi
    end subroutine fi
end module mod
