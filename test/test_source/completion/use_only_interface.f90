module some_mod
  implicit none
  private
  public :: some_sub
  interface some_sub
    module procedure a_subroutine
    module procedure b_subroutine
  end interface
contains
  subroutine a_subroutine(x)
    integer, intent(in) :: x
    write(*,*) 'x = ', x
  end subroutine a_subroutine
  subroutine b_subroutine(x, y)
    integer, intent(in) :: x, y
    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
  end subroutine b_subroutine
end module some_mod

program main
    use some_mod, only: some_sub
    implicit none
end program main
