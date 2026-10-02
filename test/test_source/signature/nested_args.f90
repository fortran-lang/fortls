module nested_args
contains
  subroutine foo(arg1, arg2, arg3)
    integer, intent(in) :: arg1, arg2, arg3
  end subroutine foo
  subroutine baz(str, arg)
    character(*), intent(in) :: str
    integer, intent(in) :: arg
  end subroutine baz
  subroutine bar()
    integer :: arr(4, 4)
    arr = 0
    call foo(arr(2, 3), arr(1, 1), arr(4, 4))
    call baz("a, b", 1)
    call foo("it's", "Bob's", 1)
  end subroutine bar
end module nested_args
