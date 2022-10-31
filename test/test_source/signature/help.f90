module sig_help_markdown
  implicit none
  private

contains
  !> Top level Doc
  subroutine sub2call(arg1, arg2)
    integer, intent(in) :: arg1 !< Doc for arg1
    integer, intent(in), optional :: arg2 !< Doc for arg2
    print*, "sub2call: arg1=", arg1
    if (present(arg2)) print*, "sub2call: arg2=", arg2
  end subroutine sub2call

  !> Top level Doc
  function fun2fcall(arg1, arg2) result(res)
    integer, intent(in) :: arg1 !< Doc for arg1
    integer, intent(in), optional :: arg2 !< Doc for arg2
    integer :: res
    res = arg1
    if (present(arg2)) res = res + arg2
  end function fun2fcall

  subroutine calling()
    call sub2call(1, 2)
    print*, "fun2fcall(1, 2)=", fun2fcall(1, 2)
  end subroutine calling

end module sig_help_markdown
