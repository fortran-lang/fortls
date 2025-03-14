module mixed_case_interface_sub_child
  implicit none

contains
  subroutine foo(Func)
    interface
      function Func()
      end function Func
    end interface
  end subroutine foo
end module mixed_case_interface_sub_child
