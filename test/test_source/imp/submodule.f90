module parent_mod
  implicit none
  type :: typ
    real(kind=8) :: value
  contains
    procedure :: method1 => submod_method1
  end type typ
  interface
    module subroutine submod_method1(this)
      class(typ), intent(inout) :: this
    end subroutine submod_method1
    module subroutine submod_method2(this, value)
      class(typ), intent(inout) :: this
      real, intent(in) :: value
    end subroutine submod_method2
  end interface
end module parent_mod
submodule(parent_mod) submod
contains
  module subroutine submod_method1(this)
    class(typ), intent(inout) :: this
    this%value = 0
  end subroutine submod_method1
end submodule submod
