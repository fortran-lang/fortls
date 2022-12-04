module import_mod
  implicit none
  type :: type1
    real(kind=8) :: value
  contains
    procedure :: abs_int => abs_int1
  end type type1
  type :: type2
    type(type1) :: t
  end type type2
  interface
    subroutine abs_int1(this)
      import type1
      class(type1), intent(inout) :: this ! only type1
    end subroutine abs_int1
    subroutine abs_int2(this)
      import, only: type2
      class(type2), intent(inout) :: this ! only type2
    end subroutine abs_int2
    subroutine abs_int3(this)
      import, none
      class(type1), intent(inout) :: this ! no comp results
    end subroutine abs_int3
    subroutine abs_int4(this)
      import, all
      class(type1), intent(inout) :: this ! type1 and type2
    end subroutine abs_int4
    subroutine abs_int5(this)
      import
      class(type1), intent(inout) :: this ! type1 and type2
    end subroutine abs_int5
    subroutine abs_int6(this)
      import type1
      import type2
      class(type1), intent(inout) :: this ! type1 and type2
    end subroutine abs_int6
    subroutine abs_int7(this)
      import :: type1, type2
      class(type1), intent(inout) :: this ! type1 and type2
    end subroutine abs_int7
  end interface
end module import_mod

program main
  use import_mod
  type(type1) :: obj
  call obj%abs_int()
end program main
