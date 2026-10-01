program associate_self_reference
  implicit none
  type :: inner_t
    integer :: a
  end type inner_t
  type :: outer_t
    type(inner_t) :: c
  end type outer_t
  integer :: i
  integer :: y(10) = [(i, i = 1, 10)]
  type(outer_t) :: t
  associate (y => y)
    print *, y
  end associate
  associate (t => t%c)
    print *, t%a
  end associate
end program associate_self_reference
