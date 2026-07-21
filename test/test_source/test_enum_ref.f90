program enum_refs
  implicit none

  enum, bind(c)
    enumerator :: red = 1, blue = 2, green = 3
  end enum

  integer :: color
  color = blue
  if (color == blue) color = red
end program enum_refs
