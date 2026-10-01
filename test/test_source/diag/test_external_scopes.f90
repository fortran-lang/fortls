subroutine type_first_1
  integer bar
  external bar
end subroutine type_first_1

subroutine type_first_2
  integer bar
  external bar
end subroutine type_first_2

subroutine external_first_1
  external baz
  real baz
end subroutine external_first_1

subroutine external_first_2
  external baz
  real baz
end subroutine external_first_2

subroutine external_only
  external qux
end subroutine external_only

subroutine external_then_type
  external qux
  real qux
end subroutine external_then_type
