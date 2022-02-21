! simple function
function fun1(arg)
    integer, intent(in) :: arg
    integer :: fun1
end function fun1

! function with type on definition, implied result
integer function fun2(arg)
    integer, intent(in) :: arg
end function fun2

! function with return
function fun3(arg) result(retval)
    integer, intent(in) :: arg
    integer :: retval
end function fun3

! function with type on definition and return
integer function fun4(arg) result(retval)
    integer, intent(in) :: arg
end function fun4

! function with type on definition, return and keywords
pure integer elemental function fun5(arg) result(retval)
    integer, intent(in) :: arg
end function fun5

