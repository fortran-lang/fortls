! simple function
function fun1(arg)
    integer, intent(in) :: arg
    integer :: fun1
end function fun1

! function with type on definition, implied result
integer function fun2(arg)
    integer, intent(in) :: arg
end function fun2
