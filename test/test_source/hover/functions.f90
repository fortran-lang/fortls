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

! function with type on definition and return
function fun6(arg) result(retval)
    integer, intent(in) :: arg
    integer, dimension(10,10) :: retval
end function fun6

! functions with complex result type
pure function outer_product(x, y)
    real, dimension(:), intent(in) :: x, y
    real, dimension(size(x), size(y)) :: outer_product
    integer :: i, j
    forall (i=1:size(x))
        forall (j=1:size(y))
            outer_product(i, j) = x(i) * y(j)
        end forall
    end forall
end function outer_product

! functions with no result type, common in interfaces
function dlamch(CMACH)
    character :: CMACH
end function dlamch

! intrinsic functions like c_loc display a return type
function fun7() result(val)
    use, intrinsic :: iso_c_binding
    integer, dimension(1), target :: ar
    type(c_ptr) :: val
    val = c_loc(ar)
end function fun7

real function foobar(val1, &
                     val2) &
              result(val4)
integer, intent(in) :: val1, val2
end function foobar
