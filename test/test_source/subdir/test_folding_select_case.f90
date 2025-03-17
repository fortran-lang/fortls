program select_case

integer :: to_test, six

select case (to_test)
case (1)
    six = 6*to_test
case (2)
    six = 3*to_test
case (3)
    six = 2*to_test
case default
    six = 6
end select

end program select_case
