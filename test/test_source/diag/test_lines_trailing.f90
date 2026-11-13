program test_lines_trailing
    implicit none
    integer :: x = 1  ! a trailing comment that takes this line past eighty columns
    integer :: y = 2  ! a trailing comment that is long enough to take this line past one hundred columns
    character(len=90) :: msg = "a string with a ! in it that makes this line longer than eighty"
end program test_lines_trailing
