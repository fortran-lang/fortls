program test_semicolon
    implicit none
    integer :: a = 1; character(len=1) :: v; real, parameter :: p = 0.1E-4; character(len=10), parameter :: str = "a;val;that"
    character(len=100), parameter :: str2 = "a;string;"&
    "that;becomes"//       &
    ";"&
    &"multiline";integer&
    :: b;real           &
    &,&
    parameter&
    ::&
    c&
    =&
    100&
    &0090;real :: d;real::e;real::f
    print*, "one";
    print*, str2
    print*, a; print*, p; ! a; comment; that;contains; semi-colons
end program test_semicolon
