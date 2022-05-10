program params
    implicit none
    integer, parameter :: var = &
                          1000, &
                          var2 = 23, var3 = &
                          var*var2, &
                          var4 = 123
    double precision, parameter :: somevar = 23.12, some = 1e-19
    logical(kind=8), parameter :: long_bool = .true.
    character(len=5), parameter :: sq_str = '12345'
    character(len=5), parameter :: dq_str = "12345"
end program params