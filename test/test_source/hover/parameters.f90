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
    integer, parameter :: var_no_space=123
    integer, parameter :: var_more_space  =   123
    integer, parameter :: var_sum1  = 1 + 23
    integer, parameter :: var_ex1  = 1 - 23
    integer, parameter :: var_mul1  = 1  *   23
    integer, parameter :: var_div1  = 1/1
    INTEGER, PARAMETER :: var_multi2 = 1 *   &
                                        23 + &
                                        2 /1        ! comment
    INTEGER(4), PARAMETER :: SIG$ERR   = -1
end program params
