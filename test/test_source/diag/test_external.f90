program test_external
    implicit none
    REAL, EXTERNAL :: VAL
    REAL         VAR_A
    EXTERNAL     VAR_A
    EXTERNAL     VAR_B
    REAL         VAR_B
    EXTERNAL     VAR_B   ! throw error
    REAL         VAR_A   ! throw error
    EXTERNAL     VAR_C
end program test_external
