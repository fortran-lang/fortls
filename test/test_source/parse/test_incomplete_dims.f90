
! Tests that the parser will not break, when parsing incomplete variables
! constructs. This is particularly important for autocompletion.
program test_incomplete_dims
    implicit none
    integer   :: dim_val(1, 2
    character :: char_val*(10
    integer   :: (
end program test_incomplete_dims
