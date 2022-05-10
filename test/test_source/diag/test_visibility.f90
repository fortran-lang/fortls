program test_visibility
    use nonexisting_module  ! Info: missing module
    implicit none
    use mod
end program test_visibility
public
