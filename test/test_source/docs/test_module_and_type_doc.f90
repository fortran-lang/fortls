!> module doc for doxygen_doc_mod
!!
!! with info
module doxygen_doc_mod
    implicit none

    !> Doc for a_t
    type :: a_t
    end type
end module


module ford_doc_mod
    !! Doc for ford_doc_mod
    implicit none

    type :: b_t
        !! Doc for b_t
    end type

end module


program main
    use doxygen_doc_mod
    use ford_doc_mod

    type(a_t) :: a
    type(b_t) :: b
end program
