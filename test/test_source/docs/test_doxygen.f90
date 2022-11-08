module test_doxygen
    implicit none

contains

    !> @brief inserts a value into an ordered array
    !!
    !! An array "list" consisting of n ascending ordered values. The method insert a
    !! "new_entry" into the array.
    !! hint: use cshift and eo-shift
    !!
    !! @param[in,out]   list    a real array, size: max_size
    !! @param[in]       n       current values in the array
    !! @param[in]       max_size    size if the array
    !! @param[in]       new_entry   the value to insert
    subroutine insert(list, n, max_size, new_entry)
        real, dimension (:), intent (inout) :: list
        integer, intent (in) :: n, max_size
        real, intent (in) :: new_entry
    end subroutine insert

    !> @brief calcs the angle between two given vectors
    !!
    !! using the standard formula:
    !!  \f$\cos \theta = \frac{ \vec v \cdot \vec w}{\abs{v}\abs{w}}\f$.
    !!
    !! @param[in]   \f$v,w\f$   real vectors
    !!                      size: n
    !! @return  a real value describing the angle. 0 if \f$\abs v\f$ or \f$\abs w\f$ below a
    !!          threshold.
    pure function calc_angle(v, w) result (theta)
        real, dimension (:), intent (in) :: v, w
        real :: theta
    end function calc_angle

end module test_doxygen
