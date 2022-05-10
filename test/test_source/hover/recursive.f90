module tree
    type tree_inode
        integer :: value = 0
        type (tree_inode), pointer :: left=>null()
        type (tree_inode), pointer :: right=>null()
        type (tree_inode), pointer :: parent=>null()
    end type tree_inode

contains
    recursive subroutine recursive_assign_descending(node, vector, current_loc)
        type(tree_inode), pointer, intent(in) :: node
        integer, dimension(:), intent(inout)  :: vector
        integer, intent(inout)                :: current_loc

        if (associated(node)) then
            call recursive_assign_descending(node%right, vector, current_loc)
            vector(current_loc) = node%value
            current_loc = current_loc + 1
            call recursive_assign_descending(node%left, vector, current_loc)
        end if
        return
    end subroutine recursive_assign_descending
end module tree
