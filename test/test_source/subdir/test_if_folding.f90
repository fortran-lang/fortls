program test_if_folding

    implicit none
    integer   :: i, j

    if (i > 0 .and. j > 0) then
        if (i > j) then
        j = j + 1
            if (mod(j,100) == 0) then
                print*, "j = ", j
            end if
        end if
    end if

    if (j == i) then
    print*, "well done"
    else   if(.true.) then
    print*, "missed something..."
    print*, "something more"
    else
    print*, "something else"
    end if

end program test_if_folding
