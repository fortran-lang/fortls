program test_if_folding

! adding some comment lines
! to check is comment folding
! works
! as expected

    implicit none
    integer   :: i, j

    if (i > 0 .and. j > 0) then
        if (i > j) then
        j = j + 1
            if (mod(j,100) == 0) then
                print*, "j = ", j
            else if (mod(j,100) < 50) then
                print*, "j = ", j
            else
                print*, "j = ", j
            end if
        end if
    else
        print*, i-j
    end if

    if (i==0) &
    i = 1;    &
    j = 2


    if (j == i) then
      ! testing some
      ! comment lines
      ! right here
    print*, "well done"
    else   if(.true.) then
    print*, "missed something..."
    print*, "something more"
    ! random comment here
    else
    print*, "something else"
    end if

end program test_if_folding
