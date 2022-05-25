subroutine normal_kinds()
    integer, parameter :: r15 = selected_real_kind(15)
    integer(kind=4) :: a, b(3,4)
    integer*8 aa, bb(3,4)
    integer(8) :: aaa, bbb(3,4)
    real(kind=r15) :: r
    real(kind(0.d0)) :: rr
end subroutine normal_kinds

real*8 function foo(val) result(r)
    real(8), intent(in) :: val
    r = val
end function foo

real(kind=8) function phi(val) result(r)
    real(8), intent(in) :: val
    r = val
end function phi

subroutine character_len_parsing(input)
    ! global variable_type * length variable_name1, variable_name2,...
    CHARACTER*17 A, B(3,4), V(9)
    CHARACTER*(6+3) C
    CHARACTER*10D(3,4)
    CHARACTER*(LEN(B))DD(3,4)
    ! local variable_type variable_name1 * length, variable_name2 * length,...
    CHARACTER AA*17, BB(3,4)*17, VV(9)*17
    CHARACTER CC*(6+3)
    CHARACTER AAA*(LEN(A))
    CHARACTER INPUT(*)*10
    ! explicit len and kind for characters
    CHARACTER(LEN=200) F
    CHARACTER(KIND=4, LEN=200) FF(3,4)
    CHARACTER(KIND=4, LEN=200) AAAA(3,4)*100

    ! override global length with local length
    CHARACTER*10 BBB(3,4)*(LEN(B))      ! has the length of len(b)
    CHARACTER*10CCC(3,4)*(LEN(B))       ! no-space
    CHARACTER(KIND=4) BBBB(3,4)*(LEN(B))      ! cannot have *10(kind=4) or vice versa

    INTEGER((4)) INT_KIND_IMP   ! FIXME: (()) trips up the regex
end subroutine character_len_parsing
