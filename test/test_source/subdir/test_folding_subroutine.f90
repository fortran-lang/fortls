!test folding of subroutine arguments and of subroutine body
subroutine too_many_args(one, two, &
    three,    &
    four,     &
    five,     &
    six       )

!test multiline folding
integer, intent(in)  :: one, two,&
     three,   &
     four,    &
     five
integer, intent(out) :: six

!test_multiline_folding
six = five + one + four - two + &
2*three

end subroutine too_many_args
