subroutine too_many_args(one, two, &
    three,    &
    four,     &
    five,     &
    six       )

integer, intent(in)  :: one, two,&
     three,   &
     four,    &
     five
integer, intent(out) :: six

six = five + one + four - two + &
2*three

end subroutine too_many_args
