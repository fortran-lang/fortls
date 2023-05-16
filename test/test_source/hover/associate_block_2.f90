program associate_block_2
  implicit none
  associate (hi => say_hi())
    if (hi) print *, 'Bye'
  end associate
contains
  logical function say_hi()
    say_hi = .true.
  end
end program
