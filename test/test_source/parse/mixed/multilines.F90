program multiline_tests
  implicit none
  integer :: result
  character(len=100) :: str

  ! Test: Simple multi-line continuation
  result = 1 + &
    2 + &
    3

  ! Test: Multi-line continuation with a preprocessor directive
  result = 10 + &
#ifdef TEST
    20 + &
#endif
    30

  ! Test: Multi-line continuation with string concatenation
  str = 'Hello' // &
  & ' ' // &
  &  'World'

  ! Test: Multi-line continuation with mixed preprocessor and arithmetic operations
  result = &
#ifdef MULT
    (10*2) + &
#else
    (10 * 3) + &
#endif
  & 10 * 4

  ! Test: Multi-line continuation with C preprocessor && sequence
  result = 100 + &
#if defined(TEST) && defined(MULT)
  &(20) + &
#endif
  &10

  ! Test: multiplee Multi-line continuation with C preprocessor and comments
  result = 1000 + & ! Comment 0
#if defined( TEST ) && defined( MULT )
  &100 + &  ! Comment 1
  &200+&    !! Comment 2
#else
    500 + & !!! Comment 3
#endif
  &600

end program multiline_tests
