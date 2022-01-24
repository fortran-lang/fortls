program test_enum
   implicit none
   enum, bind(c)

      enumerator :: red =1, blue, black =5
      enumerator yellow
      enumerator gold, silver, bronze
      enumerator :: purple
      enumerator :: pink, lavender

   endenum
end program test_enum
