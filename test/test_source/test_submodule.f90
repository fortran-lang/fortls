submodule( foo_module ) submodule1
   implicit none
   contains
   module procedure foo1
      WRITE(*,"(A)") "testing :: "// trim(a) // "::"// trim(b)
   end procedure foo1
end submodule submodule1
