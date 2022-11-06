MODULE test_free
USE, INTRINSIC :: iso_fortran_env, ONLY: error_unit
IMPLICIT NONE
! ą
TYPE :: scale_type
  REAL(8) :: val = 1.d0
END TYPE scale_type
!
TYPE :: vector
  INTEGER(4) :: n
  REAL(8), POINTER, DIMENSION(:) :: v => NULL()
  PROCEDURE(fort_wrap), NOPASS, POINTER :: bound_nopass => NULL()
CONTAINS
  PROCEDURE :: create => vector_create !< Doc 1
  PROCEDURE :: norm => vector_norm !< Doc 2
  PROCEDURE, PASS(self) :: bound_pass => bound_pass   !< Doc 3
END TYPE vector
!
TYPE, EXTENDS(vector) :: scaled_vector
  TYPE(scale_type) :: scale
CONTAINS
  PROCEDURE :: set_scale => scaled_vector_set !<
  PROCEDURE :: norm => scaled_vector_norm !< Doc 3
END TYPE scaled_vector
!
INTERFACE
  SUBROUTINE fort_wrap(a,b)
  INTEGER(4), INTENT(in) :: a
  REAL(8), INTENT(out) :: b
  END SUBROUTINE fort_wrap
END INTERFACE
!
LOGICAL :: module_variable
CONTAINS
!> Doc 4
SUBROUTINE vector_create(self, n)
CLASS(vector), INTENT(inout) :: self
INTEGER(4), INTENT(in) :: n !! Doc 5
self%n=n
ALLOCATE(self%v(n))
self%v=0.d0
END SUBROUTINE vector_create
!> Doc 6
FUNCTION vector_norm(self) RESULT(norm)
CLASS(vector), INTENT(in) :: self
REAL(8) :: norm
norm = SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION vector_norm
!> Doc 7
SUBROUTINE scaled_vector_set(self, scale)
CLASS(scaled_vector), INTENT(inout) :: self ! no documentation
REAL(8), INTENT(in) :: scale !< Doc 8
self%scale%val = scale
END SUBROUTINE scaled_vector_set
!> Top level docstring
FUNCTION scaled_vector_norm(self) RESULT(norm)
CLASS(scaled_vector), INTENT(in) :: self  !< self value docstring
REAL(8) :: norm !< return value docstring
norm = self%scale%val*SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION scaled_vector_norm
!
PURE REAL(8) FUNCTION unscaled_norm(self)
CLASS(scaled_vector), INTENT(in) :: self
! REAL(8) :: unscaled_norm
unscaled_norm = SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION unscaled_norm
!
SUBROUTINE test_sig_Sub(arg1,arg2,opt1,opt2,opt3)
INTEGER, INTENT(in) :: arg1,arg2
INTEGER, OPTIONAL, INTENT(in) :: opt1,opt2,opt3
END SUBROUTINE test_sig_Sub
!
SUBROUTINE bound_pass(arg1, self)
INTEGER(4), INTENT(in) :: arg1  !< Doc 9
  !! Doc 10

!> Doc 11
!! Doc 12
CLASS(vector), INTENT(inout) :: self
self%n = arg1
END SUBROUTINE bound_pass
END MODULE test_free
