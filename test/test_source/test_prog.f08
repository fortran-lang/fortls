PROGRAM test_program
! Here is a commonly included unicode character "–"
USE test_free, ONLY: vector, scaled_vector, module_variable, test_sig_sub
IMPLICIT NONE
!
CHARACTER(LEN=*) :: test_str1 = "i2.2,':',i2.2", test_str2 = 'i2.2,":",i2.2'
INTEGER(4) :: n,a,b,c,d
REAL(8) :: x,y
COMPLEX(8) :: xc,yc
TYPE(vector) :: loc_vector
TYPE(scaled_vector) :: stretch_vector, vector1d(1)
!
y = myfun(n,x)
CALL glob_sub(n,xc,yc)
!
CALL loc_vector%create(n)
x = loc_vector%norm()
CALL loc_vector%bound_nopass(a,x)
CALL loc_vector%bound_pass(n)
!
CALL stretch_vector%create(n)
CALL stretch_vector%set_scale(loc_vector%norm(self))
x = stretch_vector%norm()
y = stretch_vector%scale%val
!
CALL test_sig_Sub(a,b,opt2=c,opt3=d)
PRINT*, module_variable
y = stretch_vector%scale % val
y = stretch_vector % scale	%	val
y = vector1d(  1 ) % scale	%	val
END PROGRAM test_program
