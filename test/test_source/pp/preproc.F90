program preprocessor

#include "petscpc.h"
#ifdef PETSCPCDEF_H
    integer, parameter :: var = 1000
    PCType :: tmp
    print*, 999, 3.14, "some", var, PETSC_ERR_MEM
    print*, PETSC_ERR_INT_OVERFLOW
#endif
end program preprocessor