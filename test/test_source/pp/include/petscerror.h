#if !defined (PETSCERRORDEF_H)
#define PETSCERRORDEF_H

#define PETSC_ERR_MEM              55
#define PETSC_ERR_INT_OVERFLOW     84
#define PETSC_ERR_FLOP_COUNT       90

#if defined PETSC_ERR_MEM || defined PETSC_ERR_INT_OVERFLOW
#define SUCCESS .true.
#endif

#endif
