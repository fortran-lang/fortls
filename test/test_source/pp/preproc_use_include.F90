program preprocessor
#include "mpi.h"
    integer :: comm_size, ierr
    call MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)
    print*, omp_get_num_threads()
end program preprocessor
