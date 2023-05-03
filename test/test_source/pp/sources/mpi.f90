module mpi

    integer, parameter :: MPI_COMM_WORLD = 0

contains

    subroutine MPI_Comm_size(comm, size, ierr)
        integer :: comm, size, ierr
    end subroutine MPI_Comm_size
end module mpi
