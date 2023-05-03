module mpi_f08
use iso_c_binding

    type :: MPI_Comm
        integer :: MPI_VAL
    end type MPI_Comm

    type(MPI_Comm) :: MPI_COMM_WORLD

contains

    subroutine MPI_Comm_size(comm, size, ierror)
        type(MPI_Comm),           intent(in)  :: comm       !< Communicator.
        integer(C_INT),           intent(out) :: size       !< Size of communicator.
        integer(C_INT), optional, intent(out) :: ierror     !< Error status.
    end subroutine MPI_Comm_size
end module mpi_f08
