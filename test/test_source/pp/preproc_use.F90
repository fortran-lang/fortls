program preprocessor
#ifdef USE_MPI
    use mpi
#else
    use mpi_f08
#endif
    integer :: comm_size, ierr
    call MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)
end program preprocessor
