block data named_data
  implicit none
  integer :: a
  common /cblock_a/ a
  data a /1/
end block data named_data

blockdata
  implicit none
  integer :: b
  common /cblock_b/ b
  data b /2/
endblockdata

block data other_data
  integer :: c
  common /cblock_c/ c
end

program test_block_data
  implicit none
  integer :: blockdata_count
  blockdata_count = 1
  data: block
    integer :: i
    i = 1
  end block data
  call after_block_data()
end program test_block_data

subroutine after_block_data()
end subroutine after_block_data
