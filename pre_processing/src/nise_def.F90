module nise_def

  type nise_grid
     integer(kind=4)                              :: nx, ny
     real(kind=4)                                 :: res
     real(kind=4)                                 :: REarth
     real(kind=4), dimension(2)                   :: grid_centre
     integer(kind=2), allocatable, dimension(:,:) :: age
     integer(kind=2), allocatable, dimension(:,:) :: extent
  end type nise_grid

  type nise_s
     type(nise_grid)                              :: north
     type(nise_grid)                              :: south
  end type nise_s

end module nise_def

module nise_interface

  interface
     function read_nsidc_nise(path_to_file, nise, north, south) &
          result (stat)
       use nise_def
       implicit none
       character(len=300)           :: path_to_file 
       integer(kind=1)              :: north
       integer(kind=1)              :: south
       type(nise_s)                 :: nise
       integer*4                    :: stat
     end function read_nsidc_nise

     subroutine deallocate_nise(nise)
       use nise_def
       implicit none
       type(nise_s)                 :: nise
     end subroutine deallocate_nise
  end interface

end module nise_interface
