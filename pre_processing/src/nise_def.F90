module nise_m

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

contains

include 'read_nsidc_nise.F90'

end module nise_m
