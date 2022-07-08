!-------------------------------------------------------------------------------
! Name: read_nsidc_nise.F90
!
! Purpose:
! Open and read the Near-real-time Ice and Snow Extent (NISE) from
! the National Snow and Ice Data Centre.
! See http://nsidc.org/data/nise1.html for more information
!
! Description and Algorithm details:
!
! Arguments:
! Name         Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_file character    in      File name (and path) to read
! nise         type(nise_t) out     NISE output structure
! north        integer      in      If not zero, data for Northern
!                                   Hemisphere will be read
! south        integer      in      If not zero, data for Southern
! stat         integer      out     (Return value) Status value returned by the
!                                   various hdf-eos API routines. If an error
!                                   occurs, it will be returned with the value
!                                   -1, otherwise returned as 0.
!
! History:
! 2012/04/26, GT: Original
! 2013/02/25, GT: Added explicit type casts where copying data into data%extent.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function extract_nise_grid(fid, name, gridlist, data, verbose) result(stat)

   implicit none

   ! Arguments
   integer(kind=4),   intent(in)    :: fid
   character,         intent(in)    :: name*(*)
   character,         intent(in)    :: gridlist*(*)
   type(nise_grid_t), intent(inout) :: data
   logical,           intent(in)    :: verbose

   ! Return value
   integer(kind=4)                :: stat
   ! Local variables
   integer                        :: comma
   integer(kind=4)                :: gid
   integer(kind=4)                :: xdim, ydim
   real(kind=8),    dimension(2)  :: upleft, lowright
   integer(kind=4)                :: proj, zone, sphere
   real(kind=8),    dimension(13) :: param
   integer(kind=4), dimension(2)  :: start, stride, edge
   integer(kind=1), allocatable, dimension(:,:) :: tmp_data
   ! External functions (the hdf-eos library)
   integer*4, external            :: gdattach
   integer*4, external            :: gdgridinfo
   integer*4, external            :: gdprojinfo
   integer*4, external            :: gdrdfld
   integer*4, external            :: gddetach

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering extract_nise_grid()'

   if (verbose) write(*,*) 'name: ', trim(name)
   if (verbose) write(*,*) 'gridlist: ', trim(gridlist)

   ! As the gdprojinfo returns an assumed shape array, it must have a
   ! interface...
   !interface
   !   function gdprojinfo(gid, proj, zone, sphere, param) result(stat)
   !     integer*4               :: stat
   !     integer*4               :: gid, proj, zone, sphere
   !     real*8, pointer, dimension(:) :: param
   !   end function gdprojinfo
   !end interface

   ! Locate the comma in the gridlist, so that we can split the string
   comma = scan(gridlist, ",")

   ! Attach to and read the requested grid
   if (index(gridlist, trim(name)) .lt. comma) then
      if (verbose) write(*,*) 'Attaching to grid ', trim(gridlist(1:comma-1))
      gid = gdattach(fid, trim(gridlist(1:comma-1)))
   else
      if (verbose) write(*,*) 'Attaching to grid ', trim(gridlist(1:comma-1))
      gid = gdattach(fid, trim(gridlist(comma+1:)))
   end if

   ! Extract grid information and put relevant values in output structure
   if (verbose) write(*,*) 'Reading projection and grid info'
   stat = gdprojinfo(gid, proj, zone, sphere, param)
   stat = gdgridinfo(gid, xdim, ydim, upleft, lowright)

   data%nx = xdim
   data%ny = ydim
   ! First element of the grid parameters (param) should be the radius of
   ! the Earth
   data%REarth = real(param(1)) / 1000.0
   ! Fifth and sixth (I think!) define the centre point of the grid
   data%grid_centre = real(param(5:6)) / 1e6
   ! Use the upper left and lower right corners to define the grid
   ! resolution. We make the assumption that resolution is the same in both
   ! directions
   data%res = (lowright(1) - upleft(1)) / (1000.0 * real(xdim))

   if (verbose) write(*,*) 'Grid size is       ', xdim, ' x ', ydim
   if (verbose) write(*,*) 'Grid centre is     ', data%grid_centre
   if (verbose) write(*,*) 'Grid resolution is ', data%res
   if (verbose) write(*,*) 'Earth radius is    ', data%REarth

   ! Now read the data itself
   start(:)  = 0
   stride(:) = 1
   edge(1) = xdim
   edge(2) = ydim

   allocate(tmp_data(xdim,ydim))
   allocate(data%extent(xdim,ydim))
   allocate(data%age(xdim,ydim))
   if (verbose) write(*,*) 'Reading data fields'
   ! Note: The data in the HDF file is stored as unsigned bytes. Fortran 90
   ! doesn't have unsigned integers, so the data gets corrupted (all values
   ! greater than 127 come out negative). We thus copy the data out of the
   ! integer*1 tmp_data into the integer*2 output data arrays, correcting
   ! the spurious negatives
   stat = gdrdfld(gid, 'Extent', start, stride, edge, tmp_data)
   where(tmp_data .lt. 0)
      data%extent = 256 + int(tmp_data, kind=2)
   else where
      data%extent = int(tmp_data, kind=2)
   end where
   stat = gdrdfld(gid, 'Age', start, stride, edge, tmp_data)
   where(tmp_data .lt. 0)
      data%age = 256 + tmp_data
   else where
      data%age = tmp_data
   end where
   deallocate(tmp_data)
   ! Detact from the grid
   stat = gddetach(gid)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving extract_nise_grid()'

end function extract_nise_grid

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

subroutine deallocate_nise(nise)

   implicit none

   type(nise_t), intent(inout) :: nise

   if (allocated(nise%north%age)) then
      deallocate(nise%north%age)
      deallocate(nise%north%extent)
   end if
   if (allocated(nise%south%age)) then
      deallocate(nise%south%age)
      deallocate(nise%south%extent)
   end if

end subroutine deallocate_nise

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

function read_nsidc_nise(path_to_file, nise, north, south, verbose) &
     result (stat)

   use preproc_constants_m

   implicit none

   ! Input variables
   character(len=*), intent(in)  :: path_to_file
   integer(kind=1),  intent(in)  :: north
   integer(kind=1),  intent(in)  :: south
   logical,          intent(in)  :: verbose

   ! Output variables
   type(nise_t),     intent(out) :: nise
   integer(kind=4)               :: stat ! Function return

   ! Local variables
   integer(kind=4)           :: fid
   character(len=512)        :: gridlist
   integer(kind=4)           :: gridlistlen

   integer(kind=4), external :: gdinqgrid
   integer(kind=4), external :: gdopen
   integer(kind=4), external :: gdgetfill
   integer(kind=4), external :: gdclose

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_nsidc_nise()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'north: ', north
   if (verbose) write(*,*) 'south: ', south

   ! First off, find out what grids are in the file. There should be
   ! two; one for the Northern Hemisphere and one for the Southern.
   ! We'll need it's name to "attach" to it and extract the data
   if (verbose) write(*,*) 'Reading ice file ', trim(path_to_file)
   stat = gdinqgrid(path_to_file, gridlist, gridlistlen)

   if (stat .ne. 2) then
      write(*,*) 'ERROR: read_nsidc_nise(): Invalid with number of grids: ', stat
      stop error_stop_code
   end if
   if (verbose) then
      write(*,*) 'gridlist = ', trim(gridlist)
      write(*,*) 'gridlistlen = ', gridlistlen
   end if

   ! Open the datafile and get a file descriptor, and then attach to the
   ! first grid, as defined above
   fid = gdopen(path_to_file, 1)

   ! Call the extract_nise_grid function to read each grid
   stat = extract_nise_grid(fid, 'North', gridlist, nise%north, verbose)
   stat = extract_nise_grid(fid, 'South', gridlist, nise%south, verbose)

   if (verbose) write(*,*) 'Closing file'

   ! Close the hdf file
   stat = gdclose(fid)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_nsidc_nise()'

end function read_nsidc_nise
