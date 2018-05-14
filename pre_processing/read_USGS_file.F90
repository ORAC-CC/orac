!-------------------------------------------------------------------------------
! Name: read_USGS_file.F90
!
! Purpose:
! Read USGS global land use and DEM data.
!
! Description and Algorithm details:
!
! Arguments:
! Name              Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_USGS_file string in          Full path to USGS file
!
! History:
! 2014/09/23, OS: writes code to read data from USGS file.
! 2015/07/03, OS: added error status variable to nc_open call
! 2017/02/10, SP: Allow reading LSM, LUM, DEM from external file (ExtWork)
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module USGS_physiography_m

   use preproc_constants_m

   implicit none

   type USGS_t
      !    Data dimensions
      integer(4)                                      :: nlon, nlat
      real(kind=sreal)  , allocatable, dimension(:)   :: lon, lat
      integer(kind=byte), allocatable, dimension(:,:) :: lus
      integer(kind=sint), allocatable, dimension(:,:) :: dem, lsm
      !    Missing data value
      real :: fill=sreal_fill_value
   end type USGS_t

   integer :: usgs_lon_dim, usgs_lat_dim

contains

function read_USGS_file(path_to_USGS_file, usgs, verbose) result (stat)

   use orac_ncdf_m

   implicit none

   ! Input variables
   character(len=path_length), intent(in) :: path_to_USGS_file
   logical,                    intent(in) :: verbose

   ! Output variables
   type(USGS_t), intent(out) :: usgs
   integer(kind=sint)        :: stat

   ! Local variables
   integer :: fid, usgs_lat_id, usgs_lon_id
   integer :: nDim, nVar, nAtt, uDimID, ForNM

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_USGS_file()'

   call nc_open(fid, path_to_USGS_file)
   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   ! dimension IDs
   stat = nf90_inq_dimid(fid, 'lon', usgs_lon_id)
   stat = nf90_inq_dimid(fid, 'lat', usgs_lat_id)

   ! Extract the array dimensions
   stat = nf90_inquire_dimension(fid, usgs_lon_id, len=usgs_lon_dim)
   stat = nf90_inquire_dimension(fid, usgs_lat_id, len=usgs_lat_dim)

   ! Read data for each variable
   allocate(usgs%lon(usgs_lon_dim))
   call nc_read_array(fid, 'lon', usgs%lon, verbose)
   allocate(usgs%lat(usgs_lat_dim))
   call nc_read_array(fid, 'lat', usgs%lat, verbose)
   allocate(usgs%dem(usgs_lon_dim,usgs_lat_dim))
   call nc_read_array(fid, 'dem', usgs%dem, verbose)
   allocate(usgs%lus(usgs_lon_dim,usgs_lat_dim))
   call nc_read_array(fid, 'lus', usgs%lus, verbose)
   allocate(usgs%lsm(usgs_lon_dim,usgs_lat_dim))
   call nc_read_array(fid, 'lsm', usgs%lsm, verbose)

   ! We are now finished with the main data file
   stat = nf90_close(fid)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_USGS_file()'

end function read_USGS_file

function read_predef_file(path_to_file, usgs, verbose) result (stat)

   use orac_ncdf_m

   implicit none

   ! Input variables
   character(len=path_length), intent(in) :: path_to_file
   logical,                    intent(in) :: verbose

   ! Output variables
   type(USGS_t), intent(out) :: usgs
   integer(kind=sint)        :: stat

   ! Local variables
   integer :: fid, usgs_lat_id, usgs_lon_id
   integer :: nDim, nVar, nAtt, uDimID, ForNM

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_predef_file()'

   call nc_open(fid, path_to_file)
   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   ! dimension IDs
   stat = nf90_inq_dimid(fid, 'x', usgs_lon_id)
   stat = nf90_inq_dimid(fid, 'y', usgs_lat_id)

   ! Extract the array dimensions
   stat = nf90_inquire_dimension(fid, usgs_lon_id, len=usgs_lon_dim)
   stat = nf90_inquire_dimension(fid, usgs_lat_id, len=usgs_lat_dim)

   ! Read data for each variable
   allocate(usgs%dem(usgs_lon_dim,usgs_lat_dim))
   allocate(usgs%lus(usgs_lon_dim,usgs_lat_dim))
   allocate(usgs%lsm(usgs_lon_dim,usgs_lat_dim))

   call nc_read_array(fid, 'Elevation_Mask', usgs%dem, verbose)
   call nc_read_array(fid, 'Land_Use_Mask', usgs%lus, verbose)
   call nc_read_array(fid, 'Land_Sea_Mask', usgs%lsm, verbose)

   ! We are now finished with the main data file
   stat = nf90_close(fid)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_predef_file()'

end function read_predef_file

!-----------------------------------------------------------------------------

function nearest_USGS(imager_lat, imager_lon, usgs) &
     result(nearest_xy)

   implicit none

   ! input variables
   real(kind=sreal), intent(in) :: imager_lat, imager_lon
   type(USGS_t),     intent(in) :: usgs

   ! output variable
   integer(kind=sint),dimension(2) :: nearest_xy

   ! local variables
   integer :: i
   real(kind=sreal) :: imager_latlon,nearest_latlon
   integer(kind=lint) :: latlon_1000,latlon_base,latlon_dummy,check50

   do i = 1, 2 ! loop over lat and lon
      if (i .eq. 1) then
         imager_latlon = imager_lat
      else
         imager_latlon = imager_lon
      end if
      latlon_1000 = floor(imager_latlon * 1000.)
      latlon_base = floor(imager_latlon * 10.) * 100
      check50 = latlon_1000 - latlon_base
      if (imager_latlon .gt. 0) then
         if (check50 .ge. 50) then
            latlon_dummy = latlon_base + 75
         else
            latlon_dummy = latlon_base + 25
         end if
      else
         if (check50 .le. -50) then
            latlon_dummy = latlon_base - 75
         else
            latlon_dummy = latlon_base - 25
         end if

      end if
      nearest_latlon = latlon_dummy / 1000.
      if (i .eq. 1) then
         nearest_xy(1) = minloc(abs(usgs%lat - nearest_latlon), DIM=1)
      else
         nearest_xy(2) = minloc(abs(usgs%lon - nearest_latlon), DIM=1)
      end if
   end do

end function nearest_USGS

!-----------------------------------------------------------------------------

subroutine deallocate_usgs(usgs)

   implicit none

   type(USGS_t), intent(inout) :: usgs

   if (allocated(usgs%lon)) deallocate(usgs%lon)
   if (allocated(usgs%lon)) deallocate(usgs%lat)
   if (allocated(usgs%dem)) deallocate(usgs%dem)
   if (allocated(usgs%lus)) deallocate(usgs%lus)
   if (allocated(usgs%lsm)) deallocate(usgs%lsm)

end subroutine deallocate_usgs

!-----------------------------------------------------------------------------

end module USGS_physiography_m
