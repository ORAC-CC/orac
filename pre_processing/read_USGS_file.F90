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

#define ASSUME_USGS_GRID

module USGS_physiography_m

   use preproc_constants_m

   implicit none

   type USGS_t
      !    Data dimensions
      integer                                         :: nlon, nlat
      real(kind=sreal)  , allocatable, dimension(:)   :: lon, lat
      integer(kind=byte), allocatable, dimension(:,:) :: lus
      integer(kind=sint), allocatable, dimension(:,:) :: dem, lsm
      !    Missing data value
      real :: fill = sreal_fill_value
   end type USGS_t

contains

subroutine read_USGS_file(path_to_USGS_file, usgs, verbose)

   use orac_ncdf_m

   implicit none

   ! Input variables
   character(len=*), intent(in)  :: path_to_USGS_file
   logical,          intent(in)  :: verbose

   ! Output variables
   type(USGS_t),     intent(out) :: usgs

   ! Local variables
   integer :: fid

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_USGS_file()'

   call ncdf_open(fid, path_to_USGS_file, 'read_USGS_file()')

   ! Extract the array dimensions
   usgs%nlon = ncdf_dim_length(fid, 'lon', 'read_USGS_file()', verbose)
   usgs%nlat = ncdf_dim_length(fid, 'lat', 'read_USGS_file()', verbose)

   ! Read data for each variable
   allocate(usgs%lon(usgs%nlon))
   call ncdf_read_array(fid, 'lon', usgs%lon, verbose)
   allocate(usgs%lat(usgs%nlat))
   call ncdf_read_array(fid, 'lat', usgs%lat, verbose)
   allocate(usgs%dem(usgs%nlon, usgs%nlat))
   call ncdf_read_array(fid, 'dem', usgs%dem, verbose)
   allocate(usgs%lus(usgs%nlon, usgs%nlat))
   call ncdf_read_array(fid, 'lus', usgs%lus, verbose)
   allocate(usgs%lsm(usgs%nlon, usgs%nlat))
   call ncdf_read_array(fid, 'lsm', usgs%lsm, verbose)

   ! We are now finished with the main data file
   call ncdf_close(fid, 'read_USGS_file()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_USGS_file()'

end subroutine read_USGS_file

! This function reads the AHI predefined DEM, LS and LUM
subroutine read_predef_file_ahi(path_to_file, usgs, imager_geolocation, verbose)

   use orac_ncdf_m
   use imager_structures_m

   implicit none

   ! Input variables
   character(len=*),           intent(in)  :: path_to_file
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   logical,                    intent(in)  :: verbose

   ! Output variables
   type(USGS_t),               intent(out) :: usgs

   ! Local variables
   integer :: fid, start(2)

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_predef_file()'

   call ncdf_open(fid, path_to_file, 'read_predef_file_ahi()')

   start(1)    = imager_geolocation%starty
   start(2)    = imager_geolocation%startx

   ! Read data for each variable
   allocate(usgs%dem(imager_geolocation%nx, imager_geolocation%ny))
   allocate(usgs%lus(imager_geolocation%nx, imager_geolocation%ny))
   allocate(usgs%lsm(imager_geolocation%nx, imager_geolocation%ny))

   call ncdf_read_array(fid, "Elevation_Mask", usgs%dem, .false., start=start)
   call ncdf_read_array(fid, "Land_Use_Mask", usgs%lus, .false., start=start)
   call ncdf_read_array(fid, "Land_Sea_Mask", usgs%lsm, .false., start=start)

   ! We are now finished with the main data file
   call ncdf_close(fid, 'read_predef_file_ahi()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_predef_file()'

   ! In future, it may be necessary to position an AHI field within the
   ! larger swath. To do that,
   ! line0 = imager_geolocation%startx - 1
   ! line1 = line0 + imager_geolocation%ny - 1
   ! column0 = imager_geolocation%starty - 1
   ! column1 = column0 + imager_geolocation%nx - 1
   ! y_min = line0 + 1
   ! y_max = line1 + 1

end subroutine read_predef_file_ahi

! This function reads the SEVIRI predefined DEM, LS and LUM
subroutine read_predef_file_sev(path_to_file, usgs, verbose)

   use orac_ncdf_m

   implicit none

   ! Input variables
   character(len=*), intent(in)  :: path_to_file
   logical,          intent(in)  :: verbose

   ! Output variables
   type(USGS_t),     intent(out) :: usgs

   ! Local variables
   integer :: fid

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_predef_file()'

   call ncdf_open(fid, path_to_file, 'read_predef_file_sev()')

   ! Extract the array dimensions
   usgs%nlon = ncdf_dim_length(fid, 'x', 'read_USGS_file()', verbose)
   usgs%nlat = ncdf_dim_length(fid, 'y', 'read_USGS_file()', verbose)

   ! Read data for each variable
   allocate(usgs%dem(usgs%nlon, usgs%nlat))
   allocate(usgs%lus(usgs%nlon, usgs%nlat))
   allocate(usgs%lsm(usgs%nlon, usgs%nlat))

   call ncdf_read_array(fid, 'Elevation_Mask', usgs%dem, verbose)
   call ncdf_read_array(fid, 'Land_Use_Mask', usgs%lus, verbose)
   call ncdf_read_array(fid, 'Land_Sea_Mask', usgs%lsm, verbose)

   ! We are now finished with the main data file
   call ncdf_close(fid, 'read_predef_file_sev()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_predef_file()'

end subroutine read_predef_file_sev

!-----------------------------------------------------------------------------
function nearest_USGS(imager_lat, imager_lon, usgs) &
     result(nearest_xy)

   implicit none

   ! input variables
   real(kind=sreal),     intent(in) :: imager_lat, imager_lon
   type(USGS_t),         intent(in) :: usgs

   ! output variable
   integer(kind=sint), dimension(2) :: nearest_xy

#ifdef ASSUME_USGS_GRID
   ! The USGS grid starts at (-179.975, 89.975)
   nearest_xy(1) = floor((90. - imager_lat) / 180. * real(usgs%nlat)) + 1
   nearest_xy(2) = floor((imager_lon + 180.) / 360. * real(usgs%nlon)) + 1

#else
   nearest_xy(1) = minloc(abs(usgs%lat - imager_lat), dim=1)
   nearest_xy(2) = minloc(abs(usgs%lon - imager_lon), dim=1)

#endif

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
