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
      real :: fill=sreal_fill_value
   end type USGS_t

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

   call nc_open(fid, path_to_USGS_file, 'read_USGS_file()')
   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   ! Extract the array dimensions
   usgs%nlon = nc_dim_length(fid, 'lon', 'read_USGS_file()', verbose)
   usgs%nlat = nc_dim_length(fid, 'lat', 'read_USGS_file()', verbose)

   ! Read data for each variable
   allocate(usgs%lon(usgs%nlon))
   call nc_read_array(fid, 'lon', usgs%lon, verbose)
   allocate(usgs%lat(usgs%nlat))
   call nc_read_array(fid, 'lat', usgs%lat, verbose)
   allocate(usgs%dem(usgs%nlon, usgs%nlat))
   call nc_read_array(fid, 'dem', usgs%dem, verbose)
   allocate(usgs%lus(usgs%nlon, usgs%nlat))
   call nc_read_array(fid, 'lus', usgs%lus, verbose)
   allocate(usgs%lsm(usgs%nlon, usgs%nlat))
   call nc_read_array(fid, 'lsm', usgs%lsm, verbose)

   ! We are now finished with the main data file
   call nc_close(fid, 'read_USGS_file()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_USGS_file()'

end function read_USGS_file

! This function reads the AHI predefined DEM, LS and LUM
function read_predef_file_ahi(path_to_file, usgs, imager_geolocation, verbose) result (stat)

   use orac_ncdf_m
   use imager_structures_m

   implicit none

   ! Input variables
   character(len=path_length), intent(in) :: path_to_file
   type(imager_geolocation_t), intent(in) :: imager_geolocation
   logical,                    intent(in) :: verbose

   ! Output variables
   type(USGS_t), intent(out) :: usgs
   integer(kind=sint)        :: stat

   ! Local variables
   integer :: fid, vid
   integer :: nDim, nVar, nAtt, uDimID, ForNM
   integer,dimension(2)            ::      start,countval
   integer :: startx,starty,nx,ny,line0,line1,column0,column1
   integer :: x_min,y_min,x_max,y_max,x_size,y_size

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_predef_file()'

   call nc_open(fid, path_to_file, 'read_predef_file_ahi()')
   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny


   line0   = startx - 1
   line1   = startx - 1 + ny - 1
   column0 = starty - 1
   column1 = starty - 1 + nx - 1

   y_min = line0 + 1
   y_max = line1 + 1
   y_size = line1-line0 +1

   x_min = column0 + 1
   x_max = column1 + 1
   x_size = column1-column0 +1


   start(1)        =       x_min
   start(2)        =       y_min

   countval(1)     =       x_max - x_min + 1
   countval(2)     =       y_max - y_min + 1

   ! Read data for each variable
   allocate(usgs%dem(countval(1), countval(2)))
   allocate(usgs%lus(countval(1), countval(2)))
   allocate(usgs%lsm(countval(1), countval(2)))

   stat =  nf90_inq_varid(fid, "Elevation_Mask", vid)
   stat =  nf90_get_var(fid, vid, usgs%dem, start = start, count = countval)
   stat =  nf90_inq_varid(fid, "Land_Use_Mask", vid)
   stat =  nf90_get_var(fid, vid, usgs%lus, start = start, count = countval)
   stat =  nf90_inq_varid(fid, "Land_Sea_Mask", vid)
   stat =  nf90_get_var(fid, vid, usgs%lsm, start = start, count = countval)

   ! We are now finished with the main data file
   call nc_close(fid, 'read_predef_file_ahi()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_predef_file()'

end function read_predef_file_ahi

! This function reads the SEVIRI predefined DEM, LS and LUM
function read_predef_file_sev(path_to_file, usgs, verbose) result (stat)

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

   call nc_open(fid, path_to_file, 'read_predef_file_sev()')
   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   ! Extract the array dimensions
   usgs%nlon = nc_dim_length(fid, 'x', 'read_USGS_file()', verbose)
   usgs%nlat = nc_dim_length(fid, 'y', 'read_USGS_file()', verbose)

   ! Read data for each variable
   allocate(usgs%dem(usgs%nlon, usgs%nlat))
   allocate(usgs%lus(usgs%nlon, usgs%nlat))
   allocate(usgs%lsm(usgs%nlon, usgs%nlat))

   call nc_read_array(fid, 'Elevation_Mask', usgs%dem, verbose)
   call nc_read_array(fid, 'Land_Use_Mask', usgs%lus, verbose)
   call nc_read_array(fid, 'Land_Sea_Mask', usgs%lsm, verbose)

   ! We are now finished with the main data file
   call nc_close(fid, 'read_predef_file_sev()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_predef_file()'

end function read_predef_file_sev

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
