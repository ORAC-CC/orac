!-------------------------------------------------------------------------------
! Name: read_python.F90
!
! Purpose:
! A reader for data produced by the python prepre-processor.
!
! History:
! 2023/06/02, SRP: First version.
!
!-------------------------------------------------------------------------------


module read_python_m

    implicit none
 
 contains
 subroutine read_python(infile, imager_geolocation, imager_measurements, &
                        imager_angles, imager_time, channel_info, &
                        global_atts, verbose)

   use iso_c_binding
   use netcdf
   use orac_ncdf_m
   use calender_m
   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),            intent(in)    :: infile
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose


   real(sreal), allocatable     :: in_slopes(:)
   integer(c_int)               :: n_bands
   integer(c_int), allocatable  :: band_ids(:)
   integer(c_int), allocatable  :: band_units(:)
   integer                      :: startx, nx
   integer                      :: starty, ny

   ! netCDF stuff
   integer                     :: ncid


   integer :: nc
 
   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_python()'
   call ncdf_open(ncid, infile, 'read_agri_data()')

   ! Read the channel data
   call ncdf_read_array(ncid, "channel_data", imager_measurements%data)
   where(imager_measurements%data(:, :,:)  .gt. 500) &
      imager_measurements%data(:, :,:) = sreal_fill_value
   where(imager_measurements%data(:, :,:)  .lt. 0) &
      imager_measurements%data(:, :,:) = sreal_fill_value

   ! Read the geolocation data
   call ncdf_read_array(ncid, "latitude", imager_geolocation%latitude)
   where(imager_geolocation%latitude(:,:)  .gt. 100) &
      imager_geolocation%latitude(:,:) = sreal_fill_value
   where(imager_geolocation%latitude(:,:)  .lt. -100) &
      imager_geolocation%latitude(:,:) = sreal_fill_value
   call ncdf_read_array(ncid, "longitude", imager_geolocation%longitude)
   where(imager_geolocation%longitude(:,:)  .gt. 200) &
      imager_geolocation%longitude(:,:) = sreal_fill_value
   where(imager_geolocation%longitude(:,:)  .lt. -200) &
      imager_geolocation%longitude(:,:) = sreal_fill_value

   ! Read solar and satellite angle data
   call ncdf_read_array(ncid, "relative_azimuth_angle", imager_angles%relazi)
   where(imager_angles%relazi(:, :,:)  .lt. -200) &
      imager_angles%relazi(:, :,:) = sreal_fill_value

   call ncdf_read_array(ncid, "solar_zenith_angle", imager_angles%solzen)
   where(imager_angles%solzen(:, :,:)  .lt. -200) &
      imager_angles%solzen(:, :,:) = sreal_fill_value

   call ncdf_read_array(ncid, "solar_azimuth_angle", imager_angles%solazi)
   where(imager_angles%solazi(:, :,:)  .lt. -200) &
      imager_angles%solazi(:, :,:) = sreal_fill_value

   call ncdf_read_array(ncid, "satellite_zenith_angle", imager_angles%satzen)
   where(imager_angles%satzen(:, :,:)  .lt. -200) &
      imager_angles%satzen(:, :,:) = sreal_fill_value

   call ncdf_read_array(ncid, "satellite_azimuth_angle", imager_angles%satazi)
   where(imager_angles%satazi(:, :,:)  .lt. -200) &
      imager_angles%satazi(:, :,:) = sreal_fill_value

   ! Read per-pixel time information
   call ncdf_read_array(ncid, "time_data", imager_time%time)
 

   ! Close the netCDF4 file
   call ncdf_close(ncid, 'read_python()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_python()'
 
 end subroutine read_python

 end module read_python_m
 
