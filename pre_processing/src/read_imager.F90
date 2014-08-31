module read_imager_m

implicit none

contains

!-------------------------------------------------------------------------------
! Name: read_imager.F90
!
! Purpose:
! A wrapper to call the routine appropriate for reading L1B and geolocation
! data for this sensor.
!
! Description and Algorithm details:
! 1) Call read_L1B routine.
! 2) Call read_geolocation routine.
!
! Arguments:
! Name                      Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! sensor                    string in   Name of instrument
! platform                  string in   Name of satellite
! path_to_l1b_file          string in   Full path to level 1B data
! path_to_geo_file          string in   Full path to geolocation data
! path_to_aatsr_drift_table string in   Full path to the AATSR calibration file
! imager_geolocation        struct both Summary of pixel positions
! imager_angles             struct both Summary of sun/satellite viewing angles
! imager_flags              struct both Summary of land/sea/ice flags
! imager_time               struct both Summary of pixel observation time
! imager_measurements       struct both Satellite observations
! channel_info              struct in   Summary of channel information
! n_along_track             lint   in   Number of pixels available in the
!                                       direction of travel
! verbose                   logic  in   T: print status information; F: don't
!
! History:
! 2011/12/12, MJ: produces draft code which opens and reads MODIS L1b file.
! 2012/01/24, MJ: includes code to read AVHRR L1b file.
! 2012/06/22, GT: Added code to read AATSR L1b file.
! 2012/08/28,   : Set imager_flags%cflag = 1 for MODIS, AVHHR, and AATSR as we
!   don't have proper masks.
! 2012/09/04, GT: Corrected AATSR sensor name
! 2013/09/06, AP: tidying
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_imager(sensor,platform,path_to_l1b_file,path_to_geo_file, &
     path_to_aatsr_drift_table, imager_geolocation,imager_angles,imager_flags, &
     imager_time,imager_measurements,channel_info,n_along_track,verbose)

   use channel_structures
   use imager_structures
   use preproc_constants
   use read_aatsr
   use read_avhrr
   use read_modis

   implicit none

   character(len=sensor_length),   intent(in)    :: sensor
   character(len=platform_length), intent(in)    :: platform
   character(len=path_length),     intent(in)    :: path_to_l1b_file
   character(len=path_length),     intent(in)    :: path_to_geo_file
   character(len=path_length),     intent(in)    :: path_to_aatsr_drift_table
   type(imager_geolocation_s),     intent(inout) :: imager_geolocation
   type(imager_angles_s),          intent(inout) :: imager_angles
   type(imager_flags_s),           intent(inout) :: imager_flags
   type(imager_time_s),            intent(inout) :: imager_time
   type(imager_measurements_s),    intent(inout) :: imager_measurements
   type(channel_info_s),           intent(in)    :: channel_info
   integer(kind=lint),             intent(in)    :: n_along_track
   logical,                        intent(in)    :: verbose

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_imager()'

   if (verbose) write(*,*) 'sensor: ',           trim(sensor)
   if (verbose) write(*,*) 'platform: ',         trim(platform)
   if (verbose) write(*,*) 'path_to_l1b_file: ', trim(path_to_l1b_file)
   if (verbose) write(*,*) 'path_to_geo_file: ', trim(path_to_geo_file)

   !branches for the sensors
   if (trim(adjustl(sensor)) .eq. 'MODIS') then
      call read_modis_time_lat_lon_angles(path_to_geo_file,imager_geolocation, &
           imager_angles,imager_flags,imager_time,n_along_track,verbose)

      !read MODIS L1b data. SW:reflectances, LW:brightness temperatures
      call read_modis_l1b_radiances(sensor,platform,path_to_l1b_file, &
           imager_geolocation,imager_measurements,channel_info,verbose)

      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
      !read the angles and lat/lon info of the orbit
      call read_avhrr_time_lat_lon_angles(path_to_geo_file,imager_geolocation, &
           imager_angles, imager_flags,imager_time,n_along_track,verbose)

      !read land/sea flag from physiography file
      call read_avhrr_land_sea_mask(path_to_geo_file,imager_geolocation, &
           imager_angles,imager_flags,imager_time)

      !read the (subset) of the orbit etc. SW:reflectances, LW:brightness temp
      call read_avhrr_l1b_radiances(sensor,platform,path_to_l1b_file, &
           imager_geolocation,imager_measurements,channel_info,verbose)

      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   else if (trim(adjustl(sensor)) .eq. 'AATSR') then
      if (verbose) write(*,*) 'path_to_aatsr_drift_table: ', &
                              trim(path_to_aatsr_drift_table)

      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_aatsr_l1b(path_to_l1b_file, path_to_aatsr_drift_table, &
           imager_geolocation, imager_measurements, imager_angles, &
           imager_flags,imager_time, channel_info, verbose)
      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1
   end if

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_imager()'

end subroutine read_imager

end module read_imager_m
