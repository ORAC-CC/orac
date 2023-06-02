!-------------------------------------------------------------------------------
! Name: read_modis_time_lat_lon_angles.F90
!
! Purpose:
! Open and read MODIS geolocation input files.
!
! Description and Algorithm details:
! 1) Allocate and initialise temporary arrays.
! 2) Read each geolocation field in turn.
! 3) Deallocate arrays and close files.
!
! Arguments:
! Name               Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_geo_file   string in   Full path to geolocation data
! imager_geolocation struct both Summary of pixel positions
! imager_angles      struct both Summary of sun/satellite viewing angles
! imager_flags       struct in   Summary of land/sea/ice flags
! imager_time        struct both Summary of pixel observation time
! n_along_track      lint   in   Number of pixels in the direction of travel
! verbose            logic  in   T: print status information; F: don't
!
! History:
! 2011/12/12, MJ: produces draft code which opens and reads MODIS geo hdf files
! 2012/04/24, GT: Added a line assigning the solar azimuth angle
!    to imager structure (needed by surface reflectance routines)
! 2013/05/21, GT: Removed 180.0 degree correction to relative
!    azimuth to match the convention used in ORAC lookup table code and surface
!    reflectance calculation.
! 2013/05/21, GT: On further investigation, reintroduced the 180 degree
!    correction to relative azimuth. This means that
!    forward scattering = 0 relative azimuth.
! 2013/09/11, AP: tidying, added n_along_track, removed modis2oraclsflag
! 2015/04/22, OS: bug fix in calculating relative azimuth angle
! 2017/06/21, OS: bug fix: with cray-fortran compiler, variables need to be
!    declared as inout if input values are to be used within SR
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_time_lat_lon_angles(path_to_geo_file, imager_geolocation, &
     imager_angles, imager_flags, imager_time, n_along_track, verbose)

   use imager_structures_m
   use preproc_constants_m
   use hdf_m, only: DFACC_READ

   implicit none

   character(len=*),           intent(in)    :: path_to_geo_file
   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   type(imager_flags_t),       intent(in)    :: imager_flags
   type(imager_time_t),        intent(inout) :: imager_time
   integer(kind=lint),         intent(in)    :: n_along_track
   logical,                    intent(in)    :: verbose

!  integer(kind=lint)              :: ix, jy
   real(kind=sreal),   allocatable :: temp(:,:)
   integer(kind=byte), allocatable :: btemp(:,:)
   integer                         :: geo_id, err_code

   integer(kind=4), external       :: sfstart, sfend

   if (verbose) &
        write(*,*) '<<<<<<<<<<<<<<< Entering read_modis_time_lat_lon_angles()'

   ! allocate temporary data
   allocate(temp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   ! get file id
   geo_id = sfstart(path_to_geo_file, DFACC_READ)

   ! read time
   call get_modis_time(geo_id, imager_geolocation, imager_time, n_along_track)

   ! read latitude
   call read_modis_lat_lon(geo_id, "Latitude", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, imager_geolocation%latitude)

   ! read longitude
   call read_modis_lat_lon(geo_id, "Longitude", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, imager_geolocation%longitude)

   ! From the MODIS Level 1A Earth Location: Algorithm Theoretical Basis
   ! Document Version 3.0 (https://modis.gsfc.nasa.gov/data/atbd/
   ! atbd_mod28_v3.pdf), page 3-37,
   !   5: cos(sensor_zenith) = normal_to_geodetic_surface . view_direction
   !   6: tan(sensor_azimuth) = view_direction . east / view_direction . north
   ! Hence, this zenith is measured out from the vertical and this azimuth is
   ! clockwise from north, relative to the geodetic position. (The geodetic
   ! position is the intersection of the satellite view with the 1km Platte
   ! Carree EOS DEM.) In (7), it then states the solar angles are worked out
   ! using the same formulae.

   ! read solzen
   call read_modis_angles(geo_id, "SolarZenith", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, imager_angles%solzen(:,:,1))

   ! read senszen
   call read_modis_angles(geo_id, "SensorZenith", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, imager_angles%satzen(:,:,1))

   ! read solazi
   call read_modis_angles(geo_id, "SolarAzimuth", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, imager_angles%solazi(:,:,1))

   ! read sensazi
   call read_modis_angles(geo_id, "SensorAzimuth", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, temp)

   imager_angles%satazi(:,:,1) = temp

   ! make rel azi
   ! Note: Relative azimuth is defined so that if the satellite is looking
   ! towards the sun (i.e. forward scattering), relative azimuth is 180 deg.
   where (imager_angles%solazi(:,:,1) .ne. sreal_fill_value .and. &
          temp .ne. sreal_fill_value)

       imager_angles%relazi(:,:,1) = abs(imager_angles%solazi(:,:,1) - temp)

       where (imager_angles%relazi(:,:,1) .gt. 180.)
          imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
       end where

   end where

   ! free temp arrays
   deallocate(temp)

   ! read modis ls flag
   allocate(btemp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   call read_modis_land_sea_mask(geo_id, "Land/SeaMask", imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, btemp)

   ! make orac ls flag by mapping the MODIS L/S definitions to the ones for ORAC
   ! (approximate). MODIS DEFINITIONS - DN values:
   !                0:      Shallow Ocean (Ocean <5k from coast OR <50m deep).
   !                1:      Land (not anything else).
   !                2:      Ocean Coastlines and Lake Shorelines.
   !                3:      Shallow Inland Water (Inland Water < 5km from shore
   !                                OR < 50m deep).
   !                4:      Ephemeral (intermittent) Water.
   !                5:      Deep Inland Water (Inland water > 5km from shoreline
   !                                AND > 50m deep).
   !                6:      Moderate or Continental Ocean (Ocean > 5km from coast
   !                                AND > 50m deep AND < 500m deep).
   !                7:      Deep Ocean (Ocean > 500m deep).
   ! which of these is most efficient is compiler-dependant

   where (btemp.eq.0 .or. btemp.eq.5 .or. btemp.eq.6 .or. btemp.eq.7)
      btemp = 0
   else where
      btemp = 1
   end where

   imager_flags%lsflag = btemp

   ! free temp byte array
   deallocate(btemp)

   ! end access to geofile
   err_code = sfend(geo_id)

   if (verbose) &
        write(*,*) '>>>>>>>>>>>>>>> Leaving read_modis_time_lat_lon_angles()'

end subroutine read_modis_time_lat_lon_angles
