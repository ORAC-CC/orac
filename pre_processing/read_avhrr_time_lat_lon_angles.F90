
!-------------------------------------------------------------------------------
! Name: read_avhrr_time_lat_lon_angles.F90
!
! Purpose:
! Open and read AVHRR geo input files.
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
! imager_time        struct both Summary of pixel observation time
! n_along_track      lint   in   Number of pixels in the direction of travel
! verbose            logic  in   T: print status information; F: don't
!
! History:
! 2012/01/24, MJ: writes code to read geolocation and geometry information for
!    AVHRR.
! 2013/09/06, AP: tidying, removed array initialisations as read routines
!    cover it
! 2015/04/22, OS: bug fix in calculating relative azimuth angle
! 2020/05/28, AP: Relative azimuth should be 180 when looking into the sun.
! 2020/07/28, GT: Adding valid range checking on Lat-Lon values.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_avhrr_time_lat_lon_angles(path_to_geo_file, imager_geolocation, &
     imager_angles, imager_time, n_along_track, verbose)

   use calender_m
   use hdf5
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=*),            intent(in)    :: path_to_geo_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   integer(kind=lint),          intent(in)    :: n_along_track
   logical,                     intent(in)    :: verbose

   integer(kind=HID_T)                           :: geo_id

   real(kind=sreal), allocatable, dimension(:,:) :: temp, temp2

   integer(kind=lint)                            :: startepochs, endepochs

   real(kind=dreal)                              :: refjulianday=0.00_dreal

   integer                                       :: err_code

   !reference point of time
   integer(kind=sint) :: refday = 1_sint, refyear = 1970_sint, refmonth = 1_sint

   if (verbose) &
        write(*,*) '<<<<<<<<<<<<<<< Entering read_avhrr_time_lat_lon_angles()'

   !allocate temporary data
   allocate(temp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   !initialize the f90 interface for hdf5
   call h5open_f(err_code)

   !open the geo file
   call h5fopen_f(path_to_geo_file, h5f_acc_rdonly_f, geo_id, err_code)

   !this converts the reference point of time (epoch) to the julian date
   call GREG2JD(refyear, refmonth, refday, refjulianday)

   !read start and endtime of orbit
   call read_avhrr_time(geo_id, "how", startepochs, endepochs)

   call create_time_for_pixel(imager_geolocation%startx, &
        imager_geolocation%endx, imager_geolocation%starty, &
        imager_geolocation%endy, n_along_track, startepochs, endepochs, &
        imager_time, refjulianday)

   !read latitude
   call read_avhrr_lat_lon(geo_id, "where/lat", "data", "where/lat/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy,temp)
   ! Check for bad values in latitude (as this is not done in read_avhrr_lat_lon)
   where(temp .gt. 90.0 .or. temp .lt. -90.0)
      temp = sreal_fill_value
   end where
   imager_geolocation%latitude = temp

   !read longitude
   call read_avhrr_lat_lon(geo_id, "where/lon", "data", "where/lon/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy,temp)
   ! Check for bad values in longitude (as this is not done in read_avhrr_lat_lon)
   where(temp .gt. 180.0 .or. temp .lt. -180.0)
      temp = sreal_fill_value
   end where
   imager_geolocation%longitude = temp

   !read solzen
   call read_avhrr_angles(geo_id, "image1", "data", "image1/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy, temp)
   imager_angles%solzen(:,:,1) = temp

   !read senszen
   call read_avhrr_angles(geo_id, "image2", "data", "image2/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy, temp)
   imager_angles%satzen(:,:,1) = temp

   !read solazi
   allocate(temp2(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))
   call read_avhrr_angles(geo_id, "image4", "data", "image4/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy, temp2)
   imager_angles%solazi(:,:,1) = temp2

   !read sensazi
   call read_avhrr_angles(geo_id, "image5", "data", "image5/what", &
        imager_geolocation%startx, imager_geolocation%endx, &
        imager_geolocation%starty, imager_geolocation%endy, temp)

   ! make rel azi
   ! Note: Relative azimuth is defined so that if the satellite is looking
   ! towards the sun (i.e. forward scattering), relative azimuth is 180.

   where ( temp2 .ne. sreal_fill_value .AND. temp .ne. sreal_fill_value )

      imager_angles%relazi(:,:,1) = abs( temp2 - temp )
      imager_angles%satazi(:,:,1) = temp

      where ( imager_angles%relazi(:,:,1) .gt. 180. )
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where

   end where

   !free temp arrays
   deallocate(temp2)
   deallocate(temp)

   !close the file
   call h5fclose_f(geo_id, err_code)

   !close access to hdf5 interface
   call h5close_f(err_code)

   if (verbose) &
        write(*,*) '>>>>>>>>>>>>>>> Leaving read_avhrr_time_lat_lon_angles()'

end subroutine read_avhrr_time_lat_lon_angles
