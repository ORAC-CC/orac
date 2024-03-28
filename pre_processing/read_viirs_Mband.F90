!-------------------------------------------------------------------------------
! Name: read_viirs.F90
!
! Purpose:
! Module for VIIRS I/O routines.
! To run the preprocessor with VIIRS use:
! VIIRS as the sensor name (1st line of driver)
! Any M-band file as the l1b filename (2nd line)
! The corresponding GMTCO file as the geo filename (3rd line)
!
! All M-band filenames to be read must have the same filename format!
!
! History:
! 2016/05/17, SP: Initial version.
! 2016/05/19, SP: Bugfix to prevent stack smashing when HDF5 file from NOAA
!                 doesn't conform to their own standard.
! 2016/05/23, SP: Some tidying and better commenting
! 2016/08/01, SP: Changed zenith angle bounds
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_viirs_mband_m

   implicit none

   private

   public :: read_viirs_mband_dimensions, &
             read_viirs_mband

contains

!-------------------------------------------------------------------------------
! Name: read_viirs_mband_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! geo_file       string  in   Full path to one VIIRS GMTCO file
! n_across_track lint    out  Number columns in the viirs image (usually constant)
! n_along_track  lint    out  Number lines   in the viirs image (usually constant)
! verbose        logical in   If true then print verbose information.
!
! Note: startx,endx,starty,endy currently ignored.
! It will always process the full scene. This will be fixed.
!-------------------------------------------------------------------------------
subroutine read_viirs_mband_dimensions(geo_file, n_across_track, n_along_track, &
                                       verbose)

   use iso_c_binding
   use hdf5
   use preproc_constants_m

   implicit none

   character(len=*),   intent(in)    :: geo_file
   integer(kind=lint), intent(out)   :: n_across_track, n_along_track
   logical,            intent(in)    :: verbose

   integer(HID_T) :: file_id   ! File identifier
   integer(HID_T) :: dset_id   ! Dataset identifier
   integer(HID_T) :: dataspace ! Dataspace identifier

   integer                        :: error
   integer(HSIZE_T), dimension(2) :: dimsr, maxdimsr

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_viirs_mdband_dimensions()'

   ! Open the file.
   call h5fopen_f (geo_file, H5F_ACC_RDONLY_F, file_id, error)

   ! Open the  dataset.
   call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/Latitude", &
        dset_id, error)
   if (error .ne. 0) then
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/Latitude", &
           dset_id, error)
   end if
   if (error .ne. 0) then
      print*, "Problem determining VIIRS dimensions"
      stop
   end if

   ! Get dataset's dataspace handle.
   call h5dget_space_f(dset_id, dataspace, error)

   ! Get dataspace's dimensinons.
   call h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)

   n_across_track = dimsr(1)
   n_along_track = dimsr(2)

   call h5dclose_f(dset_id, error)
   call h5fclose_f(file_id, error)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_viirs_mband_dimensions()'

end subroutine read_viirs_mband_dimensions


!-------------------------------------------------------------------------------
! Name: read_viirs_mband
!
! Purpose:
! To read the requested VIIRS data from HDF5-format files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infile              string  in   Full path to any M-band image file
! geofile             string  in   Full path to the GMTCO geolocation file
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_flags        struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_viirs_mband(infile, geofile, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, verbose)

   use iso_c_binding
   use hdf5
   use calender_m
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),            intent(in)    :: infile
   character(len=*),            intent(in)    :: geofile
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer                          :: i, j
   integer(c_int)                   :: n_bands
   integer(c_int), allocatable      :: band_ids(:)
   integer(c_int), allocatable      :: band_units(:)
   integer                          :: startx, nx
   integer                          :: starty, ny
   integer(c_int)                   :: line0, line1
   integer(c_int)                   :: column0, column1
   integer                          :: index2
   integer(kind=sint)               :: year, month, day
   integer(kind=sint)               :: hour1, minute1, second1
   integer(kind=sint)               :: hour2, minute2, second2
   double precision                 :: dfrac1, dfrac2, jd1, jd2, slo

   ! Used for reading the HDF5 files correctly (prevents stack smashing)
   integer(HSIZE_T), dimension(1:2) :: pxcount
   integer(HSIZE_T), dimension(1)   :: pxcount_fac

   ! Various variables for reading the HDF5 data and choosing filenames
   integer(HID_T)                   :: file_id
   integer(HID_T)                   :: dset_id
   integer                          :: error
   real,    allocatable             :: data0(:,:)
   integer, allocatable             :: data1(:,:)
   real                             :: factors(8)
   character(len=path_length)       :: bandfile
   character(len=path_length)       :: banddir
   character(len=path_length)       :: varname, facname
   character(len=3)                 :: band
   character(len=path_length)       :: regex

   ! This ignores the last digit on seconds
   character(len=var_length), parameter :: date_format = &
        '(5X, I4, I2, I2, 2X, I2, I2, I2, 3X, I2, I2, I2)'

   ! Figure out the channels to process
   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr
   allocate(band_units(n_bands))


   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   ! Setup the temporary data arrays.
   ! Two are needed as VIIRS data is usually UINT but sometimes FLOAT
   allocate(data0(nx,ny)) !Float, also used for geoinfo
   allocate(data1(nx,ny)) !UInt, used for most channels

   ! Size of the SDR/GEO arrays in the HDF5 files
   pxcount(1) = nx
   pxcount(2) = ny

   ! Size of the 'factors' array that stores slope/offset for channel data
   pxcount_fac(1) = 8

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ! This section computes the time value for each pixel in the image. It does
   ! this by examinine the start and end times listed in the l1b filename. The
   ! time value is constant across each image line (approximately true, good
   ! enough as far as we're concerned). Each line is calculated simply by using
   ! the start time as an offset for a linear fit. The line slope is:
   ! (end_time-start_time) / ny.

   ! Technically we could use data in the geofile (GMTCO) instead, but it gives
   ! same result whilst taking a lot of extra I/O
   index2 = index(geofile, 'npp_d', .true.)
   if (index2 .le. 0) then
      index2 = index(geofile, 'j01_d', .true.)
      if (index2 .le. 0) then
         write(*,*) "Unsupported VIIRS platform."
         stop
      end if
   end if

   ! get year, doy, hour and minute as strings
   read(geofile(index2:), date_format) year, month, day, hour1, minute1, &
        second1

   ! Convert start and end times to julian

   !!!! WARNING: Assumes both are the same (geofile name contains 2 times but
   !!!! only 1 date). Results in undefined behaviour if a granule spans midnight

   ! NOTE: Should investigate how this is dealt with in the l1b files...

   call GREG2JD(year, month, day, jd1)
   call GREG2JD(year, month, day, jd2)

   ! Add on a fraction to account for the start / end times
   dfrac1 = (float(hour1)/24.0) + (float(minute1)/(24.0*60.0)) + &
                        (float(second1)/(24.0*60.0*60.0))
   dfrac2 = (float(hour2)/24.0) + (float(minute2)/(24.0*60.0)) + &
                        (float(second2)/(24.0*60.0*60.0))
   jd1 = jd1 + dfrac1
   jd2 = jd2 + dfrac2

   ! Compute linear regression slope
   slo = (jd2 - jd1)/ny

   ! Put correct julian date into each location in the time array
   do j = 1, ny
      imager_time%time(:,j) = jd1 + (slo*float(j))
   end do

   ! This bit reads all the data.
   ! First it loads geo/angle info from GMTCO file.
   call h5open_f(error)
   if (index(geofile, 'GMTCO') .gt. 0) then
      call h5fopen_f (geofile, H5F_ACC_RDONLY_F, file_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/Latitude", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_geolocation%latitude(:,:) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/Longitude", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_geolocation%longitude(:,:) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/SolarZenithAngle", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%solzen(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/SolarAzimuthAngle", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%solazi(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/" // &
                     "SatelliteZenithAngle", dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%satzen(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO-TC_All/" // &
                     "SatelliteAzimuthAngle", dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%satazi(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5fclose_f(file_id, error)
   else
      call h5fopen_f (geofile, H5F_ACC_RDONLY_F, file_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/Latitude", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_geolocation%latitude(:,:) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/Longitude", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_geolocation%longitude(:,:) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/SolarZenithAngle", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%solzen(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/SolarAzimuthAngle", &
                     dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%solazi(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/" // &
                     "SatelliteZenithAngle", dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%satzen(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5dopen_f(file_id, "//All_Data/VIIRS-MOD-GEO_All/" // &
                     "SatelliteAzimuthAngle", dset_id, error)
      call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
      imager_angles%satazi(:,:,1) = data0
      call h5dclose_f(dset_id, error)
      call h5fclose_f(file_id, error)
   end if

   ! Move on to image data, in this case from individual SVM files (per-band)

   index2 = index(infile, 'SVM', .true.)
   do i = 1, n_bands
      banddir = infile(1:index2-2)
      write (band, "(I2.2)") band_ids(i)
      regex = trim(infile(index2:index2+2))//trim(adjustl(band))// &
              trim(infile(index2+5:index2+35))//"*"

      ! Check if we find the appropriate band
      if (match_file(trim(banddir), trim(regex), bandfile) .ne. 0) then
         write(*,*) 'ERROR: read_viirs(): Unable to locate VIIRS SVM ' // &
                    'file: ', trim(banddir)//'/'//trim(regex)
         stop error_stop_code
      end if

      ! Setup and read the relevant refl/bt/factors
      bandfile = trim(banddir)//'/'//trim(bandfile)
      write (band, "(I2)") band_ids(i)
      band = "M"//trim(adjustl(band))
      call h5fopen_f(bandfile, H5F_ACC_RDONLY_F, file_id, error)
      if (band_ids(i) .lt. 12) then
         varname = "//All_Data/VIIRS-"//trim(adjustl(band))// &
              "-SDR_All/Reflectance"
         facname = "//All_Data/VIIRS-"//trim(adjustl(band))// &
              "-SDR_All/ReflectanceFactors"
      else if (band_ids(i) .ge. 12) then
         varname = "//All_Data/VIIRS-"//trim(adjustl(band))// &
              "-SDR_All/BrightnessTemperature"
         facname = "//All_Data/VIIRS-"//trim(adjustl(band))// &
              "-SDR_All/BrightnessTemperatureFactors"
      end if

      ! Standard case, data is integer
      if (band_ids(i) .ne. 13) then
         call h5dopen_f(file_id, trim(adjustl(varname)), dset_id, error)
         call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data1, pxcount, error)
         call h5dclose_f(dset_id, error)
         call h5dopen_f(file_id, trim(adjustl(facname)), dset_id, error)
         call h5dread_f(dset_id, H5T_NATIVE_REAL, factors, pxcount_fac, error)
         call h5dclose_f(dset_id, error)
         call h5fclose_f(file_id, error)

         ! Scale and account for missing values
         imager_measurements%data(:,:,i) = factors(2) + (data1*factors(1))
         where(data1 .gt. 65500) &
              imager_measurements%data(:,:,i) = sreal_fill_value

      ! Band 13, data is float
      else
         call h5dopen_f(file_id, trim(adjustl(varname)), dset_id, error)
         call h5dread_f(dset_id, H5T_NATIVE_REAL, data0, pxcount, error)
         call h5dclose_f(dset_id, error)
         call h5fclose_f(file_id, error)

         ! Account for missing values, data doesn't need scaling
         imager_measurements%data(:,:,i) = data0
         where(data0 .lt. -999) imager_measurements%data(:,:,i) = sreal_fill_value
      end if

      if (verbose) &
         write(*,*) i, band_ids(i), maxval(imager_measurements%data(:,:,i)), &
                                    minval(imager_measurements%data(:,:,i))
   end do

   deallocate(band_ids)
   deallocate(band_units)

   call h5close_f(error)

   imager_angles%solzen(startx:,:,1) = abs(imager_angles%solzen(startx:,:,1))
   imager_angles%satzen(startx:,:,1) = abs(imager_angles%satzen(startx:,:,1))

   ! Check units to remove anything that's out-of-range.
   where(imager_geolocation%latitude(startx:,:)  .gt. 100) &
      imager_geolocation%latitude(startx:,:) = sreal_fill_value
   where(imager_geolocation%latitude(startx:,:)  .lt. -100) &
      imager_geolocation%latitude(startx:,:) = sreal_fill_value
   where(imager_geolocation%longitude(startx:,:) .gt. 200) &
      imager_geolocation%longitude(startx:,:) = sreal_fill_value
   where(imager_geolocation%longitude(startx:,:) .lt. -200) &
      imager_geolocation%longitude(startx:,:) = sreal_fill_value
   where(imager_angles%solazi(startx:,:,1)       .gt. 900) &
      imager_angles%solazi(startx:,:,1) = sreal_fill_value
   where(imager_angles%solzen(startx:,:,1)       .gt. 900) &
      imager_angles%solzen(startx:,:,1) = sreal_fill_value
   where(imager_angles%satzen(startx:,:,1)       .gt. 900) &
      imager_angles%satzen(startx:,:,1) = sreal_fill_value
   where(imager_angles%satazi(startx:,:,1)       .gt. 900) &
      imager_angles%satazi(startx:,:,1) = sreal_fill_value

   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%satazi(startx:,:,1) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,1) = abs(imager_angles%solazi(startx:,:,1) - &
                                        imager_angles%satazi(startx:,:,1))
   end where

   where (imager_angles%relazi(:,:,1) .gt. 180.)
      imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
   end where

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_viirs()'

end subroutine read_viirs_mband

end module read_viirs_mband_m
