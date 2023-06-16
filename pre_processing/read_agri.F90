!-------------------------------------------------------------------------------
! Name: read_agri.F90
!
! Purpose:
! Module for FY4 AGRI I/O routines.
! To run the preprocessor with AGRI use:
! AGRI as the sensor name (1st line of driver)
! Any 4km file as the l1b filename (2nd line)
! TThe same file as the geo filename (3rd line)
!!
! History:
! 2018/08/14, SP: Initial version.
!
! Bugs:
! Only tested with full disk AGRI images, not China regional coverage..
!-------------------------------------------------------------------------------

module read_agri_m

   implicit none

   private

   public :: read_agri_dimensions, &
             read_agri_data

contains

!-----------------------------------------------------------------------------
! Name: read_agri_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! fname          string  in   Full path to one AGRI file
! n_across_track lint    out  Number columns in the AGRI image
! n_along_track  lint    out  Number lines   in the AGRI image
! verbose        logical in   If true then print verbose information.
!
!-----------------------------------------------------------------------------
subroutine read_agri_dimensions(fname, n_across_track, n_along_track, verbose)

   use iso_c_binding
   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*),   intent(in)  :: fname
   integer(kind=lint), intent(out) :: n_across_track, n_along_track
   logical,            intent(in)  :: verbose

   integer :: fid

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_agri_dimensions()'

   ! Open the file.
   call ncdf_open(fid, fname, 'read_agri_dimensions()')

   n_across_track = ncdf_dim_length(fid, 'lat', 'read_agri_dimensions()')
   n_along_track = ncdf_dim_length(fid, 'lon', 'read_agri_dimensions()')

   call ncdf_close(fid, 'read_agri_dimensions()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_agri_dimensions()'

end subroutine read_agri_dimensions


subroutine compute_time(ncid, imager_time, ny)
!-----------------------------------------------------------------------------
! Name: compute_time
!
! Purpose:
! To compute the time array
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! ncid                int     in   The ID of the netCDF file open for reading
! imager_time         struct  out  The output structure
! ny                  int     in   The size of the y dimension of the image
!-----------------------------------------------------------------------------

   use preproc_constants_m
   use imager_structures_m
   use orac_ncdf_m
   use calender_m
   use netcdf, only: nf90_get_att, NF90_GLOBAL, NF90_NOERR, nf90_strerror

   implicit none

   integer,             intent(in)  :: ncid
   type(imager_time_t), intent(out) :: imager_time
   integer,             intent(in)  :: ny

   ! Time stuff
   character(len=12)  :: start_time
   character(len=12)  :: end_time
   integer(kind=sint) :: st_yr, st_mn, st_dy, st_hr, st_mi
   integer(kind=sint) :: en_yr, en_mn, en_dy, en_hr, en_mi
   real(kind=dreal)   :: jd1, jd2, dfrac1, dfrac2, slo

   ! netCDF stuff
   integer :: ierr

   integer :: j

   character(len=var_length), parameter :: date_format = &
        '(I4, I2, I2, I2, I2)'

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'start_time', start_time)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: read_agri_data(), ', trim(nf90_strerror(ierr)), &
           ', name: start_time'
      stop -1
   end if
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'end_time', end_time)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: read_agri_data(), ', trim(nf90_strerror(ierr)), &
           ', name: end_time'
      stop -1
   end if

   read(start_time, date_format) st_yr, st_mn, st_dy, st_hr, st_mi
   read(end_time, date_format) en_yr, en_mn, en_dy, en_hr, en_mi
   call GREG2JD(st_yr, st_mn, st_dy, jd1)
   call GREG2JD(en_yr, en_mn, en_dy, jd2)

   ! Add on a fraction to account for the start / end times
   dfrac1 = (float(st_hr)/24.0) + (float(st_mi)/(24.0*60.0))
   dfrac2 = (float(en_hr)/24.0) + (float(en_mi)/(24.0*60.0))
   jd1 = jd1 + dfrac1
   jd2 = jd2 + dfrac2

   ! Compute linear regression slope
   slo = (jd2-jd1)/ny

   ! Put correct julian date into each location in the time array
   do j = 1, ny
      imager_time%time(:,j) = jd1 + (slo*float(j))
   end do

end subroutine compute_time


subroutine agri_retr_anc(ncid, imager_angles, startx, starty, imager_geolocation)
!-----------------------------------------------------------------------------
! Name: agri_retr_anc
!
! Purpose:
! To retrieve geodata and angles data for AGRI
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! ncid                int     in   The ID of the netCDF file open for reading
! imager_angles       struct  out  Holds angles data
! imager_geolocation  struct  out  Holds geolocation data
!-----------------------------------------------------------------------------

   use preproc_constants_m
   use imager_structures_m
   use orac_ncdf_m
   use calender_m

   implicit none

   integer,                    intent(in)  :: ncid
   integer,                    intent(in)  :: startx
   integer,                    intent(in)  :: starty
   type(imager_angles_t),      intent(out) :: imager_angles
   type(imager_geolocation_t), intent(out) :: imager_geolocation

   call ncdf_read_array(ncid, 'latitude', imager_geolocation%latitude, start=[startx, starty])
   call ncdf_read_array(ncid, 'longitude', imager_geolocation%longitude, start=[startx, starty])
   call ncdf_read_array(ncid, 'solar_zenith_angle', imager_angles%solzen(:,:,1), start=[startx, starty])
   call ncdf_read_array(ncid, 'solar_azimuth_angle', imager_angles%solazi(:,:,1), start=[startx, starty])
   call ncdf_read_array(ncid, 'satellite_zenith_angle', imager_angles%satzen(:,:,1), start=[startx, starty])
   call ncdf_read_array(ncid, 'satellite_azimuth_angle', imager_angles%satazi(:,:,1), start=[startx, starty])

   imager_angles%solzen(:,:,1) = abs(imager_angles%solzen(:,:,1))
   imager_angles%satzen(:,:,1) = abs(imager_angles%satzen(:,:,1))

   ! Check units to remove anything that's out-of-range.
   where(imager_geolocation%latitude(:,:)  .gt. 100) &
        imager_geolocation%latitude(:,:) = sreal_fill_value
   where(imager_geolocation%latitude(:,:)  .lt. -100) &
        imager_geolocation%latitude(:,:) = sreal_fill_value
   where(imager_geolocation%longitude(:,:) .gt. 200) &
        imager_geolocation%longitude(:,:) = sreal_fill_value
   where(imager_geolocation%longitude(:,:) .lt. -200) &
        imager_geolocation%longitude(:,:) = sreal_fill_value
   where(imager_angles%solazi(:,:,1)       .gt. 900) &
        imager_angles%solazi(:,:,1) = sreal_fill_value
   where(imager_angles%solzen(:,:,1)       .gt. 900) &
        imager_angles%solzen(:,:,1) = sreal_fill_value
   where(imager_angles%satzen(:,:,1)       .gt. 900) &
        imager_angles%satzen(:,:,1) = sreal_fill_value
   where(imager_angles%satazi(:,:,1)       .gt. 900) &
        imager_angles%satazi(:,:,1) = sreal_fill_value

   ! Rescale zens + azis into correct format

   where(imager_angles%solazi(:,:,1) .ne. sreal_fill_value .and. &
        imager_angles%satazi(:,:,1) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,1) = abs(imager_angles%satazi(:,:,1) - &
           imager_angles%solazi(:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where

      imager_angles%solazi(:,:,1) = imager_angles%solazi(:,:,1) + 180.
      where (imager_angles%solazi(:,:,1) .gt. 360.)
         imager_angles%solazi(:,:,1) = imager_angles%solazi(:,:,1) - 360.
      end where
      imager_angles%satazi(:,:,1) = imager_angles%satazi(:,:,1) + 180.
      where (imager_angles%satazi(:,:,1) .gt. 360.)
         imager_angles%satazi(:,:,1) = imager_angles%satazi(:,:,1) - 360.
      end where
   end where

end subroutine agri_retr_anc


subroutine agri_retr_band(ncid, band, iband, irband, startx, starty, imager_measurements)
!-----------------------------------------------------------------------------
! Name: agri_retr_band
!
! Purpose:
! To retrieve one band of AGRI data from a netCDF file
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! ncid                int     in   The ID of the netCDF file open for reading
! band                str     in   The in-file band variable name
! iband               int     in   The band location in the output struct
! irband              int     in   Switch for infrared bands
! imager_measurements struct  out  The struct storing the actual data
!-----------------------------------------------------------------------------

   use preproc_constants_m
   use imager_structures_m
   use orac_ncdf_m
   use calender_m

   implicit none

   integer,                     intent(in)  :: ncid
   character(len=*),            intent(in)  :: band
   integer,                     intent(in)  :: iband
   integer,                     intent(in)  :: irband
   integer,                    intent(in)   :: startx
   integer,                    intent(in)   :: starty
   type(imager_measurements_t), intent(out) :: imager_measurements

   call ncdf_read_array(ncid, band, imager_measurements%data(:,:,iband), start=[startx, starty])

   ! If it's a non-IR band then we have to divide by 100 as Satpy refl is in range 0->100
   if (irband .eq. 0) then
      imager_measurements%data(:,:,iband) = imager_measurements%data(:,:,iband) / 100
      ! Check units to remove anything that's out-of-range for solar bands
      where(imager_measurements%data(:,:,iband)  .gt. 2) &
           imager_measurements%data(:,:,iband) = sreal_fill_value
      where(imager_measurements%data(:,:,iband)  .lt. -2) &
           imager_measurements%data(:,:,iband) = sreal_fill_value
   else
      ! Check units to remove anything that's out-of-range for thermal bands
      where(imager_measurements%data(:,:,iband)  .gt. 600) &
           imager_measurements%data(:,:,iband) = sreal_fill_value
      where(imager_measurements%data(:,:,iband)  .lt. 10) &
           imager_measurements%data(:,:,iband) = sreal_fill_value
   end if

end subroutine agri_retr_band


!-----------------------------------------------------------------------------
! Name: read_agri
!
! Purpose:
! To read the requested AGRI data from netcdf-format files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infile              string  in   Full path to any AGRI datafile
! geofile             string  in   Full path to the same file
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_flags        struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-----------------------------------------------------------------------------
subroutine read_agri_data(infile, imager_geolocation, imager_measurements, &
                          imager_angles, imager_time, channel_info, &
                          global_atts, verbose)

   use iso_c_binding
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

   ! Various
   integer                     :: i
   character(len=3)            :: cur_band

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_agri_data()'

   ! Figure out the channels to process
   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr
   allocate(band_units(n_bands))

   ! Temporary array for calibration slopes
   allocate(in_slopes(channel_info%all_nchannels_total))

   call ncdf_open(ncid, infile, 'read_agri_data()')

   startx = imager_geolocation%startx
   nx = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny = imager_geolocation%ny

   ! First we sort out the time data
   call compute_time(ncid, imager_time, ny)

   ! Now we load the ancillary data
   call agri_retr_anc(ncid, imager_angles, startx, starty, imager_geolocation)

   ! Load the full calibration slopes array from input data
   call ncdf_read_array(ncid, 'CAL_Slope', in_slopes)

   ! Loop over bands and load data
   do i = 1, n_bands
      write(cur_band, '("C",i2.2)') band_ids(i)
      call agri_retr_band(ncid, cur_band, i, channel_info%channel_lw_flag(i), startx, starty, imager_measurements)
      ! Store the correct calibration slope from the input file.
      imager_measurements%cal_gain(i) = in_slopes(band_ids(i))
   end do

   deallocate(in_slopes)

   call ncdf_close(ncid, 'read_agri_data()')

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_agri_data()'

end subroutine read_agri_data

end module read_agri_m
