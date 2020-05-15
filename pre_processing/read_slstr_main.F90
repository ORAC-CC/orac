!-------------------------------------------------------------------------------
! Name: read_slstr_main.F90
!
! Purpose:
! Module for SLSTR I/O routines.
! To run the preprocessor with SLSTR use:
!    SLSTR as the sensor name (1st line of driver)
!    Any S-band radiance file (eg: S4_radiance_an.nc) as the data file (2nd line)
!    The corresponding tx file (eg: geodetic_tx.nc) as the geo file (3rd line)
!
! All M-band filenames to be read must have the same filename format!
!
! History:
! 2016/06/14, SP: Initial version.
! 2016/07/08, SP: Bug fixes.
! 2016/07/22, SP: Implement the second view (oblique).
! 2016/11/24, SP: Pass view to TIR reading functions for offset correction
! 2017/05/12, SP: Added 'correction' to bypass problems with S7 channel.
!                 Now bad data (T>305K) is replaced by values from F1 channel.
!                 This is not elegant due to F1 issues, but better than no data.
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2020/02/24, SP: Remove the S7/F1 correction, as new processing baseline makes
!                 this harder to implement, but also makes it less necessary.
!                 The S7 band now saturates at 312K rather than 305K.
! 2020/03/17, AP: Read land/sea mask from the flags_XX:confidence_XX field.
! 2020/04/13, AP: Have read_slstr_dimensions() use get_slstr_gridsize() so it
!                 explictly checks the geodetic_in file, rather than whatever
!                 file the user happened to pass as l1b_file.
! 2020/04/14, AP: Add subsetting to the read functions. The entire tx grid is
!                 read as it's difficult to predict how much will be needed.
!
! Bugs:
! None known (in ORAC)
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: read_slstr_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! geo_file       string  in   Full path to one slstr image file
! n_across_track lint    out  Number columns in the slstr image (usually constant)
! n_along_track  lint    out  Number lines   in the slstr image (usually constant)
! verbose        logical in   If true then print verbose information.
!
!-------------------------------------------------------------------------------
subroutine read_slstr_dimensions(img_file, n_across_track, n_along_track, verbose)

   use iso_c_binding
   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*),   intent(in)    :: img_file
   integer(kind=lint), intent(out)   :: n_across_track, n_along_track
   logical,            intent(in)    :: verbose

   integer :: i

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_slstr_dimensions()'

   ! To ensure consistency across different processings, the SLSTR scene is
   ! defined by the in grid. All other grids (e.g. processing the oblique view
   ! only) are referenced to here, such that the x/y coords don't need to be
   ! changed when processing different grids.
   i = index(img_file, '/', .true.)
   call get_slstr_gridsize(img_file(1:i), 'in', n_across_track, n_along_track)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_slstr_dimensions()'

end subroutine read_slstr_dimensions


!-------------------------------------------------------------------------------
! Name: read_slstr
!
! Purpose:
! To read the requested VIIRS data from HDF5-format files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infile              string  in   Full path to any M-band image file
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_slstr(infile, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, imager_flags, channel_info, verbose)

   use iso_c_binding
   use calender_m
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),            intent(in)    :: infile
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(imager_flags_t),        intent(inout) :: imager_flags
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer                       :: i, j
   integer(c_int)                :: n_bands
   integer(c_int), allocatable   :: band_ids(:)
   integer(c_int), allocatable   :: band_units(:)
   integer                       :: startx, endx, nx
   integer                       :: starty, ny

   character(len=path_length)    :: indir

   real(kind=sreal), allocatable :: txlats(:,:)
   real(kind=sreal), allocatable :: txlons(:,:)
   real(kind=sreal), allocatable :: interp(:,:,:)

   integer(kind=sint)            :: surface_flag(imager_geolocation%nx,imager_geolocation%ny)
   integer                       :: txnx, txny
   integer                       :: sx_nad, sx_obl, ex_nad, ex_obl

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_slstr()'

   ! Figure out the channels to process
   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr
   allocate(band_units(n_bands))

   startx = imager_geolocation%startx
   endx   = imager_geolocation%endx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   ! Sort out the start and end times, place into the time array
   call get_slstr_startend(imager_time, infile, starty, ny)

   j = index(infile, '/', .true.)
   indir = infile(1:j)

   if (verbose) write(*,*) 'Reading geoinformation data for SLSTR grids'

   ! Find size of tx grid. Should be 130,1200 but occasionally isn't
   call get_slstr_gridsize(indir, 'tx', txnx, txny)

   ! Then allocate arrays for tx data
   allocate(txlats(txnx,txny))
   allocate(txlons(txnx,txny))

   ! Then allocate arrays for the interpolation results
   allocate(interp(nx,ny,3))

   ! Read primary dem, lat and lon
   call read_slstr_int_field(indir, 'geodetic', 'in', 'elevation', startx, starty, &
        imager_geolocation%dem)
   call read_slstr_field(indir, 'geodetic', 'in', 'latitude', startx, starty, &
        imager_geolocation%latitude)
   call read_slstr_field(indir, 'geodetic', 'in', 'longitude', startx, starty, &
        imager_geolocation%longitude)

   ! Read reduced grid lat/lon
   call read_slstr_field(indir, 'geodetic', 'tx', 'latitude', 1, 1, txlats)
   call read_slstr_field(indir, 'geodetic', 'tx', 'longitude', 1, 1, txlons)

   if (imager_angles%nviews .eq. 2) then
      ! Get alignment factor between oblique and nadir views
      call slstr_get_alignment(indir, startx, endx, sx_nad, sx_obl, &
           ex_nad, ex_obl)
   end if

   ! Get interpolation factors between reduced and TIR grid for each pixel
   call slstr_get_interp(imager_geolocation%longitude, txlons, txnx, txny, nx, ny, &
        interp)

   deallocate(txlats)
   deallocate(txlons)

   if (verbose) write(*,*) 'Reading geometry data for SLSTR geo grid'

   ! Read satellite and solar angles for the nadir viewing geometry
   call read_slstr_satsol(indir, imager_angles, interp, txnx, txny, nx, ny, startx, 1)
   if (imager_angles%nviews .eq. 2) then
      ! Read satellite and solar angles for the oblique viewing geometry
      call read_slstr_satsol(indir, imager_angles, interp, txnx, txny, nx, ny, startx, 2)
   end if

   deallocate(interp)

   ! Read land/sea mask. This is a bitmask where,
   !   1:   coastline        256:   cosmetic
   !   2:   ocean            512:   duplicate
   !   4:   tidal            1024:  day
   !   8:   land             2048:  twilight
   !   16:  inland_water     4096:  sun_glint
   !   32:  unfilled         8192:  snow
   !   64:  spare            16348: summary_cloud
   !   128: spare            32768: summary_pointing
   ! To be consistent with read_modis_time-lat_lon_angles(), we accept
   ! only classes 2 and 16 as sea.
   call read_slstr_int_field(indir, 'flags', 'in', 'confidence', startx, starty, &
        surface_flag)
   where (btest(surface_flag, 1) .or. btest(surface_flag, 4))
      imager_flags%lsflag = 0
   else where
      imager_flags%lsflag = 1
   end where

   ! This bit reads all the data.
   do i = 1, n_bands
      if (verbose) write(*,*) 'Reading SLSTR data for band', band_ids(i)
      if (band_ids(i) .lt. 7) then
         call read_slstr_visdata(indir, band_ids(i), &
              imager_measurements%data(:,:,i), imager_angles, startx, starty, &
              nx, ny)
      else if (band_ids(i) .le. 9) then
         call read_slstr_tirdata(indir, band_ids(i), &
              imager_measurements%data(:,:,i), startx, starty)
      else if (band_ids(i) .lt. 16) then
         if (sx_nad .gt. 0 .and. ex_nad .gt. 0) then
            call read_slstr_visdata(indir, band_ids(i), &
                 imager_measurements%data(sx_nad:ex_nad,:,i), &
                 imager_angles, sx_obl, starty, ex_obl-sx_obl+1, ny)
         end if
      else if (band_ids(i) .le. 18) then
         if (sx_nad .gt. 0 .and. ex_nad .gt. 0) then
            call read_slstr_tirdata(indir, band_ids(i), &
                 imager_measurements%data(sx_nad:ex_nad,:,i), sx_obl, starty)
         end if
      else
         write(*,*) 'Invalid band_id! Must be in range 1->18', band_ids(i)
         stop error_stop_code
      end if

      ! Apply some correction factors
      if (band_ids(i) .eq. 1) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 0.95
      else if (band_ids(i) .eq. 5) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 1.1
      else if (band_ids(i) .eq. 6) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 1.1
      else if (band_ids(i) .eq. 10) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 0.92
      else if (band_ids(i) .eq. 11) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 0.93
      else if (band_ids(i) .eq. 12) then
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i) * 0.93
      end if

      ! Other corrections have previously been applied in this code to minimise
      ! problems with the geolocation, dynamic range, and calibration of SLSTR.
      ! These have been removed at this point, as many are now included in the
      ! L1B files but may still be necessary for earlier data. The main one is
      ! channels 10-15 had a pixel offset one less than channels 16-18
   end do
   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_slstr()'

end subroutine read_slstr
