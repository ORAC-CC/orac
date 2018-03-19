!-------------------------------------------------------------------------------
! Name: read_himawari.F90
!
! Purpose:
! Module for Himawari I/O routines.
! To run the preprocessor with Himawari use:
! AHI as the sensor name (1st line of driver)
! Any AHI segment as the L1b file and Geofile.
!
! Segment filenames MUST have this format:
! HS_H08_(YYYYMMDD)_(hhmm)_B(xx)_FLDK_R(rr)_S(ss)10.DAT
! Where:
!    YYYY: year
!    MM: month
!    DD: day
!    hh: hour
!    mm: minute
!    xx: Band number (01 -> 16
!    rr: Resolution (05 for band 3, 10 for other VIS bands, 20 for IR)
!    ss: Segment number (00 -> 10)
!
! Example: HS_H08_20150801_0300_B01_FLDK_R10_S0910.DAT
! In the future other satellites will be launched. Then H08 will become H09,
! etc. This isn't currently supported. Not a big issue, though, as calibration
! info is contained in the files. Nothing external is needed in terms of calib.
!
! Reading Himawari data requires the external HSD_Reader library.
! It can be downloaded from:
!    https://github.com/simonrp84/Himawari_HSD_Reader/
! To link with this util you must add:
!     -DINCLUDE_HIMAWARI_SUPPORT
! and a link to the HSD_Reader lib directory to your orac LIB file.
!
! History:
! 2016/02/23, SP: Initial version.
! 2016/08/04, SP: Set NaN values in angle arrays (deep space pixels) to fill.
! 2017/04/25, SP: Support for verbose mode. A multitude of speed-ups and fixes
!                 such as passing lat/lon file info to himawari util. (ExtWork)
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_himawari_m

   implicit none

   private

   public :: read_himawari_dimensions, &
             read_himawari_bin

contains

!-------------------------------------------------------------------------------
! Name: read_himawari_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! l1_5_file      string  in   Full path to one HSD-format AHI segment (any band)
! n_across_track lint    out  Number columns in the himawari disk image (constant)
! n_along_track  lint    out  Number lines   in the himawari disk image (constant)
! startx         lint    both First column desired by the caller
! endx           lint    both First line desired by the caller
! starty         lint    both Last column desired by the caller
! endy           lint    both Last line desired by the caller
! verbose        logical in   If true then print verbose information.
!
! Note: startx,endx,starty,endy currently ignored.
! It will always process the full disk. This will be fixed.
!-------------------------------------------------------------------------------
subroutine read_himawari_dimensions(l1_5_file, n_across_track, n_along_track, &
                                    startx, endx, starty, endy,verbose)

   use iso_c_binding
   use preproc_constants_m

   implicit none

   character(path_length), intent(in)    :: l1_5_file
   integer(lint),          intent(out)   :: n_across_track, n_along_track
   integer(lint),          intent(inout) :: startx, endx, starty, endy
   logical,                intent(in)    :: verbose

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_himawari_dimensions()'

   ! These are constant for the full disk image.
   ! We process only on the lowest-res band size (IR, 2km). VIS bands
   ! are scaled from 0.5 or 1km to this 2km resolution.
   n_along_track  = 5500
   n_across_track = 5500

   startx = 1
   starty = 1

   endx=n_across_track
   endy=n_along_track

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_himawari_dimensions()'

end subroutine read_himawari_dimensions


!-------------------------------------------------------------------------------
! Name: read_himawari_bin
!
! Purpose:
! To read the requested Himawari data from HSD-format file segments.
! To compute geolocation and solar position information for each pixel in the
! AHI image.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! l1_5_file           string  in   Full path to the native level 1.5 image data
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_himawari_bin(infile, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, use_predef_geo,geo_file_path, &
   verbose)

   use iso_c_binding
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m
#ifdef INCLUDE_HIMAWARI_SUPPORT
   use himawari_readwrite
#endif
   implicit none

   character(len=path_length),  intent(in)    :: infile
   character(len=path_length),  intent(in)    :: geo_file_path
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: use_predef_geo
   logical,                     intent(in)    :: verbose

   integer                     :: i
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: startx, nx
   integer                     :: starty, ny
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1

#ifdef INCLUDE_HIMAWARI_SUPPORT
   type(himawari_t_data)		 ::	preproc
   type(himawari_t_extent)		 ::	ahi_extent
#endif
   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_himawari_bin()'

#ifdef INCLUDE_HIMAWARI_SUPPORT
   ! Figure out the channels to process
   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr
   allocate(band_units(n_bands))

   ! We want everything in reflectance or BT
   do i = 1, n_bands
      band_units(i) = HIMAWARI_UNIT_RBT
   end do

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny


   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ahi_extent%x_min = line0 + 1
   ahi_extent%x_max = line1 + 1
   ahi_extent%y_min = column0 + 1
   ahi_extent%y_max = column1 + 1
   ahi_extent%x_size = imager_geolocation%nx
   ahi_extent%y_size = imager_geolocation%ny

   if (verbose) write(*,*) 'Calling AHI_Main_Read() from ' // &
                           'the himawari_read module'

   ! Load all the data
   if (AHI_Main_Read(trim(infile)//C_NULL_CHAR, &
                     trim(geo_file_path)//C_NULL_CHAR, preproc, ahi_extent, n_bands,&
                     band_ids, 0, 1, use_predef_geo, .false., .false.,verbose) .ne. 0) then
      write(*,*) 'ERROR: in read_himawari_read(), calling ' // &
                 'AHI_Main_Read(), filename = ', trim(infile)
      stop error_stop_code
   end if

   ! Copy arrays between the reader and ORAC. This could (should!) be done more efficiently.
   imager_time%time(:,:)             = preproc%time
   imager_geolocation%latitude(:,:)  = preproc%lat


   imager_geolocation%longitude(:,:) = preproc%lon
   imager_angles%solzen(:,:,1)       = preproc%sza
   imager_angles%solazi(:,:,1)       = preproc%saa
   imager_angles%satzen(:,:,1)       = preproc%vza
   imager_angles%relazi(:,:,1)       = preproc%vaa
   imager_measurements%data(:,:,:)   = preproc%indata

   deallocate(band_ids)
   deallocate(band_units)

   ! Check units to remove anything that's out-of-range.
   ! Can be complicated for Himawari as it takes some deep space measurements.
   ! But the lat/lon should prevent those from being processed, even though
   ! image data will exist.
   where(imager_measurements%data(startx:,:,:)   .lt. -900) &
      imager_measurements%data(startx:,:,:) = sreal_fill_value
   where(imager_geolocation%latitude(startx:,:)  .lt. -900) &
      imager_geolocation%latitude(startx:,:) = sreal_fill_value
   where(imager_geolocation%longitude(startx:,:) .lt. -900) &
      imager_geolocation%longitude(startx:,:) = sreal_fill_value
   where(imager_angles%solazi(startx:,:,1)       .lt. -900) &
      imager_angles%solazi(startx:,:,1) = sreal_fill_value
   where(imager_angles%solzen(startx:,:,1)       .lt. -900) &
      imager_angles%solzen(startx:,:,1) = sreal_fill_value
   where(imager_angles%satzen(startx:,:,1)       .lt. -900) &
      imager_angles%satzen(startx:,:,1) = sreal_fill_value
   where(imager_angles%relazi(startx:,:,1)       .lt. -900) &
      imager_angles%relazi(startx:,:,1) = sreal_fill_value

   where(is_nan(imager_angles%solzen)) imager_angles%solzen = sreal_fill_value
   where(is_nan(imager_angles%solazi)) imager_angles%solazi = sreal_fill_value
   where(is_nan(imager_angles%satzen)) imager_angles%satzen = sreal_fill_value
   where(is_nan(imager_angles%relazi)) imager_angles%relazi = sreal_fill_value

   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%relazi(startx:,:,1) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,1) = abs(imager_angles%relazi(startx:,:,1) - &
                                        imager_angles%solazi(startx:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where
   end where
#else
   write(*,*) 'ERROR: the ORAC pre-processor has not been compiled with ' // &
              'HIMAWARI support. Recompile with -DINCLUDE_HIMAWARI_SUPPORT.'
   stop error_stop_code
#endif

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_himawari_bin()'

end subroutine read_himawari_bin

end module read_himawari_m
