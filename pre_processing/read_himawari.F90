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
! Name: AHI_Solpos
!
! Purpose: To compute the solar zenith and azimuth for a given AHI scene
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! year           sreal   In          Year of sensing
! month          sreal   In          Month of sensing
! day            sreal   In          Day of sensing
! hour           sreal   In          Hour of sensing
! minute         sreal   In          Minute of sensing
! lat            sreal   In          Pixel latitude
! lon            sreal   In          Pixel longitude
! sza            sreal   Out         Solar zenith angle
! saa            sreal   Out         Solar azimuth angle
!
!-------------------------------------------------------------------------------
subroutine Get_AHI_Solpos(year, doy, hour, minute, lat, lon, sza, saa)
!$acc routine seq

   use preproc_constants_m
   use solar_position_m

   implicit none

   real(kind=sreal), intent(in)  :: year
   real(kind=sreal), intent(in)  :: doy
   real(kind=sreal), intent(in)  :: hour
   real(kind=sreal), intent(in)  :: minute
   real(kind=sreal), intent(in)  :: lat
   real(kind=sreal), intent(in)  :: lon
   real(kind=sreal), intent(out) :: sza
   real(kind=sreal), intent(out) :: saa

   saa    = 0.
   sza    = 0.
   call sun_pos_calc(year, doy, hour+minute/60., lat, lon, sza, saa)

   if (saa .gt. 360.0) then
      saa = sreal_fill_value
   end if
   if (saa .lt. 0.0) then
      saa = sreal_fill_value
   end if
!   sza = abs(sza)
   if (sza .gt. 180.0) then
      sza = sreal_fill_value
   end if
   if (sza .lt. -180.0) then
      sza = sreal_fill_value
   end if

   return

end subroutine Get_AHI_Solpos

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
!-------------------------------------------------------------------------------
subroutine read_himawari_dimensions(l1_5_file, n_across_track, n_along_track, &
                                    verbose)

   use iso_c_binding
   use preproc_constants_m

   implicit none

   character(len=*),   intent(in)  :: l1_5_file
   integer(kind=lint), intent(out) :: n_across_track, n_along_track
   logical,            intent(in)  :: verbose

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_himawari_dimensions()'

   ! These are constant for the full disk image.
   ! We process only on the lowest-res band size (IR, 2km). VIS bands
   ! are scaled from 0.5 or 1km to this 2km resolution.
   n_along_track  = 5500
   n_across_track = 5500

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
! global_atts         struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_himawari_bin(infile, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, use_predef_geo, geo_file_path, &
   global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use solar_position_m
   use system_utils_m
   use calender_m
#ifdef INCLUDE_HIMAWARI_SUPPORT
   use himawari_readwrite
#endif
#ifdef __PGI
   use ieee_arithmetic
#endif
   implicit none

   character(len=*),            intent(in)    :: infile
   character(len=*),            intent(in)    :: geo_file_path
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: use_predef_geo
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

#ifdef INCLUDE_HIMAWARI_SUPPORT
   integer                     :: i
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: startx, nx
   integer                     :: starty, ny
   integer                     :: x, y
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1

   real(kind=sreal), dimension(:,:), allocatable :: tlat, tlon, tsza, tsaa

   type(himawari_t_data)       :: preproc
   type(himawari_t_extent)     :: ahi_extent
   integer(kind=sint) :: iye, mon, idy, ihr, minu
   real(kind=sreal)   :: rye, rhr, rminu
   real(kind=sreal)   :: sza, saa, doy
   real(kind=dreal)   :: dfr, tmphr
   
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

   line0   = startx - 1
   line1   = startx - 1 + ny - 1
   column0 = starty - 1
   column1 = starty - 1 + nx - 1

   ahi_extent%y_min = line0 + 1
   ahi_extent%y_max = line1 + 1
   ahi_extent%y_size = line1-line0 +1

   ahi_extent%x_min = column0 + 1
   ahi_extent%x_max = column1 + 1
   ahi_extent%x_size = column1-column0 +1

   if (verbose) write(*,*) 'Calling AHI_Main_Read() from ' // &
                           'the himawari_read module'
   ! Load all the data
   if (AHI_Main_Read(trim(infile)//C_NULL_CHAR, & ! Input filename
                     trim(geo_file_path)//C_NULL_CHAR, & ! Geo data filename
                     preproc, & ! Preprocessing data structure
                     ahi_extent, & ! Image extent structure
                     n_bands, & ! Number of bands to process
                     band_ids, & ! Array of band numbers to process (1-16)
                     0, & ! Flag indicating whether AHI reader needs to allocate array space
                     1, & ! Flag indicating whether reader should retrieve geoinfo
                     use_predef_geo, & ! Flag indicating whether an external geo file is being used
                     .false., & ! Flag setting true color output. Currently unused and should be false anyway
                     .false., & ! True = output as VIS channel resolution, False = Output at IR res .
                     global_atts%Satpos_Metadata, & ! Struct to store the satellite position data, for parallax
                     .false., & ! Flag specifying whether to compute solar angles, we calculate them internally in ORAC
                     verbose) .ne. 0) then ! Verbosity flag.
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
   imager_angles%satazi(:,:,1)       = preproc%vaa
   imager_measurements%data(:,:,:)   = preproc%indata   

   ! This section computes the solar geometry for each pixel in the image
   allocate(tsza(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))
   allocate(tsaa(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))

   ! First test if openmp is being used
#ifdef _OPENMP
   if (verbose) write(*,*) "Computing solar geometry using OpenMP"
   !$OMP PARALLEL DO PRIVATE(y, x, iye, mon, dfr, idy, doy, tmphr, ihr, minu, sza, saa, rye, rhr, rminu)
#endif
   ! Then check if ACC/PGI is active (preferred)
#ifdef __ACC
   if (verbose) write(*,*) "Computing solar geometry using PGI_ACC"
!$acc data copyin(tlat) copyin(tlon) copyout(tsza) copyout(tsaa)
!$acc parallel
!$acc loop collapse(2) independent private(y, x, iye, doy, mon, dfr, idy, tmphr, ihr, minu, sza, saa, rye, rhr, rminu)
#endif
   ! Now loop over pixels to compute solar angles
   do y = 1, imager_geolocation%ny
      do x = imager_geolocation%startx, imager_geolocation%endx

         ! We can now use this time to retrieve the actual solar geometry
         if (imager_geolocation%latitude(x,y) .gt. -90. .and. &
             imager_geolocation%latitude(x,y) .lt. 90. .and. &
             imager_geolocation%longitude(x,y) .ge. -180. .and. &
             imager_geolocation%longitude(x,y) .le. 180.) then
               
             call JD2GREG(imager_time%time(x, y), iye, mon, dfr)
             idy = int(dfr)
             tmphr = (dfr-idy)*24.
             ihr = int(tmphr)
             tmphr = (tmphr-ihr)*60.
             minu = int(tmphr)

             call get_day_of_year(float(idy), float(mon), float(iye), doy)

             rye = float(iye)
             rhr = float(ihr)
             rminu = float(minu)
             call Get_AHI_Solpos(rye, doy, rhr, rminu, imager_geolocation%latitude(x,y), imager_geolocation%longitude(x,y), sza, saa)
             tsza(x,y) = sza
             tsaa(x,y) = saa
         else
            tsza(x,y) = sreal_fill_value
            tsaa(x,y) = sreal_fill_value
         end if
      end do
   end do
#ifdef _OPENMP
   !$OMP END PARALLEL DO
#endif
#ifdef __ACC
!$acc end parallel
!$acc end data
#endif

   imager_angles%solzen(:, :, 1) = tsza
   imager_angles%solazi(:, :, 1) = tsaa

   deallocate(tsza)
   deallocate(tsaa)
   
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
   where(imager_angles%satazi(startx:,:,1)       .lt. -900) &
      imager_angles%satazi(startx:,:,1) = sreal_fill_value


#ifdef __PGI
   where(ieee_is_nan(imager_angles%solzen)) &
        imager_angles%solzen = sreal_fill_value
   where(ieee_is_nan(imager_angles%solazi)) &
        imager_angles%solazi = sreal_fill_value
   where(ieee_is_nan(imager_angles%satzen)) &
        imager_angles%satzen = sreal_fill_value
   where(ieee_is_nan(imager_angles%satazi)) &
        imager_angles%satazi = sreal_fill_value
   where(ieee_is_nan(imager_geolocation%latitude)) &
        imager_geolocation%latitude = sreal_fill_value
   where(ieee_is_nan(imager_geolocation%longitude)) &
        imager_geolocation%longitude = sreal_fill_value
#else
   where(is_nan(imager_angles%solzen)) imager_angles%solzen = sreal_fill_value
   where(is_nan(imager_angles%solazi)) imager_angles%solazi = sreal_fill_value
   where(is_nan(imager_angles%satzen)) imager_angles%satzen = sreal_fill_value
   where(is_nan(imager_angles%satazi)) imager_angles%satazi = sreal_fill_value
   where(is_nan(imager_geolocation%latitude)) &
        imager_geolocation%latitude = sreal_fill_value
   where(is_nan(imager_geolocation%longitude)) &
        imager_geolocation%longitude = sreal_fill_value
#endif

    where(imager_angles%solazi(:,:,1) .gt. 900) &
        imager_angles%solazi(:,:,1) = sreal_fill_value
    where(imager_angles%solzen(:,:,1) .gt. 900) &
        imager_angles%solzen(:,:,1) = sreal_fill_value
    where(imager_angles%satzen(:,:,1) .gt. 900) &
        imager_angles%satzen(:,:,1) = sreal_fill_value
    where(imager_angles%satazi(:,:,1) .gt. 900) &
        imager_angles%satazi(:,:,1) = sreal_fill_value


   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%satazi(startx:,:,1) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,1) = abs(imager_angles%satazi(startx:,:,1) - &
                                        imager_angles%solazi(startx:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where
   end where

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_himawari_bin()'
#else
   write(*,*) 'ERROR: the ORAC pre-processor has not been compiled with ' // &
              'HIMAWARI support. Recompile with -DINCLUDE_HIMAWARI_SUPPORT.'
   stop error_stop_code
#endif
end subroutine read_himawari_bin

end module read_himawari_m
