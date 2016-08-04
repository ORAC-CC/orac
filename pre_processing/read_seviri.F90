!-------------------------------------------------------------------------------
! Name: read_seviri.F90
!
! Purpose:
! Module for SEVIRI I/O routines.
!
! History:
! 2015/02/15, GM: First version.
! 2015/07/30, GM: Fixed relative azimuth angle.
! 2015/08/17, GM: Adapt to the newest version of seviri_native_util.
! 2015/08/19, GM: Modifications to support the SEVIRI HRIT format.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_seviri_m

   implicit none

   private

   public :: read_seviri_dimensions, &
             read_seviri_l1_5

contains

!-------------------------------------------------------------------------------
! Name: read_seviri_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! l1_5_file      string  in   Full path to the native level 1.5 image data
! n_across_track lint    out  Number columns in the SEVIRI disk image (constant)
! n_along_track  lint    out  Number lines   in the SEVIRI disk image (constant)
! startx         lint    both First column desired by the caller
! endx           lint    both First line desired by the caller
! starty         lint    both Last column desired by the caller
! endy           lint    both Last line desired by the caller
! verbose        logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_seviri_dimensions(l1_5_file, n_across_track, n_along_track, &
                                  startx, endx, starty, endy, verbose)

   use iso_c_binding
   use preproc_constants_m
#ifdef INCLUDE_SEVIRI_SUPPORT
   use seviri_native_util
#endif
   implicit none

   character(path_length), intent(in)    :: l1_5_file
   integer(lint),          intent(out)   :: n_across_track, n_along_track
   integer(lint),          intent(inout) :: startx, endx, starty, endy
   logical,                intent(in)    :: verbose

   integer :: i_line, i_column
   integer :: n_lines, n_columns

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_seviri_dimensions()'
#ifdef INCLUDE_SEVIRI_SUPPORT
   ! These are constant for the full disk image.
   n_along_track  = 3712
   n_across_track = 3712

   ! Get the starting offset and dimensions of the actual image in the file.
   if (verbose) write(*,*) 'Calling seviri_native_get_dimens_f90() from ' // &
                           'the seviri_native_util module'
   if (seviri_get_dimens_f90(trim(l1_5_file)//C_NULL_CHAR, i_line, i_column, &
       n_lines, n_columns, 1, 0, 0, 0, 0, 0.d0, 0.d0, 0.d0, 0.d0) .ne. 0) then
      write(*,*) 'ERROR: in read_seviri_dimensions(), calling ' // &
                 'seviri_get_dimens_nat_f90(), filename = ', trim(l1_5_file)
      stop error_stop_code
   end if

   if (startx .le. 0 .or. endx .le. 0 .or. starty .le. 0 .or. endy .le. 0) then
      ! If start and end *are not* being used then set them to the start and end
      ! of the actual image in the file.
      starty = i_line + 1
      endy   = i_line + n_lines
      startx = i_column + 1
      endx   = i_column + n_columns
   else
      ! If start and end *are* being used then check that they fall within the
      ! actual image in the file relative to the full disk image.
      if (starty - 1 .lt.  i_line) then
         write(*,*) 'ERROR: user defined starty: ', starty, ', does not ' // &
                    'fall within the actual SEVIRI image starting at: ', &
                    i_line + 1
         stop error_stop_code
      end if
      if (endy - 1 .gt. i_line   + n_lines - 1) then
         write(*,*) 'ERROR: user defined endy: ', endy, ', does not ' // &
                    'fall within the actual SEVIRI image ending at: ', &
                    i_line   + n_lines
         stop error_stop_code
      end if
      if (startx - 1 .lt.  i_column) then
         write(*,*) 'ERROR: user defined startx: ', startx, ', does not ' // &
                    'fall within the actual SEVIRI image starting at: ', &
                    i_column + 1
         stop error_stop_code
      end if
      if (endx - 1 .gt. i_column + n_columns - 1) then
         write(*,*) 'ERROR: user defined endx: ', endx, ', does not ' // &
                    'fall within the actual SEVIRI image ending at: ', &
                    i_column + n_columns
         stop error_stop_code
      end if
   end if
#else
   write(*,*) 'ERROR: the ORAC pre-processor has not been compiled with ' // &
              'SEVIRI support. Recompile with -DINCLUDE_SEVIRI_SUPPORT.'
   stop error_stop_code
#endif
   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_seviri_dimensions()'

end subroutine read_seviri_dimensions


!-------------------------------------------------------------------------------
! Name: read_seviri_l1_5
!
! Purpose:
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
subroutine read_seviri_l1_5(l1_5_file, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, verbose)

   use iso_c_binding
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
#ifdef INCLUDE_SEVIRI_SUPPORT
   use seviri_native_util
#endif
   implicit none

   character(len=path_length),  intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer                     :: i
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: startx, nx
   integer                     :: starty, ny
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1
#ifdef INCLUDE_SEVIRI_SUPPORT
   type(seviri_preproc_t_f90)  :: preproc
#endif
   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_seviri_l1_5()'
#ifdef INCLUDE_SEVIRI_SUPPORT
   ! Setup some arguments to seviri_read_and_preproc_f90()

   n_bands = channel_info%nchannels_total

   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr

   allocate(band_units(n_bands))
   do i = 1, n_bands
      if (channel_info%channel_lw_flag(i) .eq. 0) then
         band_units(i) = SEVIRI_UNIT_REF
      else
         band_units(i) = SEVIRI_UNIT_BT
      end if
   end do

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ! The SEVIRI reader has the option to assume that memory for the output
   ! image arrays has already been allocated. In this case we point these output
   ! array pointers to the already allocated imager arrays to avoid copying.
   preproc%time => imager_time%time(startx:,:)
   preproc%lat  => imager_geolocation%latitude(startx:,:)
   preproc%lon  => imager_geolocation%longitude(startx:,:)
   preproc%sza  => imager_angles%solzen(startx:,:,1)
   preproc%saa  => imager_angles%solazi(startx:,:,1)
   preproc%vza  => imager_angles%satzen(startx:,:,1)
   preproc%vaa  => imager_angles%relazi(startx:,:,1)
   preproc%data => imager_measurements%data(startx:,:,:)

   ! The main reader call which populates preproc (type seviri_preproc_t_f90)
   if (verbose) write(*,*) 'Calling seviri_read_and_preproc_f90() from ' // &
                           'the seviri_native_util module'
   if (seviri_read_and_preproc_f90(trim(l1_5_file)//C_NULL_CHAR, preproc, &
       n_bands, band_ids, band_units, SEVIRI_BOUNDS_LINE_COLUMN, line0, line1, &
       column0, column1, 0.d0, 0.d0, 0.d0, 0.d0, .true.) .ne. 0) then
      write(*,*) 'ERROR: in read_seviri_l1_5(), calling ' // &
                 'seviri_read_and_preproc_f90(), filename = ', trim(l1_5_file)
      stop error_stop_code
   end if

   deallocate(band_ids)
   deallocate(band_units)

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
              'SEVIRI support. Recompile with -DINCLUDE_SEVIRI_SUPPORT.'
   stop error_stop_code
#endif
   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_seviri_l1_5()'

end subroutine read_seviri_l1_5

end module read_seviri_m
