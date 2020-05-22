!-------------------------------------------------------------------------------
! Name: read_seviri.F90
!
! Purpose:
! Module for SEVIRI I/O routines.
!
! History:
! 2015/02/15, GM: First version.
! 2015/07/30, GM: Fixed relative azimuth angle.
! 2015/08/17, GM: Adapt to the newest version of seviri_util.
! 2015/08/19, GM: Modifications to support the SEVIRI HRIT format.
! 2016/12/08, GT: Fixed solar azimuth angle.
! 2017/07/18, SP: Added a (basic) method of subsetting HRIT data
! 2018/06/03, SP: GSICS calibration is now supported for SEVIRI. The default
!                 setting is ON, meaning that GSICS coefficients will be used
!                 instead of IMPF (as previous). The new driver file option
!                 USE_GSICS enables this to be disabled.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_seviri_m

   implicit none

   integer, parameter :: SEVIRI_TYPE_METOFF = 1
   integer, parameter :: SEVIRI_TYPE_HRIT   = 2
   integer, parameter :: SEVIRI_TYPE_NAT    = 3

   private

   public :: read_seviri_dimensions, &
             read_seviri_l1_5, &
             SEV_Retrieve_Predef_Geo

contains

integer function determine_seviri_file_type(l1_file)
   implicit none

   character(len=*), intent(in) :: l1_file

   if (index(trim(l1_file), '.h5') > 0) then
      determine_seviri_file_type = SEVIRI_TYPE_METOFF
   else if (index(trim(l1_file), 'H-000-MSG') > 0) then
      determine_seviri_file_type = SEVIRI_TYPE_HRIT
   else
      determine_seviri_file_type = SEVIRI_TYPE_NAT
   end if

end function determine_seviri_file_type

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

   use preproc_constants_m
#ifdef INCLUDE_SEVIRI_SUPPORT
   use seviri_util
#endif
   implicit none

   character(len=*),   intent(in)    :: l1_5_file
   integer(kind=lint), intent(out)   :: n_across_track, n_along_track
   integer(kind=lint), intent(inout) :: startx, endx, starty, endy
   logical,            intent(in)    :: verbose

   integer :: i_line, i_column
   integer :: n_lines, n_columns
   integer :: ftype

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_seviri_dimensions()'
   ! These are constant for the full disk image.
   n_along_track  = 3712
   n_across_track = 3712

   ftype = determine_seviri_file_type(l1_5_file)

   if (ftype == SEVIRI_TYPE_METOFF) then
      ! Met Office HDF files don't have a useful header so hard code the limits
      i_line    = 0
      i_column  = 0
      n_lines   = 3712
      n_columns = 3712
   else
#ifdef INCLUDE_SEVIRI_SUPPORT
      ! Get the starting offset and dimensions of the actual image in the file.
      if (verbose) write(*,*) 'Calling seviri_native_get_dimens_f90() from ' // &
                              'the seviri_util module'
      if (seviri_get_dimens_f90(l1_5_file, i_line, i_column, n_lines, &
           n_columns, 1, 0, 0, 0, 0, 0.d0, 0.d0, 0.d0, 0.d0) .ne. 0) then
         write(*,*) 'ERROR: in read_seviri_dimensions(), calling ' // &
              'seviri_get_dimens_nat_f90(), filename = ', trim(l1_5_file)
         stop error_stop_code
      end if
#else
      write(*,*) 'ERROR: the ORAC pre-processor has not been compiled with ' // &
                 'SEVIRI support. Recompile with -DINCLUDE_SEVIRI_SUPPORT.'
      stop error_stop_code
#endif
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
   if (index(l1_5_file, trim('RSS'))>0 .and. ftype==SEVIRI_TYPE_HRIT) then
      endy = endy/3
   end if

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_seviri_dimensions()'

end subroutine read_seviri_dimensions

! Temporary function for use until seviri_util predef geo is fixed. INEFFICIENT.
subroutine SEV_Retrieve_Predef_Geo(imager_geolocation, imager_angles, &
     geofile, verbose)

   use orac_ncdf_m
   use imager_structures_m
   implicit none

   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   character(len=*),           intent(in)    :: geofile
   logical,                    intent(in)    :: verbose

   integer :: ncid, start(2)

   start(1) = imager_geolocation%startx
   start(2) = imager_geolocation%starty

   call ncdf_open(ncid, geofile, 'SEV_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, "Lat", imager_geolocation%latitude, verbose, start=start)
   call ncdf_read_array(ncid, "Lon", imager_geolocation%longitude, verbose, start=start)
   call ncdf_read_array(ncid, "VZA", imager_angles%satzen(:,:,1), verbose, start=start)
   call ncdf_read_array(ncid, "VAA", imager_angles%satazi(:,:,1), verbose, start=start)
   call ncdf_close(ncid, 'SEV_Retrieve_Predef_Geo()')

end subroutine SEV_Retrieve_Predef_Geo


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
! global_atts         struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_seviri_l1_5(l1_5_file, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, do_gsics, global_atts, verbose)

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m

   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: do_gsics
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   real, allocatable :: tmparr(:,:,:)

   integer :: startx

   if (determine_seviri_file_type(l1_5_file) == SEVIRI_TYPE_METOFF) then
      call read_seviri_l1_5_metoff(l1_5_file, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           global_atts, verbose)
   else
      call read_seviri_l1_5_nat_or_hrit(l1_5_file, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           do_gsics, global_atts, verbose)
   end if

   allocate(tmparr(1:imager_geolocation%nx,1:imager_geolocation%ny,1))

   startx = imager_geolocation%startx

   tmparr = imager_angles%satazi(imager_geolocation%endx:imager_geolocation%startx:-1,:,:)
   imager_angles%satazi = tmparr

   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%satazi(startx:,:,1) .ne. sreal_fill_value)
      imager_angles%solazi(:,:,1) = imager_angles%solazi(startx:,:,1) - 180.
      where(imager_angles%solazi(:,:,1) .lt. 0.)
         imager_angles%solazi(:,:,1) = imager_angles%solazi(:,:,1) + 360.
      end where

      imager_angles%relazi(:,:,1) = abs(imager_angles%satazi(startx:,:,1) - &
                                        imager_angles%solazi(startx:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where
   end where

   deallocate(tmparr)

end subroutine read_seviri_l1_5

subroutine read_seviri_l1_5_metoff(l1_5_file, imager_geolocation, &
   imager_measurements, imager_angles, imager_time, channel_info, &
   global_atts, verbose)

   use channel_structures_m
   use common_constants_m, only: error_stop_code
   use global_attributes_m
   use hdf5
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer             :: i
   integer             :: startx, nx, starty, ny
   integer             :: err_code
   character(len=8)    :: channel_name
   character(len=4)    :: variable_name
   integer(kind=HID_T) :: file_id
   real, pointer       :: temp2(:,:), temp3(:,:,:)

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_seviri_l1_5_metoff()'

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   ! The Met Office output calibrated radiances as an HDF5 file
   call h5open_f(err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot start HDF5'
      stop error_stop_code
   end if
   call h5fopen_f(l1_5_file, H5F_ACC_RDONLY_F, file_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot open file ', trim(l1_5_file)
      stop error_stop_code
   end if

   allocate(temp3(2, startx:imager_geolocation%endx, &
        starty:imager_geolocation%endy))
   call read_seviri_hdf_field3(file_id, "Static/MSG/Ancillary", "LatLon", &
        temp3, [1, startx, starty], [1, nx, ny])
   imager_geolocation%latitude = temp3(1,:,:)
   imager_geolocation%longitude = temp3(2,:,:)
   deallocate(temp3)

   temp2 => imager_angles%satazi(startx:,:,1)
   call read_seviri_hdf_field2(file_id, "MSG/Ancillary", "SatAzimuthAngle", &
        temp2, [startx, starty], [nx, ny])
   temp2 => imager_angles%satzen(startx:,:,1)
   call read_seviri_hdf_field2(file_id, "MSG/Ancillary", "SatZenithAngle", &
        temp2, [startx, starty], [nx, ny])
   temp2 => imager_angles%solazi(startx:,:,1)
   call read_seviri_hdf_field2(file_id, "MSG/Ancillary", "SolAziAngle", &
        temp2, [startx, starty], [nx, ny])
   temp2 => imager_angles%solzen(startx:,:,1)
   call read_seviri_hdf_field2(file_id, "MSG/Ancillary", "SolZenAngle", &
        temp2, [startx, starty], [nx, ny])

   do i = 1, channel_info%nchannels_total
      ! Name of data field is different for visible and thermal channels
      write(channel_name, '("MSG/Ch", i0.2)') channel_info%channel_ids_instr(i)
      if (channel_info%channel_lw_flag(i) == 1) then
         variable_name = "BT"
      else
         variable_name = "Refl"
      end if

      temp2 => imager_measurements%data(:,:,i)
      call read_seviri_hdf_field2(file_id, channel_name, variable_name, &
           temp2, [startx, starty], [nx, ny])

      ! Correct fill value
      where (temp2 == -1073741800.0)
         temp2 = sreal_fill_value
      end where
   end do

   call h5fclose_f(file_id, err_code)
   call h5close_f(err_code)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_seviri_l1_5_metoff()'

end subroutine read_seviri_l1_5_metoff

subroutine read_seviri_l1_5_nat_or_hrit(l1_5_file, imager_geolocation, &
   imager_measurements, imager_angles, imager_time, channel_info, do_gsics, &
   global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use common_constants_m, only: error_stop_code
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
#ifdef INCLUDE_SEVIRI_SUPPORT
   use seviri_util
#endif
   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: do_gsics
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer                     :: i
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: n_across_track, n_along_track
   integer                     :: startx, nx
   integer                     :: starty, ny
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1
   logical                     :: hrit_proc
#ifdef INCLUDE_SEVIRI_SUPPORT
   type(seviri_preproc_t_f90)  :: preproc
#endif
   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_seviri_l1_5_nat_or_hrit()'
#ifdef INCLUDE_SEVIRI_SUPPORT

   ! Test if HRIT
   if (index(trim(l1_5_file), 'H-000-MSG') .le. 0) then
      hrit_proc = .false.
   else
      hrit_proc = .true.
   end if

   ! Fetch image dimensions (start/nx are placeholders and overriden below)
   startx = 0
   nx = 0
   starty = 0
   ny = 0
   call read_seviri_dimensions(l1_5_file, n_across_track, n_along_track, &
        startx, nx, starty, ny, .false.)

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

   if (verbose) then
      if (do_gsics) then
         write(*,*) 'Applying GSICS calibration coefficients'
      else
         write(*,*) 'Applying IMPF calibration coefficients'
      end if
   end if

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   if (.not. hrit_proc .and. (nx .eq. 3712 .and. ny .eq. 3712)) then

      ! The SEVIRI reader has the option to assume that memory for the output
      ! image arrays has already been allocated. In this case we point these output
      ! array pointers to the already allocated imager arrays to avoid copying.
      preproc%time => imager_time%time(startx:,:)
      preproc%lat  => imager_geolocation%latitude(startx:,:)
      preproc%lon  => imager_geolocation%longitude(startx:,:)
      preproc%sza  => imager_angles%solzen(startx:,:,1)
      preproc%saa  => imager_angles%solazi(startx:,:,1)
      preproc%vza  => imager_angles%satzen(startx:,:,1)
      preproc%vaa  => imager_angles%satazi(startx:,:,1)
      preproc%data => imager_measurements%data(startx:,:,:)

      ! The main reader call which populates preproc (type seviri_preproc_t_f90)
      if (verbose) write(*,*) 'Calling seviri_read_and_preproc_f90() from ' // &
                              'the seviri_util module, FD w/o alloc'
      if (seviri_read_and_preproc_f90(trim(l1_5_file)//C_NULL_CHAR, preproc, &
          n_bands, band_ids, band_units, SEVIRI_BOUNDS_FULL_DISK, 0, 0, &
          0, 0, 0.d0, 0.d0, 0.d0, 0.d0, do_gsics, global_atts%Satpos_Metadata, .true.) .ne. 0) then
         write(*,*) 'ERROR: in read_seviri_l1_5(), calling ' // &
                    'seviri_read_and_preproc_f90(), filename = ', trim(l1_5_file)
         stop error_stop_code
      end if
   else if (.false.) then
      ! Because of the inversion of satazi, internal subsetting cannot be used

      ! The SEVIRI reader has the option to assume that memory for the output
      ! image arrays has already been allocated. In this case we point these output
      ! array pointers to the already allocated imager arrays to avoid copying.
      preproc%time => imager_time%time(startx:,:)
      preproc%lat  => imager_geolocation%latitude(startx:,:)
      preproc%lon  => imager_geolocation%longitude(startx:,:)
      preproc%sza  => imager_angles%solzen(startx:,:,1)
      preproc%saa  => imager_angles%solazi(startx:,:,1)
      preproc%vza  => imager_angles%satzen(startx:,:,1)
      preproc%vaa  => imager_angles%satazi(startx:,:,1)
      preproc%data => imager_measurements%data(startx:,:,:)

      ! The main reader call which populates preproc (type seviri_preproc_t_f90)
      if (verbose) write(*,*) 'Calling seviri_read_and_preproc_f90() from ' // &
                              'the seviri_util module, LC'
      if (seviri_read_and_preproc_f90(trim(l1_5_file)//C_NULL_CHAR, preproc, &
          n_bands, band_ids, band_units, SEVIRI_BOUNDS_LINE_COLUMN, line0, line1, &
          column0, column1, 0.d0, 0.d0, 0.d0, 0.d0, do_gsics, global_atts%Satpos_Metadata, .true.) .ne. 0) then
         write(*,*) 'ERROR: in read_seviri_l1_5(), calling ' // &
                    'seviri_read_and_preproc_f90(), filename = ', trim(l1_5_file)
         stop error_stop_code
      end if
   else
      ! The main reader call which populates preproc (type seviri_preproc_t_f90)
      if (verbose) write(*,*) 'Calling seviri_read_and_preproc_f90() from ' // &
                              'the seviri_util module, FD'
      if (seviri_read_and_preproc_f90(trim(l1_5_file)//C_NULL_CHAR, preproc, &
          n_bands, band_ids, band_units, SEVIRI_BOUNDS_FULL_DISK, 0, 0, &
          0, 0, 0.d0, 0.d0, 0.d0, 0.d0, do_gsics, global_atts%Satpos_Metadata, .false.) .ne. 0) then
         write(*,*) 'ERROR: in read_seviri_l1_5(), calling ' // &
                    'seviri_read_and_preproc_f90(), filename = ', trim(l1_5_file)
         stop error_stop_code
      end if

      imager_time%time(startx:,:)             = preproc%time(column0+1:column1+1,line0+1:line1+1)
      imager_geolocation%latitude(startx:,:)  = preproc%lat(column0+1:column1+1,line0+1:line1+1)
      imager_geolocation%longitude(startx:,:) = preproc%lon(column0+1:column1+1,line0+1:line1+1)
      imager_angles%solzen(startx:,:,1)       = preproc%sza(column0+1:column1+1,line0+1:line1+1)
      imager_angles%solazi(startx:,:,1)       = preproc%saa(column0+1:column1+1,line0+1:line1+1)
      imager_angles%satzen(startx:,:,1)       = preproc%vza(column0+1:column1+1,line0+1:line1+1)
      !imager_angles%satazi(startx:,:,1)       = preproc%vaa(column0+1:column1+1,line0+1:line1+1)
      imager_measurements%data(startx:,:,:)   = preproc%data(column0+1:column1+1,line0+1:line1+1,:)

      ! Offset subset for inversion of satazi
      imager_angles%satazi(startx:,:,1)       = preproc%vaa(n_across_track-column1:n_across_track-column0,line0+1:line1+1)
   end if

   ! Remove underscores added by seviri_util (easy way of converting c-string to
   ! f-string).
   i = index(global_atts%Satpos_Metadata, '_')
   global_atts%Satpos_Metadata = global_atts%Satpos_Metadata(1:i-1)

   deallocate(band_ids)
   deallocate(band_units)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_seviri_l1_5_nat_or_hrit()'
#else
   write(*,*) 'ERROR: the ORAC pre-processor has not been compiled with ' // &
              'SEVIRI support. Recompile with -DINCLUDE_SEVIRI_SUPPORT.'
   stop error_stop_code
#endif


end subroutine read_seviri_l1_5_nat_or_hrit

subroutine read_seviri_hdf_field2(file_id, group, variable, temp, starts, nums)

   use common_constants_m, only: error_stop_code
   use hdf5

   implicit none

   integer,   parameter                       :: ndims = 2
   integer(kind=HID_T),         intent(in)    :: file_id
   character(len=*),            intent(in)    :: group
   character(len=*),            intent(in)    :: variable
   real,      pointer,          intent(inout) :: temp(:,:)
   integer,   dimension(ndims), intent(in)    :: starts
   integer,   dimension(ndims), intent(in)    :: nums

   integer                                 :: err_code, i
   integer(kind=HID_T)                     :: group_id, dset_id, space_id, mem_id
   integer(kind=HSIZE_T), dimension(ndims) :: start, edge

   ! Only open the requested data
   do i = 1, ndims
      start(i) = int(starts(i) - 1, kind=HSIZE_T)
      edge(i) = int(nums(i), kind=HSIZE_T)
   end do

   ! Read array
   call h5gopen_f(file_id, group, group_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot open group ', trim(group)
      stop error_stop_code
   end if
   call h5dopen_f(group_id, variable, dset_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot open field ', trim(variable)
      stop error_stop_code
   end if
   call h5dget_space_f(dset_id, space_id, err_code)
   call h5sselect_hyperslab_f(space_id, H5S_SELECT_SET_F, start, edge, &
        err_code)
   call h5screate_simple_f(ndims, edge, mem_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot create memory space'
      stop error_stop_code
   end if
   call h5dread_f(dset_id, H5T_NATIVE_REAL, temp, edge, err_code, mem_id, &
        space_id)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot read ', trim(group), '/', &
           trim(variable)
      stop error_stop_code
   end if
   call h5sclose_f(mem_id, err_code)
   call h5sclose_f(space_id, err_code)
   call h5dclose_f(dset_id, err_code)
   call h5gclose_f(group_id, err_code)

end subroutine read_seviri_hdf_field2

subroutine read_seviri_hdf_field3(file_id, group, variable, temp, starts, nums)

   use common_constants_m, only: error_stop_code
   use hdf5

   implicit none

   integer,   parameter                       :: ndims = 3
   integer(kind=HID_T),         intent(in)    :: file_id
   character(len=*),            intent(in)    :: group
   character(len=*),            intent(in)    :: variable
   real,      pointer,          intent(inout) :: temp(:,:,:)
   integer,   dimension(ndims), intent(in)    :: starts
   integer,   dimension(ndims), intent(in)    :: nums

   integer                                 :: err_code, i
   integer(kind=HID_T)                     :: group_id, dset_id, space_id, mem_id
   integer(kind=HSIZE_T), dimension(ndims) :: start, edge

   ! Only open the requested data
   do i = 1, ndims
      start(i) = int(starts(i) - 1, kind=HSIZE_T)
      edge(i) = int(nums(i), kind=HSIZE_T)
   end do

   ! Read array
   call h5gopen_f(file_id, group, group_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot open group ', trim(group)
      stop error_stop_code
   end if
   call h5dopen_f(group_id, variable, dset_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot open field ', trim(variable)
      stop error_stop_code
   end if
   call h5dget_space_f(dset_id, space_id, err_code)
   call h5sselect_hyperslab_f(space_id, H5S_SELECT_SET_F, start, edge, &
        err_code)
   call h5screate_simple_f(ndims, edge, mem_id, err_code)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot create memory space'
      stop error_stop_code
   end if
   call h5dread_f(dset_id, H5T_NATIVE_REAL, temp, edge, err_code, mem_id, &
        space_id)
   if (err_code /= 0) then
      write(*,*) 'ERROR: read_seviri(): Cannot read ', trim(group), '/', &
           trim(variable)
      stop error_stop_code
   end if
   call h5sclose_f(mem_id, err_code)
   call h5sclose_f(space_id, err_code)
   call h5dclose_f(dset_id, err_code)
   call h5gclose_f(group_id, err_code)

end subroutine read_seviri_hdf_field3

end module read_seviri_m
