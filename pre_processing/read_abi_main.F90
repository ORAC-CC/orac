!-------------------------------------------------------------------------------
! Name: read_abi_main.F90
!
! Purpose:
! Contains functions for reading data from GOES-16 onwards. These are typically
! high level functions to control data read-in, low level in read_abi_funcs.f90
! History:
! 2018/02/10, SP: First version.
! 2018/06/08, SP: New global attribute to store satellite position information
! 2018/10/26, SP: Rename to prevent clashes with GOES-Imager reader
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: read_abi_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! l1_5_file      string  in   Full path to one netCDF GOES channel (any band)
! n_across_track lint    out  Number columns in the GOES disk image (constant)
! n_along_track  lint    out  Number lines   in the GOES disk image (constant)
! verbose        logical in   If true then print verbose information.
!
!-------------------------------------------------------------------------------
subroutine read_abi_dimensions(l1_5_file, n_across_track, n_along_track, verbose)

   use preproc_constants_m
   use orac_ncdf_m
   use orac_ncdf_m

   implicit none

   character(len=*),   intent(in)  :: l1_5_file
   integer(kind=lint), intent(out) :: n_across_track, n_along_track
   logical,            intent(in)  :: verbose

   integer          :: fid, index2, band
   integer          :: n_lines, n_cols
   character(len=2) :: cband

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_abi_dimensions()'

   index2 = index(l1_5_file, '_G1')
   cband  = l1_5_file(index2-2:index2-1)

   read(cband, '(I2)') band

   call ncdf_open(fid, l1_5_file, 'read_abi_dimensions()')

   ! Read actual size of the netCDF4 file
   n_cols  = ncdf_dim_length(fid, 'x', 'read_abi_dimensions()')
   n_lines = ncdf_dim_length(fid, 'y', 'read_abi_dimensions()')

   ! Make sure we account for the fact that some bands are hi-res
   if (band .eq. 1 .or. band .eq. 3 .or. band .eq. 5) then
      n_cols  = n_cols/2
      n_lines = n_lines/2
   else if (band .eq. 2) then
      n_cols  = n_cols/4
      n_lines = n_lines/4
   end if

   ! Close the netCDF4 file
   call ncdf_close(fid, 'read_abi_dimensions()')

   n_across_track = n_cols
   n_along_track  = n_lines

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_abi_dimensions()'

end subroutine read_abi_dimensions

!-------------------------------------------------------------------------------
! Name: get_abi_data
!
! Purpose:
! To read the requested GOES data from NetCDF-format file segments.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infiles             string  in   Full path to the native image data
! imager_angles       struct  in   Members within are populated
! imager_measurements struct  both Members within are populated
! channel_info        struct  in   Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine get_abi_data(infiles, imager_angles, imager_measurements, &
     imager_geolocation, channel_info, verbose)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),            intent(in)    :: infiles(:)
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer, allocatable  :: band_ids(:)
   real,    allocatable  :: tmprad(:,:)
   real,    allocatable  :: tmpout(:,:)

   integer :: n_bands, i
   real    :: irrad
   real    :: bc1, bc2, fk1, fk2

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< get_abi_data()'

   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr

   allocate(tmpout(imager_geolocation%nx,imager_geolocation%ny))

   do i = 1, n_bands
      if (band_ids(i) .lt. 7) then
         tmpout(:,:) = sreal_fill_value
         if (verbose) write(*,*) "Loading GOES visible band ", band_ids(i)
         if (band_ids(i) .eq. 1 .or. band_ids(i) .eq. 3 .or. band_ids(i) .eq. 5) then
            allocate(tmprad(imager_geolocation%nx*2,imager_geolocation%ny*2))
            call load_abi_band(infiles(i), imager_geolocation, tmprad, irrad, bc1, bc2, fk1, fk2, 2, verbose)
            call goes_resample_vis_to_tir(tmprad, tmpout, imager_geolocation%nx, imager_geolocation%ny, sreal_fill_value, 2, verbose)
         elseif (band_ids(i) .eq. 2) then
            allocate(tmprad(imager_geolocation%nx*4,imager_geolocation%ny*4))
            call load_abi_band(infiles(i), imager_geolocation, tmprad, irrad, bc1, bc2, fk1, fk2, 4, verbose)
            call goes_resample_vis_to_tir(tmprad, tmpout, imager_geolocation%nx, imager_geolocation%ny, sreal_fill_value, 4, verbose)
         else
            allocate(tmprad(imager_geolocation%nx,imager_geolocation%ny))
            call load_abi_band(infiles(i), imager_geolocation, tmpout, irrad, bc1, bc2, fk1, fk2, 1, verbose)
         end if

         tmpout = tmpout * irrad
         where(imager_geolocation%latitude .eq. sreal_fill_value) tmpout = sreal_fill_value
         where(tmpout .lt. -0.1) tmpout = sreal_fill_value
         imager_measurements%data(:,:,i) = tmpout

         deallocate(tmprad)
      else
         if (verbose) write(*,*) "Loading GOES thermal band ", band_ids(i)
         allocate(tmprad(imager_geolocation%nx,imager_geolocation%ny))

         call load_abi_band(infiles(i), imager_geolocation, tmprad, irrad, bc1, bc2, fk1, fk2, 1, verbose)

         tmprad = (fk2/(log((fk1/tmprad)+1))-bc1) / bc2

         where(tmprad .lt. 130) tmprad = sreal_fill_value
         where(tmprad .gt. 500) tmprad = sreal_fill_value

         imager_measurements%data(:,:,i) = tmprad

         deallocate(tmprad)
      end if
      where (imager_geolocation%latitude .eq. sreal_fill_value)
         imager_measurements%data(:,:,i) = sreal_fill_value
      end where
   end do

   deallocate(tmpout)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> get_abi_data()'

end subroutine get_abi_data


!-------------------------------------------------------------------------------
! Name: read_abi_bin
!
! Purpose:
! Main function for pre-processing GOES data, computes geo+solar+channel data
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infiles             string  in   Full path to the native image data
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! global_atts         struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_abi_bin(infiles, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, use_predef_geo, geo_file_path, &
   global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),            intent(in)    :: infiles(:)
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: use_predef_geo
   character(len=*),            intent(in)    :: geo_file_path
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer :: i, goodf

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_abi_bin()'

   goodf = -1

   do i = 1, channel_info%nchannels_total
      if(channel_info%channel_ids_instr(i) .eq. 4 .or. channel_info%channel_ids_instr(i) .gt. 6) then
         goodf = i
         exit
      end if
   end do
   if (goodf .le. 0) then
      write(*,*) "ERROR: No Infrared channels have been selected:", channel_info%channel_ids_instr(i)
      stop
   end if

   ! Determine geolocation information (lat/lon/vza/vaa)
   if (.not. use_predef_geo) then
      call get_abi_geoloc(infiles(goodf), imager_geolocation, imager_angles, global_atts, verbose)
   else
      ! This is a placeholder for now.
      if (verbose) write(*,*) "Reading geolocation from file."
   end if

   call get_abi_time(infiles(goodf), imager_time, imager_geolocation%ny, verbose)
   call get_abi_solgeom(imager_time, imager_angles, imager_geolocation, verbose)

   call get_abi_data(infiles, imager_angles, imager_measurements, imager_geolocation, channel_info, verbose)

   ! Compute relative azimuth from solar and viewing azimuths
   imager_angles%relazi = abs(imager_angles%solazi - imager_angles%satazi)
   where (imager_angles%relazi(:,:,1) .gt. 180.)
      imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
   end where
  !imager_angles%relazi(:,:,1) = 180. - imager_angles%relazi(:,:,1)

   where (imager_geolocation%latitude .eq. sreal_fill_value)
      imager_angles%relazi(:,:,1) = sreal_fill_value
      imager_angles%satazi(:,:,1) = sreal_fill_value
      imager_angles%satzen(:,:,1) = sreal_fill_value
      imager_angles%solazi(:,:,1) = sreal_fill_value
      imager_angles%solzen(:,:,1) = sreal_fill_value
   end where

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_abi_bin()'

end subroutine read_abi_bin
