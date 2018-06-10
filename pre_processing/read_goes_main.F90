!-------------------------------------------------------------------------------
! Name: read_goes_main.F90
!
! Purpose:
! Contains functions for reading GOES data. These are typically the high level
! functions that control data read-in, low level is in read_goes_funcs.f90
! History:
! 2018/02/10, SP: First version.
! 2018/06/08, SP: New global attribute to store satellite position information
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: read_goes_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! l1_5_file      string  in   Full path to one netCDF GOES channel (any band)
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
subroutine read_goes_dimensions(l1_5_file, n_across_track, n_along_track, &
                                    startx, endx, starty, endy,verbose)

   use preproc_constants_m
   use netcdf
   use orac_ncdf_m

   implicit none

   character(path_length), intent(in)    :: l1_5_file
   integer(lint),          intent(out)   :: n_across_track, n_along_track
   integer(lint),          intent(inout) :: startx, endx, starty, endy
   logical,                intent(in)    :: verbose

   integer :: fid,ierr,index2,band
   integer :: n_lines, n_cols
   character(2) :: cband

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_goes_dimensions()'

	index2	=	index(trim(adjustl(l1_5_file)),'_G1')
	cband		=	l1_5_file(index2-2:index2)

   read(cband(1:len_trim(cband)), '(I2)') band

   ierr		=	nf90_open(path=trim(adjustl(l1_5_file)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_goes_dimensions(): Error opening file ',trim(l1_5_file)
      stop error_stop_code
   end if

   ! Read actual size of the netCDF4 file
   n_cols	=	nc_dim_length(fid,'x',.false.)
   n_lines	=	nc_dim_length(fid,'y',.false.)

   ! Make sure we account for the fact that some bands are hi-res
   if (band .eq. 1 .or. band .eq. 3 .or. band .eq. 5) then
   	n_cols	=	n_cols/2
   	n_lines	=	n_lines/2
   else if (band .eq. 2) then
   	n_cols	=	n_cols/4
   	n_lines	=	n_lines/4
   end if

   ! Close the netCDF4 file
   ierr		=	nf90_close(fid)

   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_goes_dimensions(): Error closing file ',trim(l1_5_file)
      stop error_stop_code
   end if

   if (startx .le. 0 .or. endx .le. 0 .or. starty .le. 0 .or. endy .le. 0) then
      ! If start and end *are not* being used then set them to the start and end
      ! of the actual image in the file.
      starty = 1
      endy   = n_lines
      startx = 1
      endx   = n_cols
   else
      ! If start and end *are* being used then check that they fall within the
      ! actual image in the file relative to the full disk image.
      if (starty - 1 .lt.  0) then
         write(*,*) 'ERROR: read_goes_dimensions(): user defined starty (', starty, ') ' //&
         			'does not fall within the actual GOES-R image starting at: ', 1
         stop error_stop_code
      end if
      if (endy - 1 .gt. n_lines - 1) then
         write(*,*) 'ERROR: read_goes_dimensions(): user defined endy (', endy, ') does not ' // &
                    'fall within the actual GOES-R image ending at: ', n_lines
         stop error_stop_code
      end if
      if (startx - 1 .lt.  0) then
         write(*,*) 'ERROR: read_goes_dimensions(): user defined startx (', startx, ') does not ' // &
                    'fall within the actual GOES-R image starting at: ', 1
         stop error_stop_code
      end if
      if (endx - 1 .gt. n_cols - 1) then
         write(*,*) 'ERROR: read_goes_dimensions(): user defined endx (', endx, ') does not ' // &
                    'fall within the actual GOES-R image ending at: ', n_cols
         stop error_stop_code
      end if
   end if

   n_across_track = n_cols
   n_along_track  = n_lines

   print*,n_across_track,endx,startx
   print*,n_along_track,endy,starty

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_goes_dimensions()'

end subroutine read_goes_dimensions

!-------------------------------------------------------------------------------
! Name: get_goes_data
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

subroutine get_goes_data(infiles,imager_angles,imager_measurements,imager_geolocation,channel_info,verbose)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m
   use netcdf
   implicit none

   character(len=file_length),  intent(in)    :: infiles(:)
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer, allocatable  :: band_ids(:)
   real, allocatable 	 :: tmprad(:,:)
   real, allocatable 	 :: tmpout(:,:)

   integer 	::	n_bands,i
   real		::	irrad
   real		::	bc1,bc2,fk1,fk2

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< get_goes_data()'

   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr

   allocate(tmpout(imager_geolocation%nx,imager_geolocation%ny))

   do i=1,n_bands
   	if (band_ids(i) .lt. 7) then
   		tmpout(:,:)	=	sreal_fill_value
   		if (verbose) write(*,*) "Loading GOES visible band ",band_ids(i)
   		if (band_ids(i) .eq. 1 .or. band_ids(i) .eq. 3 .or. band_ids(i) .eq. 5)	then
   			allocate(tmprad(imager_geolocation%nx*2,imager_geolocation%ny*2))
				call load_goes_band(infiles(i),imager_geolocation,tmprad,irrad,bc1,bc2,fk1,fk2,2,verbose)
   			call goes_resample_vis_to_tir(tmprad,tmpout,imager_geolocation%nx,imager_geolocation%ny,sreal_fill_value,2,verbose)
   		elseif (band_ids(i) .eq. 2) then
   			allocate(tmprad(imager_geolocation%nx*4,imager_geolocation%ny*4))
				call load_goes_band(infiles(i),imager_geolocation,tmprad,irrad,bc1,bc2,fk1,fk2,4,verbose)
   			call goes_resample_vis_to_tir(tmprad,tmpout,imager_geolocation%nx,imager_geolocation%ny,sreal_fill_value,4,verbose)
   		else
   			allocate(tmprad(imager_geolocation%nx,imager_geolocation%ny))
				call load_goes_band(infiles(i),imager_geolocation,tmpout,irrad,bc1,bc2,fk1,fk2,1,verbose)
   		endif

   		tmpout	=	tmpout * irrad

   		where(imager_angles%solzen(:,:,1) .eq. sreal_fill_value) &
       		tmpout=sreal_fill_value
			imager_measurements%data(:,:,i) = tmpout

   		deallocate(tmprad)
   	else
   		if (verbose) write(*,*) "Loading GOES thermal band ",band_ids(i)
   		allocate(tmprad(imager_geolocation%nx,imager_geolocation%ny))

			call load_goes_band(infiles(i),imager_geolocation,tmprad,irrad,bc1,bc2,fk1,fk2,1,verbose)

			tmprad	=	(fk2/(log((fk1/tmprad)+1))-bc1) / bc2

			where(tmprad .lt. 130) tmprad = sreal_fill_value
			where(tmprad .gt. 500) tmprad = sreal_fill_value

			imager_measurements%data(:,:,i) = tmprad

   		deallocate(tmprad)
   	endif
   	where (imager_geolocation%latitude .eq. sreal_fill_value)
   		imager_measurements%data(:,:,i) = sreal_fill_value
   	end where
   end do

 	deallocate(tmpout)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> get_goes_data()'

end subroutine get_goes_data


!-------------------------------------------------------------------------------
! Name: read_goes_bin
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
subroutine read_goes_bin(infiles, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, use_predef_geo,geo_file_path, &
   global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m
   implicit none

   character(len=file_length),  intent(in)    :: infiles(:)
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: use_predef_geo
   character(len=file_length),  intent(in)    :: geo_file_path
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer                     	:: i,goodf
   integer(c_int)              	:: n_bands
   integer(c_int), allocatable 	:: band_ids(:)
   integer(c_int), allocatable 	:: band_units(:)
   integer                     	:: startx, nx
   integer                     	:: starty, ny
   integer(c_int)              	:: line0, line1
   integer(c_int)              	:: column0, column1

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_goes_bin()'

	goodf = -1

   do i=1, channel_info%nchannels_total
		if(channel_info%channel_ids_instr(i) .eq. 4 .or. channel_info%channel_ids_instr(i) .gt. 6) then
			goodf=i
			exit
		endif
   enddo
   if (goodf .le. 0) then
   	write(*,*)"ERROR: No Infrared channels have been selected:",channel_info%channel_ids_instr(i)
   	stop
   endif

	! Determine geolocation information (lat/lon/vza/vaa)
	if (use_predef_geo .neqv. .true.) then
		call get_goes_geoloc(infiles(goodf),imager_geolocation, imager_angles, global_atts, verbose)
	else
		! This is a placeholder for now.
		if (verbose) write(*,*) "Reading geolocation from file."
	endif

	call get_goes_time(infiles(goodf), imager_time, imager_geolocation%ny, verbose)
	call get_goes_solgeom(imager_time,imager_angles,imager_geolocation,verbose)
	call get_goes_data(infiles,imager_angles,imager_measurements,imager_geolocation,channel_info,verbose)

	! Compute relative azimuth from solar and viewing azimuths
   where (imager_angles%solazi .gt. 180.)
      imager_angles%solazi = 360. - imager_angles%solazi
   end where
   imager_angles%relazi = imager_angles%satazi
   where (imager_angles%relazi .gt. 180.)
      imager_angles%relazi = 360. - imager_angles%relazi
   end where

   imager_angles%relazi = abs(imager_angles%solazi-imager_angles%relazi)
   where (imager_angles%relazi .gt. 180.)
      imager_angles%relazi = 180. - imager_angles%relazi
   end where
   imager_angles%relazi = 180. - imager_angles%relazi
   where (imager_angles%relazi .lt. 0. .and. &
          imager_angles%relazi .ne. sreal_fill_value )
      imager_angles%relazi = 0. - imager_angles%relazi
   end where

   imager_angles%satazi = imager_angles%satazi - 180.


   where (imager_geolocation%latitude .eq. sreal_fill_value)
   	imager_angles%relazi(:,:,1) = sreal_fill_value
   	imager_angles%satazi(:,:,1) = sreal_fill_value
   	imager_angles%satzen(:,:,1) = sreal_fill_value
   	imager_angles%solazi(:,:,1) = sreal_fill_value
   	imager_angles%solzen(:,:,1) = sreal_fill_value
   end where

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_goes_bin()'

end subroutine read_goes_bin
