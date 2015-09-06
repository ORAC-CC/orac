!-------------------------------------------------------------------------------
! Name: read_input_secondary.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/20, Caroline Poulsen: creates initial file.
! 2012/06/20, C. Poulsen: modified to work for all sensors
! 2012/06/20, C. Poulsen: modified to add albedo
! 2012/06/20, C. Poulsen: changed arguments of read_input_secondary added in
!    reading of albedo added in reading of channel radiances
! 2014/09/29, C. Poulsen: changed number of arguments read in and added MODIS
!    specific call
! 2014/10/10, A. Povey: Added case statement to secondary read to very badly
! 2014/12/03, CP: uncommented albedo because need for snow mask added
!    common_constants deal with differing instrument channels.
! 2015/02/05, OS: changed nint to lint; now checks for AVHRR whether ch3a or
!    ch3b data exist in input files
! 2015/02/07, CP: changed to common constants and tidied up string reading of
!    instrument
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! Not a bug but this code assumes 5 channels of instrument data.
!-------------------------------------------------------------------------------

subroutine read_input_secondary_common(ncid, input_data, xdim, ydim, indexing, &
                                       verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim, ydim
   type(counts_and_indexes),   intent(in)    :: indexing
   logical,                    intent(in)    :: verbose

   integer           :: i
   character(len=32) :: input_num

   call nc_read_packed_array(ncid, "cot_ap", input_data%cot_ap, verbose)
   call nc_read_packed_array(ncid, "cot_fg", input_data%cot_fg, verbose)

   call nc_read_packed_array(ncid, "ref_ap", input_data%ref_ap, verbose)
   call nc_read_packed_array(ncid, "ref_fg", input_data%ref_fg, verbose)

   call nc_read_packed_array(ncid, "ctp_ap", input_data%ctp_ap, verbose)
   call nc_read_packed_array(ncid, "ctp_fg", input_data%ctp_fg, verbose)

   call nc_read_packed_array(ncid, "stemp_ap", input_data%stemp_ap, verbose)
   call nc_read_packed_array(ncid, "stemp_fg", input_data%stemp_fg, verbose)

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'firstguess_reflectance_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%y0(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'firstguess_brightness_temperature_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%y0(:,:,i), verbose)
      end if
   end do

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'reflectance_residual_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%residuals(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'brightness_temperature_residual_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%residuals(:,:,i), verbose)
      end if
   end do

end subroutine read_input_secondary_common


subroutine read_input_secondary_all(fname, input_data, xdim, ydim, indexing, &
                                    verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   character(len=path_length), intent(in)    :: fname
   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim, ydim
   type(counts_and_indexes),   intent(in)    :: indexing
   logical,                    intent(in)    :: verbose

   integer           :: ncid
   integer           :: i
   character(len=32) :: input_num

   write(*,*) 'Opening secondary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call read_input_secondary_common(ncid, input_data, xdim, ydim, &
      indexing, verbose)

!  call nc_read_packed_array(ncid, "scanline_u", input_data%scanline_u, verbose)
!  call nc_read_packed_array(ncid, "scanline_v", input_data%scanline_v, verbose)

   do i = 1, indexing%NSolar
      if (btest(indexing%Ch_Is(i), SolarBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'albedo_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%albedo(:,:,i), verbose)
      end if
   end do

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'reflectance_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%channels(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'brightness_temperature_in_channel_no_'// &
            trim(adjustl(input_num)), input_data%channels(:,:,i), verbose)
      end if
   end do

   write(*,*) 'Closing secondary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_input_secondary_all


subroutine read_input_secondary_class(fname, input_data, xdim, ydim, indexing, &
                                      verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   character(len=path_length), intent(in)    :: fname
   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim, ydim
   type(counts_and_indexes),   intent(in)    :: indexing
   logical,                    intent(in)    :: verbose

   integer :: ncid

   write(*,*) 'Opening secondary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call read_input_secondary_common(ncid, input_data, xdim, ydim, &
      indexing, verbose)

   write(*,*) 'Closing secondary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_input_secondary_class
