!-------------------------------------------------------------------------------
! Name: read_refl_and_bt.F90
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
! 2012/06/20, C. Poulsen: changed arguments of read_inter_sec_file added in
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

subroutine read_inter_sec_file_common(ncid, l2_input_2d_secondary, xdim, ydim, &
                                      indexing, verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use structures_pp
   use vartypes_pp

   implicit none

   integer,                            intent(in)    :: ncid
   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim, ydim
   type(counts_and_indexes),           intent(in)    :: indexing
   logical,                            intent(in)    :: verbose

   integer           :: i
   character(len=32) :: input_num

   call nc_read_packed_array(ncid, "cot_ap", l2_input_2d_secondary%cot_ap, verbose)
   call nc_read_packed_array(ncid, "cot_fg", l2_input_2d_secondary%cot_fg, verbose)

   call nc_read_packed_array(ncid, "ref_ap", l2_input_2d_secondary%ref_ap, verbose)
   call nc_read_packed_array(ncid, "ref_fg", l2_input_2d_secondary%ref_fg, verbose)

   call nc_read_packed_array(ncid, "ctp_ap", l2_input_2d_secondary%ctp_ap, verbose)
   call nc_read_packed_array(ncid, "ctp_fg", l2_input_2d_secondary%ctp_fg, verbose)

   call nc_read_packed_array(ncid, "stemp_ap", l2_input_2d_secondary%stemp_ap, verbose)
   call nc_read_packed_array(ncid, "stemp_fg", l2_input_2d_secondary%stemp_fg, verbose)

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'firstguess_reflectance_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%y0(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'firstguess_brightness_temperature_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%y0(:,:,i), verbose)
      end if
   end do

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'reflectance_residual_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%residuals(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'brightness_temperature_residual_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%residuals(:,:,i), verbose)
      end if
   end do

end subroutine read_inter_sec_file_common


subroutine read_inter_sec_file_all(fname, l2_input_2d_secondary, xdim, ydim, &
                                   indexing, verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use structures_pp
   use vartypes_pp

   implicit none

   character(len=path_length),         intent(in)    :: fname
   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim, ydim
   type(counts_and_indexes),           intent(in)    :: indexing
   logical,                            intent(in)    :: verbose

   integer           :: ncid
   integer           :: i
   character(len=32) :: input_num

   write(*,*) 'Opening secondary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call read_inter_sec_file_common(ncid, l2_input_2d_secondary, xdim, ydim, &
      indexing, verbose)

!  call nc_read_packed_array(ncid, "scanline_u", l2_input_2d_secondary%scanline_u, verbose)
!  call nc_read_packed_array(ncid, "scanline_v", l2_input_2d_secondary%scanline_v, verbose)

   do i = 1, indexing%NSolar
      if (btest(indexing%Ch_Is(i), SolarBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'albedo_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%albedo(:,:,i), verbose)
      end if
   end do

   do i = 1, indexing%Ny
      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'reflectance_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%channels(:,:,i), verbose)
      else
         write(input_num, "(i4)") indexing%Y_Id(i)
         call nc_read_packed_array(ncid, 'brightness_temperature_in_channel_no_'// &
            trim(adjustl(input_num)), l2_input_2d_secondary%channels(:,:,i), verbose)
      end if
   end do

   write(*,*) 'Closing secondary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_inter_sec_file_all


subroutine read_inter_sec_file_class(fname, l2_input_2d_secondary, xdim, ydim, &
                                     indexing, verbose)

   use common_constants
   use netcdf
   use orac_ncdf
   use structures_pp
   use vartypes_pp

   implicit none

   character(len=path_length),         intent(in)    :: fname
   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim, ydim
   type(counts_and_indexes),           intent(in)    :: indexing
   logical,                            intent(in)    :: verbose

   integer :: ncid

   write(*,*) 'Opening secondary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call read_inter_sec_file_common(ncid, l2_input_2d_secondary, xdim, ydim, &
      indexing, verbose)

   write(*,*) 'Closing secondary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_inter_sec_file_class
