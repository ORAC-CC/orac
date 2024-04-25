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
! 2012/02/20, CP: Creates initial file.
! 2012/06/20, CP: Modified to work for all sensors
! 2012/06/20, CP: Modified to add albedo
! 2012/06/20, CP: Changed arguments of read_input_secondary added in reading of
!    albedo added in reading of channel radiances
! 2014/09/29, CP: Changed number of arguments read in and added MODIS specific
!    call
! 2014/10/10, AP: Added case statement to secondary read to very badly
! 2014/12/03, CP: Uncommented albedo because need for snow mask added
!    common_constants deal with differing instrument channels.
! 2015/02/05, OS: Changed nint to lint; now checks for AVHRR whether ch3a or
!    ch3b data exist in input files
! 2015/02/07, CP: Changed to common constants and tidied up string reading of
!    instrument
! 2015/07/16, GM: Major cleanup.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/05/04, AP: Fix read of optional, channel-dependent fields.
! 2017/01/20, CP: made netdcf file name read more robust
! 2017/01/09, CP: ML additions.
!
! Bugs:
! Not a bug but this code assumes 5 channels of instrument data.
!-------------------------------------------------------------------------------

subroutine read_input_secondary_common(ncid, input_data, indexing, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                      intent(in)    :: ncid
   type(input_data_secondary_t), intent(inout) :: input_data
   type(input_indices_t),        intent(in)    :: indexing
   integer,                      intent(in)    :: sval
   logical,                      intent(in)    :: verbose

   integer            :: i, j, i_rho
   character(len=32)  :: input_num
   character(len=512) :: input_dummy, input_dummy2

if (indexing%flags%do_aerosol) then
   call ncdf_read_packed_array(ncid, "aot550_ap", input_data%aot550_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "aot550_fg", input_data%aot550_fg, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "aer_ap", input_data%aer_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "aer_fg", input_data%aer_fg, &
        start = [1, sval])
end if

if (indexing%flags%do_rho) then
   i_rho = 0
   do i = 1, indexing%NSolar
      write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j = 1, MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            i_rho = i_rho + 1

            call create_rho_field_name(j, 3, input_num, input_dummy)
            call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
                 input_data%rho_ap(:,:,i_rho), start = [1, sval])
            call create_rho_field_name(j, 4, input_num, input_dummy)
            call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
                 input_data%rho_fg(:,:,i_rho), start = [1, sval])
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='swansea_s_ap_in_channel_no_'//trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
              input_data%swansea_s_ap(:,:,i_rho), start = [1, sval])
         input_dummy='swansea_s_fg_in_channel_no_'//trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
              input_data%swansea_s_fg(:,:,i_rho), start = [1, sval])
      end if
   end do

   do i = 1, indexing%NViews
      write(input_num, "(i4)") i

      input_dummy='swansea_p_ap_in_view_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
           input_data%swansea_p_ap(:,:,i), start = [1, sval])
      input_dummy='swansea_p_fg_in_view_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
           input_data%swansea_p_fg(:,:,i), start = [1, sval])
   end do
end if

if (indexing%flags%do_cloud) then
   call ncdf_read_packed_array(ncid, "cot_ap", input_data%cot_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cot_fg", input_data%cot_fg, &
        start = [1, sval])

   call ncdf_read_packed_array(ncid, "cer_ap", input_data%cer_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cer_fg", input_data%cer_fg, &
        start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctp_ap", input_data%ctp_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctp_fg", input_data%ctp_fg, &
        start = [1, sval])

   call ncdf_read_packed_array(ncid, "stemp_ap", input_data%stemp_ap, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "stemp_fg", input_data%stemp_fg, &
        start = [1, sval])
end if

   do i = 1, indexing%Ny
      write(input_num, "(i4)") indexing%Y_Id(i)

      if (btest(indexing%Ch_Is(i), ThermalBit)) then
         input_dummy = 'firstguess_brightness_temperature_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2 = 'brightness_temperature_residual_in_channel_no_'// &
              trim(adjustl(input_num))
      else
         input_dummy = 'firstguess_reflectance_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2 = 'reflectance_residual_in_channel_no_'// &
              trim(adjustl(input_num))
      end if

      call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
           input_data%y0(:,:,i), start = [1, sval])
      call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy2)), &
           input_data%residuals(:,:,i), start = [1, sval])
   end do

   call ncdf_read_packed_array(ncid, "degrees_of_freedom_signal", &
        input_data%ds)

end subroutine read_input_secondary_common


subroutine read_input_secondary_optional(ncid, input_data, indexing, &
     read_flags, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                      intent(in)    :: ncid
   type(input_data_secondary_t), intent(inout) :: input_data
   type(input_indices_t),        intent(in)    :: indexing
   type(common_file_flags_t),    intent(inout) :: read_flags
   integer,                      intent(in)    :: sval
   logical,                      intent(in)    :: verbose

   integer            :: i, ii
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

   do i = 1, indexing%Ny
      if (indexing%read_optional_channel_field(i)) then
         write(input_num, "(i4)") indexing%Y_Id(i)

         if (btest(indexing%Ch_Is(i), ThermalBit)) then
            input_dummy = 'brightness_temperature_in_channel_no_'// &
                 trim(adjustl(input_num))
         else
            input_dummy = 'reflectance_in_channel_no_'// trim(adjustl(input_num))
         end if
         ii = indexing%loop_to_main_index(i)
         call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
              input_data%channels(:,:,ii), start = [1, sval])
      end if
   end do

if (indexing%flags%do_cloud .and. read_flags%do_cloud) then
   do i = 1, indexing%NSolar
      if (indexing%read_optional_channel_field(indexing%YSolar(i))) then
         write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy = 'albedo_in_channel_no_'//trim(adjustl(input_num))
         ii = indexing%ysolar_loop_to_main_index(i)
         call ncdf_read_packed_array(ncid, trim(adjustl(input_dummy)), &
              input_data%albedo(:,:,ii), start = [1, sval])
      end if
   end do
end if

end subroutine read_input_secondary_optional


subroutine read_input_secondary_once(nfile, fname, input_data, indexing, &
     loop_ind, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                      intent(in)    :: nfile
   character(len=*),             intent(in)    :: fname(:)
   type(input_data_secondary_t), intent(inout) :: input_data
   type(input_indices_t),        intent(in)    :: indexing
   type(input_indices_t),        intent(in)    :: loop_ind(:)
   integer,                      intent(in)    :: sval
   logical,                      intent(in)    :: verbose

   integer                   :: ncid, i
   type(common_file_flags_t) :: read_flags

   ! Flag which optional fields to be read. Turn false as they are read.
   read_flags = indexing%flags

   ! Read universally common fields from first input file
   call ncdf_open(ncid, fname(1), 'read_input_secondary_once()')

   call read_input_secondary_optional(ncid, input_data, loop_ind(1), &
        read_flags, sval, verbose)

   call ncdf_close(ncid, 'read_input_secondary_once()')

   do i = 2, nfile
      call ncdf_open(ncid, fname(i), 'read_input_secondary_once()')
      call read_input_secondary_optional(ncid, input_data, loop_ind(i), &
           read_flags, sval, verbose)
      call ncdf_close(ncid, 'read_input_secondary_once()')
   end do

end subroutine read_input_secondary_once


subroutine read_input_secondary_class(fname, input_data, indexing, sval, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*),             intent(in)    :: fname
   type(input_data_secondary_t), intent(inout) :: input_data
   type(input_indices_t),        intent(in)    :: indexing
   integer,                      intent(in)    :: sval
   logical,                      intent(in)    :: verbose

   integer :: ncid

   if (verbose) write(*,*) 'Opening secondary input file: ', trim(fname)
   call ncdf_open(ncid, fname, 'read_input_secondary_class()')

   call read_input_secondary_common(ncid, input_data, indexing, sval, verbose)

   if (verbose) write(*,*) 'Closing secondary input file.'
   call ncdf_close(ncid, 'read_input_secondary_class()')

end subroutine read_input_secondary_class
