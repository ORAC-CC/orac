!-------------------------------------------------------------------------------
! Name: utils_for_main.F90
!
! Purpose:
!
! History:
! 2015/02/24, GM: Pulled these out of preprocessing_for_orac.F90 to get them
!    module produced interfaces.
! 2015/09/14, GM: Move handle_parse_error() to trunk/common/parsing.F90.
! 2015/10/19, GM: Add use_modis_emis_in_rttov to parse_optional().
! 2015/11/26, GM: Add ecmwf_time_int_method, ecmwf_path_file, ecmwf_path_file2,
!    and ecmwf_path_file3 to parse_optional().
! 2016/02/02, CP: Add ecmwf_path_hr.
! 2016/02/02, GM: Add use_hr_ecmwf.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module utils_for_main

   implicit none

contains


subroutine parse_required(lun, value, name)

   use parsing
   use preproc_constants

   implicit none

   integer,          intent(in) :: lun
   character(len=*), intent(out):: value
   character(len=*), intent(in) :: name

   character(path_length) :: line

   if (parse_driver(lun, line) /= 0 .or. parse_string(line, value) /= 0) &
      call handle_parse_error(name)

end subroutine parse_required


subroutine parse_optional(label, value, n_channels, channel_ids, &
                          ecmwf_time_int_method, use_modis_emis_in_rttov, &
                          use_hr_ecmwf, ecmwf_path, ecmwf_path2, ecmwf_path3, &
                          ecmwf_path_hr)

   use parsing
   use preproc_constants

   implicit none

   character(len=*), intent(in)    :: label
   character(len=*), intent(in)    :: value
   integer,          intent(inout) :: n_channels
   integer, pointer, intent(inout) :: channel_ids(:)
   integer,          intent(inout) :: ecmwf_time_int_method
   logical,          intent(inout) :: use_modis_emis_in_rttov
   logical,          intent(inout) :: use_hr_ecmwf
   character(len=*), intent(inout) :: ecmwf_path
   character(len=*), intent(inout) :: ecmwf_path_hr
   character(len=*), intent(inout) :: ecmwf_path2
   character(len=*), intent(inout) :: ecmwf_path3

   select case (label)
   case('N_CHANNELS')
      if (parse_string(value, n_channels) /= 0) &
         call handle_parse_error(label)
      allocate(channel_ids(n_channels))
   case('CHANNEL_IDS')
      if (n_channels == 0) then
         write(*,*) 'ERROR: must set option n_channels before option channels'
         stop error_stop_code
      end if
      if (parse_string(value, channel_ids) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_TIME_INT_METHOD')
      if (parse_string(value, ecmwf_time_int_method) /= 0) &
         call handle_parse_error(label)
   case('USE_MODIS_EMIS_IN_RTTOV')
      if (parse_string(value, use_modis_emis_in_rttov) /= 0) &
         call handle_parse_error(label)
   case('USE_HR_ECMWF')
      if (parse_string(value, use_hr_ecmwf) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH_2')
      if (parse_string(value, ecmwf_path) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH2_2')
      if (parse_string(value, ecmwf_path2) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH3_2')
      if (parse_string(value, ecmwf_path3) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH_HR')
      if (parse_string(value, ecmwf_path_hr) /= 0) &
         call handle_parse_error(label)
   case default
      write(*,*) 'ERROR: Unknown option: ', trim(label)
      stop error_stop_code
   end select

end subroutine parse_optional


integer function parse_logical(string, value) result(status)

   use preproc_constants

   implicit none

   character(len=*), intent(in)  :: string
   logical,          intent(out) :: value

   status = 0

   if (trim(adjustl(string)) .eq. '1' .or.&
       trim(adjustl(string)) .eq. 't' .or. &
       trim(adjustl(string)) .eq. 'true' .or. &
       trim(adjustl(string)) .eq. 'T' .or. &
       trim(adjustl(string)) .eq. 'True') then
        value = .true.
   else if &
      (trim(adjustl(string)) .eq. '0' .or. &
       trim(adjustl(string)) .eq. 'f' .or. &
       trim(adjustl(string)) .eq. 'false' .or. &
       trim(adjustl(string)) .eq. 'F' .or. &
       trim(adjustl(string)) .eq. 'False') then
        value = .false.
   else
        status = -1
   end if

end function parse_logical


function calc_n_chunks(n_segments, segment_starts, segment_ends, &
                       chunk_size) result (n_chunks)

   implicit none

   integer, intent(in) :: n_segments
   integer, intent(in) :: segment_starts(n_segments)
   integer, intent(in) :: segment_ends(n_segments)
   integer, intent(in) :: chunk_size
   integer             :: n_chunks

   integer :: i

   n_chunks = 0

   do i = 1, n_segments
      n_chunks = n_chunks + (segment_ends(i) - segment_starts(i)) / chunk_size + 1
   end do

end function calc_n_chunks


subroutine chunkify(n_segments, segment_starts, segment_ends, &
                    chunk_size, n_chunks, chunk_starts, chunk_ends)

   implicit none

   integer, intent(in)  :: n_segments
   integer, intent(in)  :: segment_starts(n_segments)
   integer, intent(in)  :: segment_ends(n_segments)
   integer, intent(in)  :: chunk_size
   integer, intent(out) :: n_chunks
   integer, intent(out) :: chunk_starts(*)
   integer, intent(out) :: chunk_ends(*)

   integer :: i

   n_chunks = 1

   do i = 1, n_segments
      chunk_starts(n_chunks) = segment_starts(i)

      do while (chunk_starts(n_chunks) + chunk_size .lt. segment_ends(i))
         chunk_ends(n_chunks) = chunk_starts(n_chunks) + chunk_size - 1
         n_chunks = n_chunks + 1
         chunk_starts(n_chunks) = chunk_starts(n_chunks - 1) + chunk_size
      end do

      chunk_ends(n_chunks) = segment_ends(i)

      n_chunks = n_chunks + 1
   end do

   n_chunks = n_chunks - 1

end subroutine chunkify

end module utils_for_main
