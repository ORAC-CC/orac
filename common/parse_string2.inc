integer function PARSE_STRING_NAME_0D(in, out, ch_ids) result(status)
   implicit none

   character(len=*), intent(in)  :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer, optional, intent(in) :: ch_ids(:) ! Dummy for main processor routine
   integer                       :: ios

   status = 0

   read(in, *, iostat=ios) out
   if (ios /= 0) status = PARSE_ERR_CONV
end function PARSE_STRING_NAME_0D
