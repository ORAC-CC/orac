subroutine PARSE_STRING_NAME(in, data)
   implicit none

   character(len=*), intent(in) :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: data(:)
   
   character(len=path_length)   :: val, remnant
   integer                      :: i, n, id
   character(len=1)             :: delim

   delim=','
   i = 1
   n = size(data)
   remnant = adjustl(in)

   id = index(remnant, delim)
   if (id == 0 .and. n > 1) then ! Second condition catches 1-element arrays
      ! If not commas, delimit with whitespace
      delim=' '
      id = index(remnant, delim)
   endif
      
   ! Split string at delimiter, 
   do while (id > 0 .and. i <= n)
      ! Cast string
      val = adjustl(remnant(:id-1))
      call parse_string(val, data(i))

      ! Increment to next comma
      remnant = adjustl(remnant(id+1:))
      id = index(remnant, delim)
      i = i+1
   end do

   ! Final element
   if (i <= n) call parse_string(remnant, data(i))
   
end subroutine PARSE_STRING_NAME
