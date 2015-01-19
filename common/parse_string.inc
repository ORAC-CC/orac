integer function PARSE_STRING_NAME_1D(in, data) result(status)
   implicit none

   character(len=*), intent(in) :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: data(:)

   character(len=path_length)   :: val, remnant
   integer                      :: i, n, id
   character(len=1)             :: delim

   status = 0

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
   do while (id > 0 .and. i < n)
      ! Cast string
      val = adjustl(remnant(:id-1))
      status = parse_string(val, data(i))
      if (status /= 0) return

      ! Increment to next comma
      remnant = adjustl(remnant(id+1:))
      id = index(trim(remnant), delim)
      i = i+1
   end do

   if (id /= 0) then
      status = PARSE_ERR_TOO_FEW
      return
   endif

   if (i /= n) then
      status = PARSE_ERR_TOO_MANY
      return
   endif

   ! Final element
   status = parse_string(remnant, data(i))

end function PARSE_STRING_NAME_1D

integer function PARSE_STRING_NAME_2D(in, data) result(status)
   implicit none

   character(len=*), intent(in) :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: data(:,:)

   character(len=path_length)   :: line, val, remnant
   integer                      :: i, n, id
   character(len=1)             :: delim

   status = 0

   delim=';'
   i = 1
   n = size(data, 2)
   remnant = adjustl(in)

   id = index(remnant, delim)
   if (id == 0 .and. n > 1) then ! Second condition catches 1-element arrays
      ! If not semicolons, delimit with commas
      delim=','
      id = index(remnant, delim)
   endif

   ! Split string at delimiter,
   do while (id > 0 .and. i < n)
      ! Cast string
      val = adjustl(remnant(:id-1))
      status = PARSE_STRING_NAME_1D(val, data(:, i))
      if (status /= 0) return

      ! Increment to next comma
      remnant = adjustl(remnant(id+1:))
      id = index(trim(remnant), delim)
      i = i+1
   end do

   if (id /= 0) then
      status = PARSE_ERR_TOO_FEW
      return
   endif

   if (i /= n) then
      status = PARSE_ERR_TOO_MANY
      return
   endif

   ! Final element
   status = PARSE_STRING_NAME_1D(remnant, data(:, i))

end function PARSE_STRING_NAME_2D
