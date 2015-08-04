integer function PARSE_STRING_NAME_1D(in, data, n_in) result(status)
   implicit none

   character(len=*),  intent(in) :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: data(:)
   integer, optional, intent(in) :: n_in

   character(len=path_length)   :: val, remnant
   integer                      :: i, n, id
   character(len=1)             :: delim

   status = 0

   delim=','
   i = 1
   if (present(n_in)) then
      n = n_in
   else
      n = size(data)
   end if
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
      status = PARSE_STRING_NAME_0D(val, data(i))
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
   status = PARSE_STRING_NAME_0D(remnant, data(i))

end function PARSE_STRING_NAME_1D

integer function PARSE_STRING_NAME_2D(in, data, n1_in, n2_in) result(status)
   implicit none

   character(len=*),  intent(in) :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: data(:,:)
   integer, optional, intent(in) :: n1_in, n2_in

   character(len=path_length)   :: val, remnant
   integer                      :: i, n1, n2, id
   character(len=1)             :: delim

   status = 0

   delim=';'
   i = 1
   if (present(n1_in)) then
      n1 = n1_in
   else
      n1 = size(data, 1)
   end if
   if (present(n2_in)) then
      n2 = n2_in
   else
      n2 = size(data, 2)
   end if
   remnant = adjustl(in)

   id = index(remnant, delim)
   if (id == 0 .and. n2 > 1) then ! Second condition catches 1-element arrays
      ! If not semicolons, delimit with commas
      delim=','
      id = index(remnant, delim)
   endif

   ! Split string at delimiter,
   do while (id > 0 .and. i < n2)
      ! Cast string
      val = adjustl(remnant(:id-1))
      status = PARSE_STRING_NAME_1D(val, data(:, i), n1)
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

   if (i /= n2) then
      status = PARSE_ERR_TOO_MANY
      return
   endif

   ! Final element
   status = PARSE_STRING_NAME_1D(remnant, data(:, i), n1)

end function PARSE_STRING_NAME_2D
