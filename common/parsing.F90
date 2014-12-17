!-------------------------------------------------------------------------------
! Name: parsing.F90 
!
! Purpose:
! A module containing the following routines for parsing strings:
!    PARSE_DRIVER - Extract a line of information from a driver file.
!    PARSE_STRING - Translate a text string into a Fortran variable.
!
! History:
! 2014/12/17, AP: Original version.
!
! $Id$
!-------------------------------------------------------------------------------

module parsing
   use common_constants
   
   implicit none
   
   interface parse_string
      module procedure &
           parse_string_0d_strg, parse_string_0d_byte, parse_string_0d_sint, &
           parse_string_0d_lint, parse_string_0d_sreal, parse_string_0d_dreal, &
           parse_string_1d_byte, parse_string_1d_sint, &
           parse_string_1d_lint, parse_string_1d_sreal, parse_string_1d_dreal, &
           parse_string_2d_byte, parse_string_2d_sint, &
           parse_string_2d_lint, parse_string_2d_sreal, parse_string_2d_dreal
   end interface parse_string
contains

!-------------------------------------------------------------------------------
! Name: parse_driver
!
! Purpose:
! Reads a line from a driver file, ignoring comments and dividing descriptive
! labels (for optional fields) from data.
!
! Description and Algorithm details:
! 1) Read past any lines with initial non-whitespace character of #.
!   a) If there are no such lines, return '' in both arguments and status -1.
! 2) Remove any comment fields at the end of the line (after a #).
! 3) Split the string at the first = sign. Everything before it is returned in
!   label, everything after (or the whole string if there is no = sign) is
!   returned in data.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! lun   integer In  File unit number to be read.
! data  string  Out Information conveyed by the next line of the driver file.
! label string  Out (Optional) Description of the data field, being any text in
!   the line before a = sign. Returns '' if not present.
!
! Return value:
! ios   integer Out Status value from reading.
!
! History:
! 2014/12/17, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function parse_driver(lun, data, label) result(ios)
   implicit none

   integer,          intent(in)            :: lun
   character(len=*), intent(out)           :: data
   character(len=*), intent(out), optional :: label
   
   integer                    :: ios   
   integer                    :: id
   character(len=path_length) :: line

   ios  = 0
   line = '#'
   
   ! Loop over comment lines
   do while (ios == 0 .and. line(1:1) .eq. '#')
      read(lun, '(A)', iostat=ios) line
      line = adjustl(line)
   end do

   id = index(line, '#')
   if (id == 1) then
      ! Entire line is a comment
      label = ''
      data  = ''
      ios = -1
   else
      ! Remove end-of-line comments
      if (id > 0) line(id:) = ''

      id = index(line, '=')
      if (id > 0) then
         ! Labelled field
         label = adjustl(line(:id-1))
         data  = adjustl(line(id+1:))
      else
         ! No label for data field
         label = ''
         data  = adjustl(line)
      end if
   end if
   
end function parse_driver

!-------------------------------------------------------------------------------
! Name: parse_string
!
! Purpose:
! Translate a string from a driver file into a Fortran variable. The appropriate
! translation is selected determined from the type of the output variable given.
! Scalars
!
! Description and Algorithm details:
! 1) If out is scalar, read input into output variable.
!   a) If this operation fails for a numerical cast, the output is set to the
!      appropriate fill value.
!   b) A string has any leading or trailing ' or " removed.
! 2) If out is a 1D array, the array is searched for a comma.
!   a) If no commas are found and out is not a one-element array, the array is
!      instead searched for whitespace.
!   b) All text before the identified delimiter is cast into the next element
!      of the out array (using the scalar version of this procedure).
!   c) The delimiter and text parsed are removed from the string. A new search
!      is made for the delimiter. Go to (b) if one is found.
!   e) The final section of string is put into the final element of the out
!      array.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! in    string  In  The string to translate.
! out   any     Out The variable into which the string should be written.
!
! History:
! 2014/12/17, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
#define PARSE_STRING_TYPE integer
#define PARSE_STRING_KIND byte
#define PARSE_STRING_NAME_1D parse_string_1d_byte
#define PARSE_STRING_NAME_2D parse_string_2d_byte
#include "parse_string.inc"
#undef PARSE_STRING_TYPE
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D

#define PARSE_STRING_TYPE integer
#define PARSE_STRING_KIND sint
#define PARSE_STRING_NAME_1D parse_string_1d_sint
#define PARSE_STRING_NAME_2D parse_string_2d_sint
#include "parse_string.inc"
#undef PARSE_STRING_TYPE
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D

#define PARSE_STRING_TYPE integer
#define PARSE_STRING_KIND lint
#define PARSE_STRING_NAME_1D parse_string_1d_lint
#define PARSE_STRING_NAME_2D parse_string_2d_lint
#include "parse_string.inc"
#undef PARSE_STRING_TYPE
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D

#define PARSE_STRING_TYPE real
#define PARSE_STRING_KIND sreal
#define PARSE_STRING_NAME_1D parse_string_1d_sreal
#define PARSE_STRING_NAME_2D parse_string_2d_sreal
#include "parse_string.inc"
#undef PARSE_STRING_TYPE
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D

#define PARSE_STRING_TYPE real
#define PARSE_STRING_KIND dreal
#define PARSE_STRING_NAME_1D parse_string_1d_dreal
#define PARSE_STRING_NAME_2D parse_string_2d_dreal
#include "parse_string.inc"
#undef PARSE_STRING_TYPE
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D

subroutine parse_string_0d_byte(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   integer(byte),    intent(out) :: out
   integer                       :: ios
   
   read(in, *, iostat=ios) out
   if (ios /= 0) out = byte_fill_value
end subroutine parse_string_0d_byte

subroutine parse_string_0d_sint(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   integer(sint),    intent(out) :: out
   integer                       :: ios
   
   read(in, *, iostat=ios) out
   if (ios /= 0) out = sint_fill_value
end subroutine parse_string_0d_sint

subroutine parse_string_0d_lint(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   integer(lint),    intent(out) :: out
   integer                       :: ios
   
   read(in, *, iostat=ios) out
   if (ios /= 0) out = lint_fill_value
end subroutine parse_string_0d_lint

subroutine parse_string_0d_sreal(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   real(sreal),      intent(out) :: out
   integer                       :: ios
   
   read(in, *, iostat=ios) out
   if (ios /= 0) out = sreal_fill_value
end subroutine parse_string_0d_sreal

subroutine parse_string_0d_dreal(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   real(dreal),      intent(out) :: out
   integer                       :: ios
   
   read(in, *, iostat=ios) out
   if (ios /= 0) out = dreal_fill_value
end subroutine parse_string_0d_dreal

subroutine parse_string_0d_strg(in, out)
   implicit none

   character(len=*), intent(in)  :: in
   character(len=*), intent(out) :: out
   character(len=1), parameter   :: excess(2) = ["'", '"']
   integer                       :: l, a, b

   if (any(excess == in(1:1))) then
      a = 2
   else
      a = 1
   end if

   l = len_trim(in)
   if (any(excess == in(l:l))) then
      b = l-1
   else
      b = l
   end if

   out = in(a:b)
end subroutine parse_string_0d_strg

!-------------------------------------------------------------------------------
! Name: clean_driver_label
!
! Purpose:
! Capitalise the input and convert all full stops into % such that a driver
! file label can be used in a simple select case statement. Code taken from:
! http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90
!
! Description and Algorithm details:
! 1) Loop through letters.
! 2) If lowercase, make uppercase.
! 3) If a ., make a %.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! in    string  In  The string to translate.
!
! Return value:
! out   string  Out An uppercase string.
!
! History:
! 2014/12/17, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine clean_driver_label(in)
   implicit none

   character(len=*), intent(inout) :: in

   Integer :: ic, i

   character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

   ! Capitalize each letter if it is lowercase
   do i = 1, len_trim(in)
      ic = index(low, in(i:i))
      if (ic > 0) then
         in(i:i) = cap(ic:ic)
      else
         ! Turn full stops into %
         if (in(i:i) == '.') in(i:i) = '%'
      end if
   end do
end subroutine clean_driver_label

end module parsing
