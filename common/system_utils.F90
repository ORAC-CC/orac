!-------------------------------------------------------------------------------
! Name: system_utils.F90
!
! Purpose:
!
! History:
! 2015/10/05, GM: Original version.
! 2020/09/25, AP: Added upper,lower() for string manipulation
!-------------------------------------------------------------------------------

module system_utils_m

   implicit none

   private

   public :: match_file, &
             is_nan, &
             c_to_fortran_str, &
             lower, &
             upper
contains

!-------------------------------------------------------------------------------
! Name: match_file
!
! Purpose:
!
! Description and Algorithm details:
! None
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/10/05, GM: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function match_file(dir_name, file_pattern, file_name) result(status)

   use iso_c_binding

   implicit none

   character(len=*), intent(in)  :: dir_name
   character(len=*), intent(in)  :: file_pattern
   character(len=*), intent(out) :: file_name
   integer                       :: status

   interface
      integer(c_int) function system_utils_match_file(dir_name, file_pattern, &
         file_name, n) bind(C,name='system_utils_match_file')
         use iso_c_binding

         implicit none

         character(kind=c_char), dimension(*) :: dir_name
         character(kind=c_char), dimension(*) :: file_pattern
         character(kind=c_char), dimension(*) :: file_name
         integer(kind=c_int), value           :: n
      end function system_utils_match_file
   end interface

   status = 0

   if (system_utils_match_file(trim(dir_name)//C_NULL_CHAR, &
                               trim(file_pattern)//C_NULL_CHAR, &
                               file_name, len(file_name)) .le. 0) then
      status = 1
      return
   end if

   file_name = file_name(1:index(file_name, C_NULL_CHAR))

end function match_file


elemental function is_nan(x) result(res)
#ifndef __GFORTRAN__
   use ieee_arithmetic, only : ieee_is_nan
#endif
   implicit none

   real, intent(in) :: x
   logical          :: res

#ifndef __GFORTRAN__
   res = ieee_is_nan(x)
#else
   res = isnan(x)
#endif
end function

subroutine c_to_fortran_str(str)
   use iso_c_binding, only: C_NULL_CHAR

   implicit none

   character(len=*), intent(inout) :: str
   integer :: i

   do i=1,len(str)
      if (str(i:i) == C_NULL_CHAR) exit
   end do

   str(i:len(str)) = ' '
end subroutine c_to_fortran_str

function upper(str_in) result (str_out)

   implicit none

   character(len=*)           :: str_in
   character(len=len(str_in)) :: str_out

   character :: ch
   integer   :: i

   integer, parameter :: step = ichar('A') - ichar('a')

   do i = 1, len(str_in)
      ch = str_in(i:i)
      if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch) + step)
      str_out(i:i) = ch
   end do

end function upper

function lower(str_in) result (str_out)

   implicit none

   character(len=*)           :: str_in
   character(len=len(str_in)) :: str_out

   character :: ch
   integer   :: i

   integer, parameter :: step = ichar('A') - ichar('a')

   do i = 1, len(str_in)
      ch = str_in(i:i)
      if (ch >= 'A' .and. ch <= 'Z') ch = char(ichar(ch) - step)
      str_out(i:i) = ch
   end do

end function lower

end module system_utils_m
