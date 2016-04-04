!-------------------------------------------------------------------------------
! Name: system_utils.F90
!
! Purpose:
!
! History:
! 2015/10/05, GM: Original version.
!
! $Id$
!-------------------------------------------------------------------------------

module system_utils_m

   implicit none

   private

   public :: match_file

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

         character(c_char), dimension(*) :: dir_name
         character(c_char), dimension(*) :: file_pattern
         character(c_char), dimension(*) :: file_name
         integer(c_int), value           :: n
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

end module system_utils_m
