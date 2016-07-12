!
! NAME:
!    struct_parser.F90
! PURPOSE:
!    Bison grammar to read the contents of a driver file and write its
!    contents into a Fortran structure.
! HISTORY:
!    09 Jun 2016, ACP: Initial version
!    15 Jun 2016, ACP: Errors in the new-format driver file should be terminal
!
#include "struct_parser.h"

module MODULE_NAME
   implicit none

contains

subroutine WRAPPER_NAME_F(filename, strct)
   USE_STATEMENTS
   use iso_c_binding
   implicit none

   interface
      subroutine WRAPPER_NAME_FC( &
#include XCAT3(INC_PATH, f_arg, inc)
         stat, filename) bind(C,name=XSTR(WRAPPER_NAME_C))
         USE_STATEMENTS
         use iso_c_binding
         implicit none

#include XCAT3(INC_PATH, f_def, inc)
#include XCAT3(INC_PATH, f_arr, inc)
         integer :: stat
         character(c_char) :: filename(path_length)
      end subroutine WRAPPER_NAME_FC
   end interface

   character(len=*),         intent(in)    :: filename
   type(PARENT_STRUCT_TYPE), intent(inout) :: strct

   ! Declarations of variables
#include XCAT3(INC_PATH, f_arr, inc)

   integer :: stat
   character(kind=c_char,len=path_length) :: c_filename

   ! Copy pointers to structure into variables passed to wrapper
#include XCAT3(INC_PATH, f_cpy, inc)

   ! Call C wrapper function
   c_filename = trim(filename)//C_NULL_CHAR
   call WRAPPER_NAME_FC( &
#include XCAT3(INC_PATH, f_arg2, inc)
      stat, c_filename)
   if (stat /= 0) stop error_stop_code

   ! Copy arrays allocated in wrapper into structure
#include XCAT3(INC_PATH, f_cpy2, inc)

end subroutine WRAPPER_NAME_F

! Routines called from C to allocate arrays
#define FORT_ALLOC_NAME_1D fort_alloc_bool_1d
#define FORT_ALLOC_NAME_2D fort_alloc_bool_2d
#define FORT_ALLOC_TYPE logical
#include "fort_alloc.inc"
#undef FORT_ALLOC_NAME_1D
#undef FORT_ALLOC_NAME_2D
#undef FORT_ALLOC_TYPE

#define FORT_ALLOC_NAME_1D fort_alloc_int_1d
#define FORT_ALLOC_NAME_2D fort_alloc_int_2d
#define FORT_ALLOC_TYPE integer
#include "fort_alloc.inc"
#undef FORT_ALLOC_NAME_1D
#undef FORT_ALLOC_NAME_2D
#undef FORT_ALLOC_TYPE

#define FORT_ALLOC_NAME_1D fort_alloc_float_1d
#define FORT_ALLOC_NAME_2D fort_alloc_float_2d
#define FORT_ALLOC_TYPE real
#include "fort_alloc.inc"
#undef FORT_ALLOC_NAME_1D
#undef FORT_ALLOC_NAME_2D
#undef FORT_ALLOC_TYPE


subroutine c_to_fortran_str(str)
   use iso_c_binding, only: C_NULL_CHAR
   implicit none

   character(*), intent(inout) :: str
   integer :: i

   do i=1,len(str)
      if (str(i:i) == C_NULL_CHAR) exit
   end do

   str(i:len(str)) = ' '
end subroutine c_to_fortran_str

end module MODULE_NAME
