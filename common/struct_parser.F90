!
! NAME:
!    struct_parser.F90
! PURPOSE:
!    Module to read the contents of a driver file and write its contents into a
!    Fortran structure or to read the contents of a Fortran structure and print
!    its contents into a driver character buffer.
! HISTORY:
!    09 Jun 2016, ACP: Initial version
!    15 Jun 2016, ACP: Errors in the new-format driver file should be terminal
!    11 Jul 2016, GRM: Add printer support.
!
#include "struct_parser.h"

module MODULE_NAME
   implicit none

contains

subroutine WRAPPER_NAME_F(filename, STRUCT_NAME)
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
   type(PARENT_STRUCT_TYPE), intent(inout) :: STRUCT_NAME

   ! Declarations of variables
#include XCAT3(INC_PATH, f_arr, inc)

   integer :: stat
   character(kind=c_char,len=path_length) :: c_filename

   ! Copy pointers to structure into variables passed to wrapper
#include XCAT3(INC_PATH, f_cpy, inc)

   ! Call C wrapper function
   c_filename = trim(filename) FCAT C_NULL_CHAR
   call WRAPPER_NAME_FC( &
#include XCAT3(INC_PATH, f_arg2, inc)
      stat, c_filename)
   if (stat /= 0) stop error_stop_code

   ! Copy arrays allocated in wrapper into structure
#include XCAT3(INC_PATH, f_cpy2, inc)

end subroutine WRAPPER_NAME_F


! Will write at most 'length' driver characters into the character array 'buf'.
! Returns the actual number of characters written or if the output was truncated
! to 'length' it returns the number of characters that would have been written
! if enough space had been available.  By setting length to zero a dry-run can
! be performed in order to obtain the required length of the buffer.
integer function PRINTER_NAME(STRUCT_NAME, buf, length) result(count)
   USE_STATEMENTS
   use iso_c_binding
   implicit none

   type(PARENT_STRUCT_TYPE), intent(in)    :: STRUCT_NAME
   character, target,        intent(inout) :: buf(:)
   integer,                  intent(in)    :: length

   interface
      integer(c_int) function print_string(buf, length, name, value) &
         bind(C, name="parser_print_string")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         character(c_char), intent(in)        :: value(*)
      end function print_string
   end interface

   interface
      integer(c_int) function print_char_scalar(buf, length, name, value) &
         bind(C, name="parser_print_char_scalar")
         use iso_c_binding
         implicit none
         character(c_char),      intent(inout)     :: buf(*)
         integer(c_int),         intent(in), value :: length
         character(c_char),      intent(in)        :: name(*)
         integer(c_signed_char), intent(in), value :: value
      end function print_char_scalar
   end interface

   interface
      integer(c_int) function print_char_array(buf, length, name, value, n_dims, dims) &
         bind(C, name="parser_print_char_array")
         use iso_c_binding
         implicit none
         character(c_char),      intent(inout)      :: buf(*)
         integer(c_int),         intent(in), value  :: length
         character(c_char),      intent(in)         :: name(*)
         integer(c_signed_char), intent(in)         :: value(*)
         integer(c_int),         intent(in), value  :: n_dims
         integer(c_int),         intent(in)         :: dims(*)
      end function print_char_array
   end interface

   interface
      integer(c_int) function print_int_scalar(buf, length, name, value) &
         bind(C, name="parser_print_int_scalar")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         integer(c_int),    intent(in), value :: value
      end function print_int_scalar
   end interface

   interface
      integer(c_int) function print_int_array(buf, length, name, value, n_dims, dims) &
         bind(C, name="parser_print_int_array")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         integer(c_int),    intent(in)        :: value(*)
         integer(c_int),    intent(in), value :: n_dims
         integer(c_int),    intent(in)        :: dims(*)
      end function print_int_array
   end interface

   interface
      integer(c_int) function print_float_scalar(buf, length, name, value) &
         bind(C, name="parser_print_float_scalar")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         real(c_float),     intent(in), value :: value
      end function print_float_scalar
   end interface

   interface
      integer(c_int) function print_float_array(buf, length, name, value, n_dims, dims) &
         bind(C, name="parser_print_float_array")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         real(c_float),     intent(in)        :: value(*)
         integer(c_int),    intent(in), value :: n_dims
         integer(c_int),    intent(in)        :: dims(*)
      end function print_float_array
   end interface

   character, pointer :: ptr(:)

   count = 0

   nullify(ptr)

#include XCAT3(INC_PATH, f_pri, inc)

    count = count + 1

    if (length .gt. 0) buf(count) = ' '

end function PRINTER_NAME


integer(c_int) function print_bool_scalar(buf, length, name, value) result(count)
   use iso_c_binding

   implicit none

   character, intent(inout) :: buf(*)
   integer,   intent(in)    :: length
   character, intent(in)    :: name(*)
   logical,   intent(in)    :: value

   interface
      integer(c_int) function print_bool_scalar2(buf, length, name, value) &
         bind(C, name="parser_print_bool_scalar")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         integer(c_int),    intent(in), value :: value
      end function print_bool_scalar2
   end interface

   integer(c_int) :: value2

   if (value) then
       value2 = 1
   else
       value2 = 0
   end if

   count = print_bool_scalar2(buf, length, name, value2)
end function print_bool_scalar


integer(c_int) function print_bool_array(buf, length, name, value, n_dims, dims) result(count)
   use iso_c_binding

   implicit none

   character(c_char), intent(inout)     :: buf(*)
   integer(c_int),    intent(in), value :: length
   character(c_char), intent(in)        :: name(*)
   logical,           intent(in)        :: value(*)
   integer(c_int),    intent(in), value :: n_dims
   integer(c_int),    intent(in)        :: dims(*)

   interface
      integer(c_int) function print_bool_array2(buf, length, name, value, n_dims, dims) &
         bind(C, name="parser_print_bool_array")
         use iso_c_binding
         implicit none
         character(c_char), intent(inout)     :: buf(*)
         integer(c_int),    intent(in), value :: length
         character(c_char), intent(in)        :: name(*)
         integer(c_int),    intent(in)        :: value(*)
         integer(c_int),    intent(in), value :: n_dims
         integer(c_int),    intent(in)        :: dims(*)
      end function print_bool_array2
   end interface

   integer                     :: i
   integer                     :: n
   integer(c_int), allocatable :: value2(:)

   n = 1
   do i = 1, n_dims
       n = n * dims(i)
   end do

   allocate(value2(n))

   where (value(1:n))
       value2 = 1
   else where
       value2 = 0
   end where

   count = print_bool_array2(buf, length, name, value2, n_dims, dims)

   deallocate(value2)
end function print_bool_array


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

   do i = 1, len(str)
      if (str(i:i) == C_NULL_CHAR) exit
   end do

   str(i:len(str)) = ' '
end subroutine c_to_fortran_str

end module MODULE_NAME
