!-------------------------------------------------------------------------------
! Name: parse_user.F90
!
! Purpose:
! A module containing extensions to the parsing module which allow for the use
! of human-readable parameter specification in the driver file.
!
! History:
! 2015/07/22, AP: Original version.
! 2015/08/18, AP: Added optional argument specifying the ID# for each channel
!    such that CHXX style indexing can be used.

!
! $Id$
!-------------------------------------------------------------------------------

module parse_user_m
   use parsing_m

   implicit none

   ! Return status constants (0 means no error).
   integer, parameter :: PARSE_ERR_BAD_ARR  = 4 ! Out-of-range array subscripting

   interface parse_user_text
      module procedure &
           parse_user_text_0d_sint, parse_user_text_0d_lint, &
           parse_user_text_1d_sint, parse_user_text_1d_lint, &
           parse_user_text_2d_sint, parse_user_text_2d_lint
   end interface parse_user_text

   interface parse_array_index
      module procedure &
           parse_array_index_1d_sint, parse_array_index_1d_lint, &
           parse_array_index_2d_sint, parse_array_index_2d_lint
   end interface parse_array_index
contains

!-------------------------------------------------------------------------------
! Name: parse_user_text
!
! Purpose:
! Translates a variable name into its value. Used with parameters defined in
! ECP_Constants. Only searches for () to index arrays, being Fortran syntax.
!
! Description and Algorithm details:
! 1) Check for presence of (.
!    a) If found, use string before ( as variable name and string between ( and
!       ) as the index of that array desired.
!    b) If not, compare string to a list of ECP_Constants parameters.
! 2) If no match is found, return PARSE_ERR_CONV.
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! in     string  In          The string to translate.
! out    int     Out         The value of the requested variable.
! status int     Out         (Return value) Indicates success/failure.
! ch_ids int array In        (Optional) Specifies the ID# of each channel.
!
! History:
! 2015/07/22, AP: Original version.
!
! Bugs:
! The 1D and 2D versions of the function are direct copies
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: parse_array_index
!
! Purpose:
! Used by parse_user_text with array parameters to select the correct elements.
!
! Description and Algorithm details:
! 1) Parse string input to array index.
! 2) If that succeeded, check the index is valid for the given array.
! 3) If so, return the value of that element of that array.
!
! Arguments:
! Name   Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! idx    string    In          String specifying array element to return.
! arr    int array In          Array from which to return an element.
! out    int       Out         Value of desired array element.
! status int       Out         (Return value) Indicates success/failure.
! ch_ids int array In          (Optional) Specifies the ID# of each channel.
!
! History:
! 2015/07/22, AP: Original version.
! 2015/07/27, AP: Added Pavolonis cloud typing outputs.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define PARSE_STRING_TYPE integer
#define PARSE_STRING_KIND sint
#define PARSE_STRING_NAME_0D parse_user_text_0d_sint
#define PARSE_STRING_NAME_1D parse_user_text_1d_sint
#define PARSE_STRING_NAME_2D parse_user_text_2d_sint
#define PARSE_ARRAY_INDEX_NAME_1D parse_array_index_1d_sint
#define PARSE_ARRAY_INDEX_NAME_2D parse_array_index_2d_sint
#include "../common/parse_string.inc"
#include "parse_user_text.inc"
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_0D
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D
#undef PARSE_ARRAY_INDEX_NAME_1D
#undef PARSE_ARRAY_INDEX_NAME_2D
#undef PARSE_STRING_TYPE

#define PARSE_STRING_TYPE integer
#define PARSE_STRING_KIND lint
#define PARSE_STRING_NAME_0D parse_user_text_0d_lint
#define PARSE_STRING_NAME_1D parse_user_text_1d_lint
#define PARSE_STRING_NAME_2D parse_user_text_2d_lint
#define PARSE_ARRAY_INDEX_NAME_1D parse_array_index_1d_lint
#define PARSE_ARRAY_INDEX_NAME_2D parse_array_index_2d_lint
#include "../common/parse_string.inc"
#include "parse_user_text.inc"
#undef PARSE_STRING_KIND
#undef PARSE_STRING_NAME_0D
#undef PARSE_STRING_NAME_1D
#undef PARSE_STRING_NAME_2D
#undef PARSE_ARRAY_INDEX_NAME_1D
#undef PARSE_ARRAY_INDEX_NAME_2D
#undef PARSE_STRING_TYPE

end module parse_user_m
