!-------------------------------------------------------------------------------
! Name: int_routines.F90
!
! Purpose:
! Module of interpolation routines.
!
! History:
! 2014/05/23, GM: Original version.
! 2015/01/19, GM: Put polint.F90 into this module.
! 2015/07/22, AP: Made find_in_array a module procedure.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Int_Routines_m
   use common_constants_m, only: sint, lint, sreal, dreal

   implicit none

   interface find_in_array
      module procedure &
           find_in_array_sint, find_in_array_lint, &
           find_in_array_sreal, find_in_array_dreal
   end interface find_in_array
contains

#include "linint.F90"
#include "locate.F90"


!---------------------------------------------------------------------
! Name: find_in_array
!
! Purpose:
! Returns the index of an input array at which a given value can be found.
! Useful for converting array subscripts from one basis to another.
!
! Arguments:
! Name          Type In/Out/Both Description
! ------------------------------------------------------------------------------
! arr           int  In          Array to be searched
! val           int  In          Value to be found in array
! find_in_array int  Out         The index of arr which first equals val.
!                                If none is found, 0 is returned.
!
! Algorithm:
! Done by brute force following
! www.shocksolution.com/2011/03/finding-value-in-unordered-fortran-array/
!
! History:
! 2015/01/12, AP: Original version.
! 2015/07/22, AP: Generalised to module function across all data types.
!
! Bugs
! None known.
!---------------------------------------------------------------------
#define FIND_IN_ARRAY_NAME find_in_array_sint
#define FIND_IN_ARRAY_TYPE integer
#define FIND_IN_ARRAY_KIND sint
#include "find_in_array.inc"
#undef FIND_IN_ARRAY_NAME
#undef FIND_IN_ARRAY_TYPE
#undef FIND_IN_ARRAY_KIND

#define FIND_IN_ARRAY_NAME find_in_array_lint
#define FIND_IN_ARRAY_TYPE integer
#define FIND_IN_ARRAY_KIND lint
#include "find_in_array.inc"
#undef FIND_IN_ARRAY_NAME
#undef FIND_IN_ARRAY_TYPE
#undef FIND_IN_ARRAY_KIND

#define FIND_IN_ARRAY_NAME find_in_array_sreal
#define FIND_IN_ARRAY_TYPE real
#define FIND_IN_ARRAY_KIND sreal
#include "find_in_array.inc"
#undef FIND_IN_ARRAY_NAME
#undef FIND_IN_ARRAY_TYPE
#undef FIND_IN_ARRAY_KIND

#define FIND_IN_ARRAY_NAME find_in_array_dreal
#define FIND_IN_ARRAY_TYPE real
#define FIND_IN_ARRAY_KIND dreal
#include "find_in_array.inc"
#undef FIND_IN_ARRAY_NAME
#undef FIND_IN_ARRAY_TYPE
#undef FIND_IN_ARRAY_KIND

end module Int_Routines_m
