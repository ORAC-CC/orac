!-------------------------------------------------------------------------------
! Name: IntRoutines.F90
!
! Purpose:
! Module of interpolation routines.
!
! History:
! 2014/05/23, GM: Original version.
! 2015/01/19, GM: Put polint.F90 into this module.
!
! $Id$
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Int_Routines_def

   implicit none

contains

include 'Bcuint.F90'
include 'Bcucof.F90'
include 'Linint.F90'
include 'Locate.F90'
include 'Spline.F90'
include 'Tridag.F90'
include 'polint.F90'


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
!
! Bugs
! None known.
!---------------------------------------------------------------------
function find_in_array(arr, val) result(i)
   implicit none
   integer, intent(in) :: arr(:) ! Array to be searched
   integer, intent(in) :: val    ! Value to find in that array
   integer :: n, i
   n = size(arr)
   do i=1,n
      if (arr(i) == val) return
   end do
   ! Failed to locate value
   i=0
   return
end function find_in_array

end module Int_Routines_def
