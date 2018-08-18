!-------------------------------------------------------------------------------
! Name: locate.F90
!
! Purpose:
! Find the location of a value in an ascending or descending sorted array.
!
! Description and Algorithm details:
! Bisection.
!
! Arguments:
! Name Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! a    real array In          Sorted array to search
! x    real       In          Value to search for
!
! Return value:
! Index i such that a(i) <= x < a(i+1) or zero or size(a) if x falls to the left
! of a(1) or to the right of a(size(a)), respectively.
!
! History:
! 2017/10/02, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function locate(a,x) result(low)

   implicit none

   real, intent(in) :: a(:)
   real, intent(in) :: x

   logical :: ascending
   integer :: n, low, mid, up

   n = size(a)
   if (n .eq. 0) then
      low = 0
      return
   end if

   ascending = a(1) .le. a(n)

   if (ascending) then
      if (x .lt. a(1)) then
         low = 0
         return
      else if (x .gt. a(n)) then
         low = n
         return
      end if
   else
      if (x .gt. a(1)) then
         low = 0
         return
      else if (x .lt. a(n)) then
         low = n
         return
      end if
   end if

   low = 1
   up  = n

   do while (low + 1 .lt. up)
      mid = (low + up) / 2

      if (ascending .eqv. (a(mid) .le. x)) then
         low = mid
      else
         up  = mid
      end if
   end do

end function locate
