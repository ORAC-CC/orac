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
! 2020/08/18, AP: Use dVariable = 0 to indicate unevenly spaced grids. Switch
!    to using arithmetic index finding for evenly spaced grids.
! 2020/09/25, AP: Add bounded keyword, restricting output to [1, n-1]
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function locate(grid, x, bounded) result(low)

   implicit none

   type(LUT_Dimension_t), intent(in) :: grid
   real,                  intent(in) :: x
   logical,               intent(in), optional :: bounded

   logical :: ascending, bound
   integer :: low, mid, up

   if (grid%n .eq. 0) then
      low = 0
      return
   end if

   if (present(bounded)) then
      bound = bounded
   else
      bound = .false.
   end if
   ascending = grid%x(1) .le. grid%x(grid%n)

   if (grid%d .ne. 0.) then
      if (ascending) then
         low = int((x - grid%x(1)) / grid%d) + 1
      else
         low = int((grid%x(1) - x) / grid%d) + 1
      end if
      if (bound) then
         if (low .lt. 1) low = 1
         if (low .ge. grid%n) low = grid%n-1
      end if
      return
   end if

   if (ascending) then
      if (x .lt. grid%x(1)) then
         if (bound) then
            low = 1
         else
            low = 0
         end if
         return
      else if (x .gt. grid%x(grid%n)) then
         if (bound) then
            low = grid%n-1
         else
            low = grid%n
         end if
         return
      end if
   else
      if (x .gt. grid%x(1)) then
         if (bound) then
            low = 1
         else
            low = 0
         end if
         return
      else if (x .lt. grid%x(grid%n)) then
         if (bound) then
            low = grid%n-1
         else
            low = grid%n
         end if
         return
      end if
   end if

   low = 1
   up  = grid%n

   do while (low + 1 .lt. up)
      mid = (low + up) / 2

      if (ascending .eqv. (grid%x(mid) .le. x)) then
         low = mid
      else
         up  = mid
      end if
   end do

end function locate
