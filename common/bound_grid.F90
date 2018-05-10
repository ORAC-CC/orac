!-------------------------------------------------------------------------------
! Name: bound_grid.F90
!
! Purpose:
! For a bilinear interpolation, this selects the four points that surround the
! desired point and calculates the normalised distance of that from the bottom
! left corner of the original grid cell (from which the coefficients of the
! interpolation are calculated).
!
! Description and Algorithm details:
! 1) Determine the index of the grid point just below and left of the desired
!    point.
! 2) Unless wrapping is cancelled, if the point is left of the first column or
!    right of the last column, set these as the corners (wrapping through the
!    dateline).
! 3) Set other points outside the grid to be extrapolated.
! 4) Calculate the normalised distance of the desired point from the bottom
!    corner (t and u).
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! xstart   real    In  Initial value of the x axis.
! nx       integer In  Length of the x axis.
! x_invdel real    In  1 / the separation of x grid cells.
! ystart   real    In  Initial value of the y axis.
! ny       integer In  Length of the y axis.
! y_invdel real    In  1 / the separation of y grid cells.
! interp   struct  Out Indices of points just surrounding desired point. (See
!                      interpol.F90)
! xout     real    In  Horizontal coordinate at which value desired.
! yout     real    In  Vertical coordinate at which value desired.
! no_wrap  logic   Opt If true, the interpolation will not wrap around the
!                      horizontal axis. Set if the grid to be interpolated is
!                      not global.
!
! History:
! 2014/08/05, AP: Initial version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine bound_regular_grid(start, invdel, n, out, below, above, frac, wrap)
   implicit none

   real(8),     intent(in)  :: start  ! first point on grid
   real(8),     intent(in)  :: invdel ! 1 / grid division
   integer,     intent(in)  :: n      ! length of grid
   real,        intent(in)  :: out    ! desired point
   integer,     intent(out) :: below  ! grid reference just below out
   integer,     intent(out) :: above  ! grid reference just above out
   real,        intent(out) :: frac   ! fraction of cell covered by out
   logical,     intent(in)  :: wrap

   real                     :: tmp
   logical                  :: bottom

   ! index of grid point below desired location
   tmp = real((real(out, kind=8) - start)*invdel, kind=sreal)
   below = floor(tmp) + 1

   bottom = .false.

   ! manage locations on the edge of the lon grid
   if (wrap) then
      ! loop around the longitude grid
      if (below < 1) then
         ! below bottom of grid
         below = n
         above = 1
         bottom = .true.
      else if (below >= n) then
         ! above top of grid
         below = n
         above = 1
      else
         ! within grid
         above = below + 1
      end if
   else
      ! extrapolate for points beyond the grid
      if (below < 1) then
         ! below bottom of grid
         below = 1
         above = 2
      else if (below >= n) then
         ! above top of grid
         below = n - 1
         above = n
      else
         ! within grid
         above = below + 1
      end if
   end if

   ! calculate normalised distance from the bottom corner
   if (bottom) then
      frac = tmp + 1
   else
      frac = tmp + 1 - below
   end if

end subroutine bound_regular_grid

subroutine bound_irregular_grid(grid, n, out, below, above, frac, wrap)
   implicit none

   real,        intent(in)  :: grid(:)  ! grid values
   integer,     intent(in)  :: n        ! length of grid
   real,        intent(in)  :: out      ! desired point
   integer,     intent(out) :: below    ! grid reference just below out
   integer,     intent(out) :: above    ! grid reference just above out
   real,        intent(out) :: frac     ! fraction of cell covered by out
   logical,     intent(in)  :: wrap

   integer                  :: mode, old, i

   ! determine if grid increasing or decreasing
   if (grid(1) < grid(2)) then
      below = 1
      above = n
   else
      below = n
      above = 1
   end if

   mode = 0
   if (out < grid(below)) then
      ! value below bottom of grid
      if (wrap) then
         below = n
         above = 1
         mode = 1
      else
         below = 1
         above = 2
      end if
   else if (out > grid(above)) then
      ! value above top of grid
      if (wrap) then
         below = n
         above = 1
         mode = 2
      else
         below = n-1
         above = n
      end if
   else
      ! search for coordinates that bound 'out' via bisection
      old = below
      do
         i = (below + above) / 2
         if (out > grid(i)) then
            below = i
         else
            above = i
         end if
         if (i == old) exit
         old = i
      end do
   end if

   ! calculate normalised distance from the bottom corner
   select case (mode)
   case(0) ! normal cell
      frac = (out - grid(below)) / (grid(above) - grid(below))
   case(1) ! wrapping (in lon) from below bottom cell
      frac = (out - grid(below) + 360.) / (grid(above) - grid(below) + 360.)
   case(2) ! wrapping (in lon) from above top cell
      frac = (out - grid(below)) / (grid(above) - grid(below) + 360.)
   end select

end subroutine bound_irregular_grid
