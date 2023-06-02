!-------------------------------------------------------------------------------
! Name: interp3dlut.F90
!
! Purpose:
! Interpolates values from a 3-d LUT array.
!
! Description and Algorithm details:
! The subroutine is passed an array of data in 3 dimensions, plus the x and y
! grids corresponding to the LUT array points (the third dimension in the LUT
! array is wavelength, which is not interpolated), plus the co-ordinates of a
! point in the "real" data array. The subroutine calculates the value of the
! LUT at the real data point, plus the gradients in x and y.
!
! Arguments:
! Name Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! chan  int        In          Channel number (1st index of array F)
! F     real array Both        The 2-d (ignoring chan) array to be interpolated
! xGrid real array Both         x-values for F
! yGrid real array Both         y-values for F
! delx  real       In           Grid step in x
! dely  real       In           Grid step in y
! CurX  real       In           The "current" x value, i.e. the point where we
!                               want to find the interpolated value.
! CurY  real       In           The "current" y value
! FInt  real       Out          Interpolated value of F at CurX, CurY
! dFdx  real       Out          Gradients of F wrt x at CurX, CurY
! dFdy  real       Out          Gradients of F wrt y at CurX, CurY
!
! History:
! 2000/10/26, AS: Original version
!
! Bugs:
! None known.
!---------------------------------------------------------------------

subroutine Interp3dLUT(chan, F, xGrid, yGrid, delx, dely, CurX, CurY, FInt, &
                       dFdx, dFdy)

   implicit none

   ! Argument declarations

   integer,                intent(in)  :: chan
                                       ! Channel number (1st index of array F)
   real, dimension(:,:,:), intent(in)  :: F
                                       ! The 2-d  (ignoring chan) array to be
                                       ! interpolated.
   real, dimension(:),     intent(in)  :: xGrid
                                       ! x-values for F
   real, dimension(:),     intent(in)  :: yGrid
                                       ! y-values for F
   real,                   intent(in)  :: delx, dely
                                       ! Grid step in x and y
   real,                   intent(in)  :: CurX, CurY
                                       ! The "current" x and y values, i.e. the
                                       ! point where we want to find the
                                       ! interpolated value.
   real,                   intent(out) :: FInt
                                       ! Interpolated value of F at CurX, CurY
   real,                   intent(out) :: dFdx, dFdy
                                       ! Gradients of F wrt x and y at CurX, CurY

   ! Local variables

   integer :: ix0, ix1, iy0, iy1 ! Array indices (in xGrid, yGrid) for the
                                 ! nearest neighbour grid points.
   real    :: k0, k1             ! Intermediate interpolated values
   real    :: dx, dy             ! Distance from required x,y values to
                                 ! next (lower) LUT grid point, as a
                                 ! fraction of the LUT cell length in x,y.


   ! Find the array indices of the nearest neighbour grid points
   ! Index in x is the distance from the origin along the x-axis, divided by the
   !  size of each step, plus 1 as the arrays start at index 1 (not 0).

   write(*,*)' CurX, Xgrid(1), delx:', CurX, xGrid(1), delx
   write(*,*)' Cury, ygrid(1), dely:', CurY, yGrid(1), dely

   ix0 = int((CurX - xGrid(1)) / delx) + 1 ! +1 as our arrays start at index 1
   ix1 = ix0 + 1
   iy0 = int((CurY - yGrid(1)) / dely) + 1
   iy1 = iy0 + 1

   write(*,'(a, 4i3.1)')' indices of X0, Y0 and X1, Y1: ',ix0, iy0, ix1, iy1
   write(*,*)' Function values at top corners',F(chan, ix0, iy1), F(chan, ix1, iy1)
   write(*,*)' Function values at bot corners',F(chan, ix0, iy0), F(chan, ix1, iy0)

   ! Calcuate dx, dy: these are the distances in x, y from (x0, y0) to
   ! (CurX, CurY) expressed as a fraction of the LUT grid steps.

   dx = (CurX - xGrid(ix0)) / delx
   dy = (CurY - yGrid(iy0)) / dely

   ! Calculate intermediate interpolated values along y-axis

   k0 = (1.0 - dy) * F(chan, ix0, iy0)  +  dy * F(chan, ix0, iy1)
   k1 = (1.0 - dy) * F(chan, ix1, iy0)  +  dy * F(chan, ix1, iy1)

   ! Interpolate along x-axis to get the result

   FInt = (1.0 - dx) * k0  + dx * k1

   ! Gradient in y follows from the k0, k1 values we already have, and the known
   ! step size of the LUT grid.

   dFdy = (k1 - k0) / dely

   ! k0 and k1 now become the intermediate values in the other direction

   k0 = (1.0 - dx) * F(chan, ix0, iy0)  +  dx * F(chan, ix1, iy0)
   k1 = (1.0 - dx) * F(chan, ix0, iy1)  +  dx * F(chan, ix1, iy1)

   dFdx = (k1 - k0) / delx

end subroutine Interp3dLUT
