!-------------------------------------------------------------------------------
! Name: linint.F90
!
! Purpose:
! Performs a linear interpolation on the interval ((0,0),(0,1), (1,1),(1,0))
!
! Description and Algorithm details:
! This routine is the linear equivalent of bcuint.f90
!
! Arguments:
! Name        Type    In/Out/Both    Description
! ------------------------------------------------------------------------------
! y           real array  In         Values of functions at each of
!                                    the four vertices around the
!                                    point to be interpolated to.
!                                    Values should be listed in
!                                    anti-clockwise order, starting
!                                    at the bottom left.
! x1l         real        In         The lower coordinates of the grid
!                                    square in the 1- direction
! x1u         real        In         The upper coordinates of the grid
!                                    square in the 1- direction
! x2l         real        In         The lower coordinates of the grid
!                                    square in the 2- direction
! x2u         real        In         The upper coordinates of the grid
!                                    square in the 2- direction
! x1          real        In         Coordinate of interpolation point
!                                    in the 1- direction
! x2          real        In         Coordinate of interpolation point
!                                    in the 2- direction
! ansy        real        Out        The interpolated value of the
!                                    function at t,u
! ansy1       real        Out        The interpolated gradient wrt
!                                    to x of the function at t,u
! ansy2       real        Out        The interpolated gradient wrt
!                                    to y of the function at t,u
!
! History:
! 2011/10/02, CA: Original version
! 2014/01/23, GM: Cleaned up code.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine linint(y,x1l,x1u,x2l,x2u,x1,x2,ansy,ansy1,ansy2)

   use ORAC_Constants_m

   implicit none

   ! Define arguments

   real, dimension(:), intent(in)  :: y
   real,               intent(in)  :: x1l,x1u,x2l,x2u,x1,x2
   real,               intent(out) :: ansy,ansy1,ansy2

   ! Define local variables

   real :: dh1, dh2, dl1, dl2

   dh1 = (x1)
   dl1 = (1. - dh1)
   dh2 = (x2)
   dl2 = (1. - dh2)

   ansy = ((y(1) * dl1 * dl2) + &
        (y(2) * dh1 * dl2) + &
        (y(3) * dh1 * dh2) + &
        (y(4) * dl1 * dh2))

   if (abs(x1u-x1l) .le. ditherm15) then
      ansy1 = 0.0
   else
      ansy1 = (((y(2) * dl2) + (y(3) * dh2)) - &
         ((y(1) * dl2) + (y(4) * dh2))) &
         / (x1u-x1l)
   end if

   if (abs(x2u-x2l) .le. ditherm15) then
      ansy2 = 0.0
   else
      ansy2 = (((y(4) * dl1) + (y(3) * dh1)) - &
         ((y(1) * dl1) + (y(2) * dh1))) &
         / (x2u-x2l)
   end if

end subroutine linint
