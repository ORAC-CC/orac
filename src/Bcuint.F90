!-------------------------------------------------------------------------------
! Name: Bcuint.F90
!
! Purpose:
! Performs a bicubic interpolation on the interval ((0,0),(0,1), (1,1),(1,0))
!
! Description and Algorithm details:
! From Numerical Recipes in Fortran 90 [Press, Flannery]. Uses BcuCof.F90
!
! Bicubic Interpolation within a grid square. Input quantities are y, y1, y2,
! y12, x1l, x1u, x2l, x2u, x1, x2 as described below.  The interpolated
! function value is returned as ansy and the interpolated gradient values as
! ansy1 and ansy2.
!
! Arguments:
! Name        Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! y           real array  In         Values of functions at each of
!                                    the four vertices around the
!                                    point to be interpolated to.
!                                    Values should be listed in
!                                    anti-clockwise order, starting
!                                    at the bottom left.
! y1          real array  In         Derivatives wrt to the
!                                    "x-coordinate" at the vertices.
! y2          real array  In         Derivatives wrt to the
!                                    "y-coordinate" at the vertices.
! y12         real array  In         Second order cross-derivatives
!                                    at the vertices.
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
! 2009/04/22, CA: Original version
! 2014/12/23, GM: Transpose the use of local variable c for compatibility with
!    changes made in bcucof() plus some code cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine bcuint(y,y1,y2,y12,x1l,x1u,x2l,x2u,x1,x2,ansy,ansy1,ansy2)

   use ECP_Constants

   implicit none

   ! Define arguments

   real, dimension(:), intent(in)  :: y,y1,y2,y12
   real,               intent(in)  :: x1l,x1u,x2l,x2u,x1,x2
   real,               intent(out) :: ansy,ansy1,ansy2

   ! Define local variables

   integer              :: i
   real                 :: t,u
   real, dimension(4,4) :: c

   call bcucof(y,y1,y2,y12,x1u-x1l,x2u-x2l,c)
   if (x1u == x1l .or. x2u == x2l) then
      write(*,*) 'ERROR: BCuInt(): Problem with input values - boundary ' // &
                 'pair equal?', x1u, x1l, x2u, x2l
      stop error_stop_code
   end if

   t=x1
   u=x2
   ansy=0.0
   ansy2=0.0
   ansy1=0.0
   do i=4,1,-1
       ansy=t*ansy+((c(4,i)*u+c(3,i))*u+c(2,i))*u+c(1,i)
       ansy2=t*ansy2+(3.0*c(4,i)*u+2.0*c(3,i))*u+c(2,i)
       ansy1=u*ansy1+(3.0*c(i,4)*t+2.0*c(i,3))*t+c(i,2)
   end do
   ansy1=ansy1/(x1u-x1l)
   ansy2=ansy2/(x2u-x2l)

end subroutine bcuint
