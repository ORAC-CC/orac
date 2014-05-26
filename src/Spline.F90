!-------------------------------------------------------------------------------
! spline.F90
!
! Purpose:
!
! Given a tabulated function (x_i,y_i) returns the second derivative of the
! function d2y/dx2 at the values (x_i)
!
! These derivatives are used  by the ORAC interpolation routines to perform
! cubic spline interpolation of eg RTM data
!
! Arguments:
!    Name Type in/Out/Both Description
!    x    real in          abscissa values of function
!    y    real in          value of function at abscissa points
!    y2	  real out         dx2/dy2 at abscissa points
!
! Algorithm
!    From Numerical Recipes in Fortran 90 [Press, Flannery]
!
! History
!    31st 29th August 2011 - Adapted for ORAC F90 by C. Arnold
!
! Bugs
!    None known
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine spline(x,y,y2)

   implicit none

   ! Define arguments

   real, dimension(:), intent(in)  :: x,y
   real, dimension(:), intent(out) :: y2

   ! Define local variables

   integer :: n
   real, dimension(size(x)) :: a,b,c,r

   n = size(x)

   c(1:n-1)=x(2:n)-x(1:n-1)
   r(1:n-1)=6.0*((y(2:n)-y(1:n-1))/c(1:n-1))
   r(2:n-1)=r(2:n-1)-r(1:n-2)
   a(2:n-1)=c(1:n-2)
   b(2:n-1)=2.0*(c(2:n-1)+a(2:n-1))
   b(1)=1.0
   b(n)=1.0

   ! We use a natural cubic spline.
   r(1)=0.0
   c(1)=0.0
   r(n)=0.0
   a(n)=0.0

   call tridag_par(a(2:n),b(1:n),c(1:n-1),r(1:n),y2(1:n))

end subroutine spline
