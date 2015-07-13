!-------------------------------------------------------------------------------
! Name: Locate.F90
!
! Purpose:
! Finds the location of the pair of values in the set xx that bound x
!
! Description and Algorithm details:
! This routine is the linear equivalent of bcuint.f90 from Numerical Recipes in
! Fortran 90
!
! Arguments:
! Name   Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! xx     real array In          Sorted array to search
! x      real       In          Value to search for
! locate int        Out         Index of array element that bounds x
!
! History:
! 2009/04/22, CA: Original version
!
! $Id$
!
! Bugs:
! None known.
!---------------------------------------------------------------------

function locate(xx,x)
  implicit none
  real, dimension(:), intent(in) :: xx
  real,               intent(in) :: x
  integer :: locate
  integer :: n,jl,jm,ju
  logical :: ascnd
  n=size(xx)
  ascnd = (xx(n) >= xx(1))
  jl=0
  ju=n+1
  do
     if (ju-jl <= 1) exit
     jm=(ju+jl)/2
     if (ascnd .eqv. (x >= xx(jm))) then
        jl=jm
     else
        ju=jm
     end if
  end do
  if (x == xx(1)) then
     locate=1
  else if (x == xx(n)) then
     locate=n-1
  else
     locate=jl
  end if
end function Locate
