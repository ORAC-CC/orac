!locate.F90
!
! Purpose:
!
!    Finds the location of the pair of values in the set xx that bound x
!
! Algorithm:
! 
!    From Numerical Recipes in Fortran 90
!
! History:
!    22 April 2009 - Written by C. Arnold
!
! Bugs
!    None known
!
! $Id$
!
!---------------------------------------------------------------------
function locate(xx,x)
  implicit none
  real, dimension(:), intent(in) :: xx
  real, intent(in) :: x
  integer :: locate
  integer :: n,jl,jm,ju
  logical :: ascnd
  n=size(xx)
  !write(*,*) 'nlocate',n
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
end function locate

