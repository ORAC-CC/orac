!-------------------------------------------------------------------------------
! Name: locate.F90
!
! Purpose:
!
! Description and Algorithm details:
! From Numerical Recipes in Fortran 90 [Press, Flannery]
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/02/03, Matthias Jerg: Adapted for L2 software
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine locate_int(xx,n,x,j)

  use ECP_Constants

  implicit none

  integer(kind=lint) :: j,n
  real(kind=sreal) :: x,xx(n)
  integer(kind=lint) :: jl,jm,ju
  jl=0
  ju=n+1
10 if(ju-jl.gt.1)then
     jm=(ju+jl)/2
     if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
        jl=jm
     else
        ju=jm
     end if
     goto 10
  end if
  if(x.eq.xx(1))then
     j=1
  else if(x.eq.xx(n))then
     j=n-1
  else
     j=jl
  end if
  return
end subroutine locate_int
