!-------------------------------------------------------------------------------
! Name: polint.F90
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

subroutine polint(xa,ya,n,x,y,dy)

  use ECP_Constants

  implicit none

  integer(kind=lint) :: n,nmax
  real(kind=sreal) :: dy,x,y,xa(n),ya(n)
  parameter (nmax=10)
  integer(kind=lint) :: i,m,ns
  real(kind=sreal) :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  ns=1
  dif=abs(x-xa(1))
  do i=1,n
     dift=abs(x-xa(i))
     if (dift.lt.dif) then
        ns=i
        dif=dift
     end if
     c(i)=ya(i)
     d(i)=ya(i)
  end do

  y=ya(ns)
  ns=ns-1
  do  m=1,n-1
     do  i=1,n-m
        ho=xa(i)-x
        hp=xa(i+m)-x
        w=c(i+1)-d(i)
        den=ho-hp
        if(den.eq.0.) stop 'failure in polint'
        den=w/den
        d(i)=hp*den
        c(i)=ho*den
     end do
     if (2*ns.lt.n-m)then
        dy=c(ns+1)
     else
        dy=d(ns)
        ns=ns-1
     end if
     y=y+dy
  end do
  return
end subroutine polint
