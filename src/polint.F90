! Name:
!
!
! Purpose:
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!
!
! $Id$
!
! Bugs:
!
!

SUBROUTINE polint(xa,ya,n,x,y,dy)

  use ECP_Constants

  implicit none

  INTEGER(kind=nint) :: n,NMAX
  REAL(kind=sreal) :: dy,x,y,xa(n),ya(n)
  PARAMETER (NMAX=10)
  INTEGER(kind=nint) :: i,m,ns
  REAL(kind=sreal) :: den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
  ns=1
  dif=abs(x-xa(1))
  do i=1,n
     dift=abs(x-xa(i))
     if (dift.lt.dif) then
        ns=i
        dif=dift
     endif
     c(i)=ya(i)
     d(i)=ya(i)
  enddo

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
     enddo
     if (2*ns.lt.n-m)then
        dy=c(ns+1)
     else
        dy=d(ns)
        ns=ns-1
     endif
     y=y+dy
  enddo
  return
END SUBROUTINE polint
