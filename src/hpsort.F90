!-------------------------------------------------------------------------------
! Name: hpsort.F90
!
! Purpose:
! Sorts an array into ascending numerical order.
!
! Description and Algorithm details:
! From Numerical Recipes in Fortran 90 [Press, Flannery]
!
! Arguments:
! Name Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! n    int        In          Length of input array
! ra   real array Both        Array to sort.
!
! History:
! 2011/02/03, MJ: Adapted for L3 software
! 2012/06/13, CP: Adapted for preprocessing software
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine hpsort(n,ra)
!  use vartypes
!  use preproc_constants
!  use preproc_structures

  integer n
  real ra(n)
  integer i,ir,j,l
  real rra
  if (n.lt.2) return
  l=n/2+1
  ir=n
10 continue
  if(l.gt.1)then
     l=l-1
     rra=ra(l)
  else
     rra=ra(ir)
     ra(ir)=ra(1)
     ir=ir-1
     if(ir.eq.1)then
        ra(1)=rra
        return
     end if
  end if
  i=l
  j=l+l
20 if(j.le.ir)then
     if(j.lt.ir)then
        if(ra(j).lt.ra(j+1))j=j+1
     end if
     if(rra.lt.ra(j))then
        ra(i)=ra(j)
        i=j
        j=j+j
     else
        j=ir+1
     end if
     goto 20
  end if
  ra(i)=rra
  goto 10
end subroutine hpsort
