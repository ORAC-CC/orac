! Name:
!    Check_FloatArray
!
! Purpose:
!    Determines whether all elements in a real array are within specified
!    limits.  Sets elements of a mask to zero, when an 'out of range' array
!    element is found.
!
! Arguments
!    Name     Type       In/Out   Description
!    array    real arr   In       Input array to be examined
!    Nx       int        In       Number of columns to be examined
!    Ny       int        In       Number of rows to be examined
!    mask     int arr    Out      Mask (1 = good, 0 = out of range)
!    ulim     real       In       Upper limit
!    llim     real       In       Lower limit
!    status   int        Out      Status flag 
!
! Local variables:
!    Name   Type   Description
!    i, j   int    Loop counters
!
! History:
!    Dec 2000, Kevin M Smith : Original version 
!     2nd April 2001, Andy Smith:
!        Re-writing with Fortran 90 where statement to test for improved
!        efficiency. Performance was slowed down (probably doesn't work well
!        on relatively small arrays). 
!        Improved efficiency of existing code by using else if and reversing
!        the order of the do loops.
!
! Bugs:
!    None known.
! 
! $Id$
!
!---------------------------------------------------------------------
subroutine Check_FloatArray(Nx, Ny, array, mask, ulim, llim, status)

   implicit none
   
!  define arguments

   integer :: Nx
   integer :: Ny
   real    :: array(Nx, Ny)
   integer :: mask(Nx, Ny)
   real    :: ulim
   real    :: llim
   integer :: status
   
!  define local variables

   integer :: i, j
   
   status = 0

   do j = 1, Ny
      do i = 1, Nx
         if (array(i,j) > ulim) then
            mask(i,j) = 0
	    status    = 1
         else if (array(i,j) < llim) then
            mask(i,j) = 0
	    status    = 1
         end if
      end do
   end do

!   where (array > ulim) mask = 0
!   where (array < llim) mask = 0
!   
!   if (any(mask == 0)) status = 1
   
end subroutine Check_FloatArray
