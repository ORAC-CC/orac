! Name:
!   Invert_Cholesky
!
! Purpose:
!   Given a positive definite matrix A of dimension n * n, returns its inverse
!   in D. This function makes approximately n^3 / 2 multiplications.
!
! Algorithm:
!   This function encapsulates a series of routines which derive and operate on
!   the Cholesky decomposition L of A. First calculates the Cholesky
!   decomposition L, then its inverse L^-1, then calculates the full inverse
!   S^-1 = (L^-1)^T.L^-1 for the upper triangle only, as the inverse will also
!   be symmetric, then copies across the symmetric values from the upper to the
!   lower triangle.
!  
! Arguments:
!   Name        Type         In/Out   Description
!   A           float array  In       Positive definite matrix to be inverted
!   D           float array  Out      Inverse of positive definite matrix
!   n           integer      In       Dimensions of A, D, p; loop limit
!   Status      integer      Out      Status flag: 0 = Success, 1 = Not positive
!                                     definite
!
! Local variables:
!   Name        Type                  Description
!   i           integer               Loop index 
!
! Subprograms
!   subroutine Decompose_Cholesky
!   subroutine Invert_Cholesky_DC
!   subroutine Square_Cholesky_DC
!
! History:
!   27th April, 2001, Tim Nightingale : Original version.
!   17th May, 2001, Tim Nightingale: Add Decompose_Cholesky to subroutine
!      as decomposition is only used once.
!   5th July, 2001, Tim Nightingale: Return if decomposition fails.
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------

subroutine Invert_Cholesky(A, D, n, Status)
   implicit none

   integer, intent(IN) :: n
   real, dimension(n,n), intent(IN) :: A
   real, dimension(n,n), intent(OUT) :: D
   integer, intent(OUT) :: Status
   integer :: i

   D = A
   
   call Decompose_Cholesky(D, n, Status)
   if (Status /= 0) return
   call Invert_Cholesky_DC(D, n) 
   call Square_Cholesky_DC(D, n)

   do i = 2, n
      D(i,1:i-1) = D(1:i-1,i)
   end do
end subroutine Invert_Cholesky
