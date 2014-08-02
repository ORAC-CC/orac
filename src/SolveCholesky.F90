! Name:
!   Solve_Cholesky
!
! Purpose:
!   Given a positive definite matrix A of dimension n * n, and a column vector
!   b of dimension n, returns the solution x to A.x = b. This function makes
!   approximately n^3 / 6 (+ n^2) multiplications.
!
! Algorithm:
!   This function encapsulates two routines which derive and operate on
!   the Cholesky decomposition L of A. First calculates the Cholesky
!   decomposition L, then multiplies out the solution, x.
!
! Arguments:
!   Name        Type         In/Out   Description
!   A           float array  In       Positive definite matrix A
!   b           float array  In       RHS vector to A.x = b
!   x           float array  Out      Solution vector to A.x = b
!   n           integer      In       Dimensions of A; loop limit
!   Status      integer      Out      Status flag: 0 = Success, 1 = Not positive
!                                     definite
! Local variables:
!   Name        Type                  Description
!   i           integer               Loop index
!
! Subprograms:
!   subroutine Decompose_Cholesky
!   subroutine Solve_Cholesky_DC
!
! History:
!   18th May, 2001, Tim Nightingale : Original version.
!   5th July, 2001, Tim Nightingale : Return if decomposition fails
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------

subroutine Solve_Cholesky(A, b, x, n, Status)
   implicit none

   integer, intent(IN) :: n
   real, dimension(n,n), intent(IN) :: A
   real, dimension(n), intent(IN) :: b
   real, dimension(n), intent(INOUT) :: x
   integer, intent(OUT) :: Status
   real, dimension(n,n) :: D

   D = A
    
   call Decompose_Cholesky(D, n, Status)
   if (Status /= 0) return
   call Solve_Cholesky_DC(D, b, x, n)
end subroutine Solve_Cholesky
