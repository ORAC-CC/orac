!-------------------------------------------------------------------------------
! Name: SolveCholesky.F90
!
! Purpose:
! Given a positive definite matrix A of dimension n * n, and a column vector
! b of dimension n, returns the solution x to A.x = b. This function makes
! approximately n^3 / 6 (+ n^2) multiplications.
!
! Description and Algorithm details:
! This function encapsulates two routines which derive and operate on the
! Cholesky decomposition L of A. First calculates the Cholesky decomposition
! L, then multiplies out the solution, x.
!
! Arguments:
! Name   Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! A      float array In          Positive definite matrix A
! b      float array In          RHS vector to A.x = b
! x      float array Both        Solution vector to A.x = b
! n      integer     In          Dimensions of A; loop limit
! status integer     Out         Status flag: 0 = Success, 1 = Not positive
!                                definite
!
! History:
! 2001/05/18, TN: Original version.
! 2001/06/05, TN: Return if decomposition fails
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Solve_Cholesky(A, b, x, n, Status)

   implicit none

   integer,              intent(in)    :: n
   real, dimension(:,:), intent(in)    :: A
   real, dimension(:),   intent(in)    :: b
   real, dimension(:),   intent(inout) :: x
   integer,              intent(out)   :: Status

   real, dimension(n,n) :: D

   Status = 0

   D = A

   call Decompose_Cholesky(D, n, Status)
   if (Status /= 0) return
   call Solve_Cholesky_DC(D, b, x, n)

end subroutine Solve_Cholesky
