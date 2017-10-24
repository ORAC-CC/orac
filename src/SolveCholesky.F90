!-------------------------------------------------------------------------------
! Name: SolveCholesky.F90
!
! Purpose:
! Given a positive definite matrix A of dimension n * n, and a column vector
! b of dimension n, returns the solution x to A.x = b. This function makes
! approximately n^3 / 6 (+ n^2) multiplications.
!
! Description and Algorithm details:
! Calls LINPACK/SLATEC or LAPACK.
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
! 2017/10/24, GN: Change to use LINPACK/SLATEC or LAPACK.
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
   real, dimension(n)   :: work

   Status = 0

   D = A
#ifdef LINPACK_OR_SLATEC
   call spofa(D, n, n, Status)
   if (Status /= 0) return

   x = b
   call spofs(D, n, n, x, 2, Status, work);
   if (Status .lt. 0) return
#else
   call spotrf("u", n, D, n, Status)
   if (Status /= 0) return

   x = b
   call spotrs("u", n, 1, D, n, x, n, Status);
   if (Status /= 0) return
#endif
end subroutine Solve_Cholesky
