!-------------------------------------------------------------------------------
! Name: SolveCholeskyDC.F90
!
! Purpose:
! Solves the set of n linear equations M.x = b , where M is a n * n positive
! definite symmetric matrix, from its decomposition in the lower triangle of
! A. b is the input right-hand-side vector, of length n. The solution vector,
! also of length n, is returned in x. A is not modified and can be left in
! place for successive calls with different right-hand sides b. b is not
! modified unless b and x are identified in the calling sequence, which is
! allowed. This subroutine makes approximately n^2 multiplications.
!
! Description and Algorithm details:
! See "Numerical Recipies in FORTRAN 77: The Art of Scientific Computing"
! (ISBN 0-521-43064-X), pages 89-91.
!
! Arguments:
! Name Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! A    float array In          Cholesky decomposition (lower triangle)
! b    float array In          RHS vector to M.x = b
! x    float array Both        Solution vector to M.x = b
! n    integer     In          Dimensions of A; loop limit
!
! History:
! 2001/04/20, TN: Original version. Adapted from "cholsl"
!    in "Numerical Recipes in Fortran 90: The Art of Parallel Scientific
!    Computing" (ISBN 0-521-57439-0), page 1039. Error handling and types
!    changed, diagonal p incorporated in A.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Solve_Cholesky_DC(A, b, x, n)

   implicit none

   real, dimension(n,n), intent(in) :: A
   real, dimension(n),   intent(in) :: b
   real, dimension(n),   intent(inout) :: x
   integer,              intent(in) :: n

   integer :: i

   ! Solve L.y = b, storing y in x
   do i = 1, n
      x(i) = (b(i) - dot_product(A(i,1:i-1), x(1:i-1))) / A(i,i)
   end do

   ! Solve L^T·x = y
   do i = n, 1, -1
      x(i) = (x(i) - dot_product(A(i+1:n,i), x(i+1:n))) / A(i,i)
   end do

end subroutine Solve_Cholesky_DC
