!-------------------------------------------------------------------------------
! Name: InvertCholeskyDC.F90
!
! Purpose:
! Given the Cholesky decomposition of a positive definite matrix in the lower
! triangle of A, overwrites the lower triangle with the inverse of the
! decomposition. This subroutine makes approximately n^3 / 6 multiplications.
!
! Description and Algorithm details:
! Substantial rework of code example in "Numerical Recipies in FORTRAN 77: The
! Art of Scientific Computing" (ISBN 0-521-43064-X), page 91. Loop ordering
! changed, inner loop  parallelised, diagonal p incorporated in A.
!
! Arguments:
! Name Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! A    float array Both        Cholesky decomposition in, inverse of
!                              decomposition out (lower triangle)
! n    integer     In          Dimension of A, loop limit
!
! History:
! 2001/04/25, TN: Original version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Invert_Cholesky_DC(A, n)

   implicit none

   real, dimension(:,:), intent(inout) :: A
   integer,              intent(in)    :: n

   integer :: i, j

   do i = 1, n
      do j = 1, i - 1
         A(i,j) = -dot_product(A(i,j:i-1), A(j:i-1,j)) / A(i,i)
      end do
      A(i,i) = 1. / A(i,i)
   end do

end subroutine Invert_Cholesky_DC
