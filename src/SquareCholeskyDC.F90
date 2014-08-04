!-------------------------------------------------------------------------------
! Name:
!    Square_Cholesky_DC
!
! Purpose:
!    Given a matrix A which contains the Cholesky decomposition of a positive
!    definite matrix in the lower triangle, overwrites the upper triangle only
!    with the "square" of the decomposition, equivalent to the original positive
!    definite matrix. The missing values in the lower triangle can be
!    reconstructed, trivially, by copying across the corresponding upper
!    triangle elements, A[i,j] = A[j,i]. This subroutine makes approximately
!    n^3 / 6 multiplications.
!
! Algorithm:
!    Calculates the "square" of the Cholesky decomposition L, S = L^T.L, for
!    elements A[1:i,i] only. As L^-1 is the Cholesky decomposition of S^-1,
!    this applies equally for the inverse as well: S^-1 = (L^-1)^T.L^-1 .
!
! Arguments:
!    Name Type        In/Out/Both Description
!    A    float array Both        Decomposition or inverse decomposition in,
!                                 Positive definite or inverse positive definite
!                                 out (upper triangle only)
!    n    integer     In          Dimensions of A; loop limit
!
! Local variables:
!    Name Type Description
!
! History:
!    27th April, 2001, Tim Nightingale: Original version.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Square_Cholesky_DC(A, n)

   implicit none

   real, dimension(n,n), intent(inout) :: A
   integer,              intent(in) :: n

   integer :: i

   do i = 1, n
      A(1:i,i) = matmul(A(i:n,i), A(i:n,1:i))
   end do

end subroutine Square_Cholesky_DC
