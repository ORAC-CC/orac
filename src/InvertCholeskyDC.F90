! Name:
!   Invert_Cholesky_DC
!
! Purpose:
!   Given the Cholesky decomposition of a positive definite matrix in the lower
!   triangle of A, overwrites the lower triangle with the inverse of the
!   decomposition. This subroutine makes approximately n^3 / 6 multiplications.
!
! Arguments:
!   Name        Type         In/Out   Description
!   A           float array  In/Out   Cholesky decomposition in, inverse of
!                                     decomposition out (lower triangle)
!   n           integer      In       Dimension of A, loop limit
!   
! Local variables:
!   Name        Type                  Description
!   i, j        integer               Loop index
!
! Subprograms:
!   None
!
! History:
!   25th April, 2001, Tim Nightingale : Original version. Substantial rework of
!      code example in "Numerical Recipies in FORTRAN 77: The Art of Scientific
!      Computing" (ISBN 0-521-43064-X), page 91. Loop ordering changed, inner
!      loop parallelised, diagonal p incorporated in A.
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
!
subroutine Invert_Cholesky_DC(A, n)
   implicit none

   integer, intent(IN) :: n
   real, dimension(n,n), intent(INOUT) :: A
   integer :: i, j

   do i = 1, n
      do j = 1, i - 1
         A(i,j) = -dot_product(A(i,j:i-1), A(j:i-1,j)) / A(i,i)
      end do
      A(i,i) = 1. / A(i,i)
   end do
end subroutine Invert_Cholesky_DC
