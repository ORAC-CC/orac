! Name:
!   Decompose_Cholesky
!
! Purpose:
!   Given an n * n positive-definite symmetric matrix A, constructs its Cholesky
!   decomposition L, such that A = L.L^T . The decomposition is calculated only
!   from the upper triangle of A and is returned in the lower triangle of A.
!   This subroutine makes approximately n^3 / 6 multiplications.
!
! Algorithm:
!   See "Numerical Recipies in FORTRAN 77: The Art of Scientific Computing"
!   (ISBN 0-521-43064-X), pages 89-91.
!
! Arguments:
!   Name        Type	     In/Out   Description
!   A           float array  In/Out   Positive definite in (only upper triangle plus
!                                     diagonal used), Cholesky decomposition out
!                                     (lower triangle overwritten)
!   n           integer      In       Dimensions of A; loop limit
!   Status      integer      Out      Status flag: 0 = Success, 
!                                      InvCholNotPosDef = not positive definite
!                          
! Local variables:
!   Name            Type          Description
!   i               int           Loop index
!
! Subprograms:
!   None
!
! History:
!   20th April, 2001, Tim Nightingale : Original version. Adapted from "choldc" in
!      "Numerical Recipes in Fortran 90: The Art of Parallel Scientific Computing"
!      (ISBN 0-521-57439-0), pages 1038-39. Error handling and types changed,
!      diagonal p incorporated in A.
!    6th July 2001, Andy Smith:
!      Changed error code to "ECP style": uses a named constant from
!      ECP_Constants module.
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
!
subroutine Decompose_Cholesky(A, n, Status)

   use ECP_Constants

   implicit none

   integer, intent(IN) :: n
   real, dimension(n,n), intent(INOUT) :: A
   integer, intent(OUT) :: Status
   integer :: i

   Status = 0

   do i = 1, n
      A(i,i) = A(i,i) - dot_product(A(i,1:i-1), A(i,1:i-1))
      if (A(i,i) <= 0.0) then
         Status = InvCholNotPosDef
         return
      end if
      A(i,i) = sqrt(A(i,i))
      A(i+1:n,i) = (A(i,i+1:n) - matmul(A(i+1:n,1:i-1), A(i,1:i-1))) / A(i,i)
   end do
end subroutine Decompose_Cholesky
