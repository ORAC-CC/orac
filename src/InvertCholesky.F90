!-------------------------------------------------------------------------------
! Name: InvertCholesky.F90
!
! Purpose:
! Given a positive definite matrix A of dimension n * n, returns its inverse
! in D. This function makes approximately n^3 / 2 multiplications.
!
! Description and Algorithm details:
! This function encapsulates a series of routines which derive and operate on
! the Cholesky decomposition L of A. First calculates the Cholesky
! decomposition L, then its inverse L^-1, then calculates the full inverse
! S^-1 = (L^-1)^T.L^-1 for the upper triangle only, as the inverse will also
! be symmetric, then copies across the symmetric values from the upper to the
! lower triangle.
!
! Arguments:
! Name   Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! A      float array In          Positive definite matrix to be inverted
! D      float array Out         Inverse of positive definite matrix
! n      integer     In          Dimensions of A, D, p; loop limit
! Status integer     Out         Status flag: 0 = Success, 1 = Not positive
!                                definite
!
! History:
! 2001/04/27, TN: Original version.
! 2001/05/17, TN: Add Decompose_Cholesky to subroutine as
!    decomposition is only used once.
! 2001/06/05, TN: Return if decomposition fails.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Invert_Cholesky(A, D, n, Status)

   implicit none

   real, dimension(n,n), intent(in)  :: A
   real, dimension(n,n), intent(out) :: D
   integer,              intent(in)  :: n
   integer,              intent(out) :: Status

   integer :: i

   Status = 0

   D = A

   call Decompose_Cholesky(D, n, Status)
   if (Status /= 0) return
   call Invert_Cholesky_DC(D, n)
   call Square_Cholesky_DC(D, n)

   do i = 2, n
      D(i,1:i-1) = D(1:i-1,i)
   end do

end subroutine Invert_Cholesky
