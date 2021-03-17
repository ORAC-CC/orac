!-------------------------------------------------------------------------------
! Name: invert_cholesky.F90
!
! Purpose:
! Given a positive definite matrix A of dimension n * n, returns its inverse
! in D. This function makes approximately n^3 / 2 multiplications.
!
! Description and Algorithm details:
! Calls SLATEC or LAPACK.
!
! Arguments:
! Name   Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! A      float array In          Positive definite matrix to be inverted
! D      float array Out         Inverse of positive definite matrix
! n      integer     In          Dimensions of A, D, p; loop limit
! status integer     Out         Status flag: 0 = Success, 1 = Not positive
!                                definite
!
! History:
! 2001/04/27, TN: Original version.
! 2001/05/17, TN: Add Decompose_Cholesky to subroutine as decomposition is only
!    used once.
! 2001/06/05, TN: Return if decomposition fails.
! 2017/10/24, GN: Change to use SLATEC or LAPACK.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Invert_Cholesky(A, D, n, status)

   implicit none

   real, dimension(:,:), intent(in)    :: A
   real, dimension(:,:), intent(inout) :: D
   integer,              intent(in)    :: n
   integer,              intent(out)   :: status

   integer :: i
#ifdef USE_SLATEC
   real    :: det(2)
#endif
   status = 0

   D = A
#ifdef USE_SLATEC
   call spofa(D, n, n, status)
   if (status /= 0) return

   call spodi(D, n, n, det, 01)
#else
   call spotrf("u", n, D, n, status)
   if (status /= 0) return

   call spotri("u", n, D, n, status)
   if (status /= 0) return
#endif
   do i = 2, n
      D(i,1:i-1) = D(1:i-1,i)
   end do
end subroutine Invert_Cholesky
