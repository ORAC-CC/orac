!-------------------------------------------------------------------------------
! Name: set_sy.F90
!
! Purpose:
! Sets up the state dependent part of the error covariance matrix Sy (errors
! in the measurements Y). SPixel%Sy is already populated with instrument
! dependent data and pixel dependent data. This routine adds the state
! dependent terms. Currently only Rs is supported. SPixel%Sy is not
! overwritten as the current state vector might not be kept (if the step
! takes the inversion away from convergence).
!
! Description and Algorithm details:
! If Ctrl flag indicates that Rs errors should be included in Sy:
!  - calculate Syb = Kbj * Sb * transpose(Kbj)
!  - Sy = scene Sy + Syb
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct In          Control structure
! SPixel struct In          Info on the current super-pixel
! Kj     real array  In     Gradients in measurements calculated by the
!                           forward model w.r.t. model parameters.
! Sj     real array  In     Covariance matrix for parameters.
! Sy     real array  Out    The local (to Invert_Marquardt) error covariance
!                           in the measurements. Set to the sum of the scene
!                           Sy (from SPixel) and the requested model
!                           parameter values.
!
! History:
! 2001/05/16, AS: Original version
! 2001/06/06, AS: Implicit none statement was wrongly placed. Ctrl argument
!    added.
! 2001/07/05, AS: Added test for NSolar > 0 before adding Kbj terms.
! 2014/05/21, GM: Cleaned up the code.
! 2015/08/21, AP: Variables to include in Jacobian (but not retrieve) now listed
!    in SPixel%XJ.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Sy(Ctrl, SPixel, Kj, Sj, Sy)

   use Ctrl_m
   use SPixel_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   real,           intent(in)  :: Kj(:,:)
   real,           intent(in)  :: Sj(:,:)
   real,           intent(out) :: Sy(:,:)


   Sy = SPixel%Sy

   ! Include forward model parameter errors, Syb = Kj * Sj * Kj^T.
   if (SPixel%NXJ > 0) &
      Sy = Sy + matmul(Kj, matmul(Sj, transpose(Kj)))

end subroutine Set_Sy
