!-------------------------------------------------------------------------------
! Name: SetKx.F90
!
! Purpose:
! Set up the gradient matrices Kx and Kbj from the forward model return.
!
! Description and Algorithm details:
! Set Kx = dY_dX for corresponding channel / scale factor for state variable
! If Ctrl flag indicates user wants to use EqMPN for Rs:
!    Set diagonal terms of Kbj = Rs part of dY_dX for corresponding channel
!
! Arguments:
! Name   Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct   In          Control structure
! SPixel struct   In          Super-pixel structure (required for array
!                             sizing)
! dY_dX  real arr In          Gradients from forward model
! Kx     real arr Out         Scaled FM gradients (w.r.t state variables)
! Kbj    real arr Out         Scaled FM gradients w.r.t. model parameters
!                             (only Rs at present).
! status integer  Out         Error status
!
! History:
! 2001/01/30, KS: Original version
! 2001/04/27, AS: Now uses SPixel instead of Ctrl for active state variable
!    indices and channels.
! 2001/06/06, AS: Implicit none statement was wrongly placed.
! 2012/01/20, CP: Bug fix changed dy_dx array changed spixe%nx to maxstatevar
!    definition
! 2012/01/01, MJ: Changes Kx assignment
! 2014/05/21, GM: Cleaned up the code.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, status)

   use Ctrl_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   real,           intent(in)  :: dY_dX(:,:)
   real,           intent(out) :: Kx(:,:)
   real,           intent(out) :: Kbj(:,:)
   integer,        intent(out) :: status

   ! Declare local variables

   integer :: i

   status = 0

   ! Set values in Kx for the active state variables.
   ! SPixel%X acts as a "vector subscript", picking out the active state
   ! variable parts of dY_dX and XScale. A loop is required because dY_dX
   ! and XScale are not conformable.

   do i = 1, SPixel%Ind%Ny
!     Kx(i, :) = dY_dX(i, SPixel%X) / Ctrl%Invpar%XScale(SPixel%X)
      Kx(i,1:SPixel%Nx) = dY_dX(i, SPixel%X) / Ctrl%Invpar%XScale(SPixel%X)
   end do

   ! If Eqmpn%Rs flag is set in Ctrl, set Kbj using the Rs part of dY_dX.
   ! Kbj should be initialised to 0 in the calling routine.

   if (Ctrl%Eqmpn%Rs == 1) then
      do i = 1, SPixel%Ind%NSolar
         Kbj(SPixel%Ind%YSolar(i),i) = dY_dX(SPixel%Ind%YSolar(i),IRs(1,1))
      end do
   end if

end subroutine Set_Kx
