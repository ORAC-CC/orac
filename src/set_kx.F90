!-------------------------------------------------------------------------------
! Name: set_kx.F90
!
! Purpose:
! Set up the gradient matrices Kx and Kj from the forward model return.
!
! Description and Algorithm details:
! Copy dY_dX into Kx for each active variable, appyling the appropriate scale
! factors. Copy dY_dX into Kj for each parameter.
!
! Arguments:
! Name   Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct   In          Control structure
! SPixel struct   In          Super-pixel structure (required for array
!                             sizing)
! dY_dX  real arr In          Gradients from forward model
! Kx     real arr Out         Scaled FM gradients (w.r.t state variables)
! Kj     real arr Out         Scaled FM gradients w.r.t. model parameters
!                             (only Rs at present).
!
! History:
! 2001/01/30, KS: Original version
! 2001/04/27, AS: Now uses SPixel instead of Ctrl for active state variable
!    indices and channels.
! 2001/06/06, AS: Implicit none statement was wrongly placed.
! 2012/01/20, CP: Bug fix: Changed dy_dx array changed spixe%nx to MaxStateVar
!    definition.
! 2012/01/01, MJ: Changes Kx assignment
! 2014/05/21, GM: Cleaned up the code.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
! 2015/08/21, AP: Variables to include in Jacobian (but not retrieve) now listed
!    in SPixel%XJ.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kj)

   use Ctrl_m
   use ORAC_Constants_m
   use SPixel_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   real,           intent(in)  :: dY_dX(:,:)
   real,           intent(out) :: Kx(:,:)
   real,           intent(out) :: Kj(:,:)

   ! Declare local variables

   integer :: i

   ! Set values in Kx for the active state variables.
   do i = 1, SPixel%Nx
      Kx(:,i) = dY_dX(:, SPixel%X(i)) / Ctrl%Invpar%XScale(SPixel%X(i))
   end do

   ! Set Jacobian for requested parameter errors
   if (SPixel%NXJ > 0) then
      do i = 1, SPixel%NXJ
         Kj(:,i) = dY_dX(:, SPixel%XJ(i)) / Ctrl%Invpar%XScale(SPixel%XJ(i))
      end do
   end if

end subroutine Set_Kx
