!-------------------------------------------------------------------------------
! Name:
!    Set_Kx
!
! Purpose:
!    Set up the gradient matrices Kx and Kbj from the forward model return.
!
! Arguments:
!    Name   Type     In/Out/Both Description
!    Ctrl   struct   In          Control structure
!    SPixel struct   In          Super-pixel structure (required for array
!                                sizing)
!    dY_dX  real arr In          Gradients from forward model
!    Kx     real arr Out         Scaled FM gradients (w.r.t state variables)
!    Kbj    real arr Out         Scaled FM gradients w.r.t. model parameters
!                                (only Rs at present).
!    status integer  Out         Error status
!
! Algorithm:
!    Set Kx = dY_dX for corresponding channel / scale factor for state variable
!    If Ctrl flag indiactes user wants to use EqMPN for Rs:
!       Set diagonal terms of Kbj = Rs part of dY_dX for corresponding channel
!
! Local variables:
!    Name Type Description
!
! History:
!    30th Jan 2001, Kevin M. Smith: Original version
!    27th Apr 2001, Andy Smith:
!       Now uses SPixel instead of Ctrl for active state variable indices and
!       channels.
!     6th Jun 2001, Andy Smith:
!       Implicit none statement was wrongly placed.
!    20th Jan 2012, Caroline Poulsen:
!       Bug fix changed dy_dx array changed spixe%nx to maxstatevar dfinition
!     1st Jan 2012, MJ:
!       Changes Kx assignement
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, status)

   use Ctrl_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(in)   :: Ctrl
   type(SPixel_t), intent(in) :: SPixel
   real, intent(in)           :: dY_dX(SPixel%Ind%Ny, Maxstatevar+1)
   real, intent(out)          :: Kx(SPixel%Ind%Ny, SPixel%Nx)
   real, intent(out)          :: Kbj(SPixel%Ind%Ny, SPixel%Ind%NSolar)
   integer, intent(out)       :: status

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
         Kbj(i,i) = dY_dX(i,MaxStateVar+1)
      end do
   end if

end subroutine Set_Kx
