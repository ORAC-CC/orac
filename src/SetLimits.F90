!-------------------------------------------------------------------------------
! Name:
!    Set_Limits
!
! Description:
!    Sets up arrays of upper and lower limits on the state vector for bounds
!    checking in Invert_Marquardt.
!
! Arguments:
!    Name   Type   In/Out/Both Description
!    Ctrl   struct In          ECP control structure (source of limit values
!                              in the Ctrl case).
!    SPixel struct Both        Super-pixel structure (contains the limits arrays
!                              to be set, plus phase and current cloud class)
!    status int    Out         ECP program status value.
!
! Algorithm:
!    Set the SPixel upper and lower limit arrays using limits from Ctrl
!
!    For surface temperature Ts, set the limits using the a priori and errors
!    - the allowed range is the AP +/- 3 * the AP error
!    This keeps the value within 3 standard deviations and prevents Ts from
!    going into a non-linear part of the Planck function curve, which can lead
!    to negative radiance values in FM calculations.
!
!    MDAD and AUX methods are not supported in this implementation.
!
! Local variables:
!    Name Type Description
!
! History:
!    25th Apr 2001, Andy Smith : Original version
!    24th Jul 2001, Andy Smith :
!       Ts limits are now set dynamically using the a priori information.
!    16th Aug 2001, Andy Smith:
!       Bug fix in Ts limit setting: now SUBTRACTS delta_Ts to get lower limit
!       and ADDs to get the upper limit, rather than vice versa.
!    22nd Mar 2011, Andy Smith:
!       Removal of phase change, phase 2. SAD_CloudClass now has 1 dimension
!       rather than N cloud classes.
!     5th Apr 2011, Andy Smith:
!       Removed selection methods SAD and SDAD.
!       SAD_CloudClass argument no longer required.
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Set_Limits(Ctrl, SPixel, status)

   use Ctrl_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   integer,        intent(inout) :: status

   ! Local variable declarations

   integer :: i
   real    :: delta_Ts ! The maximum deviation from the a priori value allowed
                       ! for Ts.

   ! Set the upper and lower limits for the current super-pixel.
   ! SPixel U/LLim arrays are full length, i.e. big enough to hold the full ECP
   ! state vector (saves repeated allocation and deallocation).

   SPixel%XLLim = Ctrl%Invpar%XLLim
   SPixel%XULim = Ctrl%Invpar%XULim

   ! Set surface temperature values separately. Firstly, determine whether Ts is
   ! an active state variable. If so, set the range to 3 standard deviations
   ! away from the a priori Ts value.

   do i = 1, SPixel%Nx
      if (SPixel%X(i) == ITs) then
         delta_Ts          = 3 * sqrt(SPixel%Sx(ITs, ITs))
	 SPixel%XLLim(ITs) = SPixel%Xb(ITs) - delta_Ts
	 SPixel%XULim(ITs) = SPixel%Xb(ITs) + delta_Ts
      end if
   end do

end subroutine Set_Limits
