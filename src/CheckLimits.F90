!-------------------------------------------------------------------------------
! Name: CheckLimits.F90
!
! Purpose:
! Checks upper and lower limits on all active state variables and sets the
! phase change flag. Each active variable is checked to ensure that it's
! current value has not gone outside the bounds for the phase (stored in the
! SPixel Ulim and LLim arrays). Re (if active) and possibly Tc are checked
! for indications of a phase change.
!
! Description and Algorithm details:
! For each state variable, check bounds:
!    if (variable > Max) variable = Max
!    if (variable < Min) variable = Min
!
! Arguments:
! Name         Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl         struct     In          ECP control structure read from driver file
! X            real array Both        Contains the currently active state
!                                     variables
! SPixel       struct     Both        Super-pixel structure (contains the
!                                     current active state variable info,
!                                     plus current phase)
! RTM_Pc       struct     Out         RTM_Pc structure
! phase_change logical    Both        Result of the phase change test
! status       integer    Out         ECP program status value.
!
! History:
! 2001/04/26, AS: Original version
! 2001/06/29, AS: Added "quick exit" on detection of phase change. If phase 
!    change occurs new limits will be set by Invert_Marquardt and the state 
!    vector will be checked again. Hence there's no point checking the remaining 
!    state variables with the current phase limits.
!    **************** ECV work starts here *************************************
! 2011/03/21, AS: Removed phase change functionality. Each state variable will 
!    now simply be bound by the defined upper and lower limits. Phase_change 
!    argument kept for now to avoid updating function interface.
! 2014/05/21, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Check_Limits(Ctrl, X, SPixel, RTM_Pc, phase_change, status)

   use Ctrl_def
   use ECP_Constants
   use RTM_Pc_def
   use SPixel_def

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   real,           intent(inout) :: X(MaxStateVar)
   type(SPixel_t), intent(inout) :: SPixel
   type(RTM_Pc_t), intent(in)    :: RTM_Pc
   logical,        intent(inout) :: phase_change
   integer,        intent(out)   :: status

   ! Local variable declarations

   integer :: i

   status = 0

   phase_change = .false.

   do i=1,SPixel%Nx
      if (X(SPixel%X(i)) > SPixel%XULim(SPixel%X(i))) &
          X(SPixel%X(i)) = SPixel%XULim(SPixel%X(i))

      if (X(SPixel%X(i)) < SPixel%XLLim(SPixel%X(i))) &
          X(SPixel%X(i)) = SPixel%XLLim(SPixel%X(i))
   end do

end subroutine Check_Limits
