!-------------------------------------------------------------------------------
! Name: check_limits.F90
!
! Purpose:
! Checks upper and lower limits on all active state variables. Each active
! variable is checked to ensure that it's current value has not gone outside the
! bounds for the phase (stored in the SPixel Ulim and LLim arrays)..
!
! Description and Algorithm details:
! For each state variable, check bounds:
!    if (variable > Max) variable = Max
!    if (variable < Min) variable = Min
!
! Arguments:
! Name         Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! X            real array Both        Contains the currently active state
!                                     variables
! SPixel       struct     Both        Super-pixel structure (contains the
!                                     current active state variable info,
!                                     plus current phase)
! status       integer    Out         ORAC program status value.
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
! 2015/07/16, AP: Removed redundant arguments.
! 2017/02/08, GM: Added Pc and Pc2 to close or completely crossing check and
!    fix.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Check_Limits(Ctrl, X, SPixel, status)

   use Ctrl_m
   use ORAC_Constants_m
   use RTM_Pc_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   real,           intent(inout) :: X(:)
   type(SPixel_t), intent(in)    :: SPixel
   integer,        intent(out)   :: status

   ! Local variable declarations

   integer :: i, j
   real    :: P_mid

   status = 0

   do i = 1, SPixel%Nx
      if (X(SPixel%X(i)) > SPixel%XULim(SPixel%X(i))) then
          X(SPixel%X(i)) = SPixel%XULim(SPixel%X(i))
          status = LimitHitUpper
       else if (X(SPixel%X(i)) < SPixel%XLLim(SPixel%X(i))) then
          X(SPixel%X(i)) = SPixel%XLLim(SPixel%X(i))
          status = LimitHitLower
       end if
   end do

   ! If two cloud layers are present then check that if they are to close or
   ! crossing (upper layer lower in the atmosphere than the lower).  If either
   ! of these happen they are set a minimum distance (Ctrl%Invpar%Pc_dmz) away
   ! from each other centered around their current center.  Note: that this
   ! check is independent of the retrieval approach.
   L1: do i = 1, SPixel%Nx
      if (SPixel%X(i) == IPc) then
         L2: do j = 1, SPixel%Nx
            if (SPixel%X(j) == IPc2) then
               if (X(IPc2) - X(IPc) .lt. Ctrl%Invpar%Pc_dmz) then
                  P_mid = (X(IPc) + X(IPc2)) / 2.
                  X(IPc)  = P_mid - Ctrl%Invpar%Pc_dmz / 2.
                  X(IPc2) = P_mid + Ctrl%Invpar%Pc_dmz / 2.
                  status = LimitHitClose
               end if
               exit L1
            end if
         end do L2
      end if
   end do L1

end subroutine Check_Limits
