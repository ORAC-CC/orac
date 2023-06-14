!-------------------------------------------------------------------------------
! Name: set_limits.F90
!
! Purpose:
! Sets up arrays of upper and lower limits on the state vector for bounds
! checking in Invert_Marquardt.
!
! Description and Algorithm details:
! For surface temperature Ts, set the limits using the a priori and errors
! - the allowed range is the AP +/- 3 * the AP error
! This keeps the value within 3 standard deviations and prevents Ts from
! going into a non-linear part of the Planck function curve, which can lead
! to negative radiance values in FM calculations.
!
! MDAD and AUX methods are not supported in this implementation.
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct In          Control structure (source of limit values in the
!                           Ctrl case).
! SPixel struct Both        Super-pixel structure (contains the limits arrays
!                           to be set, plus phase and current cloud class)
! status int    Out         ORAC program status value.
!
! History:
! 2001/04/25, AS: Original version
! 2001/06/24, AS: Ts limits are now set dynamically using the a priori
!    information.
! 2001/08/16, AS: Bug fix in Ts limit setting: now SUBTRACTS delta_Ts to get
!    lower limit and ADDs to get the upper limit, rather than vice versa.
! 2011/03/22, AS: Removal of phase change, phase 2. SAD_CloudClass now has 1
!    dimension rather than N cloud classes.
! 2011/04/05, AS: Removed selection methods SAD and SDAD. SAD_CloudClass
!    argument no longer required.
! 2014/05/21, GM: Cleaned up the code.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Limits(Ctrl, SPixel, SAD_LUT, status)

   use Ctrl_m
   use ORAC_Constants_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(SAD_LUT_t),intent(in)    :: SAD_LUT(:)
   integer,        intent(out)   :: status

   ! Local variable declarations

   integer :: i
   real    :: delta_Ts ! The maximum deviation from the a priori value allowed
                       ! for Ts.

   status = 0

   ! Set the upper and lower limits for the current super-pixel.
   ! SPixel U/LLim arrays are full length, i.e. big enough to hold the full
   ! ORAC state vector (saves repeated allocation and deallocation).

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

   ! Dynamically set upper limit of cloud top pressure to lowest profile
   ! pressure of current pixel.
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) &
        SPixel%XULim(IPc) = SPixel%RTM%P(SPixel%RTM%Np)

   ! Check for logarithmic LUT axes
   if (SAD_LUT(1)%Grid%Tau%log) then
      SPixel%XLLim(ITau) = log10(SPixel%XLLim(ITau))
      SPixel%XULim(ITau) = log10(SPixel%XULim(ITau))
   end if
   if (Ctrl%Approach == AppCld2L .and. SAD_LUT(2)%Grid%Tau%log) then
      SPixel%XLLim(ITau2) = log10(SPixel%XLLim(ITau2))
      SPixel%XULim(ITau2) = log10(SPixel%XULim(ITau2))
   end if
   if (SAD_LUT(1)%Grid%Re%log) then
      SPixel%XLLim(IRe) = log10(SPixel%XLLim(IRe))
      SPixel%XULim(IRe) = log10(SPixel%XULim(IRe))
   end if
   if (Ctrl%Approach == AppCld2L .and. SAD_LUT(2)%Grid%Re%log) then
      SPixel%XLLim(IRe2) = log10(SPixel%XLLim(IRe2))
      SPixel%XULim(IRe2) = log10(SPixel%XULim(IRe2))
   end if

   ! Ensure retrieval does not go outside the LUT grid
   if (SPixel%XLLim(ITau) < SAD_LUT(1)%Grid%Tau%Min) &
        SPixel%XLLim(ITau) = SAD_LUT(1)%Grid%Tau%Min
   if (Ctrl%Approach == AppCld2L .and. SPixel%XLLim(ITau2) < SAD_LUT(2)%Grid%Tau%Min) &
        SPixel%XLLim(ITau2) = SAD_LUT(2)%Grid%Tau%Min
   if (SPixel%XULim(ITau) > SAD_LUT(1)%Grid%Tau%Max) &
        SPixel%XULim(ITau) = SAD_LUT(1)%Grid%Tau%Max
   if (Ctrl%Approach == AppCld2L .and. SPixel%XULim(ITau2) > SAD_LUT(2)%Grid%Tau%Max) &
        SPixel%XULim(ITau2) = SAD_LUT(2)%Grid%Tau%Max
   if (SPixel%XLLim(IRe) < SAD_LUT(1)%Grid%Re%Min) &
        SPixel%XLLim(IRe) = SAD_LUT(1)%Grid%Re%Min
   if (Ctrl%Approach == AppCld2L .and. SPixel%XLLim(IRe2) < SAD_LUT(2)%Grid%Re%Min) &
        SPixel%XLLim(IRe2) = SAD_LUT(2)%Grid%Re%Min
   if (SPixel%XULim(IRe) > SAD_LUT(1)%Grid%Re%Max) &
        SPixel%XULim(IRe) = SAD_LUT(1)%Grid%Re%Max
   if (Ctrl%Approach == AppCld2L .and. SPixel%XULim(IRe2) > SAD_LUT(2)%Grid%Re%Max) &
        SPixel%XULim(IRe2) = SAD_LUT(2)%Grid%Re%Max

end subroutine Set_Limits
