!-------------------------------------------------------------------------------
! Name: inversion.F90
!
! Purpose:
! Module of functions used by Invert_Marquardt.
!
! History:
! 2015/01/18, GM: Original version.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Inversion_m

   implicit none

contains

!-------------------------------------------------------------------------------
! Name: convert_state_element_to_linear
!
! Purpose:
! Performs mathematical manipulations to change an element of the state vector
! from log into linear space.
!
! Algorithm:
! For the prior, first guess, and limits: take 10 to power of the value
! For the prior uncertainty: sigma_log = sigma_lin * value * log(10)
!    Thus, multiply the requested row and column of Sx by (value * log(10)).
!    This is an approximation, using the largest of two unequal bounds.
!
! Arguments:
! Name   Type     In/Out/Both Description
! SPixel SPixel_t Both        The pixel to be manipulated
! index  int      In          Index of the state vector to be converted
!
! History:
! 2024/01/14, AP: Original version.
!
! Bugs:
! The conversion of covariance from log to linear space is *not* Gaussian.
! Optimal estimation requires Gaussian errors, so this is strictly an
! inappropriate operation. However, it is much more intuitive for the user.
!-------------------------------------------------------------------------------
subroutine convert_state_element_to_linear(SPixel, index)

   use SPixel_m, only: SPixel_t

   implicit none

   type(SPixel_t), intent(inout) :: SPixel
   integer,        intent(in)    :: index

   real :: sx_correction

   SPixel%XB(index) = 10.0**SPixel%XB(index)
   SPixel%X0(index) = 10.0**SPixel%X0(index)
   SPixel%XLLim(index) = 10.0**SPixel%XLLim(index)
   SPixel%XULim(index) = 10.0**SPixel%XULim(index)

   sx_correction = SPixel%XB(index) * alog(10.0)
   SPixel%Sx(index,:) = SPixel%Sx(index,:) * sx_correction
   SPixel%Sx(:,index) = SPixel%Sx(:,index) * sx_correction

end subroutine convert_state_element_to_linear


#include "calc_cwp.F90"
#include "calc_corrected_ctx.F90"
#include "check_limits.F90"
#include "invert_marquardt.F90"
#include "set_kx.F90"
#include "set_sy.F90"

end module Inversion_m
