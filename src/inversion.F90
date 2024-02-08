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

#include "calc_cwp.F90"
#include "calc_corrected_ctx.F90"
#include "check_limits.F90"
#include "invert_marquardt.F90"
#include "set_kx.F90"
#include "set_sy.F90"

end module Inversion_m
