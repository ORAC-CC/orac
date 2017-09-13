!-------------------------------------------------------------------------------
! Name: Inversion.F90
!
! Purpose:
! Module of functions used by Invert_Marquardt.
!
! History:
! 2015/01/18, GM: Original version.
!
! $Id$
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Inversion_m

   implicit none

contains

#include "CalcCWP.F90"
#include "CalcCorrectedCTX.F90"
#include "CheckLimits.F90"
#include "InvertMarquardt.F90"
#include "SetKx.F90"
#include "SetLimits.F90"
#include "SetSy.F90"

end module Inversion_m
