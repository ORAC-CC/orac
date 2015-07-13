!-------------------------------------------------------------------------------
! Name: Inversion.F90
!
! Purpose:
! Module of functions used by Invert_Marquardt.
!
! History:
! 2015/01/18, GM: Original version.
!
! $Id: Inversion.F90 2856 2015-01-12 18:50:33Z acpovey $
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Inversion

   implicit none

contains

#include "InvertMarquardt.F90"
#include "SetLimits.F90"
#include "CheckLimits.F90"
#include "SetKx.F90"
#include "SetSy.F90"
#include "SetUnit.F90"

end module Inversion
