!-------------------------------------------------------------------------------
! Name: planck.F90
!
! Purpose:
! Module for routines converting brightness temperature to radiance and back.
!
! History:
! 2015/01/18, GM: Original version.
!
! $Id: planck.F90 2856 2015-01-12 18:50:33Z acpovey $
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module planck

   implicit none

contains

#include "T2R.F90"
#include "R2T.F90"

end module planck
