!-------------------------------------------------------------------------------
! Name: planck.F90
!
! Purpose:
! Module for routines converting brightness temperature to radiance and back.
!
! History:
! 2015/01/18, GM: Original version.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module planck_m

   implicit none

contains

#include "T2R.F90"
#include "R2T.F90"

end module planck_m
