!-------------------------------------------------------------------------------
! Name: read_abi.F90
!
! Purpose:
! Module for GOES-16 series read routines.
!
! History:
! 2018/02/10, SP: First version.
! 2018/10/26, SP: Rename to prevent clashes with GOES-Imager reader
! 2018/11/19, SP: Remove C version of solar position calculator, replace with F
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_abi_m

   implicit none

contains

#include "read_abi_main.F90"
#include "read_abi_funcs.F90"

end module read_abi_m
