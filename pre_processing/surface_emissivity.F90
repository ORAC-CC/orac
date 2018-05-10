!-------------------------------------------------------------------------------
! Name: surface_emissivity.F90
!
! Purpose:
! Module for surface emissivity routines.
!
! History:
! 2014/05/23, GM: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module surface_emissivity_m

   implicit none

contains

#include "get_surface_emissivity.F90"
#include "select_modis_emiss_file.F90"
#include "select_camel_emiss_file.F90"

end module surface_emissivity_m
