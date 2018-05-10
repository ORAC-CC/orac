!-------------------------------------------------------------------------------
! Name: surface_reflectance.F90
!
! Purpose:
! Module for surface reflectance subroutines.
!
! History:
! 2014/05/23, GM: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module surface_reflectance_m

   implicit none

contains

#include "get_surface_reflectance.F90"
#include "select_modis_albedo_file.F90"

end module surface_reflectance_m
