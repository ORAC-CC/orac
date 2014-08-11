!-------------------------------------------------------------------------------
! Name: surface_reflectance.F90
!
! Purpose:
! Container for surface reflectance subroutines.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

module surface_reflectance

implicit none

contains

include 'get_surface_reflectance.F90'
include 'select_modis_albedo_file.F90'

end module surface_reflectance
