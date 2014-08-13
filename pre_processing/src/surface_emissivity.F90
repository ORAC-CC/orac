!-------------------------------------------------------------------------------
! Name: surface_emissivity.F90
!
! Purpose:
! Container for surface emissivity routines.
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
! None known.
!-------------------------------------------------------------------------------

module surface_emissivity

   implicit none

contains

include 'get_surface_emissivity.F90'
include 'select_modis_emiss_file.F90'

end module surface_emissivity
