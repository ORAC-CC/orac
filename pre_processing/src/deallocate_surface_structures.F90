!-------------------------------------------------------------------------------
! Name: deallocate_surface_structures.F90
!
! Purpose:
! Deallocate the array parts of the types defined in surface_structures.F90
!
! Description and Algorithm details:
! 1) Deallocate all fields of structure.
!
! Arguments:
! Name    Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! surface struct both Summary of surface properties
!
! History:
! 2012/05/01, GT: First version
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

subroutine deallocate_surface_structures(surface)

   use preproc_constants

   implicit none

   type(surface_s), intent(inout) :: surface

   deallocate(surface%albedo_chan)
   deallocate(surface%emissivity_chan)
   deallocate(surface%albedo)
   deallocate(surface%emissivity)

end subroutine deallocate_surface_structures
