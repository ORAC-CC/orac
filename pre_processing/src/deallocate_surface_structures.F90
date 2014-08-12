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
! 2014/08/10, GM: Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_surface_structures(surface,include_full_brdf)

   use preproc_constants

   implicit none

   type(surface_s), intent(inout) :: surface
   logical,         intent(in)    :: include_full_brdf

   deallocate(surface%albedo_chan)
   deallocate(surface%emissivity_chan)

   deallocate(surface%albedo)
   deallocate(surface%emissivity)

   if (include_full_brdf) then
      deallocate(surface%rho_0v)
      deallocate(surface%rho_0d)
      deallocate(surface%rho_dv)
      deallocate(surface%rho_dd)
   end if

end subroutine deallocate_surface_structures
