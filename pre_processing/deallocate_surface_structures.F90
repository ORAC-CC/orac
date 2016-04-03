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
! 2014/10/23, OS: added deallocation of nise_mask
! 2014/12/02, GM: Remove unused surface%albedo_chan and surface%emissivity_chan.
! 2016/03/31, GM: Changes to support processing only SW or only LW channels.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_surface_structures(surface,channel_info,include_full_brdf)

   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(surface_t),      intent(inout) :: surface
   type(channel_info_t), intent(in)    :: channel_info
   logical,              intent(in)    :: include_full_brdf

   deallocate(surface%nise_mask)

   if (channel_info%nchannels_sw .ne. 0) then
      deallocate(surface%albedo)

      if (include_full_brdf) then
         deallocate(surface%rho_0v)
         deallocate(surface%rho_0d)
         deallocate(surface%rho_dv)
         deallocate(surface%rho_dd)
      end if
   end if

   if (channel_info%nchannels_lw .ne. 0) then
      deallocate(surface%emissivity)
   end if

end subroutine deallocate_surface_structures
