!-------------------------------------------------------------------------------
! Name: deallocate_preproc_structures.F90
!
! Purpose:
! Deallocate the array parts of the types defined in preproc_structures.F90
!
! Description and Algorithm details:
! 1) Deallocate all fields of structure.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_dims   struct out         preprocessing grid definitions
! preproc_geoloc struct out         lat/lon values
! preproc_geo    struct out         geometry
! preproc_prtm   struct out         profiles and surface fields
! preproc_surf   struct out         surface albedo and emissivity
!
! History:
! 2012/01/19, MJ: produces draft code
! 2012/05/30, GT: Added deallocation of preproc_surf%emissivity
! 2012/06/26, CP: Added allocation swrtm information. added definitions of
!   nchan_sw and nchan_lw. included channel_info structure!
! 2012/07/29, CP: deallocated levels and layers variable
! 2012/07/30, CP: added in solazi
! 2012/08/01, MJ: adds geopotential height coordinates
! 2012/11/14, CP: adds surface pressure
! 2013/11/08, GM: Added missing deallocate statements.
! 2014/01/24, MJ: removed channel_info structure as it is not necessary and was
!                 used in the call.
! 2014/05/01, GM: Add some deallocations that were being done outside.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/07, AP: Removed unnecessary fields from preproc_dims.
! 2014/09/10, AP: Removed unnecessary LWRTM and SWRTM structures.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_preproc_structures(preproc_dims,preproc_geoloc, &
   preproc_geo,preproc_prtm,preproc_surf)

   use preproc_constants

   implicit none

   type(preproc_dims_s),   intent(inout) :: preproc_dims
   type(preproc_geoloc_s), intent(inout) :: preproc_geoloc
   type(preproc_geo_s),    intent(inout) :: preproc_geo
   type(preproc_prtm_s),   intent(inout) :: preproc_prtm
   type(preproc_surf_s),   intent(inout) :: preproc_surf

   ! preproc_dims
   deallocate(preproc_dims%counter_sw)
   deallocate(preproc_dims%counter_lw)

   ! preproc_geoloc
   deallocate(preproc_geoloc%longitude)
   deallocate(preproc_geoloc%latitude)

   ! preproc_geo
   deallocate(preproc_geo%solza)
   deallocate(preproc_geo%solazi)
   deallocate(preproc_geo%satza)
   deallocate(preproc_geo%relazi)

   ! preproc_prtm
   deallocate(preproc_prtm%pressure)
   deallocate(preproc_prtm%temperature)
   deallocate(preproc_prtm%spec_hum)
   deallocate(preproc_prtm%ozone)
   deallocate(preproc_prtm%phi_lev)
   deallocate(preproc_prtm%phi_lay)
   deallocate(preproc_prtm%geopot)
   deallocate(preproc_prtm%lnsp)
   deallocate(preproc_prtm%u10)
   deallocate(preproc_prtm%v10)
   deallocate(preproc_prtm%land_sea_mask)
   deallocate(preproc_prtm%temp2)
   deallocate(preproc_prtm%skin_temp)
   deallocate(preproc_prtm%snow_albedo)
   deallocate(preproc_prtm%snow_depth)
   deallocate(preproc_prtm%sst)
   deallocate(preproc_prtm%sea_ice_cover)
   deallocate(preproc_prtm%totcolwv)

   ! preproc_surf
   deallocate(preproc_surf%emissivity)

end subroutine deallocate_preproc_structures
