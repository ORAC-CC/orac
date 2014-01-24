! Name: deallocate_preproc_structures.f90
!
!
! Purpose:
! Allocate the array parts of the types defined in preproc_structures.f90
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/01/19: Matthias Jerg produces draft code 
! 2012/05/30: Gareth Thomas: Added deallocation of preproc_surf%emissivity
! 2012/06/26: Caroline Pouslen. Added allocation swrtm information
!                               added definitions of nchan_sw and nchan_lw
!                               included channel_info structure!
! 2012/07/29: Caroline Poulsen. deallocated levels and layers variable
! 2012/07/30: C. Poulsen added in solazi
! 2012/08/01: MJ adds geopotential height coordinates
! 2012/11/14: CP adds surface pressure
! 2013/11/08: GM Added missing deallocate statements.
!2014/01/24 MJ removec channel_info structure as it is not necessary and was used in the call.
!
! $Id$
!
! Bugs:
!
!none known

subroutine deallocate_preproc_structures(preproc_geoloc,preproc_geo,preproc_dims, &
           preproc_prtm, preproc_lwrtm, preproc_swrtm, preproc_surf)

  use preproc_constants
  use channel_structures
  use preproc_structures

  implicit none

  type(preproc_geoloc_s) :: preproc_geoloc

  type(preproc_geo_s) :: preproc_geo
  type(preproc_dims_s)   :: preproc_dims
  type(preproc_prtm_s) :: preproc_prtm
  type(preproc_lwrtm_s) :: preproc_lwrtm
  type(preproc_swrtm_s) :: preproc_swrtm
  type(preproc_surf_s) :: preproc_surf
  type(channel_info_s) :: channel_info

  deallocate(preproc_dims%counter_sw)
  deallocate(preproc_dims%counter_lw)

  deallocate(preproc_dims%channels)

  deallocate(preproc_geoloc%longitude)
  deallocate(preproc_geoloc%latitude)

  deallocate(preproc_geo%satza)
  deallocate(preproc_geo%solza)
  deallocate(preproc_geo%solazi)
  deallocate(preproc_geo%relazi)

  deallocate(preproc_prtm%temperature)
  deallocate(preproc_prtm%pressure)
  deallocate(preproc_prtm%spec_hum)
  deallocate(preproc_prtm%ozone)
  deallocate(preproc_prtm%phi_lev)
  deallocate(preproc_prtm%phi_lay)
  deallocate(preproc_prtm%geopot)
  deallocate(preproc_prtm%lnsp)
  deallocate(preproc_prtm%sea_ice_cover)
  deallocate(preproc_prtm%snow_albedo)
  deallocate(preproc_prtm%snow_depth)
  deallocate(preproc_prtm%sst)
  deallocate(preproc_prtm%totcolwv)
  deallocate(preproc_prtm%u10)
  deallocate(preproc_prtm%v10)
  deallocate(preproc_prtm%temp2)
  deallocate(preproc_prtm%land_sea_mask)
  deallocate(preproc_prtm%skin_temp)
  deallocate(preproc_prtm%surface_pressure)

  deallocate(preproc_dims%filter_array_lw)
  deallocate(preproc_dims%filter_array_sw)

  deallocate(preproc_lwrtm%radiance_cloudy)
  deallocate(preproc_lwrtm%transmission_tau_total)
  deallocate(preproc_lwrtm%trans_layer)
  deallocate(preproc_lwrtm%emissivity_used)
  deallocate(preproc_lwrtm%transmission_tau_levels)
  deallocate(preproc_lwrtm%taubc)
  deallocate(preproc_lwrtm%tauac)
  deallocate(preproc_lwrtm%radbc)
  deallocate(preproc_lwrtm%radiance_bt)
  deallocate(preproc_lwrtm%radiance_up)
  deallocate(preproc_lwrtm%radiance_down)
  deallocate(preproc_lwrtm%players)
  deallocate(preproc_lwrtm%plevels)

  deallocate(preproc_swrtm%transmission_tau_total)
  deallocate(preproc_swrtm%trans_layer)
  deallocate(preproc_swrtm%taubc)
  deallocate(preproc_swrtm%tauac)

  deallocate(preproc_surf%emissivity)

end subroutine deallocate_preproc_structures
