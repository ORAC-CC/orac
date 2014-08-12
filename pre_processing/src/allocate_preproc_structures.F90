!-------------------------------------------------------------------------------
! Name: allocate_preproc_structures.F90
!
! Purpose:
! Allocate the array parts of the types defined in preproc_structures.F90
!
! Description and Algorithm details:
! 1) Run many ALLOCATE statements.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_angles  struct in          sun/satellite viewing angles
! preproc_dims   struct out         preprocessing grid definitions
! preproc_geoloc struct out         lat/lon values
! preproc_geo    struct out         geometry
! preproc_prtm   struct out         profiles and surface fields
! preproc_lwrtm  struct out         longwave RTM data
! preproc_swrtm  struct out         shortwave RTM data
! preproc_surf   struct out         surface albedo and emissivity
! channel_info   struct in          channel information
!
! History:
! 2012/01/13, MJ: produces draft code for allocating the preprocessing types
! 2012/05/30, GT: Added allocation of preproc_surf%emissivity
! 2012/06/26, CP: Added allocation swrtm information
!                 added definitions of nchan_sw and nchan_lw
!                 included channel_info structure
! 2012/06/27, CP: Fixed small bug in allocation of swrtm structure
! 2012/07/04, CP: removed nviews from rtm array
! 2012/07/17, CP: adds in players variable
! 2012/07/30, CP: added in solazi
! 2012/07/30, MJ: added in phi_lay and phi_lev variable
! 2012/08/08, CP: changed emissivity array to be consistent with msi order
! 2012/08/28, CP: remove _lw _sw for filter_array and counter
! 2012/08/28, CP: readded _lw _sw for filter_array and counter
! 2012/11/14, CP: added surface pressure
! 2012/11/14, CP: changedkdim_pre to pressure levels
! 2013/10/16, CP: changed definition of preproc_lwrtm%players and
!                 preproc_lwrtm%plevels
! 2013/10/23, AP: Tidying. Removed unused arguments. Removed print statements.
! 2013/10/16, CP: readded def of kdim_pre changed definition of
!                 preproc_lwrtm%phi_lay and preproc_lwrtm%phi_lev
! 2013/10/16, CP: removed kdim_pre and removed ecmwf_dims form subroutine call,
!                 hanged array definition of players and plev
! 2013/11/08, GM: Removed double allocations (allocated twice in a row) of
!                 preproc_prtm%temperature through preproc_prtm%ozone.
! 2014/02/10, AP: Simplifying variable names
! 2014/05/01, GM: Add some allocations that were being done outside.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/28, MJ: Fixed bug with intent statement for preproc_dims
! 2014/07/01, AP: Redefined limits to minimise array sizes
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_preproc_structures(imager_angles,preproc_dims, &
   preproc_geoloc,preproc_geo,preproc_prtm,preproc_lwrtm,preproc_swrtm, &
   preproc_surf,channel_info)

   use channel_structures
   use imager_structures
   use preproc_constants

   implicit none

   type(imager_angles_s),  intent(in)    :: imager_angles
   type(preproc_dims_s),   intent(inout) :: preproc_dims
   type(preproc_geoloc_s), intent(out)   :: preproc_geoloc
   type(preproc_geo_s),    intent(out)   :: preproc_geo
   type(preproc_prtm_s),   intent(out)   :: preproc_prtm
   type(preproc_lwrtm_s),  intent(out)   :: preproc_lwrtm
   type(preproc_swrtm_s),  intent(out)   :: preproc_swrtm
   type(preproc_surf_s),   intent(out)   :: preproc_surf
   type(channel_info_s),   intent(inout) :: channel_info

   integer                               :: nchan_sw, nchan_lw, sx, ex, sy, ey

   nchan_sw=channel_info%nchannels_sw
   nchan_lw=channel_info%nchannels_lw
   sx=preproc_dims%min_lon
   ex=preproc_dims%max_lon
   sy=preproc_dims%min_lat
   ey=preproc_dims%max_lat

   ! preproc_dims
   write(*,*) 'preproc_dims',preproc_dims%xdim,preproc_dims%ydim
   allocate(preproc_dims%counter_sw(sx:ex,sy:ey))
   preproc_dims%counter_sw=0
   allocate(preproc_dims%counter_lw(sx:ex,sy:ey))
   preproc_dims%counter_lw=0


   ! preproc_geoloc
   allocate(preproc_geoloc%longitude(sx:ex))
   preproc_geoloc%longitude=real_fill_value
   allocate(preproc_geoloc%latitude(sy:ey))
   preproc_geoloc%latitude=real_fill_value


   ! preproc_geo (init to 0 as used for summation in build_preproc_fields)
   allocate(preproc_geo%solza(sx:ex,sy:ey,imager_angles%nviews))
   preproc_geo%solza=0.0
   allocate(preproc_geo%solazi(sx:ex,sy:ey,imager_angles%nviews))
   preproc_geo%solazi=0.0
   allocate(preproc_geo%satza(sx:ex,sy:ey,imager_angles%nviews))
   preproc_geo%satza=0.0
   allocate(preproc_geo%relazi(sx:ex,sy:ey,imager_angles%nviews))
   preproc_geo%relazi=0.0


   ! preproc_prtm
   allocate(preproc_prtm%pressure(sx:ex,sy:ey,preproc_dims%kdim))
   preproc_prtm%pressure=real_fill_value
   allocate(preproc_prtm%temperature(sx:ex,sy:ey,preproc_dims%kdim))
   preproc_prtm%temperature=real_fill_value
   allocate(preproc_prtm%spec_hum(sx:ex,sy:ey,preproc_dims%kdim))
   preproc_prtm%spec_hum=real_fill_value
   allocate(preproc_prtm%ozone(sx:ex,sy:ey,preproc_dims%kdim))
   preproc_prtm%ozone=real_fill_value

   allocate(preproc_prtm%phi_lay(sx:ex,sy:ey,preproc_dims%kdim-1))
   preproc_prtm%phi_lay=real_fill_value
   allocate(preproc_prtm%phi_lev(sx:ex,sy:ey,preproc_dims%kdim))
   preproc_prtm%phi_lev=real_fill_value

   allocate(preproc_prtm%geopot(sx:ex,sy:ey))
   preproc_prtm%geopot=real_fill_value
   allocate(preproc_prtm%lnsp(sx:ex,sy:ey))
   preproc_prtm%lnsp=real_fill_value

   allocate(preproc_prtm%u10(sx:ex,sy:ey))
   preproc_prtm%u10=real_fill_value
   allocate(preproc_prtm%v10(sx:ex,sy:ey))
   preproc_prtm%v10=real_fill_value

   allocate(preproc_prtm%land_sea_mask(sx:ex,sy:ey))
   preproc_prtm%land_sea_mask=real_fill_value

   allocate(preproc_prtm%temp2(sx:ex,sy:ey))
   preproc_prtm%temp2=real_fill_value
   allocate(preproc_prtm%skin_temp(sx:ex,sy:ey))
   preproc_prtm%skin_temp=real_fill_value

   allocate(preproc_prtm%snow_albedo(sx:ex,sy:ey))
   preproc_prtm%snow_albedo=real_fill_value
   allocate(preproc_prtm%snow_depth(sx:ex,sy:ey))
   preproc_prtm%snow_depth=real_fill_value

   allocate(preproc_prtm%sst(sx:ex,sy:ey))
   preproc_prtm%sst=real_fill_value
   allocate(preproc_prtm%sea_ice_cover(sx:ex,sy:ey))
   preproc_prtm%sea_ice_cover=real_fill_value

   allocate(preproc_prtm%totcolwv(sx:ex,sy:ey))
   preproc_prtm%totcolwv=real_fill_value


   ! preproc_lwrtm
   allocate(preproc_lwrtm%radiance_cloudy(nchan_lw))
   preproc_lwrtm%radiance_cloudy=real_fill_value

   allocate(preproc_lwrtm%transmission_tau_levels(nchan_lw, &
        preproc_dims%kdim))
   preproc_lwrtm%transmission_tau_levels=real_fill_value

   allocate(preproc_lwrtm%trans_layer(nchan_lw))
   preproc_lwrtm%trans_layer=real_fill_value

   allocate(preproc_lwrtm%emissivity_used(nchan_lw))
   preproc_lwrtm%emissivity_used=real_fill_value

   allocate(preproc_lwrtm%transmission_tau_total(nchan_lw))
   preproc_lwrtm%transmission_tau_total=real_fill_value

   allocate(preproc_lwrtm%taubc(nchan_lw,preproc_dims%kdim))
   preproc_lwrtm%taubc=real_fill_value

   allocate(preproc_lwrtm%tauac(nchan_lw,preproc_dims%kdim))
   preproc_lwrtm%tauac=real_fill_value

   allocate(preproc_lwrtm%radbc(nchan_lw,preproc_dims%kdim))
   preproc_lwrtm%radbc=real_fill_value

   allocate(preproc_lwrtm%radiance_bt(nchan_lw))
   preproc_lwrtm%radiance_bt=real_fill_value

   allocate(preproc_lwrtm%radiance_up(nchan_lw,preproc_dims%kdim))
   preproc_lwrtm%radiance_up=real_fill_value

   allocate(preproc_lwrtm%radiance_down(nchan_lw,preproc_dims%kdim))
   preproc_lwrtm%radiance_down=real_fill_value

   allocate(preproc_lwrtm%players(preproc_dims%kdim-1))
   preproc_lwrtm%players=real_fill_value
   allocate(preproc_lwrtm%plevels(preproc_dims%kdim))
   preproc_lwrtm%plevels=real_fill_value


   ! preproc_swrtm
   allocate(preproc_swrtm%transmission_tau_total(nchan_sw))
   preproc_swrtm%transmission_tau_total=real_fill_value

   allocate(preproc_swrtm%trans_layer(nchan_sw))
   preproc_swrtm%trans_layer=real_fill_value

   allocate(preproc_swrtm%taubc(nchan_sw,preproc_dims%kdim))
   preproc_swrtm%taubc=real_fill_value

   allocate(preproc_swrtm%tauac(nchan_sw,preproc_dims%kdim))
   preproc_swrtm%tauac=real_fill_value


   ! preproc_surf
   allocate(preproc_surf%emissivity(sx:ex,sy:ey,nchan_lw))
   preproc_surf%emissivity=real_fill_value

end subroutine allocate_preproc_structures
