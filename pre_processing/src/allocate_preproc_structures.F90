! Name: allocate_preproc_structures.F90
!
!
! Purpose:
! Allocate the array parts of the types defined in preproc_structures.F90
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_angles  struct in  Summary of sun/satellite viewing angles
! preproc_geoloc struct out Summary of lat/lon values
! preproc_geo    struct out Summary of geometry
! preproc_prtm   struct out Summary of profiles and surface fields
! preproc_dims   struct out Summary of preprocessing grid definitions
! preproc_lwrtm  struct out Summary of longwave RTM data
! preproc_swrtm  struct out Summary of shortwave RTM data
! preproc_surf   struct out Summary of surface albedo and emissivity
! channel_info   struct in  Summary of channel information
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/01/13: MJ produces draft code for allocating the 
!                preprocessing types
! 2012/05/30: GT Added allocation of preproc_surf%emissivity
! 2012/06/26: CP Added allocation swrtm information
!                added definitions of nchan_sw and nchan_lw
!                included channel_info structure
! 2012/06/27:    Fixed small bug in allocation of swrtm structure
! 2012/07/04: CP removed nviews from rtm array
! 2012/07/17: CP adds in players variable
! 2012/07/30: CP added in solazi
! 2012/07/30: MJ added in phi_lay and phi_lev variable
! 2012/08/08: CP changed emissivity array to be consistent with msi order
! 2012/08/28: CP remove _lw _sw for filter_array and counter
! 2012/08/28: CP readded _lw _sw for filter_array and counter
! 2012/11/14: CP added surface pressure
! 2012/11/14: CP changedkdim_pre to pressure levels
! 2013/10/16: CP changed definition of preproc_lwrtm%players and
!                preproc_lwrtm%plevels
! 2013/10/23: AP Tidying. Removed unused arguments. Removed print statements.
!
! 2013/10/16: CP readded def of kdim_pre changed definition of preproc_lwrtm%phi_lay and preproc_lwrtm%phi_lev
! 2013/10/16: CP removed kdim_pre and removed ecmwf_dims form subroutine call, changed array definition of players and plev
! 2013/11/08: GM Removed double allocations (allocated twice in a row) of
!                preproc_prtm% temperature through preproc_prtm%ozone.
!
! $Id$
!
! Bugs:
! none known
!

subroutine allocate_preproc_structures(imager_angles, &
     preproc_geoloc,preproc_geo,preproc_prtm,preproc_dims,preproc_lwrtm, &
     preproc_swrtm,preproc_surf,channel_info)

   use preproc_constants
   use channel_structures
   use preproc_structures
   use imager_structures
   use ecmwf_structures

   implicit none


   type(preproc_geoloc_s) :: preproc_geoloc
   type(preproc_geo_s)    :: preproc_geo
   type(preproc_dims_s)   :: preproc_dims
   type(preproc_prtm_s)   :: preproc_prtm
   type(preproc_lwrtm_s)  :: preproc_lwrtm
   type(preproc_swrtm_s)  :: preproc_swrtm
   type(preproc_surf_s)   :: preproc_surf
   type(channel_info_s)   :: channel_info
   type(imager_angles_s)  :: imager_angles
   integer                :: nchan_sw,nchan_lw

   allocate(preproc_dims%counter_sw(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   allocate(preproc_dims%counter_lw(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_dims%counter_sw=0
   preproc_dims%counter_lw=0


   nchan_sw=channel_info%nchannels_sw
   nchan_lw=channel_info%nchannels_lw


   allocate(preproc_dims%channels(nchan_sw+nchan_lw))
   preproc_dims%channels=real_fill_value

   allocate(preproc_geoloc%longitude(preproc_dims%xdim_pre))
   preproc_geoloc%longitude=real_fill_value
   allocate(preproc_geoloc%latitude(preproc_dims%ydim_pre))
   preproc_geoloc%latitude=real_fill_value

   allocate(preproc_geo%satza(preproc_dims%xdim_pre,preproc_dims%ydim_pre, &
        imager_angles%nviews))
   preproc_geo%satza=filter_micro
   allocate(preproc_geo%solza(preproc_dims%xdim_pre,preproc_dims%ydim_pre, &
        imager_angles%nviews))
   preproc_geo%solza=filter_micro
   allocate(preproc_geo%relazi(preproc_dims%xdim_pre,preproc_dims%ydim_pre, &
        imager_angles%nviews))
   preproc_geo%relazi=filter_micro
   allocate(preproc_geo%solazi(preproc_dims%xdim_pre,preproc_dims%ydim_pre, &
        imager_angles%nviews))
   preproc_geo%solazi=filter_micro


   allocate(preproc_prtm%temperature(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre))
   preproc_prtm%temperature=real_fill_value
   allocate(preproc_prtm%pressure(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre))
   preproc_prtm%pressure=real_fill_value
   allocate(preproc_prtm%spec_hum(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre))
   preproc_prtm%spec_hum=real_fill_value
   allocate(preproc_prtm%phi_lay(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre-1))
   preproc_prtm%phi_lay=real_fill_value
   allocate(preproc_prtm%phi_lev(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre))
   preproc_prtm%phi_lev=real_fill_value
   allocate(preproc_prtm%ozone(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,preproc_dims%kdim_pre))
   preproc_prtm%ozone=real_fill_value


   allocate(preproc_prtm%snow_albedo(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%snow_albedo=real_fill_value

   allocate(preproc_prtm%sea_ice_cover(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%sea_ice_cover=real_fill_value

   allocate(preproc_prtm%snow_depth(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%snow_depth=real_fill_value

allocate(preproc_prtm%geopot(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%geopot=real_fill_value
   allocate(preproc_prtm%lnsp(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%lnsp=real_fill_value
   allocate(preproc_prtm%sst(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%sst=real_fill_value
   allocate(preproc_prtm%totcolwv(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%totcolwv=real_fill_value
   allocate(preproc_prtm%u10(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%u10=real_fill_value
   allocate(preproc_prtm%v10(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%v10=real_fill_value
   allocate(preproc_prtm%temp2(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%temp2=real_fill_value
   allocate(preproc_prtm%land_sea_mask(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre))
   preproc_prtm%land_sea_mask=real_fill_value
   allocate(preproc_prtm%skin_temp(preproc_dims%xdim_pre,preproc_dims%ydim_pre))
   preproc_prtm%skin_temp=real_fill_value

   allocate(preproc_prtm%surface_pressure(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre))
   preproc_prtm%surface_pressure=real_fill_value

   allocate(preproc_dims%filter_array_lw(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre))
   preproc_dims%filter_array_lw=0



   allocate(preproc_dims%filter_array_sw(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre))
   preproc_dims%filter_array_sw=0


   !cloudy toa radiance for 100% fraction in each channel at ctp (we do
   !clear cky?)
   allocate(preproc_lwrtm%radiance_cloudy(nchan_lw))
   preproc_lwrtm%radiance_cloudy=real_fill_value

   !taufrom surface for each channel
   allocate(preproc_lwrtm%transmission_tau_total(nchan_lw))
   preproc_lwrtm%transmission_tau_total=real_fill_value

   !transmittance from surface for each channel
   allocate(preproc_lwrtm%trans_layer(nchan_lw))
   preproc_lwrtm%trans_layer=real_fill_value

   !emissivity used by rttov in the rt calculations in each channel
   allocate(preproc_lwrtm%emissivity_used(nchan_lw))
   preproc_lwrtm%emissivity_used=real_fill_value

   !transmittance from each pressure level to TOA (aka known as taubc in
   !opposite vertical order)
   allocate(preproc_lwrtm%transmission_tau_levels(nchan_lw, &
        preproc_dims%kdim_pre))
   preproc_lwrtm%transmission_tau_levels=real_fill_value

   !taubc itself
   allocate(preproc_lwrtm%taubc(nchan_lw,preproc_dims%kdim_pre))
   preproc_lwrtm%taubc=real_fill_value

   !tauac itself
   allocate(preproc_lwrtm%tauac(nchan_lw,preproc_dims%kdim_pre))
   preproc_lwrtm%tauac=real_fill_value

   !radiance difference between toa radiance and radiance at each pressure
   !level normalized with transmission from each level
   allocate(preproc_lwrtm%radbc(nchan_lw,preproc_dims%kdim_pre))
   preproc_lwrtm%radbc=real_fill_value

   !total (cloudy and clear) brightness temp. in each channel at TOA
   allocate(preproc_lwrtm%radiance_bt(nchan_lw))
   preproc_lwrtm%radiance_bt=real_fill_value

   !above cloud upwelling radiance from each pressure level to TOA in
   !each channel
   allocate(preproc_lwrtm%radiance_up(nchan_lw,preproc_dims%kdim_pre))
   preproc_lwrtm%radiance_up=real_fill_value

   !above cloud downwelling radiance from each pressure level down to surface
   !in each channel
   allocate(preproc_lwrtm%radiance_down(nchan_lw,preproc_dims%kdim_pre))
   preproc_lwrtm%radiance_down=real_fill_value


   allocate(preproc_lwrtm%players(preproc_dims%kdim_pre-1))
   preproc_lwrtm%players=real_fill_value
   allocate(preproc_lwrtm%plevels(preproc_dims%kdim_pre))
   preproc_lwrtm%plevels=real_fill_value

   ! Surface emissivity
   allocate(preproc_surf%emissivity(preproc_dims%xdim_pre, &
        preproc_dims%ydim_pre,nchan_lw))
   preproc_surf%emissivity=real_fill_value


!!!SWRTM

   !taufrom surface for each channel
   allocate(preproc_swrtm%transmission_tau_total(nchan_sw))
   preproc_swrtm%transmission_tau_total=real_fill_value

   !transmittance from surface for each channel
   allocate(preproc_swrtm%trans_layer(nchan_sw))
   preproc_swrtm%trans_layer=real_fill_value

   !taubc itself
   allocate(preproc_swrtm%taubc(nchan_sw,preproc_dims%kdim_pre))
   preproc_swrtm%taubc=real_fill_value

   !taubc itself
   allocate(preproc_swrtm%tauac(nchan_sw,preproc_dims%kdim_pre))
   preproc_swrtm%tauac=real_fill_value


end subroutine allocate_preproc_structures
