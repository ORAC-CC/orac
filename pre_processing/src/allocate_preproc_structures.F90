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
! preproc_geoloc struct out         lat/lon values
! preproc_geo    struct out         geometry
! preproc_prtm   struct out         profiles and surface fields
! preproc_dims   struct out         preprocessing grid definitions
! preproc_lwrtm  struct out         longwave RTM data
! preproc_swrtm  struct out         shortwave RTM data
! preproc_surf   struct out         surface albedo and emissivity
! channel_info   struct in          channel information
!
! Local variables:
! Name Type Description
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
! 2014/02/10, AP: simplifying variable names
! 2014/05/01, GM: Add some allocations that were being done outside.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/28, MJ: Fixed bug with intent statement for preproc_dims
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine allocate_preproc_structures(imager_angles,preproc_dims, &
   preproc_geoloc,preproc_geo,preproc_prtm,preproc_lwrtm,preproc_swrtm, &
   preproc_surf,channel_info)

   use channel_structures
   use ecmwf_structures
   use imager_structures
   use preproc_constants
  !use preproc_structures

   implicit none

   type(imager_angles_s),  intent(in)  :: imager_angles
   type(preproc_dims_s),   intent(inout) :: preproc_dims
   type(preproc_geoloc_s), intent(out) :: preproc_geoloc
   type(preproc_geo_s),    intent(out) :: preproc_geo
   type(preproc_prtm_s),   intent(out) :: preproc_prtm
   type(preproc_lwrtm_s),  intent(out) :: preproc_lwrtm
   type(preproc_swrtm_s),  intent(out) :: preproc_swrtm
   type(preproc_surf_s),   intent(out) :: preproc_surf
   type(channel_info_s),   intent(out) :: channel_info

   integer :: nchan_sw,nchan_lw

   nchan_sw=channel_info%nchannels_sw
   nchan_lw=channel_info%nchannels_lw


   ! preproc_dims
   allocate(preproc_dims%instr_channel_sw(preproc_dims%maxchannels_sw))
   preproc_dims%instr_channel_sw=-1
   allocate(preproc_dims%instr_channel_lw(preproc_dims%maxchannels_lw))
   preproc_dims%instr_channel_lw=-1

   allocate(preproc_dims%coef_channel_sw(preproc_dims%maxchannels_sw))
   preproc_dims%coef_channel_sw=-1
   allocate(preproc_dims%coef_channel_lw(preproc_dims%maxchannels_lw))
   preproc_dims%coef_channel_lw=-1

   write(*,*) 'preproc_dims',preproc_dims%xdim,preproc_dims%ydim
   allocate(preproc_dims%counter_sw(preproc_dims%xdim,preproc_dims%ydim))
   preproc_dims%counter_sw=0
   allocate(preproc_dims%counter_lw(preproc_dims%xdim,preproc_dims%ydim))
   preproc_dims%counter_sw=0

   allocate(preproc_dims%filter_array_lw(preproc_dims%xdim,preproc_dims%ydim))
   preproc_dims%filter_array_lw=0
   allocate(preproc_dims%filter_array_sw(preproc_dims%xdim,preproc_dims%ydim))
   preproc_dims%filter_array_sw=0


   ! preproc_geoloc
   allocate(preproc_geoloc%longitude(preproc_dims%xdim))
   preproc_geoloc%longitude=real_fill_value
   allocate(preproc_geoloc%latitude(preproc_dims%ydim))
   preproc_geoloc%latitude=real_fill_value


   ! preproc_geo
   allocate(preproc_geo%solza(preproc_dims%xdim,preproc_dims%ydim, &
                              imager_angles%nviews))
   preproc_geo%solza=filter_micro
   allocate(preproc_geo%solazi(preproc_dims%xdim,preproc_dims%ydim, &
                               imager_angles%nviews))
   preproc_geo%solazi=filter_micro
   allocate(preproc_geo%satza(preproc_dims%xdim,preproc_dims%ydim, &
                              imager_angles%nviews))
   preproc_geo%satza=filter_micro
   allocate(preproc_geo%relazi(preproc_dims%xdim,preproc_dims%ydim, &
                               imager_angles%nviews))
   preproc_geo%relazi=filter_micro


   ! preproc_prtm
   allocate(preproc_prtm%pressure(preproc_dims%xdim,preproc_dims%ydim, &
                                  preproc_dims%kdim))
   preproc_prtm%pressure=real_fill_value
   allocate(preproc_prtm%temperature(preproc_dims%xdim,preproc_dims%ydim, &
                                     preproc_dims%kdim))
   preproc_prtm%temperature=real_fill_value
   allocate(preproc_prtm%spec_hum(preproc_dims%xdim,preproc_dims%ydim, &
                                  preproc_dims%kdim))
   preproc_prtm%spec_hum=real_fill_value
   allocate(preproc_prtm%ozone(preproc_dims%xdim,preproc_dims%ydim, &
                               preproc_dims%kdim))
   preproc_prtm%ozone=real_fill_value

   allocate(preproc_prtm%phi_lay(preproc_dims%xdim,preproc_dims%ydim, &
                                 preproc_dims%kdim-1))
   preproc_prtm%phi_lay=real_fill_value
   allocate(preproc_prtm%phi_lev(preproc_dims%xdim,preproc_dims%ydim, &
                                 preproc_dims%kdim))
   preproc_prtm%phi_lev=real_fill_value

   allocate(preproc_prtm%geopot(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%geopot=real_fill_value
   allocate(preproc_prtm%lnsp(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%lnsp=real_fill_value
   allocate(preproc_prtm%surface_pressure(preproc_dims%xdim,preproc_dims%ydim))

   allocate(preproc_prtm%u10(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%u10=real_fill_value
   allocate(preproc_prtm%v10(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%v10=real_fill_value

   allocate(preproc_prtm%land_sea_mask(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%land_sea_mask=real_fill_value

   allocate(preproc_prtm%temp2(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%temp2=real_fill_value
   allocate(preproc_prtm%skin_temp(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%skin_temp=real_fill_value

   allocate(preproc_prtm%snow_albedo(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%snow_albedo=real_fill_value
   allocate(preproc_prtm%snow_depth(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%snow_depth=real_fill_value

   allocate(preproc_prtm%sst(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%sst=real_fill_value
   allocate(preproc_prtm%sea_ice_cover(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%sea_ice_cover=real_fill_value

   allocate(preproc_prtm%totcolwv(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%totcolwv=real_fill_value


   ! preproc_lwrtm
   allocate(preproc_lwrtm%radiance_cloudy(nchan_lw))
   preproc_lwrtm%radiance_cloudy=real_fill_value

   allocate(preproc_lwrtm%transmission_tau_levels(nchan_lw, preproc_dims%kdim))
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
   allocate(preproc_surf%emissivity(preproc_dims%xdim, &
                                    preproc_dims%ydim,nchan_lw))
   preproc_surf%emissivity=real_fill_value

end subroutine allocate_preproc_structures
