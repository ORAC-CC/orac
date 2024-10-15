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
! preproc_surf   struct out         surface albedo and emissivity
! channel_info   struct in          channel information
!
! History:
! 2012/01/13, MJ: produces draft code for allocating the preprocessing types
! 2012/05/30, GT: Added allocation of preproc_surf%emissivity
! 2012/06/26, CP: Added allocation swrtm information added definitions of
!    nchan_sw and nchan_lw included channel_info structure
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
!    preproc_lwrtm%plevels
! 2013/10/23, AP: Tidying. Removed unused arguments. Removed print statements.
! 2013/10/16, CP: readded def of kdim_pre changed definition of
!    preproc_lwrtm%phi_lay and preproc_lwrtm%phi_lev
! 2013/10/16, CP: removed kdim_pre and removed ecmwf_dims form subroutine call,
!    hanged array definition of players and plev
! 2013/11/08, GM: Removed double allocations (associated twice in a row) of
!    preproc_prtm%temperature through preproc_prtm%ozone.
! 2014/02/10, AP: Simplifying variable names
! 2014/05/01, GM: Add some allocations that were being done outside.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/28, MJ: Fixed bug with intent statement for preproc_dims
! 2014/07/01, AP: Redefined limits to minimise array sizes
! 2014/09/10, AP: Removed unnecessary LWRTM and SWRTM structures.
! 2015/11/26, GM: Refactored to include allocate_preproc_prtm() for use
!    elsewhere.
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2018/07/18, DE: Add tropoopause temperature
! 2024/07/01, DH: Change indexing to use preproc_dims for all dimensions
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_preproc_prtm(preproc_dims, preproc_prtm)

   use preproc_constants_m

   implicit none

   type(preproc_dims_t), intent(in)  :: preproc_dims
   type(preproc_prtm_t), intent(out) :: preproc_prtm

   allocate(preproc_prtm%pressure(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim))
   preproc_prtm%pressure=sreal_fill_value
   allocate(preproc_prtm%temperature(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim))
   preproc_prtm%temperature=sreal_fill_value
   allocate(preproc_prtm%spec_hum(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim))
   preproc_prtm%spec_hum=sreal_fill_value
   allocate(preproc_prtm%ozone(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim))
   preproc_prtm%ozone=sreal_fill_value

   allocate(preproc_prtm%phi_lay(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim))
   preproc_prtm%phi_lay=sreal_fill_value
   allocate(preproc_prtm%phi_lev(preproc_dims%xdim,preproc_dims%ydim,preproc_dims%kdim+1))
   preproc_prtm%phi_lev=sreal_fill_value
   
   allocate(preproc_prtm%geopot(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%geopot=sreal_fill_value
   allocate(preproc_prtm%lnsp(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%lnsp=sreal_fill_value
   
   allocate(preproc_prtm%u10(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%u10=sreal_fill_value
   allocate(preproc_prtm%v10(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%v10=sreal_fill_value
   
   allocate(preproc_prtm%land_sea_mask(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%land_sea_mask=sreal_fill_value
   
   allocate(preproc_prtm%temp2(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%temp2=sreal_fill_value
   allocate(preproc_prtm%skin_temp(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%skin_temp=sreal_fill_value

   allocate(preproc_prtm%snow_albedo(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%snow_albedo=sreal_fill_value
   allocate(preproc_prtm%snow_depth(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%snow_depth=sreal_fill_value

   allocate(preproc_prtm%sst(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%sst=sreal_fill_value
   allocate(preproc_prtm%sea_ice_cover(preproc_dims%xdim,preproc_dims%ydim))
      preproc_prtm%sea_ice_cover=sreal_fill_value

   allocate(preproc_prtm%totcolwv(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%totcolwv=sreal_fill_value

#ifdef INCLUDE_SATWX
   allocate(preproc_prtm%trop_p(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%trop_p=sreal_fill_value

   allocate(preproc_prtm%trop_t(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%trop_t=sreal_fill_value


   allocate(preproc_prtm%cape(preproc_dims%xdim,preproc_dims%ydim))
   preproc_prtm%cape=sreal_fill_value
#endif

end subroutine allocate_preproc_prtm


subroutine allocate_preproc_structures(imager_angles,preproc_dims, &
   preproc_geoloc,preproc_geo,preproc_prtm,preproc_surf,preproc_cld,&
   channel_info)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m

   implicit none

   type(imager_angles_t),  intent(in)    :: imager_angles
   type(preproc_dims_t),   intent(inout) :: preproc_dims
   type(preproc_geoloc_t), intent(out)   :: preproc_geoloc
   type(preproc_geo_t),    intent(out)   :: preproc_geo
   type(preproc_prtm_t),   intent(out)   :: preproc_prtm
   type(preproc_surf_t),   intent(out)   :: preproc_surf
   type(preproc_cld_t),    intent(out)   :: preproc_cld
   type(channel_info_t),   intent(inout) :: channel_info

   integer :: nchan_sw, nchan_lw

   nchan_sw=channel_info%nchannels_sw
   nchan_lw=channel_info%nchannels_lw

   ! preproc_dims
   allocate(preproc_dims%counter_sw(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_dims%counter_sw=0
   allocate(preproc_dims%counter_lw(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_dims%counter_lw=0

   ! preproc_geoloc
   allocate(preproc_geoloc%longitude(preproc_dims%xdim))
   preproc_geoloc%longitude=sreal_fill_value
   allocate(preproc_geoloc%latitude(preproc_dims%ydim))
   preproc_geoloc%latitude=sreal_fill_value

   ! preproc_geo (init to 0 as used for summation in build_preproc_fields)
   allocate(preproc_geo%solza(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_geo%solza=0.0
   allocate(preproc_geo%solazi(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_geo%solazi=0.0
   allocate(preproc_geo%satza(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_geo%satza=0.0
   allocate(preproc_geo%satazi(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_geo%satazi=0.0
   allocate(preproc_geo%relazi(preproc_dims%xdim,preproc_dims%ydim,imager_angles%nviews))
   preproc_geo%relazi=0.0

   ! preproc_prtm
   call allocate_preproc_prtm(preproc_dims, preproc_prtm)

   ! preproc_surf
   allocate(preproc_surf%emissivity(preproc_dims%xdim,preproc_dims%ydim,nchan_lw))
   preproc_surf%emissivity=sreal_fill_value

   ! preproc_cld
   allocate(preproc_cld%clear_bt(preproc_dims%xdim,preproc_dims%ydim,nchan_lw))
   preproc_cld%clear_bt=0.0
   allocate(preproc_cld%cloud_bt(preproc_dims%xdim,preproc_dims%ydim,nchan_lw))
   preproc_cld%cloud_bt=0.0

end subroutine allocate_preproc_structures
