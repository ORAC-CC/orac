! Name: preproc_structures.F90
!
!
! Purpose:
! Define variables types which hold the preprocessing output information
! 
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2012/01/17: MJ produces initial version of code
! 2012/05/30: GT Added preproc_surf_s type (containing emissivity and possibly 
!                albedo at a later date) Commented out preproc_albedo_s
! 2012/07/04: CP removed dependance on nviews
! 2012/07/17: CP adds in players variable
! 2012/07/30: CP added in solazi
! 2012/08/01: MJ adds geopotential height coordinates
! 2012/08/28: CP removed _lw _sw for filter_array and counter
! 2012/08/28: CP readded _lw _sw for filter_array and counter
! 2012/11/14: CP added surface pressure
! 2013/10/23: AP Tidying. Commented out unused types.
! 2014/02/10: AP Shortened DIM names.
!
! $Id$
!
! Bugs:
! none known
!

module preproc_structures

   use preproc_constants

   implicit none

   type preproc_dims_s
      integer(kind=lint) :: xdim,ydim,kdim
      integer(kind=lint) :: nchan_sw,nchan_lw
      integer(kind=lint) ::  min_lat, max_lat, min_lon, max_lon,del_lon,del_lat
      real(kind=sreal)   ::  dellon,dellat

      real(kind=sreal)   :: lat_offset=90.0
      real(kind=sreal)   :: lon_offset=180.0

      integer(kind=lint) :: maxchannels_sw=36,maxchannels_lw=36
      integer(kind=lint), dimension(:),pointer :: instr_channel_sw, &
           coef_channel_sw!,rttov_channel_sw
      integer(kind=lint), dimension(:),pointer :: instr_channel_lw, &
           coef_channel_lw!,rttov_channel_lw

      integer(kind=sint), dimension(:,:), pointer ::  filter_array_sw, &
           filter_array_lw

      integer(kind=lint), dimension(:,:), pointer ::  counter_sw,counter_lw

      integer(kind=lint), dimension(:), pointer   :: channels

   end type preproc_dims_s

   !rtm data
   type preproc_lwrtm_s
      integer(kind=sint) :: dummy

      !cloudy toa radiance for 100% fraction in each channel at ctp (we do clear cky?)
      real(kind=sreal), dimension(:), pointer :: radiance_cloudy
      !optical depth from surface for each channel
      real(kind=sreal), dimension(:), pointer :: transmission_tau_total
      !transmittance from surface for each channel
      real(kind=sreal), dimension(:), pointer :: trans_layer
      !emissivity used by rttov in the rt calculations in each channel
      real(kind=sreal), dimension(:), pointer ::  emissivity_used
      !transmittance from each pressure level to TOA (aka known as taubc in
      !opposite vertical order)
      real(kind=sreal), dimension(:,:), pointer :: transmission_tau_levels
      !taubc itself
      real(kind=sreal), dimension(:,:), pointer :: taubc
      !radiance difference between toa radiance and radiance at each pressure
      !level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: tauac
      !radiance difference between toa radiance and radiance at each pressure
      !level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: radbc
      !total (cloudy and clear) brightness temp. in each channel at TOA
      real(kind=sreal), dimension(:), pointer   :: radiance_bt
      !above cloud upwelling radiance from each pressure level to TOA in
      !each channel
      real(kind=sreal), dimension(:,:), pointer :: radiance_up
      !above cloud downwelling radiance from each pressure level down to surface
      !in each channel
      real(kind=sreal), dimension(:,:), pointer :: radiance_down
      !pressure levels of layers
      real(kind=sreal), dimension(:), pointer   :: players
      real(kind=sreal), dimension(:), pointer   :: plevels
   end type preproc_lwrtm_s

   !swrtm data
   type preproc_swrtm_s
      integer(kind=sint) :: dummy

      !tau from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: transmission_tau_total
      !transmittance from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: trans_layer
      !transmittance from each pressure level to TOA (aka known as taubc in
      !opposite vertical order)
      real(kind=sreal), dimension(:,:), pointer :: transmission_tau_levels
      !taubc itself
      real(kind=sreal), dimension(:,:), pointer :: taubc
      !radiance difference between toa radiance and radiance at each pressure
      !level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: tauac

   end type preproc_swrtm_s

   !ecmwf profiles and surface fields
   type preproc_prtm_s
      integer(kind=sint) :: dummy

      real(kind=sreal), dimension(:,:,:), pointer ::  temperature,spec_hum, &
           ozone,pressure,phi_lev,phi_lay

      real(kind=sreal), dimension(:,:), pointer   ::  sst,geopot,lnsp, &
           sea_ice_cover,snow_albedo,totcolwv,snow_depth,lsf

      real(kind=sreal), dimension(:,:), pointer   ::  u10,v10,temp2, &
           land_sea_mask,skin_temp,surface_pressure

   end type preproc_prtm_s

   ! Surface albedo and emissivity
   type preproc_surf_s
      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
      !     real(kind=sreal), dimension(:,:,:), pointer :: albedo -- needed?
   end type preproc_surf_s

   !geometry
   type preproc_geo_s
      integer(kind=sint) :: dummy

      real(kind=sreal), dimension(:,:,:), pointer ::  solza,satza,relazi,solazi

   end type preproc_geo_s

   !(geo)location
   type preproc_geoloc_s
      real(kind=sreal), dimension(:), pointer ::  latitude
      real(kind=sreal), dimension(:), pointer ::  longitude

   end type preproc_geoloc_s


end module preproc_structures
