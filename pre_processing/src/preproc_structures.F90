!-------------------------------------------------------------------------------
! Name: preproc_structures.F90
!
! Purpose:
! Define variables types which hold the preprocessing output information.
!
! Description and Algorithm details:
! None
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/01/17, MJ: produces initial version of code
! 2012/05/30, GT: Added preproc_surf_s type (containing emissivity and possibly
!    albedo at a later date) Commented out preproc_albedo_s
! 2012/07/04, CP: removed dependance on nviews
! 2012/07/17, CP: adds in players variable
! 2012/07/30, CP: added in solazi
! 2012/08/01, MJ: adds geopotential height coordinates
! 2012/08/28, CP: removed _lw _sw for filter_array and counter
! 2012/08/28, CP: readded _lw _sw for filter_array and counter
! 2012/11/14, CP: added surface pressure
! 2013/10/23, AP: Tidying. Commented out unused types.
! 2014/02/10, AP: Shortened DIM names.
! 2014/05/01, GM: Cleaned up the code.
! 2015/05/07, AP: Removed unneccessary fields from preproc_dims.
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

module preproc_structures

   use preproc_constants

   implicit none

   type preproc_dims_s
      integer(kind=lint) :: xdim, ydim, kdim
      integer(kind=lint) :: nchan_sw, nchan_lw
      integer(kind=lint) :: min_lat, max_lat, min_lon, max_lon
      real(kind=sreal)   :: dellon, dellat

      real(kind=sreal)   :: lat_offset=90.0, lon_offset=180.0

      integer(kind=lint), dimension(:,:), pointer :: counter_sw, counter_lw
   end type preproc_dims_s


   ! (geo)location
   type preproc_geoloc_s
      real(kind=sreal), dimension(:), pointer ::  latitude
      real(kind=sreal), dimension(:), pointer ::  longitude
   end type preproc_geoloc_s


   ! geometry
   type preproc_geo_s
      real(kind=sreal), dimension(:,:,:), pointer :: solza,solazi,satza,relazi
   end type preproc_geo_s


   ! ecmwf profiles and surface fields (prtm data)
   type preproc_prtm_s
      real(kind=sreal), dimension(:,:,:), pointer :: pressure,temperature, &
                                                     spec_hum,ozone
      real(kind=sreal), dimension(:,:,:), pointer :: phi_lev,phi_lay

      real(kind=sreal), dimension(:,:), pointer   :: geopot,lnsp, &
                                                     surface_pressure
      real(kind=sreal), dimension(:,:), pointer   :: u10,v10
      real(kind=sreal), dimension(:,:), pointer   :: land_sea_mask
      real(kind=sreal), dimension(:,:), pointer   :: temp2,skin_temp
      real(kind=sreal), dimension(:,:), pointer   :: snow_albedo,snow_depth
      real(kind=sreal), dimension(:,:), pointer   :: sst,sea_ice_cover
      real(kind=sreal), dimension(:,:), pointer   :: totcolwv
   end type preproc_prtm_s


   ! lwrtm data
   type preproc_lwrtm_s
      ! cloudy toa radiance for 100% fraction in each channel at ctp
      real(kind=sreal), dimension(:), pointer   :: radiance_cloudy
      ! optical depth from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: transmission_tau_total
      ! transmittance from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: trans_layer
      ! emissivity used by rttov in the rt calculations in each channel
      real(kind=sreal), dimension(:), pointer   :: emissivity_used
      ! transmittance from each pressure level to TOA (aka known as taubc in
      ! opposite vertical order)
      real(kind=sreal), dimension(:,:), pointer :: transmission_tau_levels
      ! taubc itself
      real(kind=sreal), dimension(:,:), pointer :: taubc
      ! radiance difference between toa radiance and radiance at each pressure
      ! level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: tauac
      ! radiance difference between toa radiance and radiance at each pressure
      ! level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: radbc
      ! total (cloudy and clear) brightness temp. in each channel at TOA
      real(kind=sreal), dimension(:), pointer   :: radiance_bt
      ! above cloud upwelling radiance from each pressure level to TOA in each
      ! channel
      real(kind=sreal), dimension(:,:), pointer :: radiance_up
      ! above cloud downwelling radiance from each pressure level down to
      ! surface in each channel
      real(kind=sreal), dimension(:,:), pointer :: radiance_down
      ! pressure levels of layers
      real(kind=sreal), dimension(:), pointer   :: players
      real(kind=sreal), dimension(:), pointer   :: plevels
   end type preproc_lwrtm_s


   ! swrtm data
   type preproc_swrtm_s
      ! tau from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: transmission_tau_total
      ! transmittance from surface for each channel
      real(kind=sreal), dimension(:), pointer   :: trans_layer
      ! transmittance from each pressure level to TOA (aka known as taubc in
      ! opposite vertical order)
      real(kind=sreal), dimension(:,:), pointer :: transmission_tau_levels
      ! taubc itself
      real(kind=sreal), dimension(:,:), pointer :: taubc
      ! radiance difference between toa radiance and radiance at each pressure
      ! level normalized with transmission from each level
      real(kind=sreal), dimension(:,:), pointer :: tauac
   end type preproc_swrtm_s


   ! surface albedo and emissivity
   type preproc_surf_s
!     real(kind=sreal), dimension(:,:,:), pointer :: albedo
      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
   end type preproc_surf_s

contains

  include 'allocate_preproc_structures.F90'
  include 'deallocate_preproc_structures.F90'

end module preproc_structures
