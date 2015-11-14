!-------------------------------------------------------------------------------
! Name: preproc_structures.F90
!
! Purpose:
! Module defining variables types which hold the preprocessing output information
!
! History:
! 2012/01/17, MJ: produces initial version of code
! 2012/05/30, GT: Added preproc_surf_s type (containing emissivity and possibly
!    albedo at a later date) Commented out preproc_albedo_s
! 2012/07/04, CP: removed dependence on nviews
! 2012/07/17, CP: adds in players variable
! 2012/07/30, CP: added in solazi
! 2012/08/01, MJ: adds geopotential height coordinates
! 2012/08/28, CP: removed _lw _sw for filter_array and counter
! 2012/08/28, CP: readded _lw _sw for filter_array and counter
! 2012/11/14, CP: added surface pressure
! 2013/10/23, AP: Tidying. Commented out unused types.
! 2014/02/10, AP: Shortened DIM names.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/07, AP: Removed unnecessary fields from preproc_dims.
! 2014/09/10, AP: Removed unnecessary LWRTM and SWRTM structures.
!
! $Id$
!
! Bugs:
! None known.
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
      real(kind=sreal), dimension(:), pointer :: latitude
      real(kind=sreal), dimension(:), pointer :: longitude
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

      real(kind=sreal), dimension(:,:), pointer   :: geopot,lnsp
      real(kind=sreal), dimension(:,:), pointer   :: u10,v10
      real(kind=sreal), dimension(:,:), pointer   :: land_sea_mask
      real(kind=sreal), dimension(:,:), pointer   :: temp2,skin_temp
      real(kind=sreal), dimension(:,:), pointer   :: snow_albedo,snow_depth
      real(kind=sreal), dimension(:,:), pointer   :: sst,sea_ice_cover
      real(kind=sreal), dimension(:,:), pointer   :: totcolwv
   end type preproc_prtm_s


   ! surface albedo and emissivity
   type preproc_surf_s
!     real(kind=sreal), dimension(:,:,:), pointer :: albedo
      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
   end type preproc_surf_s

contains

include 'allocate_preproc_structures.F90'
include 'deallocate_preproc_structures.F90'

end module preproc_structures
