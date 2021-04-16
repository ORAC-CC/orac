!-------------------------------------------------------------------------------
! Name: preproc_structures.F90
!
! Purpose:
! Module defining variables types which hold the preprocessing output information
!
! History:
! 2012/01/17, MJ: produces initial version of code
! 2012/05/30, GT: Added preproc_surf_t type (containing emissivity and possibly
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
! 2015/11/26, GM: Added linearly_combine_prtms() to facilitate linear
!    interpolation between preproc_prtm_t structures.
! 2017/03/29, SP: Add new variable for tropopause cloud emissivity (ExtWork)
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2018/07/18, DE: Add tropoopause temperature
! 2018/09/30, SP: New structure to store driver option variables, tidier than multi-var
! 2018/11/05, SP: Add CAPE
! 2021/03/09, AP: Consolidate path arguments into preproc_paths_t structure
! 2021/03/10, AP: Consolidate paths/dates into setup_args_t structure
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module preproc_structures_m

   use preproc_constants_m

   implicit none

   type preproc_dims_t
      integer(kind=lint) :: xdim, ydim, kdim
      integer(kind=lint) :: nchan_sw, nchan_lw
      integer(kind=lint) :: min_lat, max_lat, min_lon, max_lon
      real(kind=sreal)   :: dellon, dellat

      real(kind=sreal)   :: lat_offset=90.0, lon_offset=180.0

      integer(kind=lint), dimension(:,:,:), pointer :: counter_sw, counter_lw
   end type preproc_dims_t


   ! (geo)location
   type preproc_geoloc_t
      real(kind=sreal), dimension(:), pointer :: latitude
      real(kind=sreal), dimension(:), pointer :: longitude
   end type preproc_geoloc_t


   ! geometry
   type preproc_geo_t
      real(kind=sreal), dimension(:,:,:), pointer :: solza,solazi,satza,satazi,relazi
   end type preproc_geo_t


   ! optional processing variables, typically defined through the driver file
   type preproc_opts_t
      logical                    :: do_co2
      logical                    :: do_gsics
      logical                    :: disable_snow_ice_corr
      logical                    :: do_cloud_emis
      logical                    :: do_cloud_type
      logical                    :: do_ironly
      integer                    :: ecmwf_nlevels
      integer                    :: ecmwf_time_int_method
      integer, pointer           :: channel_ids(:)
      integer(kind=lint)         :: n_channels
      integer                    :: slstr_alignment
      real                       :: swansea_gamma
      logical                    :: use_camel_emis
      logical                    :: use_ecmwf_snow_and_ice
      logical                    :: use_hr_ecmwf
      logical                    :: use_l1_land_mask
      logical                    :: use_modis_emis_in_rttov
      logical                    :: use_occci
      logical                    :: use_predef_geo
      logical                    :: use_predef_lsm
      logical                    :: use_swansea_climatology

      character(len=path_length) :: ecmwf_path(2)
      character(len=path_length) :: ecmwf_path2(2)
      character(len=path_length) :: ecmwf_path3(2)
      character(len=path_length) :: ecmwf_path_hr(2)
      character(len=path_length) :: ecmwf_HR_path_file(2)
      character(len=path_length) :: ecmwf_path_file(2)
      character(len=path_length) :: ecmwf_path_file2(2)
      character(len=path_length) :: ecmwf_path_file3(2)
      character(len=path_length) :: ext_lsm_path
      character(len=path_length) :: ext_geo_path
      character(len=path_length) :: occci_path
      character(len=path_length) :: product_name

   end type preproc_opts_t

   ! ecmwf profiles and surface fields (prtm data)
   type preproc_prtm_t
      real(kind=sreal), dimension(:,:,:), pointer :: pressure, temperature, &
                                                     spec_hum, ozone
      real(kind=sreal), dimension(:,:,:), pointer :: phi_lev, phi_lay

      real(kind=sreal), dimension(:,:), pointer   :: geopot, lnsp
      real(kind=sreal), dimension(:,:), pointer   :: u10, v10
      real(kind=sreal), dimension(:,:), pointer   :: land_sea_mask
      real(kind=sreal), dimension(:,:), pointer   :: temp2, skin_temp
      real(kind=sreal), dimension(:,:), pointer   :: snow_albedo, snow_depth
      real(kind=sreal), dimension(:,:), pointer   :: sst, sea_ice_cover
      real(kind=sreal), dimension(:,:), pointer   :: totcolwv

      ! New field for Convective Available Potential Energy
      real(kind=sreal), dimension(:,:), pointer   :: cape

      ! New fields for tropopause
      real(kind=sreal), dimension(:,:), pointer   :: trop_p, trop_t
   end type preproc_prtm_t


   ! surface albedo and emissivity
   type preproc_surf_t
!     real(kind=sreal), dimension(:,:,:), pointer :: albedo
      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
   end type preproc_surf_t


   type preproc_cld_t
      real(kind=sreal), dimension(:,:,:), pointer :: clear_bt
      real(kind=sreal), dimension(:,:,:), pointer :: cloud_bt
   end type preproc_cld_t


   type preproc_paths_t
      character(len=file_length) :: alb_file    ! Surface albedo/BRDF
      character(len=file_length) :: cf_file     ! Cloud flagging
      character(len=file_length) :: config_file ! Configuration parameters
      character(len=file_length) :: geo_file    ! Viewing angles and geolocation
      character(len=file_length) :: loc_file    ! Lat/lon location
      character(len=file_length) :: lsf_file    ! Land-sea flag
      character(len=file_length) :: lwrtm_file  ! Longwave RTTOV inputs
      character(len=file_length) :: msi_file    ! Radiances/brightness temps
      character(len=file_length) :: prtm_file   ! Atmospheric RTTOV inputs
      character(len=file_length) :: swrtm_file  ! Shortwave RTTOV inputs
   end type preproc_paths_t


   type setup_args_t
      character(len=path_length)     :: l1b_file ! Path to satellite swath
      character(len=path_length)     :: geo_file ! Path to geolocation data

      character(len=sensor_length)   :: sensor   ! Name of instrument
      character(len=platform_length) :: platform ! Name of satellite it is on
      ! Date as strings
      character(len=date_length)     :: cyear, cmonth, cday
      character(len=date_length)     :: cdoy, chour, cminute, csecond
      ! Date as numbers
      integer(kind=sint)             :: doy, year, month, day
      integer(kind=sint)             :: hour, minute, second
      ! Start/end of requested subset
      integer(kind=lint)             :: startx, endx, starty, endy
      ! Dimensions of the satellite swath
      integer(kind=lint)             :: n_across_track, n_along_track
      ! Lengths and offsets for the second section of nighttime data in
      ! an (A)ATSR orbit file
      integer(kind=lint)             :: n_along_track2
      integer(kind=lint)             :: along_track_offset, along_track_offset2
      integer(kind=sint)             :: day_night
   end type setup_args_t

contains

#include "allocate_preproc_structures.F90"
#include "deallocate_preproc_structures.F90"


subroutine linearly_combine_prtms(a, b, prtm1, prtm2, prtm)

   implicit none

   real,                 intent(in)  :: a
   real,                 intent(in)  :: b
   type(preproc_prtm_t), intent(in)  :: prtm1
   type(preproc_prtm_t), intent(in)  :: prtm2
   type(preproc_prtm_t), intent(out) :: prtm

   prtm%pressure      = a * prtm1%pressure      + b * prtm2%pressure
   prtm%temperature   = a * prtm1%temperature   + b * prtm2%temperature
   prtm%spec_hum      = a * prtm1%spec_hum      + b * prtm2%spec_hum
   prtm%ozone         = a * prtm1%ozone         + b * prtm2%ozone
   prtm%phi_lev       = a * prtm1%phi_lev       + b * prtm2%phi_lev
   prtm%phi_lay       = a * prtm1%phi_lay       + b * prtm2%phi_lay
   prtm%geopot        = a * prtm1%geopot        + b * prtm2%geopot
   prtm%lnsp          = a * prtm1%lnsp          + b * prtm2%lnsp
   prtm%u10           = a * prtm1%u10           + b * prtm2%u10
   prtm%v10           = a * prtm1%v10           + b * prtm2%v10
   prtm%land_sea_mask = a * prtm1%land_sea_mask + b * prtm2%land_sea_mask
   prtm%temp2         = a * prtm1%temp2         + b * prtm2%temp2
   prtm%skin_temp     = a * prtm1%skin_temp     + b * prtm2%skin_temp
   prtm%snow_albedo   = a * prtm1%snow_albedo   + b * prtm2%snow_albedo
   prtm%snow_depth    = a * prtm1%snow_depth    + b * prtm2%snow_depth
   prtm%sst           = a * prtm1%sst           + b * prtm2%sst
   prtm%sea_ice_cover = a * prtm1%sea_ice_cover + b * prtm2%sea_ice_cover
   prtm%totcolwv      = a * prtm1%totcolwv      + b * prtm2%totcolwv
#ifdef INCLUDE_SATWX
   prtm%cape          = a * prtm1%cape          + b * prtm2%cape
   prtm%trop_p        = a * prtm1%trop_p        + b * prtm2%trop_p
#endif

end subroutine linearly_combine_prtms

end module preproc_structures_m
