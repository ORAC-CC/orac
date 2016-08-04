!-------------------------------------------------------------------------------
! Name: constants_cloud_typing_pavolonis.F90
!
! Purpose:
! Defines parameters used by the Pavolonis cloud type identification code.
! Derived from constant.f90 by Andrew Heidinger, Andrew.Heidinger@noaa.gov
! within Clouds from AVHRR Extended (CLAVR-x) 1b PROCESSING SOFTWARE Version 5.3
!
! History:
! 2014/10/23, CS: Original version.
! 2014/12/01, OS: Added PROB_OPAQUE_ICE_TYPE.
! 2015/04/22, OS: Added new ANN cloud mask thresholds.
! 2015/07/02, OS: Added uncertainty constants variables.
! 2015/07/27, AP: Converted from a structure of variables to a module of
!    parameters (to be consistent with similar files elsewhere in the code).
! 2015/11/17, OS: Added cloud types for phase switching.
! 2015/11/27, CP: Added prob clear type
! 2015/12/17, OS: Added twilight NN thresholds
! 2015/12/17, OS: Changed twilight NN thresholds for land_ice and sea
! 2016/02/23, OS: Changed day land threshold to 0.2 (from 0.3)
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module constants_cloud_typing_pavolonis_m

   use common_constants_m

   implicit none

   !--- cldmask OUTPUT
   integer(sint), parameter :: CLOUDY = 1
   integer(sint), parameter :: CLEAR = 0

   !--- constants used to determine cldmask uncertainty
   real(sreal),   parameter :: CLEAR_UNC_MIN = 11.925 !24.364
   real(sreal),   parameter :: CLEAR_UNC_MAX = 49.200 !60.066
   real(sreal),   parameter :: CLOUDY_UNC_MIN = 1.862 !1.533
   real(sreal),   parameter :: CLOUDY_UNC_MAX = 55.995 !43.620

   !--- ann_cloud_mask
   real(sreal),   parameter :: COT_THRES_DAY_SEA_ICE = 0.4
   real(sreal),   parameter :: COT_THRES_DAY_LAND_ICE = 0.35
   real(sreal),   parameter :: COT_THRES_DAY_SEA = 0.1
   real(sreal),   parameter :: COT_THRES_DAY_LAND = 0.2
   real(sreal),   parameter :: COT_THRES_NIGHT_SEA_ICE = 0.4
   real(sreal),   parameter :: COT_THRES_NIGHT_LAND_ICE = 0.35
   real(sreal),   parameter :: COT_THRES_NIGHT_SEA = 0.2
   real(sreal),   parameter :: COT_THRES_NIGHT_LAND = 0.3
   real(sreal),   parameter :: COT_THRES_TWL_SEA_ICE = 0.4
   real(sreal),   parameter :: COT_THRES_TWL_LAND_ICE = 0.4
   real(sreal),   parameter :: COT_THRES_TWL_SEA = 0.35
   real(sreal),   parameter :: COT_THRES_TWL_LAND = 0.3
   !---
   real(sreal),   parameter :: NOAA7_9_CH3B_BT_THRES=240 ! Dont use 3.7 µm channel at night,
                                                     ! if BT of ch3b is below this value.
   !---

   !--- cldtype OUTPUT
   integer(sint), parameter :: CLEAR_TYPE = 0
   integer(sint), parameter :: SWITCHED_TO_WATER_TYPE = 1
   integer(sint), parameter :: FOG_TYPE = 2
   integer(sint), parameter :: WATER_TYPE = 3
   integer(sint), parameter :: SUPERCOOLED_TYPE = 4
   integer(sint), parameter :: SWITCHED_TO_ICE_TYPE = 5
   integer(sint), parameter :: OPAQUE_ICE_TYPE = 6
   integer(sint), parameter :: CIRRUS_TYPE = 7
   integer(sint), parameter :: OVERLAP_TYPE = 8
   integer(sint), parameter :: PROB_OPAQUE_ICE_TYPE = 9 ! missing ch3.7 due to low S/N
   integer(sint), parameter :: PROB_CLEAR_TYPE = 10 ! cold antarctic

   !--- used for sunglint_mask, nise_mask
   integer(sint), parameter :: NO = 0
   integer(sint), parameter :: YES = 1
   !--- used for, parameter ch3a_on_avhrr_flag (neither Ch3a nor Ch3b)
   integer(sint), parameter :: INEXISTENT = -1

   !--- USGS: land use class (24 bit flags)
   !--- Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc
   integer(sint), parameter :: URBAN_AND_BUILTUP_LAND = 1
   integer(sint), parameter :: DRYLAND_CROPLAND_AND_PASTURE = 2
   integer(sint), parameter :: IRRIGATED_CROPLAND_AND_PASTURE = 3
   integer(sint), parameter :: MIXED_DRYLAND_IRRIGATED_CROPLAND_AND_PASTURE = 4
   integer(sint), parameter :: CROPLAND_GRASSLAND_MOSAIC = 5
   integer(sint), parameter :: CROPLAND_WOODLAND_MOSAIC = 6
   integer(sint), parameter :: GRASSLAND = 7
   integer(sint), parameter :: SHRUBLAND = 8
   integer(sint), parameter :: MIXED_SHRUBLAND_GRASSLAND = 9
   integer(sint), parameter :: SAVANNA = 10
   integer(sint), parameter :: DECIDUOUS_BROADLEAF_FOREST = 11
   integer(sint), parameter :: DECIDUOUS_NEEDLELEAF_FOREST = 12
   integer(sint), parameter :: EVERGREEN_BROADLEAF_FOREST = 13
   integer(sint), parameter :: EVERGREEN_NEEDLELEAF_FOREST = 14
   integer(sint), parameter :: MIXED_FOREST = 15
   integer(sint), parameter :: WATER_BODIES = 16
   integer(sint), parameter :: WATER_FLAG = 16       ! used in cloud_type subroutine
   integer(sint), parameter :: HERBACEOUS_WETLAND = 17
   integer(sint), parameter :: WOODED_WETLAND = 18
   integer(sint), parameter :: BARREN_OR_SPARSELY_VEGETATED = 19
   integer(sint), parameter :: DESERT_FLAG = 19      ! used in cloud_type subroutine
   integer(sint), parameter :: HERBACEOUS_TUNDRA = 20
   integer(sint), parameter :: WOODED_TUNDRA = 21
   integer(sint), parameter :: MIXED_TUNDRA = 22
   integer(sint), parameter :: BARE_GROUND_TUNDRA = 23
   integer(sint), parameter :: SNOW_OR_ICE = 24

   !!--- LandCover_CCI map (22 bit flags)

   !--- SNOW/ICE Flag based on NISE aux. data
   integer(sint), parameter :: NISE_FLAG = 30        ! used in cloud_type subroutine


   ! Used in original code cloud_type.f90, these apply to the sfc_type array
!  integer(sint), parameter :: WATER_SFC = 0
!  integer(sint), parameter :: EVERGREEN_NEEDLE_SFC = 1
!  integer(sint), parameter :: EVERGREEN_BROAD_SFC = 2
!  integer(sint), parameter :: DECIDUOUS_NEEDLE_SFC = 3
!  integer(sint), parameter :: DECIDUOUS_BROAD_SFC = 4
!  integer(sint), parameter :: MIXED_FORESTS_SFC = 5
!  integer(sint), parameter :: WOODLANDS_SFC = 6
!  integer(sint), parameter :: WOODED_GRASS_SFC = 7
!  integer(sint), parameter :: CLOSED_SHRUBS_SFC = 8
!  integer(sint), parameter :: OPEN_SHRUBS_SFC = 9
!  integer(sint), parameter :: GRASSES_SFC = 10
!  integer(sint), parameter :: CROPLANDS_SFC = 11
!  integer(sint), parameter :: BARE_SFC = 12
!  integer(sint), parameter :: URBAN_SFC = 13

end module constants_cloud_typing_pavolonis_m
