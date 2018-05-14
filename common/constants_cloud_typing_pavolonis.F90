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
! 2017/05/17, OS: Added ann phase parameters and thresholds, altered cmask
!    thresholds, added logicals whether to do certain cccot corrections,
!    included regression coefficients for spectral response correction
! 2017/06/29, SS: Added cloud phase uncertainty thresholds, altered cloud phase
!    thresholds
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
   real(sreal),   parameter :: CLEAR_UNC_MIN = 10.4475  !v2.0:11.925
   real(sreal),   parameter :: CLEAR_UNC_MAX = 48.8452  !v2.0:49.200
   real(sreal),   parameter :: CLOUDY_UNC_MIN = 0.00000 !v2.0:1.862
   real(sreal),   parameter :: CLOUDY_UNC_MAX = 60.8194 !v2.0:55.995

   !--- ann_cloud_mask_thresholds
   real(sreal),   parameter :: COT_THRES_DAY_SEA_ICE = 0.4
   real(sreal),   parameter :: COT_THRES_DAY_LAND_ICE = 0.3 !0.2 !0.35
   real(sreal),   parameter :: COT_THRES_DAY_SEA = 0.25
   real(sreal),   parameter :: COT_THRES_DAY_LAND = 0.3
   real(sreal),   parameter :: COT_THRES_NIGHT_SEA_ICE = 0.45!0.4
   real(sreal),   parameter :: COT_THRES_NIGHT_LAND_ICE = 0.35!0.25 !0.35
   real(sreal),   parameter :: COT_THRES_NIGHT_SEA = 0.25!0.2
   real(sreal),   parameter :: COT_THRES_NIGHT_LAND = 0.30!0.3
   real(sreal),   parameter :: COT_THRES_TWL_SEA_ICE = 0.50!0.5 !0.4
   real(sreal),   parameter :: COT_THRES_TWL_LAND_ICE = 0.35 !0.4
   real(sreal),   parameter :: COT_THRES_TWL_SEA = 0.35!0.35
   real(sreal),   parameter :: COT_THRES_TWL_LAND = 0.45!0.3
   !---

   !--- ann corrections
   logical, parameter :: correct_bounds = .TRUE. ! check if input is within trained range and set
                                                 ! input to minmax of trained
                                                 ! value if necessary
   logical, parameter :: correct_sst    = .TRUE. ! reduce cccot for very cold  sea surfaces
   logical, parameter :: correct_view   = .TRUE. ! correct cccot depending on viewing angle
   logical, parameter :: correct_skint  = .TRUE. ! if surface is colder than  BT11 temp. set
   ! surface to BT11; only at night
   logical, parameter :: correct_glint  = .TRUE. ! correct input reflectances with their albedo to
                                                 ! reduce sunglint impact
   logical, parameter :: correct_w_unc  = .FALSE. ! use uncertainty to correct
                                                 ! cloudy pixels , so far only
                                                 ! noaa12 and noaa15 (AM Sats
                                                 ! with strong sunglint)
   logical, parameter :: correct_early_morning_noaas = .TRUE. ! At daytime use ANN Night w/o 3.7
                                                              ! for Noaa12 and Noaa15 (high sunglint at high view)

   !--- cldphase OUTPUT
   integer(sint), parameter :: ICE = 2
   integer(sint), parameter :: LIQUID = 1

   !--- constants used to determine cldphase uncertainty
   real(sreal),   parameter :: LIQUID_UNC_MIN = 9.25893
   real(sreal),   parameter :: LIQUID_UNC_MAX = 59.0115
   real(sreal),   parameter :: ICE_UNC_MIN    = 0.00000
   real(sreal),   parameter :: ICE_UNC_MAX    = 43.2141

   !--- ann_cloud_phase_thresholds
   real(sreal),   parameter :: COT_CPH_THRES_DAY_SEA_ICE = 0.50
   real(sreal),   parameter :: COT_CPH_THRES_DAY_LAND_ICE = 0.70
   real(sreal),   parameter :: COT_CPH_THRES_DAY_SEA = 0.55
   real(sreal),   parameter :: COT_CPH_THRES_DAY_LAND = 0.70
   real(sreal),   parameter :: COT_CPH_THRES_NIGHT_SEA_ICE = 0.70
   real(sreal),   parameter :: COT_CPH_THRES_NIGHT_LAND_ICE = 0.60
   real(sreal),   parameter :: COT_CPH_THRES_NIGHT_SEA = 0.50
   real(sreal),   parameter :: COT_CPH_THRES_NIGHT_LAND = 0.65
   real(sreal),   parameter :: COT_CPH_THRES_TWL_SEA_ICE = 0.70
   real(sreal),   parameter :: COT_CPH_THRES_TWL_LAND_ICE = 0.90
   real(sreal),   parameter :: COT_CPH_THRES_TWL_SEA = 0.65
   real(sreal),   parameter :: COT_CPH_THRES_TWL_LAND = 0.50
   !--
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

#include "coefficients_spectral_response_correction.inc"

end module constants_cloud_typing_pavolonis_m
