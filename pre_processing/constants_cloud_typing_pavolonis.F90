!--------------------------------------------------------------------------------------
! Clouds from AVHRR Extended (CLAVR-x) 1b PROCESSING SOFTWARE Version 5.3
!
! NAME: constant.f90 (src)
!       CONSTANTS (program)
!
! PURPOSE: store and serve various constants for use in the CLAVR-x system
!
! DESCRIPTION: 
!
! AUTHORS:
!  Andrew Heidinger, Andrew.Heidinger@noaa.gov
!
! COPYRIGHT
! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
! SUPPORT TO USERS.
!
! (c) This code is copyrighted by the author and all NOAA restrictions apply
! 
! Reference: CLAVR-x system description document
!
! Dependencies:  None
!
! Calling Sequece:
!   use CONSTANTS
!
! Public Routines within in this Module: None
!--------------------------------------------------------------------------------------
! Cloud_cci CC4CL
!
! NAME: constants_cloud_typing_pavolonis.f90 (src)
!       CONSTANTS_CLOUD_TYPING_PAVOLONIS (module)
!
! PURPOSE: store and serve various constants for use in CC4CL 
!          pavolonis phase descrimination part
!
! NOTE:
!  part of the original code of constants.f90 used in cloud_type.f90 (see above)
!
! History:
!  23rd Oct 2014, CS: original version
!  1st  Dec 2014, OS: added PROB_OPAQUE_ICE_TYPE
!
! Calling Sequece:
!   use CONSTANTS_CLOUD_TYPING_PAVOLONIS
!
!--------------------------------------------------------------------------------------
module CONSTANTS_CLOUD_TYPING_PAVOLONIS

  !--- sint defined here
  use COMMON_CONSTANTS

  implicit none

  !--- define a structure of symbols of clarity
  TYPE, public ::symbol_struct

     !--- cldmask OUTPUT
     INTEGER(kind=sint) :: CLOUDY = 1
     INTEGER(kind=sint) :: CLEAR = 0

     !--- ann_cloud_mask
     REAL(kind=sreal)   :: COT_THRES_SEA = 0.05       !obsolete
     REAL(kind=sreal)   :: COT_THRES_SEA_ICE = 0.5    !obsolete
     REAL(kind=sreal)   :: COT_THRES_LAND = 0.3       !obsolete
     REAL(kind=sreal)   :: COT_THRES_DAY_SEA_ICE = 0.4 
     REAL(kind=sreal)   :: COT_THRES_DAY_LAND_ICE = 0.35
     REAL(kind=sreal)   :: COT_THRES_DAY_SEA = 0.1
     REAL(kind=sreal)   :: COT_THRES_DAY_LAND = 0.3
     REAL(kind=sreal)   :: COT_THRES_NIGHT_SEA_ICE = 0.4
     REAL(kind=sreal)   :: COT_THRES_NIGHT_LAND_ICE = 0.35
     REAL(kind=sreal)   :: COT_THRES_NIGHT_SEA = 0.2
     REAL(kind=sreal)   :: COT_THRES_NIGHT_LAND = 0.3

     !--- cldtype OUTPUT
     INTEGER(kind=sint) :: CLEAR_TYPE = 0
     !INTEGER(kind=sint) :: PROB_CLEAR_TYPE = 1
     INTEGER(kind=sint) :: FOG_TYPE = 2
     INTEGER(kind=sint) :: WATER_TYPE = 3
     INTEGER(kind=sint) :: SUPERCOOLED_TYPE = 4
     INTEGER(kind=sint) :: MIXED_TYPE = 5
     INTEGER(kind=sint) :: OPAQUE_ICE_TYPE = 6
     INTEGER(kind=sint) :: CIRRUS_TYPE = 7
     INTEGER(kind=sint) :: OVERLAP_TYPE = 8
     INTEGER(kind=sint) :: PROB_OPAQUE_ICE_TYPE = 9 ! missing ch3.7 due to low S/N

     !--- used for sunglint_mask, nise_mask
     INTEGER(kind=sint) :: NO = 0
     INTEGER(kind=sint) :: YES = 1
     !--- used for ch3a_on_avhrr_flag (neither Ch3a nor Ch3b)
     INTEGER(kind=sint) :: INEXISTENT = -1

     !--- USGS: land use class (24 bit flags)
     !--- Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc
     INTEGER(kind=sint) :: URBAN_AND_BUILTUP_LAND = 1
     INTEGER(kind=sint) :: DRYLAND_CROPLAND_AND_PASTURE = 2
     INTEGER(kind=sint) :: IRRIGATED_CROPLAND_AND_PASTURE = 3
     INTEGER(kind=sint) :: MIXED_DRYLAND_IRRIGATED_CROPLAND_AND_PASTURE = 4
     INTEGER(kind=sint) :: CROPLAND_GRASSLAND_MOSAIC = 5
     INTEGER(kind=sint) :: CROPLAND_WOODLAND_MOSAIC = 6
     INTEGER(kind=sint) :: GRASSLAND = 7
     INTEGER(kind=sint) :: SHRUBLAND = 8
     INTEGER(kind=sint) :: MIXED_SHRUBLAND_GRASSLAND = 9
     INTEGER(kind=sint) :: SAVANNA = 10
     INTEGER(kind=sint) :: DECIDUOUS_BROADLEAF_FOREST = 11
     INTEGER(kind=sint) :: DECIDUOUS_NEEDLELEAF_FOREST = 12
     INTEGER(kind=sint) :: EVERGREEN_BROADLEAF_FOREST = 13
     INTEGER(kind=sint) :: EVERGREEN_NEEDLELEAF_FOREST = 14
     INTEGER(kind=sint) :: MIXED_FOREST = 15
     INTEGER(kind=sint) :: WATER_BODIES = 16
     INTEGER(kind=sint) :: WATER_FLAG = 16       ! used in cloud_type subroutine
     INTEGER(kind=sint) :: HERBACEOUS_WETLAND = 17
     INTEGER(kind=sint) :: WOODED_WETLAND = 18
     INTEGER(kind=sint) :: BARREN_OR_SPARSELY_VEGETATED = 19
     INTEGER(kind=sint) :: DESERT_FLAG = 19      ! used in cloud_type subroutine
     INTEGER(kind=sint) :: HERBACEOUS_TUNDRA = 20
     INTEGER(kind=sint) :: WOODED_TUNDRA = 21
     INTEGER(kind=sint) :: MIXED_TUNDRA = 22
     INTEGER(kind=sint) :: BARE_GROUND_TUNDRA = 23
     INTEGER(kind=sint) :: SNOW_OR_ICE = 24

     !!--- LandCover_CCI map (22 bit flags)

     !--- SNOW/ICE Flag based on NISE aux. data
     INTEGER(kind=sint) :: NISE_FLAG = 30        ! used in cloud_type subroutine


!!!--- used in original code cloud_type.f90
!!!--- this apply to the sfc_type array 
     !!INTEGER(kind=sint) :: WATER_SFC = 0
     !!INTEGER(kind=sint) :: EVERGREEN_NEEDLE_SFC = 1
     !!INTEGER(kind=sint) :: EVERGREEN_BROAD_SFC = 2
     !!INTEGER(kind=sint) :: DECIDUOUS_NEEDLE_SFC = 3
     !!INTEGER(kind=sint) :: DECIDUOUS_BROAD_SFC = 4
     !!INTEGER(kind=sint) :: MIXED_FORESTS_SFC = 5
     !!INTEGER(kind=sint) :: WOODLANDS_SFC = 6
     !!INTEGER(kind=sint) :: WOODED_GRASS_SFC = 7
     !!INTEGER(kind=sint) :: CLOSED_SHRUBS_SFC = 8
     !!INTEGER(kind=sint) :: OPEN_SHRUBS_SFC = 9
     !!INTEGER(kind=sint) :: GRASSES_SFC = 10
     !!INTEGER(kind=sint) :: CROPLANDS_SFC = 11
     !!INTEGER(kind=sint) :: BARE_SFC = 12
     !!INTEGER(kind=sint) :: URBAN_SFC = 13

  END TYPE symbol_struct

  TYPE(symbol_struct), public, save :: sym

end module CONSTANTS_CLOUD_TYPING_PAVOLONIS
