!-------------------------------------------------------------------------------
! Name: read_avhrr.F90
!
! Purpose:
! Module for AVHRR read routines.
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_avhrr_m

   implicit none

contains

#include "create_time_for_pixel.F90"
#include "read_avhrr_angles.F90"
#include "read_avhrr_dimensions.F90"
#include "read_avhrr_l1b_radiances.F90"
#include "read_avhrr_l1b_radiances_2.F90"
#include "read_avhrr_land_sea_mask.F90"
#include "read_avhrr_land_sea_mask_2.F90"
#include "read_avhrr_lat_lon.F90"
#include "read_avhrr_time.F90"
#include "read_avhrr_time_lat_lon_angles.F90"

end module read_avhrr_m
