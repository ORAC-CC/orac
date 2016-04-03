!-------------------------------------------------------------------------------
! Name: read_modis.F90
!
! Purpose:
! Module for MODIS read routines.
!
! History:
! 2014/05/15, GM: First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_modis_m

   implicit none

contains

include 'get_modis_time.F90'
include 'map_time_to_pixel.F90'
include 'modis_bright.F90'
include 'read_modis_l1b_radiances.F90'
include 'read_modis_l1b_radiances_2.F90'
include 'read_modis_angles.F90'
include 'read_modis_dimensions.F90'
include 'read_modis_land_sea_mask.F90'
include 'read_modis_lat_lon.F90'
include 'read_modis_time.F90'
include 'read_modis_time_lat_lon_angles.F90'

end module read_modis_m
