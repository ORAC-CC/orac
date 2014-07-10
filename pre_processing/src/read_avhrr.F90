!-------------------------------------------------------------------------------
! Name: read_avhrr.F90
!
! Purpose:
! Container for AVHRR read routines.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

module read_avhrr

implicit none

contains

include 'create_time_for_pixel.F90'
include 'read_L1B_avhrr_reflectances_radiances.F90'
include 'read_avhrr_angles.F90'
include 'read_avhrr_dimensions.F90'
include 'read_avhrr_geo.F90'
include 'read_avhrr_l1b.F90'
include 'read_avhrr_landseamask.F90'
include 'read_avhrr_lat_lon.F90'
include 'read_avhrr_lsmask.F90'
include 'read_avhrr_time.F90'

end module read_avhrr
