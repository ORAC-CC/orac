!-------------------------------------------------------------------------------
! Name: postproc_constants.F90
!
! Purpose: F90 Module file which declares variable types and some parameters.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/03, MJ: cleans out prototype code to prepare repository upload.
! 2012/03/06, CP: modified to produce post processed files
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2014/04/01, MJ: fixes some problems/cleanup with illumination
! 2014/11/10, OS: very minor edit
! 2014/12/03, CP: removed variables duplicated in common_constants
! 2015/02/05, OS: some cleanup; changed nint to lint
! 2015/02/07, CP: removed all variables common to common_constants
! 2015/04/23, OS: added variables related to NETCDF4 compression
! 2015/07/16, GM: Major cleanup.
! 2015/07/26, GM: Changed type specific deflate levels and shuffling flags to
!    just one.
! 2015/07/26, GM: Removed unused constants.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module postproc_constants

   use common_constants

   implicit none

   integer, parameter :: SolarBit=0
   integer, parameter :: ThermalBit=1

   real(kind=sreal), parameter :: dither=1.0E-5

   ! NetCDF deflate level
   integer, parameter :: deflate_level=9

   ! Shuffling to improve compression
   logical, parameter :: shuffle_flag=.TRUE.

end module postproc_constants
