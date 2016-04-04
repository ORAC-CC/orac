!-------------------------------------------------------------------------------
! Name: preproc_constants.F90
!
! Purpose:
! Module defining data types, string lengths, constants etc.
!
! History:
! 2011/12/09, MJ: produces draft code with basic data types and lengths
! 2012/04/19, GT: Added value of pi.
! 2013/11/27, MJ: adds parameters for netcdf4 compression
! 2014/01/23, MJ: switches nc4 shuffling off.
! 2014/01/30, MJ: removes parameters for chunking.
! 2014/03/11, MJ: introduces variable for file chunking.
! 2014/07/11, AP: Removing unnecessary fields. Shortening date_length.
! 2014/08/05, AP: Adding missing fill value.
! 2014/08/06, GM: Corrected the value for Pi.
! 2014/08/06, GM: d2r is a derived constant. It should be computed.
! 2014/08/30, GM: Removed pi and d2r as they are in common_constants_m.
! 2014/08/31, GM: Removed unused lengths and made compress_level_* and
!    shuffle_* names consistent with their ORAC type names.
! 2014/09/16, GM: Rename the deflate_level_* and shuffle_* parameters to be
!    consistent with ORAC kinds.
! 2015/07/26, GM: Changed type specific deflate levels and shuffling flags to
!    just one.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module preproc_constants_m

   use common_constants_m

   implicit none


   real(kind=sreal),   parameter :: dither=1.0E-3
   real(kind=sreal),   parameter :: dither_more=1.0E-7


   ! Physical constants
   real(kind=sreal),   parameter :: pa2hpa=0.01

   ! Specific gas constants of dry air and water vapor
   real(kind=sreal),   parameter :: r_dry_air=287.05
   real(kind=sreal),   parameter :: r_water_vap=461.51


   real(kind=sreal),   parameter :: maxsza_day=80.0
   real(kind=sreal),   parameter :: maxsza_twi=110.0


   ! Some NetCDF related parameters

   ! NetCDF deflate level
   integer(kind=lint), parameter :: deflate_level=0

   ! Shuffling to improve compression
   logical,            parameter :: shuffle_flag=.FALSE.

end module preproc_constants_m
