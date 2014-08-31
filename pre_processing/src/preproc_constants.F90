!-------------------------------------------------------------------------------
! Name: preproc_constants.F90
!
! Purpose:
! Define here data types, string lengths, constants etc.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
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
! 2014/08/30, GM: Removed pi and d2r as they are in common_constants.
! 2014/08/31, GM: Removed unused lengths and made compress_level_*
!    and shuffle_* names consistent with their ORAC type names.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module preproc_constants

   use common_constants

   implicit none

   integer, parameter :: attribute_length=128
   integer, parameter :: attribute_length_long=2048
   integer, parameter :: cmd_arg_length=16
   integer, parameter :: date_length=4
   integer, parameter :: file_length=512
   integer, parameter :: path_length=1024
   integer, parameter :: var_length=64
   integer, parameter :: platform_length=8
   integer, parameter :: sensor_length=8


   real(kind=sreal),    parameter :: dither=1.0E-3
   real(kind=sreal),    parameter :: dither_more=1.0E-7


   ! Physical constants
   real(kind=sreal),    parameter :: hpa2pa=0.01

   ! Specific gas constants of dry air and water vapor
   real(kind=sreal),    parameter :: r_dry_air=287.05
   real(kind=sreal),    parameter :: r_water_vap=461.51

   ! WMO defined gravity constant which is constant  for all latitude and heights
   ! used to convert from gph (m^2/s^2) to height (m) by h=gph/g_wmo
   real(kind=sreal),    parameter :: g_wmo=9.80665


   real(kind=sreal),    parameter :: maxsza_day=80.0
   real(kind=sreal),    parameter :: maxsza_twi=110.0


   ! Some netcdf4 related parameters

   ! Compression levels for variables of different type (0:none,9:maximum)
   integer(kind=lint),  parameter :: compress_level_byte=0
   integer(kind=lint),  parameter :: compress_level_sint=0
   integer(kind=lint),  parameter :: compress_level_lint=0
   integer(kind=lint),  parameter :: compress_level_sreal=0
   integer(kind=lint),  parameter :: compress_level_dreal=0

   integer(kind=lint),  parameter :: max_chunk_latlon=10000

   ! Turn on shuffling to improve compression
   logical,             parameter :: shuffle_byte=.FALSE.
   logical,             parameter :: shuffle_sint=.FALSE.
   logical,             parameter :: shuffle_lint=.FALSE.
   logical,             parameter :: shuffle_float=.FALSE.
   logical,             parameter :: shuffle_double=.FALSE.

end module preproc_constants
