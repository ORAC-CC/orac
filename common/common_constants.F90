!-------------------------------------------------------------------------------
! Name: common_constants.F90
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
! 2014/08/15, GM: Original version.
! 2014/08/30, GM: Change integer fill values to be consistent with the main
!    processor.
! 2014/08/30, GM: Added pi and d2r.
! 2015/12/15, AP: Move IRho terms from ECP_constants.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module common_constants

   implicit none

   ! Type kind value
   integer, parameter :: byte=1
   integer, parameter :: sint=2
   integer, parameter :: lint=4
   integer, parameter :: sreal=4
   integer, parameter :: dreal=8

   integer, parameter :: cmd_arg_length=128
   integer, parameter :: file_length=512
   integer, parameter :: path_length=2048

   integer, parameter :: date_length=4
   integer, parameter :: platform_length=8
   integer, parameter :: sensor_length=8

   integer, parameter :: attribute_length=128
   integer, parameter :: attribute_length_long=2048
   integer, parameter :: unitlength=128
   integer, parameter :: var_length=64

   integer, parameter :: MAX_NC_NAME=256
   integer, parameter :: MAX_VAR_DIMS=32

   ! Error code to give back to the system on an error
   integer, parameter :: error_stop_code = 1

   ! ORAC fill values
   integer(kind=byte),  parameter :: byte_fill_value=-127
   integer(kind=sint),  parameter :: sint_fill_value=-32767
   integer(kind=lint),  parameter :: lint_fill_value=-32767
   real(kind=sreal),    parameter :: sreal_fill_value=-999.0
   real(kind=dreal),    parameter :: dreal_fill_value=-999.0

   ! Mathematical constants
   real(kind=sreal),    parameter :: pi=3.14159265
   real(kind=sreal),    parameter :: d2r=pi/180.0

   ! Indices of surface reflectance terms
   integer, parameter :: IRho_0V          = 1 ! Index of rho_0v data in array
   integer, parameter :: IRho_0D          = 2 !  "    "  rho_0d  "   "   "
   integer, parameter :: IRho_DV          = 3 !  "    "  rho_dv  "   "   "
   integer, parameter :: IRho_DD          = 4 !  "    "  rho_dd  "   "   "
   integer, parameter :: MaxRho_XX        = 4 ! Max no. of BRDF parameters

   ! Bit positions used in Ch_Is flags
   integer, parameter :: SolarBit         = 0
   integer, parameter :: ThermalBit       = 1

   integer, parameter :: MaxNumMeas       = 36 ! Max no. of measurement channels
   integer, parameter :: MaxNumViews      = 2  ! Max no. of measurement views

end module common_constants
