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
!
! $Id: common_constants.F90 2306 2014-08-15 13:56:24Z gmcgarragh $
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

   integer, parameter :: unitlength=75

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

end module common_constants
