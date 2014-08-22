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
!
! $Id: common_constants.F90 2306 2014-08-15 13:56:24Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module common_constants

   implicit none

   integer, parameter :: byte=1
   integer, parameter :: sint=2
   integer, parameter :: lint=4
   integer, parameter :: sreal=4
   integer, parameter :: dreal=8

   integer, parameter :: unitlength=75

   ! Error code to give back to the system on an error
   integer, parameter :: error_stop_code = 1

   integer(kind=byte),  parameter :: byte_fill_value=-1
   integer(kind=sint),  parameter :: sint_fill_value=-999
   integer(kind=lint),  parameter :: lint_fill_value=-999
   real(kind=sreal),    parameter :: sreal_fill_value=-999.0
   real(kind=dreal),    parameter :: dreal_fill_value=-999.0

end module common_constants
