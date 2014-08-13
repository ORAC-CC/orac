!-------------------------------------------------------------------------------
! Name:
!    check_value
!
! Purpose:
!    Ensure a specified scalar or array is within given limits. If not,
!    report this in the SPixel structure.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    value    real|byte    In          The values to be tested. Can currently be
!                                      a scalar, 1D, or 2D array.
!    max      as above     In          The maximum valid value
!    min      as above     In          The minimum valid value
!    SPixel   alloc struct Both        Super-pixel structure
!    name     string       In          A description of the field to be printed
!                                      in the log file in the event of an error.
!    flag_bit integer      In          The bit of SPixel%QC to set in the event
!                                      of an error.
!    limit    integer      In          Optional. The maximum number of points
!                                      that may tolerably be outside the valid
!                                      range.
!
! Algorithm:
!    Check value greater than min and less than max.
!       If value is a scalar, make a simple check.
!       If value is an array, use any() to check all values.
!       If limit is used, use count() to ensure the number of values outside the
!          range is less than or equal to limit.
!    If the check fails, set SPixel Mask to 0 and the given bit of SPixel QC.
!    If compiled with DEBUG, print a message to the Log File.
!
! History:
!    25th Jul 2014, AP: Original version, replacing CheckFloatArray.F90 and
!       CheckByteArray.F90.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module check_value_m
   interface check_value
      module procedure check_value_float0, &
           check_value_float1, check_value_float1_l, &
           check_value_float2, check_value_float2_l, &
           check_value_byte0, &
           check_value_byte1, check_value_byte1_l, &
           check_value_byte2, check_value_byte2_l
   end interface check_value
contains

subroutine check_value_float0(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   implicit none

   real,           intent(in)    :: val
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (val > max .or. val < min) then
#include "check_value.inc"
   end if

end subroutine check_value_float0

subroutine check_value_float1(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   implicit none

   real,           intent(in)    :: val(:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_float1

subroutine check_value_float1_l(val, max, min, SPixel, name, flag_bit, limit)
   use SPixel_def
   implicit none

   real,           intent(in)    :: val(:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_float1_l

subroutine check_value_float2(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   implicit none

   real,           intent(in)    :: val(:,:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_float2

subroutine check_value_float2_l(val, max, min, SPixel, name, flag_bit, limit)
   use SPixel_def
   implicit none

   real,           intent(in)    :: val(:,:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_float2_l

!-----------------------------------------------------------------------------

subroutine check_value_byte0(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (val > max .or. val < min) then
#include "check_value.inc"
   end if

end subroutine check_value_byte0

subroutine check_value_byte1(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_byte1

subroutine check_value_byte1_l(val, max, min, SPixel, name, flag_bit, limit)
   use SPixel_def
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_byte1_l

subroutine check_value_byte2(val, max, min, SPixel, name, flag_bit)
   use SPixel_def
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:,:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_byte2

subroutine check_value_byte2_l(val, max, min, SPixel, name, flag_bit, limit)
   use SPixel_def
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:,:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit
#ifdef DEBUG
   character(180)                :: message
#endif

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_byte2_l

end module check_value_m
