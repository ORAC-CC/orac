! Name:
!    b1_to_si1
!
! Purpose:
!    Converts byte to signed integer
!
! Arguments:
!
!   Input, character B1, the byte to be converted.
!
!    Output, integer I1, the unsigned integer.
!    
! Algorithm:

!
! Local variables:
!    Name      Type   Description
!    i         int    Loop counter
!    message   char   Error/warning message written to log file
!    stat      int    Local status variable used when checking SPixel data.
!                     subordinate routines for checking return non-zero stat
!                     if any pixel in the SPixel fails test.
!    bkp_lun   int    Logical unit number for breakpoint file
!    ios       int    I/O status value for file operations.
!
! History:
!   29th November, 2000, Caroline Poulsen orginal version copied from
!                           John Burkhadt.

! Bugs:

!
! $Id$
!
!----------------------------------------------------------------------------
subroutine b1_to_si1 ( b1, i1 )
!
!*******************************************************************************
!
!! B1_TO_SI1 converts one byte to a signed integer.
!
!
!  Modified:
!
!    07 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character B1, the byte to be converted.
!
!    Output, integer I1, the signed integer.
!
  implicit none
!
  character b1
  integer i1
!
  i1 = ichar ( b1 ) - 128

  return
end subroutine b1_to_si1
