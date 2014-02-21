! Name:
!    b1_toui1
!
! Purpose:
!    Converts byte to unsigned integer
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
! 2013 MJ changes variable type definition
! Bugs:

!
! $Id$
!
!-----------------------------------------------------------------------------
subroutine b1_to_ui1 ( b1, i1 )

  use ECP_constants	

  implicit none
!
  !MJ OLD character b1
  integer(kind=byte) :: b1
  integer :: i1
  
  !MJ OLD  i1 =int(ichar ( b1 ),kind=byte)
  
  i1=int(b1,kind=4)

  return
end
