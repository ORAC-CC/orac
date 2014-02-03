! Name:
!    BITS
!
! Purpose:
!    Sets bits in the SPixel quality control array.
!    REDUNDANT: now use Fortran 90 intrinsic function ibset.
!
! Arguments:
!    Name       Type    Description
!    BitNum     int     The bit number that is required to be set (0 to 31)
!    Target     int     The variable whose bits are to be set
!                       (the quality control array element)
!
! History:
!    22nd December, 2000, Kevin M Smith : Original version
!    11th July 2001, Andy Smith: updated comments. No longer required.
!
! Bugs:
!    None known.
! 
! $Id$
!
!---------------------------------------------------------------------
subroutine BITS(BitNum, Target)

!  Define arguments

   integer(4) :: BitNum
   integer(4) :: Target
   
!  Define local variables (currently none)

!  Set requested bit

   Target = Target + ( 2 ** BitNum )
   
end subroutine BITS
