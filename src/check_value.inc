!-------------------------------------------------------------------------------
! Name:
!    check_value.inc
!
! Purpose:
!    Code shared by all version of check_value function. See check_value.F90 for
!    detailed header information.
!
! History:
!    25th Jul 2014, AP: Original version.
!     1st Aug 2014, GM: Comment out zeroing of SPixel%Mask.
!     4th Nov 2014, AP: Remove undefined variables.
!
! $Id$
!
! Bugs:
!    None known
!-------------------------------------------------------------------------------

   ! flag pixel as bad and specify reason in quality control flag
!  SPixel%Mask = 0
   SPixel%QC = ibset(SPixel%QC, flag_bit)

#ifdef DEBUG
   write(unit=message, fmt=*) 'Get_SPixel: WARNING - Found '//trim(name)// &
        &' out of range in pixel at:', SPixel%Loc%X0, SPixel%Loc%Y0
   call Write_log(Ctrl, trim(message), 0)
#endif
