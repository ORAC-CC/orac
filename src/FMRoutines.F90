! ------------------------------------------------------------------------------
! Name:
!    FM_Routines_def
!
! Description:
!    This module contains a set of interface definitions for ECP subroutines.
!    Not all subroutines are included. These interface definitions are required
!    in order that passed-length arrays can be used as subroutine arguments.
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!    19th Jan 2001, Andy Smith : original version
!    23rd Jan 2001, Andy Smith :
!       SetCRPSolar/Thermal interfaces updated to cope with new routine
!       SetGZero.
!    16th Feb 2001, Andy Smith :
!       New argument SPixel for SetCRPSolar.
!    20th Feb 2001, Andy Smith:
!       SetCRPSolar SPixel argument replaced by Ind (sub-struct of SPixel).
!       Same change applied to SetCRPThermal.
!    27th Feb 2001, Andy Smith:
!       Set_CRP_Thermal no longer requires argument First.
!    20th Dec 2013, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2013, Greg McGarragh:
!       Some intent changes.
!    16th Dec 2014, Greg McGarragh:
!       Added LUT name mapping: IR_0v = IRBd, etc.
!    9th Jan 2015, CP:
!       Added LUT name mapping: IR_0d = IRFBd
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module FM_Routines_def

   use ECP_Constants

   implicit none

   integer, parameter :: IR_0v = IRBd
   integer, parameter :: IR_dv = IRd
   integer, parameter :: IR_dd = IRFd
   integer, parameter :: IT_00 = ITB
   integer, parameter :: IT_vv = ITB_u
   integer, parameter :: IT_dv = ITd
   integer, parameter :: IT_0d = ITFBd
   integer, parameter :: IT_dd = ITFd
   integer, parameter :: IR_0d = IRFBd

contains

#include "FM.F90"
#include "FMSolar.F90"
#include "FMThermal.F90"
#include "SetCRPSolar.F90"
#include "SetCRPThermal.F90"

end module FM_Routines_def
