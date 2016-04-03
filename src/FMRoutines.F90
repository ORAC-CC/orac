!-------------------------------------------------------------------------------
! Name: FMRoutines.F90
!
! Purpose:
! This module contains a set of interface definitions for ECP subroutines.
! Not all subroutines are included. These interface definitions are required
! in order that passed-length arrays can be used as subroutine arguments.
!
! History:
! 2001/01/19, AS: original version
! 2001/01/23, AS: SetCRPSolar/Thermal interfaces updated to cope with new routine
!    SetGZero.
! 2001/02/16, AS: New argument SPixel for SetCRPSolar.
! 2001/02/20, AS: SetCRPSolar SPixel argument replaced by Ind (sub-struct of
!    SPixel). Same change applied to SetCRPThermal.
! 2001/02/27, AS: Set_CRP_Thermal no longer requires argument First.
! 2013/12/20, GM: Cleaned up code.
! 2013/12/24, GM: Some intent changes.
! 2014/12/16, GM: Added LUT name mapping: IR_0v = IRBd, etc.
! 2015/01/09, CP: Added LUT name mapping: IR_0d = IRFBd.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module FM_Routines_m

   use ECP_Constants_m

   implicit none

   integer, parameter :: IR_0v = IRBd
   integer, parameter :: IR_0d = IRFBd
   integer, parameter :: IR_dv = IRd
   integer, parameter :: IR_dd = IRFd
   integer, parameter :: IT_00 = ITB
   integer, parameter :: IT_vv = ITB_u
   integer, parameter :: IT_dv = ITd
   integer, parameter :: IT_0d = ITFBd
   integer, parameter :: IT_dd = ITFd

   integer, parameter :: ISv   = 3
   integer, parameter :: IPv   = 4

contains

#include "FM.F90"
#include "FMSolar.F90"
#include "FMThermal.F90"
#include "SetCRPSolar.F90"
#include "SetCRPThermal.F90"

end module FM_Routines_m
