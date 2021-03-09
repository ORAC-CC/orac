!-------------------------------------------------------------------------------
! Name: fm_routines.F90
!
! Purpose:
! This module contains a set of interface definitions for ORAC subroutines.
! Not all subroutines are included. These interface definitions are required
! in order that passed-length arrays can be used as subroutine arguments.
!
! History:
! 2001/01/19, AS: original version
! 2001/01/23, AS: SetCRPSolar/Thermal interfaces updated to cope with new
!    routine SetGZero.
! 2001/02/16, AS: New argument SPixel for SetCRPSolar.
! 2001/02/20, AS: SetCRPSolar SPixel argument replaced by Ind (sub-struct of
!    SPixel). Same change applied to SetCRPThermal.
! 2001/02/27, AS: Set_CRP_Thermal no longer requires argument First.
! 2013/12/20, GM: Cleaned up code.
! 2013/12/24, GM: Some intent changes.
! 2014/12/16, GM: Added LUT name mapping: IR_0v = IRbd, etc.
! 2015/01/09, CP: Added LUT name mapping: IR_0d = IRfbd.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module FM_Routines_m

   use ORAC_Constants_m

   implicit none

   integer, parameter :: IR_0v = IRbd
   integer, parameter :: IR_0d = IRfbd
   integer, parameter :: IR_dv = IRd
   integer, parameter :: IR_dd = IRfd
   integer, parameter :: IT_00 = ITb
   integer, parameter :: IT_vv = ITb_u
   integer, parameter :: IT_0d = ITfbd
   integer, parameter :: IT_vd = ITfbd_u
   integer, parameter :: IT_dv = ITd
   integer, parameter :: IT_dd = ITfd

   integer, parameter :: ISv   = 3
   integer, parameter :: IPv   = 4
   integer, parameter :: IGv   = 5

contains

#include "fm.F90"
#include "fm_solar.F90"
#include "fm_thermal.F90"
#include "set_crp_solar.F90"
#include "set_crp_thermal.F90"

end module FM_Routines_m
