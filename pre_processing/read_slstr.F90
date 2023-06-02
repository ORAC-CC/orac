!-------------------------------------------------------------------------------
! Name: read_slstr.F90
!
! Purpose:
! Module for SLSTR read routines.
!
! History:
! 2016/06/14, SP: First version.
! 2020/05/22, AP: Moved definition of constants here.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_slstr_m

   implicit none

   ! The calibration of SLSTR is under ongoing revisions, such that the
   ! application of empirical correction factors is currently required.
   ! These are taken from personal communications and the SLSTR Vicarious
   ! calibration adjustments document: S3MPC.RAL.TN.020, Issue 2.0, Sep 2020.

   real, parameter :: slstr_correction_factor(18) = [ &
        0.97, & ! Ch1
        0.98, & ! Ch2
        0.98, & ! Ch3
        -1.0, & ! Ch4
        1.11, & ! Ch5
        1.13, & ! Ch6
        -1.0, & ! Ch7
        -1.0, & ! Ch8
        -1.0, & ! Ch9
        0.94, & ! Ch10
        0.95, & ! Ch11
        0.95, & ! Ch12
        -1.0, & ! Ch13
        1.04, & ! Ch14
        1.07, & ! Ch15
        -1.0, & ! Ch16
        -1.0, & ! Ch17
        -1.0 ]  ! Ch18

contains

#include "read_slstr_main.F90"
#include "read_slstr_funcs.F90"

end module read_slstr_m
