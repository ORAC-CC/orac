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
   ! These are taken from the SLSTR User Guide and/or personal communications.
   real, parameter :: slstr_correction_factor(18) = [ &
        0.95, & ! Ch1
        -1.0, & ! Ch2
        -1.0, & ! Ch3
        -1.0, & ! Ch4
        1.10, & ! Ch5
        1.10, & ! Ch6
        -1.0, & ! Ch7
        -1.0, & ! Ch8
        -1.0, & ! Ch9
        0.92, & ! Ch10
        0.93, & ! Ch11
        0.93, & ! Ch12
        -1.0, & ! Ch13
        -1.0, & ! Ch14
        -1.0, & ! Ch15
        -1.0, & ! Ch16
        -1.0, & ! Ch17
        -1.0 ]  ! Ch18

contains

#include "read_slstr_main.F90"
#include "read_slstr_funcs.F90"

end module read_slstr_m
