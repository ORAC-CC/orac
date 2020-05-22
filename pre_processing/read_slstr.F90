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

! When defined, the offset between the nadir and oblique views is assumed
! to be constant. Otherwise, the two longitude fields are read and compared
! to determine an appropriate offset. The value below is the mode of running
! slstr_get_alignment() on each row of 1000 random SLSTR (A) images. 548 was
! returned in 1e5 cases. 546-551 were returned O(1e3) times, while other values
! in the range 530-570 each appeared < 1e2 times.
#define CONSTANT_OBLIQUE_OFFSET 548

#include "read_slstr_main.F90"
#include "read_slstr_funcs.F90"

end module read_slstr_m
