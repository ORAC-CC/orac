!-------------------------------------------------------------------------------
! Name: nise.F90
!
! Purpose:
! Module for NISE sea ice data read routines.
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module nsidc_nise_m

   implicit none

   type nise_grid_t
      integer(kind=4)                              :: nx, ny
      real(kind=4)                                 :: res
      real(kind=4)                                 :: REarth
      real(kind=4), dimension(2)                   :: grid_centre
      integer(kind=2), allocatable, dimension(:,:) :: age
      integer(kind=2), allocatable, dimension(:,:) :: extent
   end type nise_grid_t

   type nise_t
      type(nise_grid_t)                            :: north
      type(nise_grid_T)                            :: south
   end type nise_t

contains

include 'read_nsidc_nise.F90'

end module nsidc_nise_m
