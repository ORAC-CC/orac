!-------------------------------------------------------------------------------
! Name: surface_structures.F90
!
! Purpose:
! Module defining variables types which hold the surface reflectance and
! emissivity data on the instrument data-grid.
!
! History:
! 2012/02/13, GT: First version.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/09/17, CS: Added surface%nise_mask.
! 2014/11/19, GM: C #includes should use double quotes.
! 2014/12/02, GM: Remove unused surface%albedo_chan and surface%emissivity_chan.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module surface_structures_m

   use preproc_constants_m

   implicit none

   type surface_t

      ! ice/snow mask based on NISE aux. data
      integer(kind=byte), dimension(:,:), pointer :: nise_mask

      real(kind=sreal), dimension(:,:,:), pointer :: albedo

      real(kind=sreal), dimension(:,:,:), pointer :: rho_0v
      real(kind=sreal), dimension(:,:,:), pointer :: rho_0d
      real(kind=sreal), dimension(:,:,:), pointer :: rho_dv
      real(kind=sreal), dimension(:,:,:), pointer :: rho_dd

      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
   end type surface_t

contains

#include "allocate_surface_structures.F90"
#include "deallocate_surface_structures.F90"

end module surface_structures_m
