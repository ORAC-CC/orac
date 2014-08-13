!-------------------------------------------------------------------------------
! Name: surface_structures.F90
!
! Purpose:
! Define variables types which hold the surface reflectance and emissivity data
! on the instrument data-grid.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2012/02/13, GT: First version.
! 2014/08/10, GM: Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module surface_structures

   use preproc_constants

   implicit none

   type surface_s
      ! Index numbers of measurement channels for each albedo value
      integer(kind=stint), dimension(:), pointer  :: albedo_chan

      ! Index numbers of measurement channels for each emissivity value
      integer(kind=stint), dimension(:), pointer  :: emissivity_chan

      real(kind=sreal), dimension(:,:,:), pointer :: albedo

      real(kind=sreal), dimension(:,:,:), pointer :: rho_0v
      real(kind=sreal), dimension(:,:,:), pointer :: rho_0d
      real(kind=sreal), dimension(:,:,:), pointer :: rho_dv
      real(kind=sreal), dimension(:,:,:), pointer :: rho_dd

      real(kind=sreal), dimension(:,:,:), pointer :: emissivity
   end type surface_s

contains

include 'allocate_surface_structures.F90'
include 'deallocate_surface_structures.F90'

end module surface_structures
