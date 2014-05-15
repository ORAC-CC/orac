! Name: surface_structures.F90
!
!
! Purpose:
! Define variables types which hold the surface reflectance and
! emissivity data on the instrument data-grid.
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/02/13: GT First version.
!
! $Id$
!
! Bugs:
! none known
!

module surface_structures

  use preproc_constants

  implicit none
  
  type surface_s
     ! Index numbers of measurement channels for each albedo value
     integer(kind=stint), dimension(:), pointer  :: albedo_chan
     ! Index numbers of measurement channels for each emissivity value
     integer(kind=stint), dimension(:), pointer  :: emissivity_chan
     real(kind=sreal), dimension(:,:,:), pointer :: albedo
     real(kind=sreal), dimension(:,:,:), pointer :: emissivity
  end type surface_s

contains

  include 'allocate_surface_structures.F90'
  include 'deallocate_surface_structures.F90'

end module surface_structures
