! Name: allocate_imager_structures.f90
!
!
! Purpose:
! Deallocate the array parts of the types defined in imager_structures.f90
! 
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
! 2012/01/13: Matthias Jerg produces draft code for MODIS L1b data
! 2012/02/03: Matthias Jerg adds uncertainty to measurements
! 2012/02/03: Caroline Poulsen deallocated solazi
! 2012/12/13: Caroline Poulsen deallocated uscan and vscan
! 2013/11/08: Greg McGarragh added missing deallocate statements.
!
! $Id$
!
! Bugs:
!
!none known



subroutine deallocate_imager_structures(imager_geolocation,imager_angles,imager_flags,imager_time,imager_measurements)

  use preproc_constants

  use imager_structures

  implicit none

  type(imager_geolocation_s) :: imager_geolocation
  type(imager_angles_s) :: imager_angles
  type(imager_flags_s) :: imager_flags
  type(imager_time_s) :: imager_time
  type(imager_measurements_s) :: imager_measurements

  deallocate(imager_geolocation%latitude)
  deallocate(imager_geolocation%longitude)
  deallocate(imager_geolocation%vscan)
  deallocate(imager_geolocation%uscan)
  deallocate(imager_angles%solzen)
  deallocate(imager_angles%satzen)
  deallocate(imager_angles%relazi)
  deallocate(imager_angles%solazi)
  deallocate(imager_flags%lsflag)
  deallocate(imager_flags%cflag)
  deallocate(imager_time%time)
  deallocate(imager_measurements%data)
!!$  write(*,*) associated(imager_measurements%uncertainty)
!!$  pause
  deallocate(imager_measurements%uncertainty)

end subroutine deallocate_imager_structures
