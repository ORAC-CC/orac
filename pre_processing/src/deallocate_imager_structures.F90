!-------------------------------------------------------------------------------
! Name: deallocate_imager_structures.F90
!
! Purpose:
! Deallocate the array parts of the types defined in imager_structures.F90
!
! Description and Algorithm details:
! 1) Deallocate all fields passed.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_geolocation  struct both Structure with arrays that need deallocating.
! imager_angles       struct both "
! imager_flags        struct both "
! imager_time         struct both "
! imager_measurements struct both "
!
! History:
! 2012/01/13, MJ: produces draft code for MODIS L1b data
! 2012/02/03, MJ: adds uncertainty to measurements
! 2012/02/03, CP: deallocated solazi
! 2012/12/13, CP: deallocated uscan and vscan
! 2013/11/08, GM: added missing deallocate statements.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_imager_structures(imager_geolocation,imager_angles, &
     imager_flags,imager_time,imager_measurements)

   use preproc_constants

   implicit none

   type(imager_geolocation_s),  intent(inout) :: imager_geolocation
   type(imager_angles_s),       intent(inout) :: imager_angles
   type(imager_flags_s),        intent(inout) :: imager_flags
   type(imager_time_s),         intent(inout) :: imager_time
   type(imager_measurements_s), intent(inout) :: imager_measurements

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
   deallocate(imager_measurements%uncertainty)

end subroutine deallocate_imager_structures
