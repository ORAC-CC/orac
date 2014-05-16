! Name: allocate_ecmwf_structures.f90
!
!
! Purpose:
! Deallocate the array parts of the types defined in ecmwf_structures.f90
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
!2012/01/13: Matthias Jerg produces draft code for ERA Interim grib 1 parameters required
!
! $Id$
!
! Bugs:
!
!none known

subroutine deallocate_ecmwf_structures(ecmwf_dims,ecmwf_3d,ecmwf_2d)

! use ecmwf_structures
  use preproc_constants

  implicit none

  type(ecmwf_dims_s), intent(inout) :: ecmwf_dims
  type(ecmwf_3d_s), intent(inout) :: ecmwf_3d
  type(ecmwf_2d_s), intent(inout) :: ecmwf_2d

  deallocate(ecmwf_2d%longitude)
  deallocate(ecmwf_2d%latitude)
  deallocate(ecmwf_3d%temperature)
  deallocate(ecmwf_3d%spec_hum)
  deallocate(ecmwf_3d%ozone)
  deallocate(ecmwf_2d%geopot)
  deallocate(ecmwf_2d%lnsp)
  deallocate(ecmwf_2d%sea_ice_cover)
  deallocate(ecmwf_2d%snow_albedo)
  deallocate(ecmwf_2d%snow_depth)
  deallocate(ecmwf_2d%totcolwv)
  deallocate(ecmwf_2d%u10)
  deallocate(ecmwf_2d%v10)
  deallocate(ecmwf_2d%temp2)
  deallocate(ecmwf_2d%sst)
  deallocate(ecmwf_2d%land_sea_mask)
  deallocate(ecmwf_2d%skin_temp)

end subroutine deallocate_ecmwf_structures
