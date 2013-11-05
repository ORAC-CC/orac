! Name: allocate_ecmwf_structures.f90
!
!
! Purpose:
! Allocate the array parts of the types defined in ecmwf_structures.f90
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

subroutine allocate_ecmwf_structures_nc(ecmwf_dims_nc,ecmwf_3d_nc,ecmwf_2d_nc)

  use preproc_constants

  use ecmwf_structures

  implicit none

  type(ecmwf_dims_nc_s) :: ecmwf_dims_nc
 
  type(ecmwf_3d_nc_s) :: ecmwf_3d_nc
  type(ecmwf_2d_nc_s) :: ecmwf_2d_nc

  allocate(ecmwf_2d_nc%longitude(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%longitude=real_fill_value

  allocate(ecmwf_2d_nc%latitude(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%latitude=real_fill_value

  allocate(ecmwf_3d_nc%temperature(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec,ecmwf_dims_nc%kdim_ec))
  ecmwf_3d_nc%temperature=real_fill_value
  allocate(ecmwf_3d_nc%spec_hum(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec,ecmwf_dims_nc%kdim_ec))
  ecmwf_3d_nc%spec_hum=real_fill_value
  allocate(ecmwf_3d_nc%ozone(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec,ecmwf_dims_nc%kdim_ec))
  ecmwf_3d_nc%ozone=real_fill_value

  allocate(ecmwf_2d_nc%geopot(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%geopot=real_fill_value
  allocate(ecmwf_2d_nc%lnsp(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%lnsp=real_fill_value
  allocate(ecmwf_2d_nc%sea_ice_cover(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%sea_ice_cover=real_fill_value
  allocate(ecmwf_2d_nc%snow_albedo(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%snow_albedo=real_fill_value
  allocate(ecmwf_2d_nc%snow_depth(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%snow_depth=real_fill_value
  allocate(ecmwf_2d_nc%totcolwv(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%totcolwv=real_fill_value
  allocate(ecmwf_2d_nc%u10(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%u10=real_fill_value
  allocate(ecmwf_2d_nc%v10(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%v10=real_fill_value
  allocate(ecmwf_2d_nc%temp2(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%temp2=real_fill_value
  allocate(ecmwf_2d_nc%sst(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%sst=real_fill_value
  allocate(ecmwf_2d_nc%land_sea_mask(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%land_sea_mask=real_fill_value
  allocate(ecmwf_2d_nc%skin_temp(ecmwf_dims_nc%xdim_ec,ecmwf_dims_nc%ydim_ec))
  ecmwf_2d_nc%skin_temp=real_fill_value



end subroutine allocate_ecmwf_structures_nc
