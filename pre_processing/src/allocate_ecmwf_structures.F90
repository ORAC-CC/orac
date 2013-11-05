! Name: allocate_ecmwf_structures.F90
!
!
! Purpose:
! Allocate the array parts of the types defined in ecmwf_structures.f90
! 
! Description and Algorithm details:
! 1) Run many ALLOCATE statements.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_dims struct in  Structure summarising dimensions of ECMWF files.
! ecmwf_3d   struct out Structure containing 3-D ECMWF fields.
! ecmwf_2d   struct out Structure containing 2-D ECMWF fields.
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/01/13: MJ produces draft code for ERA Interim grib 1 parameters required
!
! $Id$
!
! Bugs:
! none known
!

subroutine allocate_ecmwf_structures(ecmwf_dims,ecmwf_3d,ecmwf_2d)

   use preproc_constants
   use ecmwf_structures

   implicit none

   type(ecmwf_dims_s) :: ecmwf_dims
   type(ecmwf_3d_s)   :: ecmwf_3d
   type(ecmwf_2d_s)   :: ecmwf_2d

   allocate(ecmwf_2d%longitude(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%longitude=real_fill_value

   allocate(ecmwf_2d%latitude(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%latitude=real_fill_value

   allocate(ecmwf_3d%temperature(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec, &
        ecmwf_dims%kdim_ec))
   ecmwf_3d%temperature=real_fill_value
   allocate(ecmwf_3d%spec_hum(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec, &
        ecmwf_dims%kdim_ec))
   ecmwf_3d%spec_hum=real_fill_value
   allocate(ecmwf_3d%ozone(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec, &
        ecmwf_dims%kdim_ec))
   ecmwf_3d%ozone=real_fill_value

   allocate(ecmwf_2d%geopot(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%geopot=real_fill_value
   allocate(ecmwf_2d%lnsp(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%lnsp=real_fill_value
   allocate(ecmwf_2d%sea_ice_cover(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%sea_ice_cover=real_fill_value
   allocate(ecmwf_2d%snow_albedo(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%snow_albedo=real_fill_value
   allocate(ecmwf_2d%snow_depth(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%snow_depth=real_fill_value
   allocate(ecmwf_2d%totcolwv(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%totcolwv=real_fill_value
   allocate(ecmwf_2d%u10(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%u10=real_fill_value
   allocate(ecmwf_2d%v10(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%v10=real_fill_value
   allocate(ecmwf_2d%temp2(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%temp2=real_fill_value
   allocate(ecmwf_2d%sst(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%sst=real_fill_value
   allocate(ecmwf_2d%land_sea_mask(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%land_sea_mask=real_fill_value
   allocate(ecmwf_2d%skin_temp(ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec))
   ecmwf_2d%skin_temp=real_fill_value



end subroutine allocate_ecmwf_structures
