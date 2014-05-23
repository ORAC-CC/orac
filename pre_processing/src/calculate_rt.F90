module calculate_rt_m

implicit none

contains

! Name: calculate_rt.f90
!
!
! Purpose:
! Top level routine in which RTTOV environment is called for each preprocessing pixel
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
!2012/03/27: Matthias Jerg provides initial implementation
!2013/06/26: Caroline Poulsen modified to include swrtm information
!2013/07/29: Caroline Poulsen added month variable required for emissivity
!2012/12/01: Caroline Poulsen added in ecmwf argument
!20140204 MJ adds verbose to argument list of calculate_rt
!
! $Id$
!
! Bugs:
!
!none known

subroutine calculate_rt(coef_path,emiss_path,sensor,platform,preproc_dims,preproc_geoloc,preproc_geo,&
     & preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles,netcdf_info,channel_info,month,ecmwf_dims,verbose)

  use preproc_constants
  use preproc_structures
  use imager_structures
  use netcdf_structures
  use channel_structures
  use ecmwf_structures
  use rttov_driver_m

  implicit none

  character(len=pathlength), intent(in) :: coef_path
  character(len=pathlength), intent(in) :: emiss_path
  character(len=sensorlength), intent(in) :: sensor
  character(len=platformlength), intent(in) :: platform
  type(preproc_dims_s), intent(inout) :: preproc_dims
  type(preproc_geoloc_s), intent(in) :: preproc_geoloc
  type(preproc_geo_s), intent(in) :: preproc_geo
  type(preproc_prtm_s), intent(inout) :: preproc_prtm
  type(preproc_lwrtm_s), intent(inout) :: preproc_lwrtm
  type(preproc_swrtm_s), intent(inout) :: preproc_swrtm
  type(imager_angles_s), intent(in) :: imager_angles
  type(netcdf_info_s), intent(inout) :: netcdf_info
  type(channel_info_s), intent(in) :: channel_info
  integer(kind=stint), intent(in):: month
  type(ecmwf_dims_s), intent(in) :: ecmwf_dims

  logical :: verbose

  call rttov_driver(coef_path,emiss_path,sensor,platform,preproc_dims, &
       & preproc_geoloc,preproc_geo,&
       & preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles,&
       & netcdf_info,channel_info,month,ecmwf_dims,verbose)

end subroutine calculate_rt

end module calculate_rt_m
