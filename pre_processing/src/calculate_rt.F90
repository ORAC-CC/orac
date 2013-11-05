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
!
! $Id$
!
! Bugs:
!
!none known

subroutine calculate_rt(coef_path,emiss_path,sensor,platform,preproc_dims,preproc_geoloc,preproc_geo,&
     & preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles,netcdf_info,channel_info,month,ecmwf_dims)

  use preproc_constants
  use preproc_structures
  use imager_structures
  use netcdf_structures
  use channel_structures
  use ecmwf_structures

  implicit none

  integer(kind=lint) :: idim,jdim
  integer(kind=stint):: month

  type(preproc_dims_s) :: preproc_dims
  type(preproc_geoloc_s) :: preproc_geoloc
  type(preproc_geo_s) :: preproc_geo
  type(preproc_prtm_s) :: preproc_prtm
  type(preproc_lwrtm_s) :: preproc_lwrtm
  type(preproc_swrtm_s) :: preproc_swrtm
  type(channel_info_s) :: channel_info
  type(imager_angles_s) :: imager_angles
  type(ecmwf_dims_s) :: ecmwf_dims

  character(len=sensorlength) :: sensor
  character(len=platformlength) :: platform

  character(len=pathlength) :: coef_path,emiss_path

  type(netcdf_info_s) :: netcdf_info

  call rttov_driver(coef_path,emiss_path,sensor,platform,preproc_dims, &
       & preproc_geoloc,preproc_geo,&
       & preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles,&
       & netcdf_info,channel_info,month,ecmwf_dims)


end subroutine calculate_rt
