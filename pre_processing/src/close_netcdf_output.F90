! Name: close_netcdf_output.f90
!
!
! Purpose:
! Open netcdf output files
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
!2011/12/12: Matthias Jerg produces draft code which creates dummy output files
! 2013/11/06: MJ adds config file to preprocessing output which holds all relevant dimensional information.
!
! $Id$
!
! Bugs:
!
!none known

!---------------------------------------------------------------------------------
!---------------------------------------------------------------------------------
subroutine close_netcdf_output(output_path,&
     & lwrtm_file,swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file,&
     & netcdf_info)
!---------------------------------------------------------------------------------
!---------------------------------------------------------------------------------

  use preproc_constants 

  use netcdf_structures

  implicit none
 
  character(len=pathlength) :: output_path
  character(len=filelength) :: lwrtm_file,swrtm_file,prtm_file,config_file,&
       & msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file
  
  integer(kind=lint) :: ncid_lwrtm,ncid_swrtm,ncid_prtm, wo

  type(netcdf_info_s) :: netcdf_info

  wo=0

  call nc_close(netcdf_info%ncid_lwrtm,trim(adjustl(output_path))//'/'//trim(adjustl(lwrtm_file)),wo)
  call nc_close(netcdf_info%ncid_swrtm,trim(adjustl(output_path))//'/'//trim(adjustl(swrtm_file)),wo)
  call nc_close(netcdf_info%ncid_prtm,trim(adjustl(output_path))//'/'//trim(adjustl(prtm_file)),wo)

  call nc_close(netcdf_info%ncid_config,trim(adjustl(output_path))//'/'//trim(adjustl(config_file)),wo)
  call nc_close(netcdf_info%ncid_msi,trim(adjustl(output_path))//'/'//trim(adjustl(msi_file)),wo)
  call nc_close(netcdf_info%ncid_geo,trim(adjustl(output_path))//'/'//trim(adjustl(geo_file)),wo)
  call nc_close(netcdf_info%ncid_lsf,trim(adjustl(output_path))//'/'//trim(adjustl(lsf_file)),wo)
  call nc_close(netcdf_info%ncid_cf,trim(adjustl(output_path))//'/'//trim(adjustl(cf_file)),wo)
  call nc_close(netcdf_info%ncid_loc,trim(adjustl(output_path))//'/'//trim(adjustl(loc_file)),wo)
  call nc_close(netcdf_info%ncid_alb,trim(adjustl(output_path))//'/'//trim(adjustl(alb_file)),wo)
  call nc_close(netcdf_info%ncid_scan,trim(adjustl(output_path))//'/'//trim(adjustl(scan_file)),wo)



end subroutine close_netcdf_output
