! Name: open_netcdf_output.f90
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
!2011/12/09: Matthias Jerg produces draft code which creates dummy output files
!2012/02/14: MJ implements filenames and attributes for netcdf output.
!2012/11/14: CP modified output path so month and day are automatically added
!            tidied up the file
!2013/10/17: GM: wo was not initialised, resulting in compiler dependent
!                initialisation behaviour.  It is now set to zero.
! 2013/11/06: MJ adds config file to preprocessing output which holds all relevant dimensional information.
!
!
! $Id$
!
! Bugs:
!
!none known

!---------------------------------------------------------------------------------
!---------------------------------------------------------------------------------
subroutine open_netcdf_output(nx,ny,output_pathin, output_pathout,&
     & lwrtm_file,swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file,&
     & platform,sensor,script_input,&
     & cyear,chour,cminute,cmonth,cday,preproc_dims,imager_angles,imager_geolocation,netcdf_info,channel_info)
!---------------------------------------------------------------------------------
!---------------------------------------------------------------------------------

  use preproc_constants

  use attribute_structures

  use preproc_structures

  use imager_structures

  use netcdf_structures

  use channel_structures

  implicit none
  
  character(len=pathlength) :: output_pathin,output_pathout
  character(len=filelength) :: lwrtm_file,swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file
  character(len=platformlength) :: platform
  character(len=sensorlength) :: sensor

  integer(kind=lint) :: dims_var(2),wo = 0

  integer(kind=stint) :: nx,ny

  character(len=datelength) :: cyear,chour,cminute,cmonth,cday

  type(script_arguments_s) :: script_input

  type(preproc_dims_s) :: preproc_dims

  type(imager_angles_s) :: imager_angles

  type(imager_geolocation_s) :: imager_geolocation

  type(netcdf_info_s) :: netcdf_info

  type(channel_info_s) :: channel_info

  !Create RTM files


  !output_pathout=trim(adjustl(output_pathin))//'/'//trim(adjustl(cyear)) &
  !     & //'/'//trim(adjustl(cmonth))//'/'//trim(adjustl(cday))
  output_pathout=trim(adjustl(output_pathin))

!further test
!!$  call nc_create_file_config(script_input,cyear,chour,cminute,cmonth,cday,platform,sensor,&
!!$       & trim(adjustl(output_pathout))//'/'//trim(adjustl(config_file)), &
!!$       & wo,netcdf_info,channel_info)

  !create lwrtm file
  call nc_create_file_rtm(script_input,cyear,chour,cminute,cmonth, &
       & cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(lwrtm_file)),wo,1,&
       & preproc_dims,imager_angles,netcdf_info,channel_info)

  !create swrtm file
  call nc_create_file_rtm(script_input,cyear,chour,cminute,cmonth, &
       & cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(swrtm_file)),wo,2,&
       & preproc_dims,imager_angles,netcdf_info,channel_info)

  !create prtm file
  call nc_create_file_rtm(script_input,cyear,chour,cminute,cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(prtm_file)),wo,3,&
       & preproc_dims,imager_angles,netcdf_info,channel_info)

  !Create swath based files
  !create msi file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(msi_file)),wo,1,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create cf file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(cf_file)),wo,2,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create lsf file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(lsf_file)),wo,3,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create geometry file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(geo_file)),wo,4,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create location file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(loc_file)),wo,5,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create albedo file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(alb_file)),wo,6,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)

  !create scan file
  call nc_create_file_swath(script_input,cyear,chour,cminute, &
       & cmonth,cday,platform,sensor,&
       & trim(adjustl(output_pathout))//'/'//trim(adjustl(scan_file)),wo,7,&
       & imager_geolocation,imager_angles,netcdf_info,channel_info)



end subroutine open_netcdf_output
