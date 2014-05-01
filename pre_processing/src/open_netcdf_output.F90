!-------------------------------------------------------------------------------
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
! 2011/12/09: Matthias Jerg produces draft code which creates dummy output files
! 2012/02/14: MJ implements filenames and attributes for netcdf output.
! 2012/11/14: CP modified output path so month and day are automatically added
!    tidied up the file
! 2013/10/17: GM wo was not initialised, resulting in compiler dependent
!    initialisation behaviour.  It is now set to zero.
! 2013/11/06: MJ adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2014/01/24: MJ changes nx,ny from stint to lint to conform with definition in
!    structure.
! 2014/02/02: GM adds chunking on/off option and cleans up code.
! 2014/05/01: GM Reordered data/time arguments into a logical order.
!
!
! $Id$
!
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine open_netcdf_output(nx,ny,output_pathin, output_pathout,lwrtm_file, &
   swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file,geo_file,loc_file, &
   alb_file,scan_file,platform,sensor,script_input,cyear,cmonth,cday,chour,cminute, &
   preproc_dims,imager_angles,imager_geolocation,netcdf_info,channel_info, &
   use_chunking)

   use attribute_structures
   use channel_structures
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   integer(kind=lint),            intent(in)    :: nx,ny
   character(len=pathlength),     intent(in)    :: output_pathin
   character(len=pathlength),     intent(out)   :: output_pathout
   character(len=filelength),     intent(in)    :: lwrtm_file,swrtm_file,prtm_file, &
                                                   config_file,msi_file,cf_file, &
                                                   lsf_file,geo_file,loc_file, &
                                                   alb_file,scan_file
   character(len=platformlength), intent(in)    :: platform
   character(len=sensorlength),   intent(in)    :: sensor
   type(script_arguments_s),      intent(in)    :: script_input
   character(len=datelength),     intent(in)    :: cyear,chour,cminute,cmonth,cday
   type(preproc_dims_s),          intent(in)    :: preproc_dims

   type(imager_angles_s),         intent(in)    :: imager_angles

   type(imager_geolocation_s),    intent(in)    :: imager_geolocation

   type(netcdf_info_s),           intent(inout) :: netcdf_info

   type(channel_info_s),          intent(in)    :: channel_info
   logical,                       intent(in)    :: use_chunking


   integer(kind=lint) :: wo = 0


   output_pathout=trim(adjustl(output_pathin))

   ! Create config file
   call nc_create_file_config(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(config_file)),&
        & wo,preproc_dims,imager_geolocation,netcdf_info,channel_info)

   ! Create RTM files

   ! create lwrtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(lwrtm_file)),wo,1,&
        & preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create swrtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(swrtm_file)),wo,2,&
        & preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create prtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(prtm_file)),wo,3,&
        & preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! Create swath based files

   ! create msi file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(msi_file)),wo,1,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create cf file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(cf_file)),wo,2,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create lsf file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(lsf_file)),wo,3,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create geometry file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(geo_file)),wo,4,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create location file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(loc_file)),wo,5,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create albedo file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(alb_file)),wo,6,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create scan file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        & platform,sensor,&
        & trim(adjustl(output_pathout))//'/'//trim(adjustl(scan_file)),wo,7,&
        & imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking)

end subroutine open_netcdf_output
