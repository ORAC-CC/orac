!-------------------------------------------------------------------------------
! Name: open_netcdf_output.f90
!
! Purpose:
! Open netcdf output files
!
! Description and Algorithm details:
! 1) Call the various subroutines.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! output_pathin  string  in   Folder into which outputs should be saved
! output_pathout string  in   Formatted folder where outputs should be saved
! lwrtm_file     string  in   Full path to output file LW RTM.
! swrtm_file     string  in   Full path to output file SW RTM.
! prtm_file      string  in   Full path to output file pressure RTM.
! config_file    string  in   Full path to output file configuration data.
! msi_file       string  in   Full path to output file multispectral imagery.
! cf_file        string  in   Full path to output file cloud flag.
! lsf_file       string  in   Full path to output file land/sea flag.
! geo_file       string  in   Full path to output file geolocation.
! loc_file       string  in   Full path to output file location.
! alb_file       string  in   Full path to output file albedo.
! scan_file      string  in   Full path to output file scan position/
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! script_input   struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_angles  struct  in   Summary of satellite geometry.
! imager_geolocation
!                struct  both Summary of pixel positions
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed
! use_chunking   logic   in   T: Chunk output file; F: Don't.
!
! History:
! 2011/12/09, MJ: produces draft code which creates dummy output files
! 2012/02/14, MJ: implements filenames and attributes for netcdf output.
! 2012/11/14, CP: modified output path so month and day are automatically added
!    tidied up the file
! 2013/10/17, GM: wo was not initialised, resulting in compiler dependent
!    initialisation behaviour.  It is now set to zero.
! 2013/11/06, MJ: adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2014/01/24, MJ: changes nx,ny from stint to lint to conform with definition in
!    structure.
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/05/01, GM: Reordered data/time arguments into a logical order.
! 2014/07/10, AP: Removed nx, ny arguments.
! 2014/08/10, GM: Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine open_netcdf_output(output_pathin,output_pathout,lwrtm_file, &
   swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file,geo_file, &
   loc_file,alb_file,scan_file,platform,sensor,script_input,cyear,cmonth,cday, &
   chour,cminute,preproc_dims,imager_angles,imager_geolocation,netcdf_info, &
   channel_info,use_chunking,include_full_brdf)

   use attribute_structures
   use channel_structures
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   character(len=pathlength),     intent(in)    :: output_pathin
   character(len=pathlength),     intent(out)   :: output_pathout
   character(len=filelength),     intent(in)    :: lwrtm_file,swrtm_file, &
                                                   prtm_file,config_file, &
                                                   msi_file,cf_file,lsf_file, &
                                                   geo_file,loc_file,alb_file, &
                                                   scan_file
   character(len=platformlength), intent(in)    :: platform
   character(len=sensorlength),   intent(in)    :: sensor
   type(script_arguments_s),      intent(in)    :: script_input
   character(len=datelength),     intent(in)    :: cyear,chour,cminute,cmonth, &
                                                   cday
   type(preproc_dims_s),          intent(in)    :: preproc_dims

   type(imager_angles_s),         intent(in)    :: imager_angles

   type(imager_geolocation_s),    intent(in)    :: imager_geolocation

   type(netcdf_info_s),           intent(inout) :: netcdf_info

   type(channel_info_s),          intent(in)    :: channel_info
   logical,                       intent(in)    :: use_chunking
   logical,                       intent(in)    :: include_full_brdf


   integer(kind=lint) :: wo = 0


   output_pathout=trim(adjustl(output_pathin))

   ! Create config file
   call nc_create_file_config(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(config_file)),&
        wo,preproc_dims,imager_geolocation,netcdf_info,channel_info)

   ! Create RTM files

   ! create lwrtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(lwrtm_file)),wo,1,&
        preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create swrtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(swrtm_file)),wo,2,&
        preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! create prtm file
   call nc_create_file_rtm(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(prtm_file)),wo,3,&
        preproc_dims,imager_angles,netcdf_info,channel_info,use_chunking)

   ! Create swath based files

   ! create msi file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(msi_file)),wo,1,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create cf file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(cf_file)),wo,2,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create lsf file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(lsf_file)),wo,3,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create geometry file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(geo_file)),wo,4,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create location file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(loc_file)),wo,5,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create albedo file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(alb_file)),wo,6,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

   ! create scan file
   call nc_create_file_swath(script_input,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_pathout))//'/'//trim(adjustl(scan_file)),wo,7,&
        imager_geolocation,imager_angles,netcdf_info,channel_info,use_chunking,&
        include_full_brdf)

end subroutine open_netcdf_output
