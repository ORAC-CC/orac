!-------------------------------------------------------------------------------
! Name: netcdf_output_open.F90
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
! global_atts    struct  in   Structure detailing NCDF header contents.
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
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_create(output_path,lwrtm_file,swrtm_file,prtm_file, &
   config_file,msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file, &
   platform,sensor,global_atts,cyear,cmonth,cday,chour,cminute,preproc_dims, &
   imager_angles,imager_geolocation,netcdf_info,channel_info,use_chunking, &
   include_full_brdf,verbose)

   use channel_structures
   use global_attributes
   use imager_structures
   use preproc_constants
   use preproc_structures

   implicit none

   character(len=path_length),     intent(in)    :: output_path
   character(len=file_length),     intent(in)    :: lwrtm_file,swrtm_file, &
                                                    prtm_file,config_file, &
                                                    msi_file,cf_file,lsf_file, &
                                                    geo_file,loc_file,alb_file, &
                                                    scan_file
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear,chour,cminute,cmonth, &
                                                    cday
   type(preproc_dims_s),           intent(in)    :: preproc_dims

   type(imager_angles_s),          intent(in)    :: imager_angles

   type(imager_geolocation_s),     intent(in)    :: imager_geolocation

   type(netcdf_output_info_s),     intent(inout) :: netcdf_info

   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: use_chunking
   logical,                        intent(in)    :: include_full_brdf
   logical,                        intent(in)    :: verbose


   ! Create config file
   call netcdf_create_config(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(config_file)),&
        preproc_dims,imager_geolocation,netcdf_info,channel_info,verbose)


   ! Create RTM files

   ! create prtm file
   call netcdf_create_rtm(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(prtm_file)), &
        NETCDF_OUTPUT_FILE_PRTM, preproc_dims,netcdf_info, &
        channel_info,use_chunking,verbose)

   ! create lwrtm file
   call netcdf_create_rtm(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(lwrtm_file)), &
        NETCDF_OUTPUT_FILE_LWRTM,preproc_dims, &
        netcdf_info,channel_info,use_chunking,verbose)

   ! create swrtm file
   call netcdf_create_rtm(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(swrtm_file)), &
        NETCDF_OUTPUT_FILE_SWRTM, preproc_dims, &
        netcdf_info,channel_info,use_chunking,verbose)


   ! Create swath based files

   ! create alb file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(alb_file)), &
        NETCDF_OUTPUT_FILE_ABL,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create clf file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(cf_file)), &
        NETCDF_OUTPUT_FILE_CLF,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create geo file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(geo_file)), &
        NETCDF_OUTPUT_FILE_GEO,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create loc file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(loc_file)), &
        NETCDF_OUTPUT_FILE_LOC,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create lsf file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(lsf_file)), &
        NETCDF_OUTPUT_FILE_LSF,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create msi file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(msi_file)), &
        NETCDF_OUTPUT_FILE_MSI,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

   ! create uv file
   call netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute,&
        platform,sensor,&
        trim(adjustl(output_path))//'/'//trim(adjustl(scan_file)), &
        NETCDF_OUTPUT_FILE_UV,imager_geolocation,imager_angles, &
        netcdf_info,channel_info,use_chunking,include_full_brdf,verbose)

end subroutine netcdf_output_create
