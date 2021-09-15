!-------------------------------------------------------------------------------
! Name: netcdf_output_create.F90
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
! output_path    string  in   Folder into which outputs should be saved
! paths          struct  out Paths to the ten ORAC preprocessor outputs.
! granule        struct in   Parameters of the swath file
! global_atts    struct  in   Structure detailing NCDF header contents.
! source_atts    struct  in   Structure detailing files input to ORAC
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_angles  struct  in   Summary of satellite geometry.
! imager_geolocation
!                struct  both Summary of pixel positions
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed
! include_full_brdf logic in  T: Use BRDF surface; F: Use Lambertian surface.
! nwp_flag     logic   in   See set_ecmwf.F90
! do_cloud_emis  logic   in   T: Output cloud emissivity; F: Don't.
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
! 2014/09/28, GM: Removed use_chunking argument.
! 2014/01/12, CP: added source attributes
! 2014/12/16, GM: Put back netcdf_create_config() moved out in the above change.
! 2017/02/07, SP: Added support for NOAA GFS atmosphere data (ExtWork)
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_create(output_path, paths, granule, global_atts, &
   source_atts, preproc_dims, imager_angles, imager_geolocation, netcdf_info, &
   channel_info, include_full_brdf, nwp_flag, do_cloud_emis, verbose)

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m

   implicit none

   character(len=*),           intent(in)    :: output_path
   type(preproc_paths_t),      intent(in)    :: paths
   type(setup_args_t),         intent(in)    :: granule
   type(global_attributes_t),  intent(in)    :: global_atts
   type(source_attributes_t),  intent(in)    :: source_atts
   type(preproc_dims_t),       intent(in)    :: preproc_dims

   type(imager_angles_t),      intent(in)    :: imager_angles

   type(imager_geolocation_t), intent(in)    :: imager_geolocation

   type(netcdf_output_info_t), intent(inout) :: netcdf_info

   type(channel_info_t),       intent(in)    :: channel_info
   logical,                    intent(in)    :: include_full_brdf
   integer,                    intent(in)    :: nwp_flag
   logical,                    intent(in)    :: do_cloud_emis
   logical,                    intent(in)    :: verbose


   ! Create config file
   call netcdf_create_config(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, granule%cminute, &
        granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%config_file)), &
        preproc_dims, imager_geolocation, netcdf_info, channel_info, verbose)


   ! Create RTM files

   ! create prtm file
   call netcdf_create_rtm(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%prtm_file)), &
        NETCDF_OUTPUT_FILE_PRTM, preproc_dims, netcdf_info, channel_info, &
        nwp_flag, verbose)

   ! create lwrtm file
   call netcdf_create_rtm(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%lwrtm_file)), &
        NETCDF_OUTPUT_FILE_LWRTM, preproc_dims, netcdf_info, channel_info, &
        nwp_flag, verbose)

   ! create swrtm file
   call netcdf_create_rtm(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%swrtm_file)), &
        NETCDF_OUTPUT_FILE_SWRTM, preproc_dims, netcdf_info, channel_info, &
        nwp_flag, verbose)


   ! Create swath based files

   ! create alb file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%alb_file)), &
        NETCDF_OUTPUT_FILE_ABL, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

   ! create clf file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%cf_file)), &
        NETCDF_OUTPUT_FILE_CLF, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

   ! create geo file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%geo_file)), &
        NETCDF_OUTPUT_FILE_GEO, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

   ! create loc file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%loc_file)), &
        NETCDF_OUTPUT_FILE_LOC, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

   ! create lsf file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%lsf_file)), &
        NETCDF_OUTPUT_FILE_LSF, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

   ! create msi file
   call netcdf_create_swath(global_atts, source_atts, granule%cyear, &
        granule%cmonth, granule%cday, granule%chour, &
        granule%cminute, granule%platform, granule%sensor, &
        trim(adjustl(output_path))//'/'//trim(adjustl(paths%msi_file)), &
        NETCDF_OUTPUT_FILE_MSI, imager_geolocation, imager_angles, &
        netcdf_info, channel_info, include_full_brdf, do_cloud_emis, verbose)

end subroutine netcdf_output_create
