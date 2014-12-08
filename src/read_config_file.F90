!-------------------------------------------------------------------------------
! Name: read_config_file.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2013/11/14, MJ: Initial version
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/07, AP: Replaced with preprocessor's NCDF routines.
! 2012/12/01, CP: read in global and source attributes
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_config_file(Ctrl, conf,global_atts,source_atts)

   use config_def
   use CTRL_def
   use orac_ncdf
   use global_attributes
   use source_attributes

   implicit none

   type(Ctrl_t),        intent(in)    :: Ctrl
   type(config_struct), intent(inout) :: conf
   type(global_attributes_s) :: global_atts
   type(source_attributes_s) :: source_atts

   logical                            :: verbose = .true.

   integer                            :: ncid

   ! Open config file for reading
   call nc_open(ncid, Ctrl%FID%CONFIG)

!  conf%nx = nc_dim_length(ncid, 'nx_conf', verbose)
!  conf%ny = nc_dim_length(ncid, 'ny_conf', verbose)
   conf%nc = nc_dim_length(ncid, 'nc_conf', verbose)
!  conf%nalb = nc_dim_length(ncid, 'nc_alb', verbose)
!  conf%nemis = nc_dim_length(ncid, 'nc_emis', verbose)

   allocate(conf%channel_ids_instr(conf%nc))
   call nc_read_array(ncid, "msi_instr_ch_numbers", conf%channel_ids_instr, &
        verbose)
   if (verbose) write(*,*) 'msi channel numbers instr: ',conf%channel_ids_instr

   allocate(conf%channel_ids_abs(conf%nc))
   call nc_read_array(ncid, "msi_abs_ch_numbers", conf%channel_ids_abs, verbose)
   if (verbose) write(*,*) 'msi channel numbers file: ',conf%channel_ids_abs

   allocate(conf%channel_sw_flag(conf%nc))
   call nc_read_array(ncid, "msi_ch_swflag", conf%channel_sw_flag, verbose)
   if (verbose) write(*,*) 'sw flag: ',conf%channel_sw_flag

   allocate(conf%channel_lw_flag(conf%nc))
   call nc_read_array(ncid, "msi_ch_lwflag", conf%channel_lw_flag, verbose)
   if (verbose) write(*,*) 'lw flag: ',conf%channel_lw_flag

!
!Read global attributes
!
 !----------------------------------------------------------------------------
   ! Set global_attributes structure
   !----------------------------------------------------------------------------


  


   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.

    if (nf90_get_att(ncid, NF90_GLOBAL, "Conventions", global_atts%Conventions)== &
        NF90_NOERR) then
endif

 

   if (nf90_get_att(ncid, NF90_GLOBAL, "Institution", global_atts%institution) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "Source", global_atts%source) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "History", global_atts%history) == &
        NF90_NOERR) then
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "References", global_atts%references) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "Comment", global_atts%comment) == &
        NF90_NOERR) then
   endif

   ! Extra global attributes defined by ORAC
   if (nf90_get_att(ncid, NF90_GLOBAL, "Project", global_atts%project) == &
        NF90_NOERR) then 
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "File_Name", global_atts%file_name) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "UUID", global_atts%UUID) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "NetCDF_Version", global_atts%NetCDF_Version) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Product_Name", global_atts%Product_Name) == &
        NF90_NOERR) then
   endif


!   temp_string=trim('year!!!')//trim('month!!!')//trim('day!!!')

   if (nf90_get_att(ncid, NF90_GLOBAL, "Product_date", global_atts%Product_Date) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Production_Time", global_atts%Production_Time) == &
        NF90_NOERR) then 
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "L2_Processor",global_atts%L2_Processor ) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "L2_Processor_Version", global_atts%L2_Processor_Version) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Platform", global_atts%platform) == &
        NF90_NOERR) then
   endif
   write(*,*)'platform',global_atts%platform

   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", global_atts%sensor) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "AATSR_Processing_Version", global_atts%AATSR_Processing_Version) == &
        NF90_NOERR) then
   endif



   if (nf90_get_att(ncid, NF90_GLOBAL, "Contact_Email", global_atts%contact_email) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Contact_Website", global_atts%contact_website) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Keywords", global_atts%keywords) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Summary", global_atts%summary) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "License", global_atts%license) == &
        NF90_NOERR) then
   endif

write(*,*)'license',NF90_NOERR


  if (nf90_get_att(ncid, NF90_GLOBAL, "RTTOV_version", global_atts%rttov_version) == &
        NF90_NOERR) then
   endif

write(*,*)'rttov_Version', global_atts%rttov_version 


  if (nf90_get_att(ncid, NF90_GLOBAL, "ECMWF_version", global_atts%ecmwf_version) == &
        NF90_NOERR) then
   endif




  if (nf90_get_att(ncid, NF90_GLOBAL, "SVN_version", global_atts%svn_version) == &
        NF90_NOERR) then
   endif




!
!Read source attributes
!

  if (nf90_get_att(ncid, NF90_GLOBAL, "Albedo_file", source_atts%albedo_file) == &
        NF90_NOERR) then 
   endif
write(*,*)'albedo_file',source_atts%albedo_file,NF90_NOERR


  if (nf90_get_att(ncid, NF90_GLOBAL, "BRDF_file", source_atts%brdf_file) == &
        NF90_NOERR) then 
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Emissivity_file", source_atts%emissivity_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "USGS_file", source_atts%usgs_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Sea_Ice_file", source_atts%sea_ice_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Snow_file", source_atts%snow_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Level1b_file", source_atts%level1b_file) == &
        NF90_NOERR) then
   endif



  if (nf90_get_att(ncid, NF90_GLOBAL, "Geo_file", source_atts%geo_file) == &
        NF90_NOERR) then
   endif







   ! Close config file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
      stop 'ERROR: read_config_file(): Failure to close file.'

end subroutine read_config_file
