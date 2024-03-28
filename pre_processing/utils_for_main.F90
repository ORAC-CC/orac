!-------------------------------------------------------------------------------
! Name: utils_for_main.F90
!
! Purpose:
!
! History:
! 2015/02/24, GM: Pulled these out of preprocessing_for_orac.F90 to get them
!    module produced interfaces.
! 2015/09/14, GM: Move handle_parse_error() to trunk/common/parsing.F90.
! 2015/10/19, GM: Add use_modis_emis_in_rttov to parse_optional().
! 2015/11/26, GM: Add ecmwf_time_int_method, nwp_path_file, nwp_path_file2,
!    and nwp_path_file3 to parse_optional().
! 2016/02/02, CP: Add nwp_path_hr.
! 2016/02/02, GM: Add use_hr_ecmwf.
! 2016/02/02, GM: Add use_ecmwf_snow_and_ice.
! 2016/04/05, SP: Added ECMWF_NLEVELS option to choose between 60,91 and 137
!    level ECMWF input files.
! 2016/05/31, GT: Added use_l1_land_mask option to prevent USGS DEM from
!    overwriting the land/sea mask provided by L1 data (assuming the L1 data
!    provides one!)
! 2016/07/11, SP: Removed chunking routines to separate library in chunk_utils.
! 2017/02/10, SP: Allow reading LSM, LUM, DEM from external file (ExtWork)
! 2017/02/24, SP: Allow option to disable snow/ice correction
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/04/08, SP: New flag to disable VIS processing, saves proc time (ExtWork)
! 2017/04/26, SP: Support for loading geoinfo (lat/lon/vza/vaa) from an
!                 external file. Supported by AHI, not yet by SEVIRI (ExtWork)
! 2017/08/09, SP: Add option to disable the cloud masking (ExtWork)
! 2017/09/14, GT: Added product_name optional argument
! 2018/06/03, SP: GSICS calibration is now supported for SEVIRI. The default
!                 setting is ON, meaning that GSICS coefficients will be used
!                 instead of IMPF (as previous). The new driver file option
!                 USE_GSICS enables this to be disabled.
! 2018/08/30, SP: Allow variable CO2 in RTTOV, linear scaling from 2006 value
! 2021/12/14, DP: Added SEVIRI external ANN option
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module utils_for_main_m

   implicit none

contains

subroutine parse_required(lun, value, name)

   use parsing_m
   use preproc_constants_m

   implicit none

   integer,          intent(in)  :: lun
   character(len=*), intent(out) :: value
   character(len=*), intent(in)  :: name

   character(len=path_length)    :: line

   if (parse_driver(lun, line) /= 0 .or. parse_string(line, value) /= 0) &
      call handle_parse_error(name)

end subroutine parse_required


subroutine parse_optional(label, value, preproc_opts)

   use parsing_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: label
   character(len=*),     intent(in)    :: value
   type(preproc_opts_t), intent(inout) :: preproc_opts


   select case (label)
   case('N_CHANNELS')
      if (parse_string(value, preproc_opts%n_channels) /= 0) &
         call handle_parse_error(label)
      allocate(preproc_opts%channel_ids(preproc_opts%n_channels))
   case('CHANNEL_IDS')
      if (preproc_opts%n_channels == 0) then
         write(*,*) 'ERROR: must set option n_channels before option channels'
         stop error_stop_code
      end if
      if (parse_string(value, preproc_opts%channel_ids) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_TIME_INT_METHOD')
      if (parse_string(value, preproc_opts%ecmwf_time_int_method) /= 0) &
         call handle_parse_error(label)
   case('USE_ECMWF_SNOW_AND_ICE')
      if (parse_string(value, preproc_opts%use_ecmwf_snow_and_ice) /= 0) &
         call handle_parse_error(label)
   case('USE_MODIS_EMIS_IN_RTTOV')
      if (parse_string(value, preproc_opts%use_modis_emis_in_rttov) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH_2')
      if (parse_string(value, preproc_opts%nwp_fnames%nwp_path(2)) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH2_2')
      if (parse_string(value, preproc_opts%nwp_fnames%nwp_path2(2)) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_PATH3_2')
      if (parse_string(value, preproc_opts%nwp_fnames%nwp_path3(2)) /= 0) &
         call handle_parse_error(label)
   case('ECMWF_NLEVELS')
      if (parse_string(value, preproc_opts%nwp_nlevels) /= 0) &
         call handle_parse_error(label)
   case('USE_L1_LAND_MASK')
      if (parse_string(value, preproc_opts%use_l1_land_mask) /= 0) &
         call handle_parse_error(label)
   case('USE_OCCCI')
      if (parse_string(value, preproc_opts%use_occci) /= 0) &
           call handle_parse_error(label)
   case('OCCCI_PATH')
      if (parse_string(value, preproc_opts%occci_path) /= 0) &
           call handle_parse_error(label)
   case('NWP_TIME_FACTOR')
      if (parse_string(value, preproc_opts%nwp_time_factor) /= 0) &
           call handle_parse_error(label)
   case('USE_PREDEF_LSM')
      if (parse_string(value, preproc_opts%use_predef_lsm) /= 0) &
           call handle_parse_error(label)
   case('EXT_LSM_PATH')
      if (parse_string(value, preproc_opts%ext_lsm_path) /= 0) &
           call handle_parse_error(label)
   case('USE_PREDEF_GEO')
      if (parse_string(value, preproc_opts%use_predef_geo) /= 0) &
           call handle_parse_error(label)
   case('EXT_GEO_PATH')
      if (parse_string(value, preproc_opts%ext_geo_path) /= 0) &
           call handle_parse_error(label)
   case('DISABLE_SNOW_ICE_CORR')
      if (parse_string(value, preproc_opts%disable_snow_ice_corr) /= 0) &
           call handle_parse_error(label)
   case('DO_CLOUD_EMIS')
      if (parse_string(value, preproc_opts%do_cloud_emis) /= 0) &
           call handle_parse_error(label)
   case('DO_IRONLY')
      if (parse_string(value, preproc_opts%do_ironly) /= 0) &
           call handle_parse_error(label)
   case('DO_CLDTYPE')
      if (parse_string(value, preproc_opts%do_cloud_type) /= 0) &
           call handle_parse_error(label)
   case('PRODUCT_NAME')
      if (parse_string(value, preproc_opts%product_name) /= 0) &
           call handle_parse_error(label)
   case('USE_CAMEL_EMIS')
      if (parse_string(value, preproc_opts%use_camel_emis) /= 0) &
           call handle_parse_error(label)
   case('USE_GSICS')
      if (parse_string(value, preproc_opts%do_gsics) /= 0) &
           call handle_parse_error(label)
   case('USE_NASA_SEVCALIB')
      if (parse_string(value, preproc_opts%do_nasa) /= 0) &
           call handle_parse_error(label)
   case('USE_CO2')
      if (parse_string(value, preproc_opts%do_co2) /= 0) &
           call handle_parse_error(label)
   case('USE_SWANSEA_CLIMATOLOGY')
      if (parse_string(value, preproc_opts%use_swansea_climatology) /= 0) &
           call handle_parse_error(label)
   case('SWANSEA_GAMMA')
      if (parse_string(value, preproc_opts%swansea_gamma) /= 0) &
           call handle_parse_error(label)
   case('CALCULATE_SLSTR_ALIGNMENT')
      if (parse_string(value, preproc_opts%calculate_slstr_alignment) /= 0) &
           call handle_parse_error(label)
   case('MCD43_MAX_QAFLAG')
      if (parse_string(value, preproc_opts%mcd43_max_qaflag) /= 0) &
           call handle_parse_error(label)
   case('USE_SEVIRI_ANN_CMA_CPH')
      if (parse_string(value, preproc_opts%use_seviri_ann_cma_cph) /= 0) &
           call handle_parse_error(label)
   case('USE_SEVIRI_ANN_CTP_FG')
      if (parse_string(value, preproc_opts%use_seviri_ann_ctp_fg) /= 0) &
           call handle_parse_error(label)
   case('USE_SEVIRI_ANN_MLAY')
      if (parse_string(value, preproc_opts%use_seviri_ann_mlay) /= 0) &
           call handle_parse_error(label)
   case default
      write(*,*) 'ERROR: Unknown option: ', trim(label)
      stop error_stop_code
   end select

end subroutine parse_optional


integer function parse_logical(string, value) result(status)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: string
   logical,          intent(out) :: value

   status = 0

   if (trim(adjustl(string)) .eq. '1' .or.&
       trim(adjustl(string)) .eq. 't' .or. &
       trim(adjustl(string)) .eq. 'true' .or. &
       trim(adjustl(string)) .eq. 'T' .or. &
       trim(adjustl(string)) .eq. 'True') then
        value = .true.
   else if &
      (trim(adjustl(string)) .eq. '0' .or. &
       trim(adjustl(string)) .eq. 'f' .or. &
       trim(adjustl(string)) .eq. 'false' .or. &
       trim(adjustl(string)) .eq. 'F' .or. &
       trim(adjustl(string)) .eq. 'False') then
        value = .false.
   else
        status = -1
   end if

end function parse_logical

end module utils_for_main_m
