!-------------------------------------------------------------------------------
! Name: def_output_primary.F90
!
! Purpose:
! Define primary output variables for netcdf. Variable type, scale, offset,
! fill value and/or range are defined and applied to the variable definition.
! Variable names are also defined.
!
! Description and Algorithm details:
! 1) For each variable, use ncdf_def_var routine to define variable.
! 2) Loop over views to define view geometry variables.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct In          Retrieval control parameters
! ncid        int    In          ID for open NCDF file to define
! dims_var    int(2) In          ID #s for lat and lon dimensions
! output_data struct Both        Structure of data for primary output file
!
! History:
! 2011/12/19, MJ: Creates initial version
! 2012/01/06, CP: Added cwp
! 2012/01/15, CP: Changed scale and range for cot
! 2012/06/15, CP: Changed cost scaling factor
! 2012/09/20, CP: Changed scaling factors
! 2012/11/03, MJ: Changed range specifications of ctp and stemp
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2013/xx/xx, MJ: Various changes to avoid cut-offs other fixes.
! 2013/10/06, CP: Bug fix created relative azimuth angle in primary file
! 2014/04/01, MJ: Cleanup and fix for illum
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/07/13, CP: Added AATSR time string and changed definition of land/sea mask
! 2014/xx/xx: CP: Added extra illumination options!
! 2014/08/07, GM: Fixes to attributes related to the above change.
! 2014/08/07, GM: Hoisted calls to nf90_redef() and nf90_enddef() from the
!    individual variable definition subroutines to be called once in this
!    subroutine.
! 2014/08/31, GM: Update to use general routines now in the common library.
! 2014/09/16, GM: Update for changes in the general routines in the common
!    library.
! 2014/10/24, OS: Some minor refactoring. Added output variables cldtype,
!    cldmask, cccot_pre, lusflag, DEM, and nisemask
! 2014/11/25, AP: Move scaling/offset definitions to output_routines.
! 2014/12/01, OS: added flag 9 = prob_opaque_ice to Pavolonis cloud type
! 2014/11/12, CP: added cloud albedo
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2014/12/31, GM: Remove useless error control especially since nc_def_var_*
!    routines handle errors to exit.
! 2015/02/05, CP: Added usgs flag meanings.
! 2015/07/01, CP: Added corrected cth
! 2015/07/26, GM: Added deflate_level and shuffle_flag arguments to
!    nc_def_var_*.
! 2015/07/31, AP: Rejig Diag for longer, variable state vector.
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/08/08, CP: Added in ATSR time stamp
! 2015/10/24, CP: Added in st and CF compliance
! 2015/10/24, CP: bug fix time
! 2015/11/14, GM: Fix some of the recent CF compliance fixes.
! 2015/11/17, OS: line 71 was too long for compiler
! 2015/10/24, GM: Fix flag_values and flag_meanings for lsflag.
! 2015/12/17, OS: Updated NetCDF time variable definition.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Move declarations of scale/offset/vmin/vmax from here to
!    alloc_ routines for fields that could be BTs or reflectances.
! 2015/01/07, AP: Make QCFlag long to accomodate longer state vectors.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/07/08, GM: Add fields for cloud layer 2.
! 2017/01/08, CP: Added multi layer phase type
! 2017/05/17, OS: Added ann phase variables, ann phase uncertainty is
!    a placeholder.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/01/19, GT: Removed QCFlag scale_factor and add_offset values, as these
!    should only be used for packed floating point data.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_output_primary(ncid, dim3d_var, output_data, indexing, &
   deflate_level, shuffle_flag, verbose, ch_var, phases)

   use orac_ncdf_m

   implicit none

   integer,                     intent(in)    :: ncid
   integer,                     intent(in)    :: dim3d_var(:)
   type(output_data_primary_t), intent(inout) :: output_data
   type(common_indices_t),      intent(in)    :: indexing
   integer,                     intent(in)    :: deflate_level
   logical,                     intent(in)    :: shuffle_flag
   logical,                     intent(in)    :: verbose
   integer,          optional,  intent(in)    :: ch_var(:)
   character(len=*), optional,  intent(in)    :: phases(:)

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   integer            :: i, j
   integer            :: i_view, i_rho
   integer            :: dims_var(2), view_var(1)

   dims_var = dim3d_var(1:2)
   view_var = dim3d_var(3)

   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_redef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_redef()'
      stop error_stop_code
   end if


   if (verbose) &
        write(*,*) 'def_output_primary: using deflate_level = ',deflate_level

   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   input_dummy='days since -4712-01-01 12:00:00'

   call ncdf_def_var_double_packed_double( &
           ncid, &
           dims_var, &
           'time', &
           output_data%vid_time, &
           verbose, &
           long_name     = 'Time in Julian days', &
           standard_name = 'Julian Days', &
           fill_value    = dreal_fill_value, &
           scale_factor  = output_data%time_scale, &
           add_offset    = output_data%time_offset, &
           valid_min     = output_data%time_vmin, &
           valid_max     = output_data%time_vmax, &
           units         = input_dummy, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lat
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'lat', &
           output_data%vid_lat, &
           verbose, &
           long_name     = 'latitude', &
           standard_name = 'latitude', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%lat_scale, &
           add_offset    = output_data%lat_offset, &
           valid_min     = output_data%lat_vmin, &
           valid_max     = output_data%lat_vmax, &
           units         = 'degrees_north', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lon
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'lon', &
           output_data%vid_lon, &
           verbose, &
           long_name     = 'longitude', &
           standard_name = 'longitude', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%lon_scale, &
           add_offset    = output_data%lon_offset, &
           valid_min     = output_data%lon_vmin, &
           valid_max     = output_data%lon_vmax, &
           units         = 'degrees_east', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! Loop over view angles
   !----------------------------------------------------------------------------
   do i_view=1,indexing%NViews

      write(input_num,"(i4)") i_view

      !-------------------------------------------------------------------------
      ! solar_zenith_view_no*
      !-------------------------------------------------------------------------
      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      input_dummy2='solar zenith angle for view no '//trim(adjustl(input_num))

      call ncdf_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sol_zen(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = 'solar_zenith_angle', &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%sol_scale, &
              add_offset    = output_data%sol_offset, &
              valid_min     = output_data%sol_vmin, &
              valid_max     = output_data%sol_vmax, &
              units         = 'degrees', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! satellite_zenith_view_no*
      !-------------------------------------------------------------------------
      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      input_dummy2='sensor zenith angle for view no '//trim(adjustl(input_num))

      call ncdf_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sat_zen(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = 'platform_zenith_angle', &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%sat_scale, &
              add_offset    = output_data%sat_offset, &
              valid_min     = output_data%sat_vmin, &
              valid_max     = output_data%sat_vmax, &
              units         = 'degrees', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! rel_azimuth_view_no*
      !-------------------------------------------------------------------------
      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      input_dummy2='relative azimuth angle for view no '// &
           trim(adjustl(input_num))

      call ncdf_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_rel_azi(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = 'relative_platform_azimuth_angle', &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%azi_scale, &
              add_offset    = output_data%azi_offset, &
              valid_min     = output_data%azi_vmin, &
              valid_max     = output_data%azi_vmax, &
              units         = 'degrees', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! sat_azimuth_view_no*
      !-------------------------------------------------------------------------
      input_dummy='sat_azimuth_view_no'//trim(adjustl(input_num))
      input_dummy2='satellite azimuth angle for view no '// &
           trim(adjustl(input_num))

      call ncdf_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sat_azi(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = 'satellite_platform_azimuth_angle', &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%azi_scale, &
              add_offset    = output_data%azi_offset, &
              valid_min     = output_data%azi_vmin, &
              valid_max     = output_data%azi_vmax, &
              units         = 'degrees', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)

   end do

if (indexing%flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot550
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot550', &
           output_data%vid_aot550, &
           verbose, &
           long_name     = 'aerosol optical thickness at 550 nm', &
           standard_name = 'atmosphere_optical_thickness_due_to_ambient_' // &
                           'aerosol_particles', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot550_scale, &
           add_offset    = output_data%aot550_offset, &
           valid_min     = output_data%aot550_vmin, &
           valid_max     = output_data%aot550_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aot550_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot550_uncertainty', &
           output_data%vid_aot550_uncertainty, &
           verbose, &
           long_name     = 'uncertainty in aerosol optical thickness at ' // &
                           '550 nm', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot550_uncertainty_scale, &
           add_offset    = output_data%aot550_uncertainty_offset, &
           valid_min     = output_data%aot550_uncertainty_vmin, &
           valid_max     = output_data%aot550_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aot870
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot870', &
           output_data%vid_aot870, &
           verbose, &
           long_name     = 'aerosol optical thickness at 870 nm', &
           standard_name = 'atmosphere_optical_thickness_due_to_ambient_' // &
                           'aerosol_particles', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot870_scale, &
           add_offset    = output_data%aot870_offset, &
           valid_min     = output_data%aot870_vmin, &
           valid_max     = output_data%aot870_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aot870_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot870_uncertainty', &
           output_data%vid_aot870_uncertainty, &
           verbose, &
           long_name     = 'uncertainty in aerosol optical thickness at ' // &
                           '870 nm', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot870_uncertainty_scale, &
           add_offset    = output_data%aot870_uncertainty_offset, &
           valid_min     = output_data%aot870_uncertainty_vmin, &
           valid_max     = output_data%aot870_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aer
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aer', &
           output_data%vid_aer, &
           verbose, &
           long_name     = 'aerosol effective radius', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aer_scale, &
           add_offset    = output_data%aer_offset, &
           valid_min     = output_data%aer_vmin, &
           valid_max     = output_data%aer_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aer_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aer_uncertainty', &
           output_data%vid_aer_uncertainty, &
           verbose, &
           long_name     = 'effective radius uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aer_uncertainty_scale, &
           add_offset    = output_data%aer_uncertainty_offset, &
           valid_min     = output_data%aer_uncertainty_vmin, &
           valid_max     = output_data%aer_uncertainty_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_rho) then
   !----------------------------------------------------------------------------
   ! rho_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar

      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j = 1, MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            i_rho = i_rho + 1

            call create_rho_field_name(j, 1, input_num, &
                 input_dummy2, input_dummy)

            call ncdf_def_var_short_packed_float( &
                 ncid, &
                 dims_var, &
                 trim(adjustl(input_dummy2)), &
                 output_data%vid_rho(i_rho), &
                 verbose, &
                 long_name     = trim(adjustl(input_dummy)), &
                 standard_name = '', &
                 fill_value    = sint_fill_value, &
                 scale_factor  = output_data%rho_scale, &
                 add_offset    = output_data%rho_offset, &
                 valid_min     = output_data%rho_vmin, &
                 valid_max     = output_data%rho_vmax, &
                 units         = '1', &
                 deflate_level = deflate_level, &
                 shuffle       = shuffle_flag)
         end if
      end do
   end do

   !----------------------------------------------------------------------------
   ! rho_uncertainty_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar

      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j = 1, MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            i_rho = i_rho + 1

            call create_rho_field_name(j, 2, input_num, &
                 input_dummy2, input_dummy)

            call ncdf_def_var_short_packed_float( &
                 ncid, &
                 dims_var, &
                 trim(adjustl(input_dummy2)), &
                 output_data%vid_rho_uncertainty(i_rho), &
                 verbose, &
                 long_name     = trim(adjustl(input_dummy)), &
                 standard_name = '', &
                 fill_value    = sint_fill_value, &
                 scale_factor  = output_data%rho_uncertainty_scale, &
                 add_offset    = output_data%rho_uncertainty_offset, &
                 valid_min     = output_data%rho_uncertainty_vmin, &
                 valid_max     = output_data%rho_uncertainty_vmax, &
                 units         = '1', &
                 deflate_level = deflate_level, &
                 shuffle       = shuffle_flag)
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   !----------------------------------------------------------------------------
   ! swansea_s_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='s parameter for Swansea surface reflectance model in ' // &
                     'channel no '//trim(adjustl(input_num))
         input_dummy2='swansea_s_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_swansea_s(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%swansea_s_scale, &
              add_offset    = output_data%swansea_s_offset, &
              valid_min     = output_data%swansea_s_vmin, &
              valid_max     = output_data%swansea_s_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! swansea_s_uncertainty_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='uncertainty in s parameter for Swansea surface ' // &
                     'reflectance model in channel no '//trim(adjustl(input_num))
         input_dummy2='swansea_s_uncertainty_in_channel_no_'// &
              trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_swansea_s_uncertainty(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%swansea_s_uncertainty_scale, &
              add_offset    = output_data%swansea_s_uncertainty_offset, &
              valid_min     = output_data%swansea_s_uncertainty_vmin, &
              valid_max     = output_data%swansea_s_uncertainty_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! swansea_p_in_view_no_*
   !----------------------------------------------------------------------------
   do i_view = 1, indexing%NViews

      write(input_num,"(i4)") i_view

      input_dummy='p parameter for Swansea surface reflectance model in ' // &
                  'view no '//trim(adjustl(input_num))
      input_dummy2='swansea_p_in_view_no_'//trim(adjustl(input_num))

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy2)), &
           output_data%vid_swansea_p(i_view), &
           verbose, &
           long_name     = trim(adjustl(input_dummy)), &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%swansea_p_scale, &
           add_offset    = output_data%swansea_p_offset, &
           valid_min     = output_data%swansea_p_vmin, &
           valid_max     = output_data%swansea_p_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! swansea_p_uncertainty_in_view_no_*
   !----------------------------------------------------------------------------
   do i_view = 1, indexing%NViews

      write(input_num,"(i4)") i_view

      input_dummy='uncertainty in p parameter for Swansea surface ' // &
                  'reflectance model in view no '//trim(adjustl(input_num))
      input_dummy2='swansea_p_uncertainty_in_view_no_'//trim(adjustl(input_num))

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy2)), &
           output_data%vid_swansea_p_uncertainty(i_view), &
           verbose, &
           long_name     = trim(adjustl(input_dummy)), &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%swansea_p_uncertainty_scale, &
           add_offset    = output_data%swansea_p_uncertainty_offset, &
           valid_min     = output_data%swansea_p_uncertainty_vmin, &
           valid_max     = output_data%swansea_p_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! diffuse_frac_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='diffuse fraction of radiation in channel no '// &
              trim(adjustl(input_num))
         input_dummy2='diffuse_frac_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_diffuse_frac(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%diffuse_frac_scale, &
              add_offset    = output_data%diffuse_frac_offset, &
              valid_min     = output_data%diffuse_frac_vmin, &
              valid_max     = output_data%diffuse_frac_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! diffuse_frac_uncertainty_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='uncertainty in diffuse fraction of radiation in ' // &
                     'channel no '//trim(adjustl(input_num))
         input_dummy2='diffuse_frac_uncertainty_in_channel_no_'// &
              trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_diffuse_frac_uncertainty(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%diffuse_frac_uncertainty_scale, &
              add_offset    = output_data%diffuse_frac_uncertainty_offset, &
              valid_min     = output_data%diffuse_frac_uncertainty_vmin, &
              valid_max     = output_data%diffuse_frac_uncertainty_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do
end if

if (indexing%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot', &
           output_data%vid_cot, &
           verbose, &
           long_name     = 'cloud optical thickness', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_scale, &
           add_offset    = output_data%cot_offset, &
           valid_min     = output_data%cot_vmin, &
           valid_max     = output_data%cot_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cot_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_uncertainty', &
           output_data%vid_cot_uncertainty, &
           verbose, &
           long_name     = 'cloud optical thickness uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_uncertainty_scale, &
           add_offset    = output_data%cot_uncertainty_offset, &
           valid_min     = output_data%cot_uncertainty_vmin, &
           valid_max     = output_data%cot_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer', &
           output_data%vid_cer, &
           verbose, &
           long_name     = 'cloud effective radius', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particles_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_scale, &
           add_offset    = output_data%cer_offset, &
           valid_min     = output_data%cer_vmin, &
           valid_max     = output_data%cer_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer_uncertainty', &
           output_data%vid_cer_uncertainty, &
           verbose, &
           long_name     = 'cloud effective radius uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_uncertainty_scale, &
           add_offset    = output_data%cer_uncertainty_offset, &
           valid_min     = output_data%cer_uncertainty_vmin, &
           valid_max     = output_data%cer_uncertainty_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp', &
           output_data%vid_ctp, &
           verbose, &
           long_name     = 'cloud top pressure', &
           standard_name = 'air_pressure_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_scale, &
           add_offset    = output_data%ctp_offset, &
           valid_min     = output_data%ctp_vmin, &
           valid_max     = output_data%ctp_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_uncertainty', &
           output_data%vid_ctp_uncertainty, &
           verbose, &
           long_name     = 'cloud top pressure uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_uncertainty_scale, &
           add_offset    = output_data%ctp_uncertainty_offset, &
           valid_min     = output_data%ctp_uncertainty_vmin, &
           valid_max     = output_data%ctp_uncertainty_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp_corrected
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_corrected', &
           output_data%vid_ctp_corrected, &
           verbose, &
           long_name     = 'corrected cloud top pressure', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_scale, &
           add_offset    = output_data%ctp_offset, &
           valid_min     = output_data%ctp_vmin, &
           valid_max     = output_data%ctp_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp_corrected_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_corrected_uncertainty', &
           output_data%vid_ctp_corrected_uncertainty, &
           verbose, &
           long_name     = 'corrected cloud top pressure uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_uncertainty_scale, &
           add_offset    = output_data%ctp_uncertainty_offset, &
           valid_min     = output_data%ctp_uncertainty_vmin, &
           valid_max     = output_data%ctp_uncertainty_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total', &
           output_data%vid_cc_total, &
           verbose, &
           long_name     = 'cloud fraction', &
           standard_name = 'cloud_area_fraction', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cc_total_scale, &
           add_offset    = output_data%cc_total_offset, &
           valid_min     = output_data%cc_total_vmin, &
           valid_max     = output_data%cc_total_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total_uncertainty', &
           output_data%vid_cc_total_uncertainty, &
           verbose, &
           long_name     = 'cloud fraction uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cc_total_uncertainty_scale, &
           add_offset    = output_data%cc_total_uncertainty_offset, &
           valid_min     = output_data%cc_total_uncertainty_vmin, &
           valid_max     = output_data%cc_total_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cot2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot2', &
           output_data%vid_cot2, &
           verbose, &
           long_name     = 'cloud optical thickness of layer 2', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_scale, &
           add_offset    = output_data%cot_offset, &
           valid_min     = output_data%cot_vmin, &
           valid_max     = output_data%cot_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cot2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot2_uncertainty', &
           output_data%vid_cot2_uncertainty, &
           verbose, &
           long_name     = 'cloud optical thickness of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_uncertainty_scale, &
           add_offset    = output_data%cot_uncertainty_offset, &
           valid_min     = output_data%cot_uncertainty_vmin, &
           valid_max     = output_data%cot_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer2', &
           output_data%vid_cer2, &
           verbose, &
           long_name     = 'cloud effective radius of layer 2', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particles_at_cloud_top_in_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_scale, &
           add_offset    = output_data%cer_offset, &
           valid_min     = output_data%cer_vmin, &
           valid_max     = output_data%cer_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer2_uncertainty', &
           output_data%vid_cer2_uncertainty, &
           verbose, &
           long_name     = 'cloud effective radius of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_uncertainty_scale, &
           add_offset    = output_data%cer_uncertainty_offset, &
           valid_min     = output_data%cer_uncertainty_vmin, &
           valid_max     = output_data%cer_uncertainty_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp2', &
           output_data%vid_ctp2, &
           verbose, &
           long_name     = 'cloud top pressure of layer 2', &
           standard_name = 'air_pressure_at_cloud_top_of_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_scale, &
           add_offset    = output_data%ctp_offset, &
           valid_min     = output_data%ctp_vmin, &
           valid_max     = output_data%ctp_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp2_uncertainty', &
           output_data%vid_ctp2_uncertainty, &
           verbose, &
           long_name     = 'cloud top pressure of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_uncertainty_scale, &
           add_offset    = output_data%ctp_uncertainty_offset, &
           valid_min     = output_data%ctp_uncertainty_vmin, &
           valid_max     = output_data%ctp_uncertainty_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total2', &
           output_data%vid_cc_total2, &
           verbose, &
           long_name     = 'cloud fraction of layer 2', &
           standard_name = 'cloud_area_fraction_of_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cc_total_scale, &
           add_offset    = output_data%cc_total_offset, &
           valid_min     = output_data%cc_total_vmin, &
           valid_max     = output_data%cc_total_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total2_uncertainty', &
           output_data%vid_cc_total2_uncertainty, &
           verbose, &
           long_name     = 'cloud fraction of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cc_total_uncertainty_scale, &
           add_offset    = output_data%cc_total_uncertainty_offset, &
           valid_min     = output_data%cc_total_uncertainty_vmin, &
           valid_max     = output_data%cc_total_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! stemp
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp', &
           output_data%vid_stemp, &
           verbose, &
           long_name     = 'surface temperature', &
           standard_name = 'surface_temperature', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_scale, &
           add_offset    = output_data%stemp_offset, &
           valid_min     = output_data%stemp_vmin, &
           valid_max     = output_data%stemp_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! stemp_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp_uncertainty', &
           output_data%vid_stemp_uncertainty, &
           verbose, &
           long_name     = 'surface temperature uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_uncertainty_scale, &
           add_offset    = output_data%stemp_uncertainty_offset, &
           valid_min     = output_data%stemp_uncertainty_vmin, &
           valid_max     = output_data%stemp_uncertainty_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth', &
           output_data%vid_cth, &
           verbose, &
           long_name     = 'cloud top height', &
           standard_name = 'cloud_top_altitude', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_scale, &
           add_offset    = output_data%cth_offset, &
           valid_min     = output_data%cth_vmin, &
           valid_max     = output_data%cth_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_uncertainty', &
           output_data%vid_cth_uncertainty, &
           verbose, &
           long_name     = 'cloud top height uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_uncertainty_scale, &
           add_offset    = output_data%cth_uncertainty_offset, &
           valid_min     = output_data%cth_uncertainty_vmin, &
           valid_max     = output_data%cth_uncertainty_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth_corrected
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_corrected', &
           output_data%vid_cth_corrected, &
           verbose, &
           long_name     = 'corrected cloud top height', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_scale, &
           add_offset    = output_data%cth_offset, &
           valid_min     = output_data%cth_vmin, &
           valid_max     = output_data%cth_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth_corrected_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_corrected_uncertainty', &
           output_data%vid_cth_corrected_uncertainty, &
           verbose, &
           long_name     = 'corrected cloud top height uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_uncertainty_scale, &
           add_offset    = output_data%cth_uncertainty_offset, &
           valid_min     = output_data%cth_uncertainty_vmin, &
           valid_max     = output_data%cth_uncertainty_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt', &
           output_data%vid_ctt, &
           verbose, &
           long_name     = 'cloud top temperature', &
           standard_name = 'air_temperature_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_scale, &
           add_offset    = output_data%ctt_offset, &
           valid_min     = output_data%ctt_vmin, &
           valid_max     = output_data%ctt_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt_uncertainty', &
           output_data%vid_ctt_uncertainty, &
           verbose, &
           long_name     = 'cloud top temperature uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_uncertainty_scale, &
           add_offset    = output_data%ctt_uncertainty_offset, &
           valid_min     = output_data%ctt_uncertainty_vmin, &
           valid_max     = output_data%ctt_uncertainty_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt_corrected
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt_corrected', &
           output_data%vid_ctt_corrected, &
           verbose, &
           long_name     = 'corrected cloud top temperature', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_scale, &
           add_offset    = output_data%ctt_offset, &
           valid_min     = output_data%ctt_vmin, &
           valid_max     = output_data%ctt_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt_corrected_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt_corrected_uncertainty', &
           output_data%vid_ctt_corrected_uncertainty, &
           verbose, &
           long_name     = 'corrected cloud top temperature uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_uncertainty_scale, &
           add_offset    = output_data%ctt_uncertainty_offset, &
           valid_min     = output_data%ctt_uncertainty_vmin, &
           valid_max     = output_data%ctt_uncertainty_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cwp
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp', &
           output_data%vid_cwp, &
           verbose, &
           long_name     = 'cloud liquid water path', &
           standard_name = 'atmosphere_mass_content_of_cloud_liquid_water', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_scale, &
           add_offset    = output_data%cwp_offset, &
           valid_min     = output_data%cwp_vmin, &
           valid_max     = output_data%cwp_vmax, &
           units         = 'g/m2', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cwp_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp_uncertainty', &
           output_data%vid_cwp_uncertainty, &
           verbose, &
           long_name     = 'cloud liquid water path uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_uncertainty_scale, &
           add_offset    = output_data%cwp_uncertainty_offset, &
           valid_min     = output_data%cwp_uncertainty_vmin, &
           valid_max     = output_data%cwp_uncertainty_vmax, &
           units         = 'g/m2', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cth2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth2', &
           output_data%vid_cth2, &
           verbose, &
           long_name     = 'cloud top height of layer 2', &
           standard_name = 'cloud_top_altitude_of_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_scale, &
           add_offset    = output_data%cth_offset, &
           valid_min     = output_data%cth_vmin, &
           valid_max     = output_data%cth_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth2_uncertainty', &
           output_data%vid_cth2_uncertainty, &
           verbose, &
           long_name     = 'cloud top height of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_uncertainty_scale, &
           add_offset    = output_data%cth_uncertainty_offset, &
           valid_min     = output_data%cth_uncertainty_vmin, &
           valid_max     = output_data%cth_uncertainty_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt2', &
           output_data%vid_ctt2, &
           verbose, &
           long_name     = 'cloud top temperature of layer 2', &
           standard_name = 'air_temperature_at_cloud_top_of_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_scale, &
           add_offset    = output_data%ctt_offset, &
           valid_min     = output_data%ctt_vmin, &
           valid_max     = output_data%ctt_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt2_uncertainty', &
           output_data%vid_ctt2_uncertainty, &
           verbose, &
           long_name     = 'cloud top temperature of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_uncertainty_scale, &
           add_offset    = output_data%ctt_uncertainty_offset, &
           valid_min     = output_data%ctt_uncertainty_vmin, &
           valid_max     = output_data%ctt_uncertainty_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cwp2
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp2', &
           output_data%vid_cwp2, &
           verbose, &
           long_name     = 'cloud liquid water path of layer 2', &
           standard_name = 'atmosphere_mass_content_of_cloud_liquid_water_' // &
                           'of_layer_2', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_scale, &
           add_offset    = output_data%cwp_offset, &
           valid_min     = output_data%cwp_vmin, &
           valid_max     = output_data%cwp_vmax, &
           units         = 'g/m2', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cwp2_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp2_uncertainty', &
           output_data%vid_cwp2_uncertainty, &
           verbose, &
           long_name     = 'cloud liquid water path of layer 2 uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_uncertainty_scale, &
           add_offset    = output_data%cwp_uncertainty_offset, &
           valid_min     = output_data%cwp_uncertainty_vmin, &
           valid_max     = output_data%cwp_uncertainty_vmax, &
           units         = 'g/m2', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud) then

   !----------------------------------------------------------------------------
   ! cloud_albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%alb_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='cloud_albedo in channel no '//trim(adjustl(input_num))
         input_dummy2='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_cloud_albedo(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = 'cloud_albedo', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%cloud_albedo_scale, &
              add_offset    = output_data%cloud_albedo_offset, &
              valid_min     = output_data%cloud_albedo_vmin, &
              valid_max     = output_data%cloud_albedo_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! cloud_albedo_uncertainty_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NSolar
      if (indexing%alb_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='cloud_albedo_uncertainty in channel no '// &
              trim(adjustl(input_num))
         input_dummy2='cloud_albedo_uncertainty_in_channel_no_'// &
              trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_cloud_albedo_uncertainty(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%cloud_albedo_uncertainty_scale, &
              add_offset    = output_data%cloud_albedo_uncertainty_offset, &
              valid_min     = output_data%cloud_albedo_uncertainty_vmin, &
              valid_max     = output_data%cloud_albedo_uncertainty_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! cee_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NThermal
      if (indexing%cee_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YThermal(i))

         input_dummy='cee in channel no '//trim(adjustl(input_num))
         input_dummy2='cee_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_cee(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = 'cee', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%cee_scale, &
              add_offset    = output_data%cee_offset, &
              valid_min     = output_data%cee_vmin, &
              valid_max     = output_data%cee_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! cee_uncertainty_in_channel_no_*
   !----------------------------------------------------------------------------
   i_rho = 0
   do i = 1, indexing%NThermal
      if (indexing%cee_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)")indexing% Y_Id(indexing%YThermal(i))

         input_dummy='cee_uncertainty in channel no '//trim(adjustl(input_num))
         input_dummy2='cee_uncertainty_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy2)), &
              output_data%vid_cee_uncertainty(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%cee_uncertainty_scale, &
              add_offset    = output_data%cee_uncertainty_offset, &
              valid_min     = output_data%cee_uncertainty_vmin, &
              valid_max     = output_data%cee_uncertainty_vmax, &
              units         = '1', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do

   !----------------------------------------------------------------------------
   ! cccot_pre (cloud optical thickness)
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dim3d_var, &
           'cccot_pre', &
           output_data%vid_cccot_pre, &
           verbose, &
           long_name     = 'neural network cloud optical thickness', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cccot_pre_scale, &
           add_offset    = output_data%cccot_pre_offset, &
           valid_min     = output_data%cccot_pre_vmin, &
           valid_max     = output_data%cccot_pre_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

end if

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'niter', &
           output_data%vid_niter, &
           verbose, &
           long_name     = 'number of retrieval iterations', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%niter_scale, &
           add_offset    = output_data%niter_offset, &
           valid_min     = output_data%niter_vmin, &
           valid_max     = output_data%niter_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costja', &
           output_data%vid_costja, &
           verbose, &
           long_name     = 'a priori cost at solution', &
           standard_name = '', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costja_scale, &
           add_offset    = output_data%costja_offset, &
           valid_min     = output_data%costja_vmin, &
           valid_max     = output_data%costja_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costjm', &
           output_data%vid_costjm, &
           verbose, &
           long_name     = 'measurement cost at solution', &
           standard_name = '', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costjm_scale, &
           add_offset    = output_data%costjm_offset, &
           valid_min     = output_data%costjm_vmin, &
           valid_max     = output_data%costjm_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   call ncdf_def_var_long_packed_long( &
           ncid, &
           dims_var, &
           'qcflag', &
           output_data%vid_qcflag, &
           verbose, &
           long_name     = 'quality control flag', &
           standard_name = '', &
           fill_value    = int(-1,kind=lint), &
!           scale_factor  = output_data%qcflag_scale, &
!           add_offset    = output_data%qcflag_offset, &
           valid_min     = output_data%qcflag_vmin, &
           valid_max     = output_data%qcflag_vmax, &
           flag_masks    = trim(output_data%qc_flag_masks), &
           flag_meanings = trim(output_data%qc_flag_meanings), &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! channels_used
   !----------------------------------------------------------------------------
   call ncdf_def_var_dlong_packed_dlong( &
           ncid, &
           dims_var, &
           'channels_used', &
           output_data%vid_channels_used, &
           verbose, &
           long_name     = 'channels used by retrieval', &
           standard_name = '', &
!          fill_value    = int(0, kind=dint), &
!          scale_factor  = output_data%channels_used_scale, &
!          add_offset    = output_data%channels_used_offset, &
!          valid_min     = output_data%channels_used_vmin, &
!          valid_max     = output_data%channels_used_vmax, &
           flag_masks    = trim(output_data%ch_flag_masks), &
           flag_meanings = trim(output_data%ch_flag_meanings), &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   ! Fields commented out for now owning to a bug in the netCDF library
   ! when dealing with INT64 variables. - 11 Jul 2017

   !----------------------------------------------------------------------------
   ! variables_retrieved
   !----------------------------------------------------------------------------
   call ncdf_def_var_dlong_packed_dlong( &
           ncid, &
           dims_var, &
           'variables_retrieved', &
           output_data%vid_variables_retrieved, &
           verbose, &
           long_name     = 'variables calculated by retrieval', &
           standard_name = '', &
!          fill_value    = int(0, kind=dint), &
!          scale_factor  = output_data%variables_retrieved_scale, &
!          add_offset    = output_data%variables_retrieved_offset, &
!          valid_min     = output_data%variables_retrieved_vmin, &
!          valid_max     = output_data%variables_retrieved_vmax, &
           flag_masks    = trim(output_data%vr_flag_masks), &
           flag_meanings = trim(output_data%vr_flag_meanings), &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'lsflag', &
           output_data%vid_lsflag, &
           verbose, &
           long_name     = 'land/sea flag', &
           standard_name = 'land_binary_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%lsflag_scale, &
           add_offset    = output_data%lsflag_offset, &
           valid_min     = output_data%lsflag_vmin, &
           valid_max     = output_data%lsflag_vmax, &
           units         = '1', &
           flag_values   = '0b 1b', &
           flag_meanings = 'sea land', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'lusflag', &
           output_data%vid_lusflag, &
           verbose, &
           long_name     = 'land use flag', &
           standard_name = 'land_use_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%lusflag_scale, &
           add_offset    = output_data%lusflag_offset, &
           valid_min     = output_data%lusflag_vmin, &
           valid_max     = output_data%lusflag_vmax, &
           units         = '1', &
           flag_values   = '0b 1b 2b 3b 4b 5b 6b 7b 8b 9b 10b ' // &
                           '11b 12b 13b 14b 15b 16b 17b 18b 19b ' // &
                           '20b 21b 22b 23b 24b', &
           flag_meanings = '1:Urban_and_Built-Up_Land ' // &
                           '2:Dryland_Cropland_and_Pasture ' // &
                           '3:Irrigated_Cropland_and_Pasture ' // &
                           '4:Mixed_Dryland/Irrigated_Cropland_and_Pasture ' // &
                           '5:Cropland/Grassland Mosaic ' // &
                           '6:Cropland/Woodland Mosaic ' // &
                           '7:Grassland ' // &
                           '8:Shrubland ' // &
                           '9:Mixed Shrubland/Grassland ' // &
                           '10:Savanna ' // &
                           '11:Deciduous_Broadleaf_Forest ' // &
                           '12:Deciduous_Needleleaf_Forest ' // &
                           '13:Evergreen_Broadleaf_Forest ' // &
                           '14:Evergreen_Needleleaf_Forest ' // &
                           '15:Mixed_Forest ' // &
                           '16:Water_Bodies ' // &
                           '17:Herbaceous_Wetland ' // &
                           '18:Wooded_Wetland ' // &
                           '19:Barren_or_Sparsely_Vegetated ' // &
                           '20:Herbaceous_Tundra ' // &
                           '21:Wooded_Tundra ' // &
                           '22:Mixed_Tundra ' // &
                           '23:Bare_Ground_Tundra ' // &
                           '24:Snow_or_Ice ' // &
                           '99:Interrupted_Areas ' // &
                           '100:Missing_Data', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_short( &
           ncid, &
           dims_var, &
           'dem', &
           output_data%vid_dem, &
           verbose, &
           long_name     = 'Digital elevation model', &
           standard_name = '', &
           fill_value    = int(-1,kind=sint), &
           scale_factor  = output_data%dem_scale, &
           add_offset    = output_data%dem_offset, &
           valid_min     = output_data%dem_vmin, &
           valid_max     = output_data%dem_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'illum', &
           output_data%vid_illum, &
           verbose, &
           long_name     = 'illumination flag', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%illum_scale, &
           add_offset    = output_data%illum_offset, &
           valid_min     = output_data%illum_vmin, &
           valid_max     = output_data%illum_vmax, &
           units         = '1', &
           flag_values   ='1b 2b 3b', &
           flag_meanings ='day twilight night', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cldtype (ie. Pavolonis phase)
   !----------------------------------------------------------------------------
   input_dummy='clear ' // &
               'N/A ' // &
               'fog ' // &
               'water ' // &
               'supercooled ' // &
               'mixed ' // &
               'opaque_ice ' // &
               'cirrus ' // &
               'overlap ' // &
               'prob_opaque_ice'

   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dim3d_var, &
           'cldtype', &
           output_data%vid_cldtype, &
           verbose, &
           long_name     = 'Pavolonis cloud type', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldtype_scale, &
           add_offset    = output_data%cldtype_offset, &
           valid_min     = output_data%cldtype_vmin, &
           valid_max     = output_data%cldtype_vmax, &
           units         = '1', &
           flag_values   = '0b 1b 2b 3b 4b 5b 6b 7b 8b 9b', &
           flag_meanings = trim(adjustl(input_dummy)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

if (indexing%flags%do_cldmask) then
   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dim3d_var, &
           'cldmask', &
           output_data%vid_cldmask, &
           verbose, &
           long_name     = 'Neural net cloud mask (radiance based)', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldmask_scale, &
           add_offset    = output_data%cldmask_offset, &
           valid_min     = output_data%cldmask_vmin, &
           valid_max     = output_data%cldmask_vmax, &
           units         = '1', &
           flag_values   = '0b 1b', &
           flag_meanings = 'clear cloudy', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if
if (indexing%flags%do_cldmask_uncertainty) then
   !----------------------------------------------------------------------------
   ! cldmask_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dim3d_var, &
           'cldmask_uncertainty', &
           output_data%vid_cldmask_uncertainty, &
           verbose, &
           long_name     = 'Neural net cloud mask (radiance based) ' // &
                           'uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cldmask_uncertainty_scale, &
           add_offset    = output_data%cldmask_uncertainty_offset, &
           valid_min     = output_data%cldmask_uncertainty_vmin, &
           valid_max     = output_data%cldmask_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_ann_phase) then
   !----------------------------------------------------------------------------
   ! ann_phase
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dim3d_var, &
           'ann_phase', &
           output_data%vid_ann_phase, &
           verbose, &
           long_name     = 'Neural net cloud phase mask (radiance based)', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%ann_phase_scale, &
           add_offset    = output_data%ann_phase_offset, &
           valid_min     = output_data%ann_phase_vmin, &
           valid_max     = output_data%ann_phase_vmax, &
           units         = '1', &
           flag_values   = '0b 1b 2b', &
           flag_meanings = 'clear liquid ice', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cphcot (ANN phase cloud optical thickness)
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dim3d_var, &
           'cphcot', &
           output_data%vid_cphcot, &
           verbose, &
           long_name     = 'neural network cloud optical thickness (phase)', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cphcot_scale, &
           add_offset    = output_data%cphcot_offset, &
           valid_min     = output_data%cphcot_vmin, &
           valid_max     = output_data%cphcot_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

end if

if (indexing%flags%do_ann_phase_uncertainty) then
   !----------------------------------------------------------------------------
   ! ann_phase_uncertainty
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dim3d_var, &
           'ann_phase_uncertainty', &
           output_data%vid_ann_phase_uncertainty, &
           verbose, &
           long_name     = 'Neural net cloud phase mask (radiance based) ' // &
                           'uncertainty', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ann_phase_uncertainty_scale, &
           add_offset    = output_data%ann_phase_uncertainty_offset, &
           valid_min     = output_data%ann_phase_uncertainty_vmin, &
           valid_max     = output_data%ann_phase_uncertainty_vmax, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_phase) then
   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   if (present(phases)) then
      input_dummy = phases(1)
      input_dummy2 = '1b'
      do i = 2, size(phases)
         input_dummy = trim(input_dummy) // ' ' // phases(i)

         write(input_num,"(I4,A)") i, 'b'
         input_dummy2 = trim(input_dummy2) // ' ' // trim(adjustl(input_num))
      end do
   else
      input_dummy = 'clear/unknown liquid ice multi_layer'
      input_dummy2 = '0b 1b 2b 3b'
   end if

   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'phase', &
           output_data%vid_phase, &
           verbose, &
           long_name     = 'cloud phase flag', &
           standard_name = 'thermodynamic_phase_of_cloud_water_particles_' // &
                           'at_cloud_top', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_scale, &
           add_offset    = output_data%phase_offset, &
           valid_min     = output_data%phase_vmin, &
           valid_max     = output_data%phase_vmax, &
           flag_values   = input_dummy2, &
           flag_meanings = input_dummy, &
           units         = '1', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_phase_pavolonis) then
   !----------------------------------------------------------------------------
   ! phase_pavolonis
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'phase_pavolonis', &
           output_data%vid_phase_pavolonis, &
           verbose, &
           long_name     = 'cloud phase flag Pavolonis', &
           standard_name = 'thermodynamic_phase_of_cloud_water_particles_' // &
                           'at_cloud_top', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_pavolonis_scale, &
           add_offset    = output_data%phase_pavolonis_offset, &
           valid_min     = output_data%phase_pavolonis_vmin, &
           valid_max     = output_data%phase_pavolonis_vmax, &
           units         = '1', &
           flag_values   = '0b 1b 2b', &
           flag_meanings = 'clear/unknown liquid ice', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_indexing .and. present(ch_var)) then
   !----------------------------------------------------------------------------
   ! y_id
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           ch_var, &
           'y_id', &
           output_data%vid_y_id, &
           verbose, &
           long_name     = 'instrument channel index', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%y_id_scale, &
           add_offset    = output_data%y_id_offset, &
           valid_min     = output_data%y_id_vmin, &
           valid_max     = output_data%y_id_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ch_is
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           ch_var, &
           'ch_is', &
           output_data%vid_ch_is, &
           verbose, &
           long_name     = 'instrument channel flags', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%ch_is_scale, &
           add_offset    = output_data%ch_is_offset, &
           valid_min     = output_data%ch_is_vmin, &
           valid_max     = output_data%ch_is_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! rho_flags
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           ch_var, &
           'rho_flags', &
           output_data%vid_rho_flags, &
           verbose, &
           long_name     = 'surface reflectance output flags', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%rho_flags_scale, &
           add_offset    = output_data%rho_flags_offset, &
           valid_min     = output_data%rho_flags_vmin, &
           valid_max     = output_data%rho_flags_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_indexing) then
   !----------------------------------------------------------------------------
   ! view_id
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
           ncid, &
           view_var, &
           'view_id', &
           output_data%vid_view_id, &
           verbose, &
           long_name     = 'instrument channel view index', &
           standard_name = '', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%view_id_scale, &
           add_offset    = output_data%view_id_offset, &
           valid_min     = output_data%view_id_vmin, &
           valid_max     = output_data%view_id_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine def_output_primary
