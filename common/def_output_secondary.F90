!-------------------------------------------------------------------------------
! Name: def_output_secondary.F90
!
! Purpose:
! Define secondary output variables for netcdf. Variable type, scale, offset,
! fill value and /or range are defined and applied to the variable definition.
! Variable names are also defined.
!
! Description and Algorithm details:
! 1) For each variable, use ncdf_def_var routine to define variable.
! 2) Loop over views to define measurement channel variables.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct In          Retrieval control parameters
! lcovar      logic  In
! ncid        int    In          ID for open NCDF file to define
! dims_var    int(2) In          ID #s for lat and lon dimensions
! output_data struct Both        Structure of data for primary output file
!
! History:
! 2011/12/19, MJ: Creates initial version
! 2012/01/05, CP: Add channel info
! 2012/01/15, CP: Modified scales and offsets for residuals
! 2012/02/28, CP: Bug fix to attributes of scanline_v
! 2012/02/28, CP: Added in albedo
! 2012/02/28, CP: Indexedy_id array correctly
! 2013/01/25, CP: Modified how channels are labelled now reads form the input
!    file the channel names.
! 2013/05/29, GT: Added degrees of freedom for signal
! 2013/07/24, AP: Fixed bug in writing output_data%vid_y0
! 2013/12/05, MJ: Fixes bug with writing of albedo and radiance and bt output.
! 2013/12/09, MJ: Slightly adapts range of reflectances.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/08/07, GM: Hoisted calls to nf90_redef() and nf90_enddef() from the
!    individual variable definition subroutines to be called once in this
!    subroutine.
! 2014/08/31, GM: Update to use general routines now in the common library.
! 2014/09/16, GM: Update for changes in the general routines in the common
!    library.
! 2014/12/17, CP: added offset value to stemp and brightness temperature values
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2014/12/31, GM: Remove useless error control especially since nc_def_var_*
!    routines handle errors to exit.
! 2015/01/12, AP: Bug fix in my previous commit.
! 2015/01/12, AP: Switching to Ctrl%Ind%Ch_Is rather than any() logic.
! 2015/07/26, GM: Added deflate_level and shuffle_flag arguments to
!    nc_def_var_*.
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Move declarations of scale/offset/vmin/vmax from here to alloc_
!    routines for fields that could be BTs or reflectances. Have all albedo
!    fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure. Pass logical array
!    to identify thermal channels rather than dealing with Ch_Is.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/07/08, GM: Add fields for cloud layer 2.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_output_secondary(ncid, dim3d_var, output_data, indexing, &
   deflate_level, shuffle_flag, verbose)

   use orac_ncdf_m

   implicit none

   integer,                       intent(in)    :: ncid
   integer,                       intent(in)    :: dim3d_var(:)
   type(output_data_secondary_t), intent(inout) :: output_data
   type(common_indices_t),        intent(in)    :: indexing
   integer,                       intent(in)    :: deflate_level
   logical,                       intent(in)    :: shuffle_flag
   logical,                       intent(in)    :: verbose

   character(len=32)  :: input_num
   character(len=512) :: input_dummy, input_dummy2, input_dummy3
   integer            :: i, j, i_rho
   integer            :: dims_var(2)

   dims_var = dim3d_var(1:2)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_redef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_redef()'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! scanline_u
   !----------------------------------------------------------------------------
   call ncdf_def_var_long_packed_long( &
           ncid, &
           dims_var, &
           'scanline_u', &
           output_data%vid_scanline_u, &
           verbose, &
           long_name     = 'across track pixel index', &
           standard_name = 'across_track_pixel_index', &
           fill_value    = lint_fill_value, &
           scale_factor  = output_data%scanline_u_scale, &
           add_offset    = output_data%scanline_u_offset, &
           valid_min     = output_data%scanline_u_vmin, &
           valid_max     = output_data%scanline_u_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! scanline_v
   !----------------------------------------------------------------------------
   call ncdf_def_var_long_packed_long( &
           ncid, &
           dims_var, &
           'scanline_v', &
           output_data%vid_scanline_v, &
           verbose, &
           long_name     = 'along track pixel index', &
           standard_name = 'along_track_pixel_index', &
           fill_value    = lint_fill_value, &
           scale_factor  = output_data%scanline_v_scale, &
           add_offset    = output_data%scanline_v_offset, &
           valid_min     = output_data%scanline_v_vmin, &
           valid_max     = output_data%scanline_v_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

if (indexing%flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot550_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot550_ap', &
           output_data%vid_aot550_ap, &
           verbose, &
           long_name     = 'aerosol optical thickness at 550 nm a priori', &
           standard_name = 'atmosphere_optical_thickness_due_to_ambient_' // &
                           'aerosol_particles a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot550_ap_scale, &
           add_offset    = output_data%aot550_ap_offset, &
           valid_min     = output_data%aot550_ap_vmin, &
           valid_max     = output_data%aot550_ap_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aot550_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aot550_fg', &
           output_data%vid_aot550_fg, &
           verbose, &
           long_name     = 'aerosol optical thickness at 550 nm first guess', &
           standard_name = 'atmosphere_optical_thickness_due_to_ambient_' // &
                           'aerosol_particles first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aot550_fg_scale, &
           add_offset    = output_data%aot550_fg_offset, &
           valid_min     = output_data%aot550_fg_vmin, &
           valid_max     = output_data%aot550_fg_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aer_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aer_ap', &
           output_data%vid_aer_ap, &
           verbose, &
           long_name     = 'aerosol effective radius a priori', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aer_ap_scale, &
           add_offset    = output_data%aer_ap_offset, &
           valid_min     = output_data%aer_ap_vmin, &
           valid_max     = output_data%aer_ap_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! aer_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'aer_fg', &
           output_data%vid_aer_fg, &
           verbose, &
           long_name     = 'aerosol effective radius first guess', &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%aer_fg_scale, &
           add_offset    = output_data%aer_fg_offset, &
           valid_min     = output_data%aer_fg_vmin, &
           valid_max     = output_data%aer_fg_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_rho) then
   i_rho = 0
   do i=1,indexing%NSolar

      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j=1,MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            i_rho = i_rho + 1

   !----------------------------------------------------------------------------
   ! rho_XX_ap_in_channel_no_*
   !----------------------------------------------------------------------------
            call create_rho_field_name(j, 3, input_num, &
                 input_dummy, input_dummy2)

            call ncdf_def_var_short_packed_float( &
                 ncid, &
                 dims_var, &
                 trim(adjustl(input_dummy)), &
                 output_data%vid_rho_ap(i_rho), &
                 verbose, &
                 long_name     = trim(adjustl(input_dummy2)), &
                 standard_name = '', &
                 fill_value    = sint_fill_value, &
                 scale_factor  = output_data%rho_ap_scale, &
                 add_offset    = output_data%rho_ap_offset, &
                 valid_min     = output_data%rho_ap_vmin, &
                 valid_max     = output_data%rho_ap_vmax, &
                 deflate_level = deflate_level, &
                 shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! rho_XX_fg_in_channel_no_*
   !----------------------------------------------------------------------------
            call create_rho_field_name(j, 4, input_num, &
                 input_dummy, input_dummy2)

            call ncdf_def_var_short_packed_float( &
                 ncid, &
                 dims_var, &
                 trim(adjustl(input_dummy)), &
                 output_data%vid_rho_fg(i_rho), &
                 verbose, &
                 long_name     = trim(adjustl(input_dummy2)), &
                 standard_name = '', &
                 fill_value    = sint_fill_value, &
                 scale_factor  = output_data%rho_fg_scale, &
                 add_offset    = output_data%rho_fg_offset, &
                 valid_min     = output_data%rho_fg_vmin, &
                 valid_max     = output_data%rho_fg_vmax, &
                 deflate_level = deflate_level, &
                 shuffle       = shuffle_flag)
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   i_rho = 0
   do i=1,indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

   !----------------------------------------------------------------------------
   ! swansea_s_ap_in_channel_no_*
   !----------------------------------------------------------------------------
         input_dummy2='s parameter a priori in channel no '// &
              trim(adjustl(input_num))
         input_dummy='swansea_s_ap_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_swansea_s_ap(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%swansea_s_ap_scale, &
              add_offset    = output_data%swansea_s_ap_offset, &
              valid_min     = output_data%swansea_s_ap_vmin, &
              valid_max     = output_data%swansea_s_ap_vmax, &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! swansea_s_fg_in_channel_no_*
   !----------------------------------------------------------------------------
         input_dummy2='s parameter first guess in channel no '// &
              trim(adjustl(input_num))
         input_dummy='swansea_s_fg_in_channel_no_'//trim(adjustl(input_num))

         call ncdf_def_var_short_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_swansea_s_fg(i_rho), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = '', &
              fill_value    = sint_fill_value, &
              scale_factor  = output_data%swansea_s_fg_scale, &
              add_offset    = output_data%swansea_s_fg_offset, &
              valid_min     = output_data%swansea_s_fg_vmin, &
              valid_max     = output_data%swansea_s_fg_vmax, &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)
      end if
   end do


   do i=1,indexing%NViews

      write(input_num,"(i4)") i

   !----------------------------------------------------------------------------
   ! swansea_p_ap_in_channel_no_*
   !----------------------------------------------------------------------------
      input_dummy2='p parameter a priori in channel no '//&
           trim(adjustl(input_num))
      input_dummy='swansea_p_ap_in_view_no_'//trim(adjustl(input_num))

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_swansea_p_ap(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%swansea_p_ap_scale, &
           add_offset    = output_data%swansea_p_ap_offset, &
           valid_min     = output_data%swansea_p_ap_vmin, &
           valid_max     = output_data%swansea_p_ap_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! swansea_p_fg_in_channel_no_*
   !----------------------------------------------------------------------------
      input_dummy2='p parameter first guess in channel no '//&
           trim(adjustl(input_num))
      input_dummy='swansea_p_fg_in_view_no_'//trim(adjustl(input_num))

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_swansea_p_fg(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = '', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%swansea_p_fg_scale, &
           add_offset    = output_data%swansea_p_fg_offset, &
           valid_min     = output_data%swansea_p_fg_vmin, &
           valid_max     = output_data%swansea_p_fg_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do
end if

if (indexing%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_ap', &
           output_data%vid_cot_ap, &
           verbose, &
           long_name     = 'cloud optical thickness a priori', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud ' // &
                           'a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_ap_scale, &
           add_offset    = output_data%cot_ap_offset, &
           valid_min     = output_data%cot_ap_vmin, &
           valid_max     = output_data%cot_ap_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cot_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_fg', &
           output_data%vid_cot_fg, &
           verbose, &
           long_name     = 'cloud optical thickness first guess', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud ' // &
                           'first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_fg_scale, &
           add_offset    = output_data%cot_fg_offset, &
           valid_min     = output_data%cot_fg_vmin, &
           valid_max     = output_data%cot_fg_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer_ap', &
           output_data%vid_cer_ap, &
           verbose, &
           long_name     = 'cloud effective radius a priori', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particle_at_cloud_top a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_ap_scale, &
           add_offset    = output_data%cer_ap_offset, &
           valid_min     = output_data%cer_ap_vmin, &
           valid_max     = output_data%cer_ap_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer_fg', &
           output_data%vid_cer_fg, &
           verbose, &
           long_name     = 'cloud effective radius first guess', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particle_at_cloud_top first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_fg_scale, &
           add_offset    = output_data%cer_fg_offset, &
           valid_min     = output_data%cer_fg_vmin, &
           valid_max     = output_data%cer_fg_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_ap', &
           output_data%vid_ctp_ap, &
           verbose, &
           long_name     = 'cloud top pressure a priori', &
           standard_name = 'air_pressure_at_cloud_top a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_ap_scale, &
           add_offset    = output_data%ctp_ap_offset, &
           valid_min     = output_data%ctp_ap_vmin, &
           valid_max     = output_data%ctp_ap_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_fg', &
           output_data%vid_ctp_fg, &
           verbose, &
           long_name     = 'cloud top pressure first guess', &
           standard_name = 'air_pressure_at_cloud_top first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_fg_scale, &
           add_offset    = output_data%ctp_fg_offset, &
           valid_min     = output_data%ctp_fg_vmin, &
           valid_max     = output_data%ctp_fg_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cot2_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot2_ap', &
           output_data%vid_cot2_ap, &
           verbose, &
           long_name     = 'cloud optical thickness of layer 2 a priori', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud ' // &
                           'of_layer_2 a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_ap_scale, &
           add_offset    = output_data%cot_ap_offset, &
           valid_min     = output_data%cot_ap_vmin, &
           valid_max     = output_data%cot_ap_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cot2_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot2_fg', &
           output_data%vid_cot2_fg, &
           verbose, &
           long_name     = 'cloud optical thickness of layer 2 first guess', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud ' // &
                           'of_layer_2 first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_fg_scale, &
           add_offset    = output_data%cot_fg_offset, &
           valid_min     = output_data%cot_fg_vmin, &
           valid_max     = output_data%cot_fg_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer2_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer2_ap', &
           output_data%vid_cer2_ap, &
           verbose, &
           long_name     = 'cloud effective radius of layer 2 a priori', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particle_at_cloud_top of_layer_2 a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_ap_scale, &
           add_offset    = output_data%cer_ap_offset, &
           valid_min     = output_data%cer_ap_vmin, &
           valid_max     = output_data%cer_ap_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cer2_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cer2_fg', &
           output_data%vid_cer2_fg, &
           verbose, &
           long_name     = 'cloud effective radius of layer 2 first guess', &
           standard_name = 'effective_radius_of_cloud_condensed_water_' // &
                           'particle_at_cloud_top of_layer_2 first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cer_fg_scale, &
           add_offset    = output_data%cer_fg_offset, &
           valid_min     = output_data%cer_fg_vmin, &
           valid_max     = output_data%cer_fg_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp2_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp2_ap', &
           output_data%vid_ctp2_ap, &
           verbose, &
           long_name     = 'cloud top pressure of layer 2 a priori', &
           standard_name = 'air_pressure_at_cloud_top of_layer_2 a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_ap_scale, &
           add_offset    = output_data%ctp_ap_offset, &
           valid_min     = output_data%ctp_ap_vmin, &
           valid_max     = output_data%ctp_ap_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp2_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp2_fg', &
           output_data%vid_ctp2_fg, &
           verbose, &
           long_name     = 'cloud top pressure of layer 2 first guess', &
           standard_name = 'air_pressure_at_cloud_top of_layer_2 first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_fg_scale, &
           add_offset    = output_data%ctp_fg_offset, &
           valid_min     = output_data%ctp_fg_vmin, &
           valid_max     = output_data%ctp_fg_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if

if (indexing%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! stemp_ap
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp_ap', &
           output_data%vid_stemp_ap, &
           verbose, &
           long_name     = 'surface temperature a priori', &
           standard_name = 'surface_temperature a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_ap_scale, &
           add_offset    = output_data%stemp_ap_offset, &
           valid_min     = output_data%stemp_ap_vmin, &
           valid_max     = output_data%stemp_ap_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! stemp_fg
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp_fg', &
           output_data%vid_stemp_fg, &
           verbose, &
           long_name     = 'surface temperature first guess', &
           standard_name = 'surface_temperature first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_fg_scale, &
           add_offset    = output_data%stemp_fg_offset, &
           valid_min     = output_data%stemp_fg_vmin, &
           valid_max     = output_data%stemp_fg_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%NSolar

      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))
      input_dummy2='albedo in channel no '//trim(adjustl(input_num))

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_albedo(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%albedo_scale, &
           add_offset    = output_data%albedo_offset, &
           valid_min     = output_data%albedo_vmin, &
           valid_max     = output_data%albedo_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do
end if

   !----------------------------------------------------------------------------
   ! reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (btest(indexing%Ch_Is(i), ThermalBit)) then
         input_dummy='brightness_temperature_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2='brightness temperature in channel no '// &
              trim(adjustl(input_num))
         input_dummy3='kelvin'
      else
         input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='reflectance in channel no '//trim(adjustl(input_num))
         input_dummy3='1'

      end if

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_channels(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%channels_scale(i), &
           add_offset    = output_data%channels_offset(i), &
           valid_min     = output_data%channels_vmin(i), &
           valid_max     = output_data%channels_vmax(i), &
           units         = trim(adjustl(input_dummy3)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! firstguess reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (btest(indexing%Ch_Is(i), ThermalBit)) then
         input_dummy='firstguess_brightness_temperature_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2='firstguess brightness temperature in channel no '// &
              trim(adjustl(input_num))
         input_dummy3='kelvin'
      else
         input_dummy='firstguess_reflectance_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2='firstguess reflectance in channel no '// &
              trim(adjustl(input_num))
         input_dummy3='1'
      end if

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_y0(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%y0_scale(i), &
           add_offset    = output_data%y0_offset(i), &
           valid_min     = output_data%y0_vmin(i), &
           valid_max     = output_data%y0_vmax(i), &
           units         = trim(adjustl(input_dummy3)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! reflectances and brightness temperature _residual_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (btest(indexing%Ch_Is(i), ThermalBit)) then
         input_dummy='brightness_temperature_residual_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2='brightness temperature residual in channel no '// &
              trim(adjustl(input_num))
         input_dummy3='kelvin'
      else
         input_dummy='reflectance_residual_in_channel_no_'// &
              trim(adjustl(input_num))
         input_dummy2='reflectance residual in channel no '// &
              trim(adjustl(input_num))
         input_dummy3='1'
      end if

      call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_residuals(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%residuals_scale(i), &
           add_offset    = output_data%residuals_offset(i), &
           valid_min     = output_data%residuals_vmin(i), &
           valid_max     = output_data%residuals_vmax(i), &
           units         = trim(adjustl(input_dummy3)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! degrees_of_freedom_signal
   !----------------------------------------------------------------------------
   call ncdf_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'degrees_of_freedom_signal', &
           output_data%vid_ds, &
           verbose, &
           long_name     = 'degrees of freedom for signal', &
           standard_name = 'degrees_of_freedom_for_signal', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ds_scale, &
           add_offset    = output_data%ds_offset, &
           valid_min     = output_data%ds_vmin, &
           valid_max     = output_data%ds_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! covariance_matrix_element_*
   !----------------------------------------------------------------------------
   if (indexing%flags%do_covariance) then

   end if


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine def_output_secondary
