!-------------------------------------------------------------------------------
! Name: def_output_primary.F90
!
! Purpose:
! Define primary output variables for netcdf. Variable type, scale, offset,
! fill value and/or range are defined and applied to the variable definition.
! Variable names are also defined.
!
! Description and Algorithm details:
! 1) For each variable, use nc_def_var routine to define variable.
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_output_primary(ncid, dims_var, output_data, inst_name, NViews, Ny, NSolar, YSolar, Y_Id, Ch_Is, MaxIter, qc_flag_meanings, deflate_level, shuffle_flag, verbose, do_phase_pavolonis, do_cldmask, do_cldmask_uncertainty, do_cloudmask_pre, do_dem)

   use netcdf
   use orac_ncdf

   implicit none

   integer,                   intent(in)    :: ncid
   integer,                   intent(in)    :: dims_var(:)
   type(output_data_primary), intent(inout) :: output_data
   character(len=*),          intent(in)    :: inst_name
   integer,                   intent(in)    :: NViews
   integer,                   intent(in)    :: Ny
   integer,                   intent(in)    :: NSolar
   integer,                   intent(in)    :: YSolar(:)
   integer,                   intent(in)    :: Y_Id(:)
   integer,                   intent(in)    :: Ch_Is(:)
   integer,                   intent(in)    :: MaxIter
   character(len=*),          intent(in)    :: qc_flag_meanings
   integer,                   intent(in)    :: deflate_level
   logical,                   intent(in)    :: shuffle_flag
   logical,                   intent(in)    :: verbose
   logical,                   intent(in)    :: do_phase_pavolonis
   logical,                   intent(in)    :: do_cldmask
   logical,                   intent(in)    :: do_cldmask_uncertainty
   logical,                   intent(in)    :: do_cloudmask_pre
   logical,                   intent(in)    :: do_dem

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   character(len=512) :: input_dummy3
   integer            :: i
   integer            :: i_view


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_redef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_redef()'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   if (inst_name(1:5) .eq. 'AATSR') then
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 2000'
   else
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 4713 BC'
   end if

   call nc_def_var_double_packed_double( &
           ncid, &
           dims_var, &
           'time', &
           output_data%vid_time, &
           verbose, &
           long_name     = 'time', &
           standard_name = 'time', &
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
   call nc_def_var_float_packed_float( &
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
   call nc_def_var_float_packed_float( &
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
   do i_view=1,NViews

      write(input_num,"(i4)") i_view

      !-------------------------------------------------------------------------
      ! solar_zenith_view_no*
      !-------------------------------------------------------------------------
      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      input_dummy2='solar zenith angle for view no '//trim(adjustl(input_num))
      input_dummy3='solar_zenith_angle_for_view_no_'//trim(adjustl(input_num))

      call nc_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sol_zen(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
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
      input_dummy3='sensor_zenith_angle_for_view_no_'//trim(adjustl(input_num))

      call nc_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sat_zen(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
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
      input_dummy2='relative azimuth angle for view no '//trim(adjustl(input_num))
      input_dummy3='relative_azimuth_angle_for_view_no_'//trim(adjustl(input_num))

      call nc_def_var_float_packed_float( &
              ncid, &
              dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_rel_azi(i_view), &
              verbose, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%azi_scale, &
              add_offset    = output_data%azi_offset, &
              valid_min     = output_data%azi_vmin, &
              valid_max     = output_data%azi_vmax, &
              units         = 'degrees', &
              deflate_level = deflate_level, &
              shuffle       = shuffle_flag)

   end do

   !----------------------------------------------------------------------------
   ! cot
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cot_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_uncertainty', &
           output_data%vid_cot_error, &
           verbose, &
           long_name     = 'cloud optical thickness uncertainty', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_error_scale, &
           add_offset    = output_data%cot_error_offset, &
           valid_min     = output_data%cot_error_vmin, &
           valid_max     = output_data%cot_error_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ref
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref', &
           output_data%vid_ref, &
           verbose, &
           long_name     = 'effective radius', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_scale, &
           add_offset    = output_data%ref_offset, &
           valid_min     = output_data%ref_vmin, &
           valid_max     = output_data%ref_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ref_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref_uncertainty', &
           output_data%vid_ref_error, &
           verbose, &
           long_name     = 'effective radius uncertainty', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_error_scale, &
           add_offset    = output_data%ref_error_offset, &
           valid_min     = output_data%ref_error_vmin, &
           valid_max     = output_data%ref_error_vmax, &
           units         = 'micrometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_uncertainty', &
           output_data%vid_ctp_error, &
           verbose, &
           long_name     = 'cloud top pressure uncertainty', &
           standard_name = 'air_pressure_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_error_scale, &
           add_offset    = output_data%ctp_error_offset, &
           valid_min     = output_data%ctp_error_vmin, &
           valid_max     = output_data%ctp_error_vmax, &
           units         = 'hPa', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total', &
           output_data%vid_cct, &
           verbose, &
           long_name     = 'cloud fraction', &
           standard_name = 'cloud_area_fraction', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cct_scale, &
           add_offset    = output_data%cct_offset, &
           valid_min     = output_data%cct_vmin, &
           valid_max     = output_data%cct_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cc_total_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total_uncertainty', &
           output_data%vid_cct_error, &
           verbose, &
           long_name     = 'cloud fraction uncertainty', &
           standard_name = 'cloud_area_fraction uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cct_error_scale, &
           add_offset    = output_data%cct_error_offset, &
           valid_min     = output_data%cct_error_vmin, &
           valid_max     = output_data%cct_error_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! stemp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp_uncertainty', &
           output_data%vid_stemp_error, &
           verbose, &
           long_name     = 'surface temperature uncertainty', &
           standard_name = 'surface_temperature uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_error_scale, &
           add_offset    = output_data%stemp_error_offset, &
           valid_min     = output_data%stemp_vmin, &
           valid_max     = output_data%stemp_error_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_uncertainty', &
           output_data%vid_cth_error, &
           verbose, &
           long_name     = 'cloud top height uncertainty', &
           standard_name = 'cloud_top_altitude uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_error_scale, &
           add_offset    = output_data%cth_error_offset, &
           valid_min     = output_data%cth_error_vmin, &
           valid_max     = output_data%cth_error_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cth_corrected
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_corrected', &
           output_data%vid_cth_corrected, &
           verbose, &
           long_name     = 'corrected cloud top height', &
           standard_name = 'corrected cloud_top_altitude', &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_corrected_uncertainty', &
           output_data%vid_cth_corrected_error, &
           verbose, &
           long_name     = 'corrected cloud top height uncertainty', &
           standard_name = 'corrected cloud_top_altitude uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_error_scale, &
           add_offset    = output_data%cth_error_offset, &
           valid_min     = output_data%cth_error_vmin, &
           valid_max     = output_data%cth_error_vmax, &
           units         = 'kilometer', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! ctt
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt_uncertainty', &
           output_data%vid_ctt_error, &
           verbose, &
           long_name     = 'cloud top temperature uncertainty', &
           standard_name = 'air_temperature_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_error_scale, &
           add_offset    = output_data%ctt_error_offset, &
           valid_min     = output_data%ctt_error_vmin, &
           valid_max     = output_data%ctt_error_vmax, &
           units         = 'kelvin', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cwp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
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
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp_uncertainty', &
           output_data%vid_cwp_error, &
           verbose, &
           long_name     = 'cloud liquid water path uncertainty', &
           standard_name = 'atmosphere_mass_content_of_cloud_liquid_water uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_error_scale, &
           add_offset    = output_data%cwp_error_offset, &
           valid_min     = output_data%cwp_error_vmin, &
           valid_max     = output_data%cwp_error_vmax, &
           units         = 'g/m2', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cloud_albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,NSolar

      write(input_num,"(i4)") Y_Id(YSolar(i))

      input_dummy='cloud_albedo in channel no '//trim(adjustl(input_num))
      input_dummy2='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))

      call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy2)), &
           output_data%vid_cloud_albedo(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy)), &
           standard_name = trim(adjustl(input_dummy2)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cloud_albedo_scale, &
           add_offset    = output_data%cloud_albedo_offset, &
           valid_min     = output_data%cloud_albedo_vmin, &
           valid_max     = output_data%cloud_albedo_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
   end do

   !----------------------------------------------------------------------------
   ! convergence
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'convergence', &
           output_data%vid_convergence, &
           verbose, &
           long_name     = 'retrieval convergence flag', &
           standard_name = 'retrieval_convergence_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%convergence_scale, &
           add_offset    = output_data%convergence_offset, &
           valid_min     = output_data%convergence_vmin, &
           valid_max     = output_data%convergence_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'yes, no', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   output_data%niter_vmax=MaxIter

   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'niter', &
           output_data%vid_niter, &
           verbose, &
           long_name     = 'number of retrieval iterations', &
           standard_name = 'number_of_retrieval_iterations', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%niter_scale, &
           add_offset    = output_data%niter_offset, &
           valid_min     = output_data%niter_vmin, &
           valid_max     = output_data%niter_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costja', &
           output_data%vid_costja, &
           verbose, &
           long_name     = 'a priori cost at solution', &
           standard_name = 'a_priori_cost_at_solution', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costja_scale, &
           add_offset    = output_data%costja_offset, &
           valid_min     = output_data%costja_vmin, &
           valid_max     = output_data%costja_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costjm', &
           output_data%vid_costjm, &
           verbose, &
           long_name     = 'measurement cost at solution', &
           standard_name = 'measurement_cost_at_solution', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costjm_scale, &
           add_offset    = output_data%costjm_offset, &
           valid_min     = output_data%costjm_vmin, &
           valid_max     = output_data%costjm_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_short( &
           ncid, &
           dims_var, &
           'qcflag', &
           output_data%vid_qcflag, &
           verbose, &
           long_name     = 'quality control flag', &
           standard_name = 'quality_control_flag', &
           fill_value    = int(-1,kind=sint), &
           scale_factor  = output_data%qcflag_scale, &
           add_offset    = output_data%qcflag_offset, &
           valid_min     = output_data%qcflag_vmin, &
           valid_max     = output_data%qcflag_vmax, &
           flag_masks    = '1s, 2s, ... 2^(n state variables plus one)', &
           flag_meanings = trim(adjustl(qc_flag_meanings)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
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
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b', &
           flag_meanings = 'sea, land, sunglint, snow, ice, snow_and_ice', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
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
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, ' // &
                           '11b, 12b, 13b, 14b, 15b, 16b, 17b, 18b, 19b, ' // &
                           '20b, 21b, 22b, 23b, 24b', &
           flag_meanings ='1:Urban and Built-Up Land, ' // &
                          '2:Dryland Cropland and Pasture, ' // &
                          '3:Irrigated, Cropland and Pasture, ' // &
                          '4:Mixed Dryland/Irrigated Cropland and Pasture, ' // &
                          '5:Cropland/Grassland Mosaic, ' // &
                          '6:Cropland/Woodland Mosaic, ' // &
                          '7:Grassland, ' // &
                          '8:Shrubland, ' // &
                          '9:Mixed Shrubland/Grassland, ' // &
                          '10:Savanna, ' // &
                          '11:Deciduous Broadleaf Forest, ' // &
                          '12:Deciduous Needleleaf Forest, ' // &
                          '13:Evergreen Broadleaf Forest, ' // &
                          '14:Evergreen Needleleaf Forest, ' // &
                          '15:Mixed Forest, ' // &
                          '16:Water Bodies, ' // &
                          '17:Herbaceous Wetland, ' // &
                          '18:Wooded Wetland, ' // &
                          '19:Barren or Sparsely Vegetated, ' // &
                          '20:Herbaceous Tundra, ' // &
                          '21:Wooded Tundra, ' // &
                          '22:Mixed Tundra, ' // &
                          '23:Bare Ground Tundra, ' // &
                          '24:Snow or Ice, ' // &
                          '99:Interrupted Areas, ' // &
                          '100:Missing Data', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
if (do_dem) then
   call nc_def_var_short_packed_short( &
           ncid, &
           dims_var, &
           'dem', &
           output_data%vid_dem, &
           verbose, &
           long_name     = 'Digital elevation model', &
           standard_name = 'dem', &
           fill_value    = int(-1,kind=sint), &
           scale_factor  = output_data%dem_scale, &
           add_offset    = output_data%dem_offset, &
           valid_min     = output_data%dem_vmin, &
           valid_max     = output_data%dem_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if
   !----------------------------------------------------------------------------
   ! nise mask
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'nisemask', &
           output_data%vid_nisemask, &
           verbose, &
           long_name     = 'NISE snow/ice mask', &
           standard_name = 'NISE_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%nisemask_scale, &
           add_offset    = output_data%nisemask_offset, &
           valid_min     = output_data%nisemask_vmin, &
           valid_max     = output_data%nisemask_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'snow/ice free, snow/ice', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'illum', &
           output_data%vid_illum, &
           verbose, &
           long_name     = 'illumination flag', &
           standard_name = 'illumination_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%illum_scale, &
           add_offset    = output_data%illum_offset, &
           valid_min     = output_data%illum_vmin, &
           valid_max     = output_data%illum_vmax, &
           flag_values   ='1b, 2b, 3b', &
           flag_meanings ='Day, Twilight, Night', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cldtype (ie. Pavolonis phase)
   !----------------------------------------------------------------------------
   input_dummy='clear, ' // &
               'N/A, ' // &
               'fog, ' // &
               'water, ' // &
               'supercooled, ' // &
               'mixed, ' // &
               'opaque_ice, ' // &
               'cirrus, ' // &
               'overlap, ' // &
               'prob_opaque_ice'

   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'cldtype', &
           output_data%vid_cldtype, &
           verbose, &
           long_name     = 'Pavolonis cloud type', &
           standard_name = 'Pavolonis_cloud_type', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldtype_scale, &
           add_offset    = output_data%cldtype_offset, &
           valid_min     = output_data%cldtype_vmin, &
           valid_max     = output_data%cldtype_vmax, &
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b', &
           flag_meanings = trim(adjustl(input_dummy)), &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
if (do_cldmask) then
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'cldmask', &
           output_data%vid_cldmask, &
           verbose, &
           long_name     = 'Neural net cloud mask (radiance based)', &
           standard_name = 'Neural_net_cloud_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldmask_scale, &
           add_offset    = output_data%cldmask_offset, &
           valid_min     = output_data%cldmask_vmin, &
           valid_max     = output_data%cldmask_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'clear, cloudy', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if
   !----------------------------------------------------------------------------
   ! cldmask_uncertainty
   !----------------------------------------------------------------------------
if (do_cldmask_uncertainty) then
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cldmask_uncertainty', &
           output_data%vid_cldmask_uncertainty, &
           verbose, &
           long_name     = 'Neural net cloud mask (radiance based) uncertainty', &
           standard_name = 'Neural_net_cloud_mask_uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cldmask_uncertainty_scale, &
           add_offset    = output_data%cldmask_uncertainty_offset, &
           valid_min     = output_data%cldmask_uncertainty_vmin, &
           valid_max     = output_data%cldmask_uncertainty_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if
   !----------------------------------------------------------------------------
   ! cloudmask_pre
   !----------------------------------------------------------------------------
if (do_cloudmask_pre) then
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'cloudmask_pre', &
           output_data%vid_cldmask, &
           verbose, &
           long_name     = 'Neural net cloud mask (radiance based)', &
           standard_name = 'Neural_net_cloud_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldmask_scale, &
           add_offset    = output_data%cldmask_offset, &
           valid_min     = output_data%cldmask_vmin, &
           valid_max     = output_data%cldmask_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'clear, cloudy', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)
end if
   !----------------------------------------------------------------------------
   ! cccot_pre (cloud optical thickness)
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cccot_pre', &
           output_data%vid_cccot_pre, &
           verbose, &
           long_name     = 'neural network cloud optical thickness', &
           standard_name = 'NN_CCCOT', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cccot_pre_scale, &
           add_offset    = output_data%cccot_pre_offset, &
           valid_min     = output_data%cccot_pre_vmin, &
           valid_max     = output_data%cccot_pre_vmax, &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'phase', &
           output_data%vid_phase, &
           verbose, &
           long_name     = 'cloud phase flag', &
           standard_name = 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_scale, &
           add_offset    = output_data%phase_offset, &
           valid_min     = output_data%phase_vmin, &
           valid_max     = output_data%phase_vmax, &
           flag_values   = '0b, 1b, 2b', &
           flag_meanings = 'clear/unknown, liquid, ice', &
           deflate_level = deflate_level, &
           shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! phase_pavolonis
   !----------------------------------------------------------------------------
if (do_phase_pavolonis) then
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'phase_pavolonis', &
           output_data%vid_phase_pavolonis, &
           verbose, &
           long_name     = 'cloud phase flag Pavolonis', &
           standard_name = 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_pavolonis_scale, &
           add_offset    = output_data%phase_pavolonis_offset, &
           valid_min     = output_data%phase_pavolonis_vmin, &
           valid_max     = output_data%phase_pavolonis_vmax, &
           flag_values   = '0b, 1b, 2b', &
           flag_meanings = 'clear/unknown, liquid, ice', &
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
