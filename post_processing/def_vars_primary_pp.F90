!-------------------------------------------------------------------------------
! Name: def_vars_primary_pp.F90
!
! Purpose:
! Define primary output variables for netcdf. Variable type, scale, offset,
! fill value and /or range are defined and applied to the variable definition.
! Variable names are also defined.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2011/12/19, Matthias Jerg creates initial output for main output variables.
! 2012/01/06, Caroline Poulsen added cwp
! 2012/01/15, Caroline Poulsen changed scale and range for cot
! 2012/03/18, Caroline Poulsen modified to add cloud flag
! 2012/07/06, Matthias Jerg extensively overhauls and restructures the code
! 2012/10/17, Somebody: changing cwp_scale and cwp_error_scale to 0.1
! 2012/11/27, Somebody: Numerous changes to make output CF compliant
! 2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt and
!    cth
! 2014/04/01, Matthias Jerg: fixes some problems/cleanup with illumination
! 2014/07/08, CP: added more illumination options
! 2014/10/24, OS: added variables lusflag, cldtype, cloudmask, DEM (currently
!    deactivated), and nisemask
! 2014/11/20, OS: corrected Julian Date format; added Pavolonis cloud phase
!    variable
! 2014/12/01, OS: added flag 9 = prob_opaque_ice
! 2014/12/03, CP: added cloud albedo
! 2014/12/17, CP: removed unknown form mask naming
! 2015/02/05, CP: changed time attribute and common constants
! 2015/02/05, CP: add usgs meanings
! 2015/04/20, CP: fixed up time attributes
! 2015/04/23, OS: CCT vmin set to -100
! 2015/04/23, OS: CCT vmax set to 10000
! 2015/07/04, CP: added corrected cth
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_primary_pp(ncid, indexing, dims_var, output_data, &
                               global_atts, verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use structures_pp
   use scanline_structure
   use vartypes_pp

   implicit none

   integer,                              intent(in)    :: ncid
   type(counts_and_indexes),             intent(in)    :: indexing
   integer,                              intent(in)    :: dims_var(2)
   type(spixel_scanline_primary_output), intent(inout) :: output_data
   type(global_attributes_s),            intent(in)    :: global_atts
   logical,                              intent(in)    :: verbose

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   character(len=512) :: input_dummy3
   integer            :: i_chan
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
!  if (global_atts%sensor .eq. 'AATSR') then
!     input_dummy='Julian Date, days elapsed since 12:00 UTC on Jan 1 2000'
!  else
      input_dummy='Julian Date, days elapsed since 12:00 UTC on Jan 1 4713 BC'
!  end if

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
           units         = trim(input_dummy))

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
           units         = 'degrees_north')

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
           units         = 'degrees_east')

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
              units         = 'degrees')

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
              units         = 'degrees')

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
              units         = 'degrees')

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
           valid_max     = output_data%cot_vmax)

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
           valid_max     = output_data%cot_error_vmax)

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
           units         = 'micrometer')

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
           units         = 'micrometer')

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
           units         = 'hPa')

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
           units         = 'hPa')

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
           valid_max     = output_data%cct_vmax)

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
           valid_max     = output_data%cct_error_vmax)

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
           units         = 'kelvin')

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
           units         = 'kelvin')

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
           units         = 'kilometer')

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
           units         = 'kilometer')

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
           units         = 'kilometer')

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
           units         = 'kilometer')

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
           units         = 'kelvin')

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
           units         = 'kelvin')

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
           units         = 'g/m2')

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
           units         = 'g/m2')

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
           flag_meanings = 'yes, no')

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
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
           valid_max     = output_data%niter_vmax)

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
           flag_meanings = 'clear/unknown, liquid, ice')

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
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
           flag_meanings = 'clear/unknown, liquid, ice')

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
           valid_max     = output_data%costja_vmax)

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
           valid_max     = output_data%costjm_vmax)

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
           flag_meanings = 'sea, land, sunglint, snow, ice, snow_and_ice')

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   input_dummy2='Bit 0 unused, always set to 0, Bits 1-5 set to 1 if state variable error out of bounds'
   input_dummy2=trim(adjustl(input_dummy2))//', Bit 6 set to 1 if no convergence achieved'
   input_dummy2=trim(adjustl(input_dummy2))//', Bit 7 set to 1 if cost too large.'
   input_dummy2=trim(adjustl(input_dummy2))//' Bit 1=COT Bit 2=REF Bit 3=CTP Bit 4=CCT Bit 5=STEMP'

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
           flag_meanings = trim(adjustl(input_dummy2)))

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'illum', &
           output_data%vid_illum, &
           verbose, &
           flag_values   ='1b, 2b, 3b', &
           flag_meanings ='Day, Twilight, Night', &
           long_name     = 'illumination flag', &
           standard_name = 'illumination_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%illum_scale, &
           add_offset    = output_data%illum_offset, &
           valid_min     = output_data%illum_vmin, &
           valid_max     = output_data%illum_vmax &
           )
   !----------------------------------------------------------------------------
   ! cloud type (ie. Pavolonis phase)
   !----------------------------------------------------------------------------
   input_dummy2='Bit 0=clear Bit 1=N/A Bit 2=fog Bit 3=water Bit 4=supercooled'
   input_dummy2=trim(adjustl(input_dummy2))//' Bit 5=mixed Bit 6=opaque_ice Bit 7=cirrus'
   input_dummy2=trim(adjustl(input_dummy2))//' Bit 8=overlap Bit 9=prob_opaque_ice'

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
           flag_meanings = trim(adjustl(input_dummy2)))

   !----------------------------------------------------------------------------
   ! cloud mask
   !----------------------------------------------------------------------------
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
           flag_meanings = 'clear, cloudy')

   !----------------------------------------------------------------------------
   ! CCCOT_pre (cloud optical thickness)
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
           valid_max     = output_data%cccot_pre_vmax)

   !----------------------------------------------------------------------------
   ! CCCOT (cloud optical thickness)
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cccot', &
           output_data%vid_cccot, &
           verbose, &
           long_name     = 'neural network cloud optical thickness', &
           standard_name = 'NN_CCCOT', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cccot_scale, &
           add_offset    = output_data%cccot_offset, &
           valid_min     = output_data%cccot_vmin, &
           valid_max     = output_data%cccot_vmax)

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
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, 11b, &
              & 12b, 13b, 14b, 15b, 16b, 17b, 18b, 19b, 20b, 21b, 22b, 23b, 24b', &
           flag_meanings ='1:Urban and Built-Up Land, &
                          &2:Dryland Cropland and Pasture, &
                          &3:Irrigated, Cropland and Pasture, &
                          &4:Mixed Dryland/Irrigated Cropland and Pasture, &
                          &5:Cropland/Grassland Mosaic, &
                          &6:Cropland/Woodland Mosaic, 7:Grassland, &
                          &8:Shrubland, &
                          &9:Mixed Shrubland/Grassland, &
                          &10:Savanna, &
                          &11:Deciduous Broadleaf Forest, &
                          &12:Deciduous Needleleaf Forest, &
                          &13:Evergreen Broadleaf Forest, &
                          &14:Evergreen Needleleaf Forest, &
                          &15:Mixed Forest, &
                          &16:Water Bodies, &
                          &17:Herbaceous Wetland, &
                          &18:Wooded Wetland, &
                          &19:Barren or Sparsely Vegetated, &
                          &20:Herbaceous Tundra, &
                          &21:Wooded Tundra, &
                          &22:Mixed Tundra, &
                          &23:Bare Ground Tundra, &
                          &24:Snow or Ice, &
                          &99:Interrupted Areas, &
	                  &100:Missing Data')

   !----------------------------------------------------------------------------
   ! DEM
   !----------------------------------------------------------------------------
if (.false.) then
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
           valid_max     = output_data%dem_vmax)
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
           flag_meanings = 'snow/ice free, snow/ice')

   !----------------------------------------------------------------------------
   ! cloud_albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i_chan=1,indexing%NSolar
      write(input_num,"(i4)") indexing%Y_Id(i_chan)

      output_data%cloud_albedo_scale=0.0001
      output_data%cloud_albedo_offset=0.0
      output_data%cloud_albedo_vmin=0
      output_data%cloud_albedo_vmax=11000

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      input_dummy2='cloud_albedo in channel no '//trim(adjustl(input_num))

      call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_cloud_albedo(i_chan), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cloud_albedo_scale, &
           add_offset    = output_data%cloud_albedo_offset, &
           valid_min     = output_data%cloud_albedo_vmin, &
           valid_max     = output_data%cloud_albedo_vmax)
   end do


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine def_vars_primary_pp
