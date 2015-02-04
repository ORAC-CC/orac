!-------------------------------------------------------------------------------
! Name: def_vars_primary.F90
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
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial version
! 2012/01/06, Caroline Poulsen: Added cwp
! 2012/01/15, Caroline Poulsen: Changed scale and range for cot
! 2012/06/15, Caroline Poulsen: Changed cost scaling factor
! 2012/09/20, Caroline Poulsen: Changed scaling factors
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
! 2010/11/12, CP: added cloud albedo
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2014/12/31, GM: Remove useless error control especially since nc_def_var_*
!    routines handle errors to exit.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_primary(Ctrl, ncid, dims_var, output_data)

   use CTRL_def
   use orac_ncdf
   use SPixel_def

   use netcdf

   implicit none

   type(CTRL_t),              intent(in)    :: Ctrl
   integer,                   intent(in)    :: ncid
   integer,                   intent(in)    :: dims_var(2)
   type(output_data_primary), intent(inout) :: output_data

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   character(len=512) :: input_dummy3
   integer            :: i
   integer            :: iviews
   logical            :: verbose = .false.


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
   if (Ctrl%Inst%Name(1:5) .eq. 'AATSR') then
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
           units         = input_dummy)

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
   do iviews=1,Ctrl%Ind%NViews

      write(input_num,"(i4)") iviews

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
              output_data%vid_sol_zen(iviews), &
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
              output_data%vid_sat_zen(iviews), &
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
              output_data%vid_rel_azi(iviews), &
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
           output_data%vid_coterror, &
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
           output_data%vid_referror, &
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
           output_data%vid_ctperror, &
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
           output_data%vid_ccterror, &
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
           output_data%vid_stemperror, &
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
           standard_name = 'cloud_top_altitide', &
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
           output_data%vid_ctherror, &
           verbose, &
           long_name     = 'cloud top height uncertainty', &
           standard_name = 'cloud_top_altitide uncertainty', &
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
           output_data%vid_ctterror, &
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
           output_data%vid_cwperror, &
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
   output_data%niter_vmax=Ctrl%Invpar%MaxIter

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
           standard_name = 'cloud_phase_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_scale, &
           add_offset    = output_data%phase_offset, &
           valid_min     = output_data%phase_vmin, &
           valid_max     = output_data%phase_vmax, &
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
           flag_values='1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, 11b, 12b', &
           flag_meanings='Day, Twilight, Night, Daynore, DayMissingSingleVisFirst, ' // &
               'DayMissingSingleVisSecond, DayMissingSingleIRFirst, DayMissingSingleIRSecond, ' // &
               'DayMissingSingleIRThird, NightMissingSingleIRFirst, NightMissingSingleIRSecond, ' // &
               'NightMissingSingleIRThird', &
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
           flag_meanings = 'clear, cloudy')

   !----------------------------------------------------------------------------
   ! CCCOT_pre (cloud optical thickness)
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'cccot_pre', &
           output_data%vid_cccot_pre, &
           verbose, &
           long_name     = 'neural network cloud optical thickness', &
           standard_name = 'NN_CCCOT', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%cccot_pre_scale, &
           add_offset    = output_data%cccot_pre_offset, &
           valid_min     = output_data%cccot_pre_vmin, &
           valid_max     = output_data%cccot_pre_vmax)

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
           flag_meanings = 'see usgs.gov')

   !----------------------------------------------------------------------------
   ! DEM
   !----------------------------------------------------------------------------
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
   ! cloud a albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,Ctrl%Ind%NSolar

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%YSolar(i))

      output_data%cloud_albedo_scale(i)=0.0001
      output_data%cloud_albedo_offset(i)=0.0
      output_data%cloud_albedo_vmin(i)=0
      output_data%cloud_albedo_vmax(i)=11000

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      input_dummy2='cloud_albedo in channel no '//trim(adjustl(input_num))

      call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_cloud_albedo(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cloud_albedo_scale(i), &
           add_offset    = output_data%cloud_albedo_offset(i), &
           valid_min     = output_data%cloud_albedo_vmin(i), &
           valid_max     = output_data%cloud_albedo_vmax(i))
   end do


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine def_vars_primary
