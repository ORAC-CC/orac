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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_primary(Ctrl, ncid, dims_var, output_data, status)

   use CTRL_def
   use orac_ncdf
   use SPixel_def

   use netcdf

   implicit none

   type(CTRL_t),              intent(in)    :: Ctrl
   integer,                   intent(in)    :: ncid
   integer,                   intent(in)    :: dims_var(2)
   type(output_data_primary), intent(inout) :: output_data
   integer,                   intent(inout) :: status

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   character(len=512) :: input_dummy3
   integer            :: ierr
   integer            :: iviews
   logical            :: verbose = .false.


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   ierr = nf90_redef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_redef()'
      stop
   end if


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   if (trim(Ctrl%inst%name) .eq. 'AATSR') then
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 2000'
   else
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 4713 BC'
   end if

   call nc_def_var_double_packed_double( &
           ncid, &
           dims_var, &
           'time', &
           output_data%vid_time, &
           verbose,ierr, &
           long_name     = 'time', &
           standard_name = 'time', &
           fill_value    = dreal_fill_value, &
           scale_factor  = output_data%time_scale, &
           add_offset    = output_data%time_offset, &
           valid_min     = output_data%time_vmin, &
           valid_max     = output_data%time_vmax, &
           units         = input_dummy)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lat
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'lat', &
           output_data%vid_lat, &
           verbose,ierr, &
           long_name     = 'latitude', &
           standard_name = 'latitude', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%lat_scale, &
           add_offset    = output_data%lat_offset, &
           valid_min     = output_data%lat_vmin, &
           valid_max     = output_data%lat_vmax, &
           units         = 'degrees_north')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lon
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'lon', &
           output_data%vid_lon, &
           verbose,ierr, &
           long_name     = 'longitude', &
           standard_name = 'longitude', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%lon_scale, &
           add_offset    = output_data%lon_offset, &
           valid_min     = output_data%lon_vmin, &
           valid_max     = output_data%lon_vmax, &
           units         = 'degrees_east')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

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
              verbose,ierr, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%sol_scale, &
              add_offset    = output_data%sol_offset, &
              valid_min     = output_data%sol_vmin, &
              valid_max     = output_data%sol_vmax, &
              units         = 'degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

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
              verbose,ierr, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%sat_scale, &
              add_offset    = output_data%sat_offset, &
              valid_min     = output_data%sat_vmin, &
              valid_max     = output_data%sat_vmax, &
              units         = 'degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

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
              verbose,ierr, &
              long_name     = trim(adjustl(input_dummy2)), &
              standard_name = trim(adjustl(input_dummy3)), &
              fill_value    = sreal_fill_value, &
              scale_factor  = output_data%azi_scale, &
              add_offset    = output_data%azi_offset, &
              valid_min     = output_data%azi_vmin, &
              valid_max     = output_data%azi_vmax, &
              units         = 'degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   end do

   !----------------------------------------------------------------------------
   ! cot
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot', &
           output_data%vid_cot, &
           verbose, ierr, &
           long_name     = 'cloud optical thickness', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_scale, &
           add_offset    = output_data%cot_offset, &
           valid_min     = output_data%cot_vmin, &
           valid_max     = output_data%cot_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cot_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_uncertainty', &
           output_data%vid_coterror, &
           verbose,ierr, &
           long_name     = 'cloud optical thickness uncertainty', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_error_scale, &
           add_offset    = output_data%cot_error_offset, &
           valid_min     = output_data%cot_error_vmin, &
           valid_max     = output_data%cot_error_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref', &
           output_data%vid_ref, &
           verbose,ierr, &
           long_name     = 'effective radius', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_scale, &
           add_offset    = output_data%ref_offset, &
           valid_min     = output_data%ref_vmin, &
           valid_max     = output_data%ref_vmax, &
           units         = 'micrometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref_uncertainty', &
           output_data%vid_referror, &
           verbose,ierr, &
           long_name     = 'effective radius uncertainty', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_error_scale, &
           add_offset    = output_data%ref_error_offset, &
           valid_min     = output_data%ref_error_vmin, &
           valid_max     = output_data%ref_error_vmax, &
           units         = 'micrometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp', &
           output_data%vid_ctp, &
           verbose,ierr, &
           long_name     = 'cloud top pressure', &
           standard_name = 'air_pressure_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_scale, &
           add_offset    = output_data%ctp_offset, &
           valid_min     = output_data%ctp_vmin, &
           valid_max     = output_data%ctp_vmax, &
           units         = 'hPa')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctp_uncertainty', &
           output_data%vid_ctperror, &
           verbose,ierr, &
           long_name     = 'cloud top pressure uncertainty', &
           standard_name = 'air_pressure_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctp_error_scale, &
           add_offset    = output_data%ctp_error_offset, &
           valid_min     = output_data%ctp_error_vmin, &
           valid_max     = output_data%ctp_error_vmax, &
           units         = 'hPa')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total', &
           output_data%vid_cct, &
           verbose,ierr, &
           long_name     = 'cloud fraction', &
           standard_name = 'cloud_area_fraction', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cct_scale, &
           add_offset    = output_data%cct_offset, &
           valid_min     = output_data%cct_vmin, &
           valid_max     = output_data%cct_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cc_total_uncertainty', &
           output_data%vid_ccterror, &
           verbose,ierr, &
           long_name     = 'cloud fraction uncertainty', &
           standard_name = 'cloud_area_fraction uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cct_error_scale, &
           add_offset    = output_data%cct_error_offset, &
           valid_min     = output_data%cct_error_vmin, &
           valid_max     = output_data%cct_error_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp', &
           output_data%vid_stemp, &
           verbose,ierr, &
           long_name     = 'surface temperature', &
           standard_name = 'surface_temperature', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_scale, &
           add_offset    = output_data%stemp_offset, &
           valid_min     = output_data%stemp_vmin, &
           valid_max     = output_data%stemp_vmax, &
           units         = 'kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'stemp_uncertainty', &
           output_data%vid_stemperror, &
           verbose,ierr, &
           long_name     = 'surface temperature uncertainty', &
           standard_name = 'surface_temperature uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%stemp_error_scale, &
           add_offset    = output_data%stemp_error_offset, &
           valid_min     = output_data%stemp_vmin, &
           valid_max     = output_data%stemp_error_vmax, &
           units         = 'kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth', &
           output_data%vid_cth, &
           verbose,ierr, &
           long_name     = 'cloud top height', &
           standard_name = 'cloud_top_altitide', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_scale, &
           add_offset    = output_data%cth_offset, &
           valid_min     = output_data%cth_vmin, &
           valid_max     = output_data%cth_vmax, &
           units         = 'kilometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cth_uncertainty', &
           output_data%vid_ctherror, &
           verbose,ierr, &
           long_name     = 'cloud top height uncertainty', &
           standard_name = 'cloud_top_altitide uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cth_error_scale, &
           add_offset    = output_data%cth_error_offset, &
           valid_min     = output_data%cth_error_vmin, &
           valid_max     = output_data%cth_error_vmax, &
           units         = 'kilometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt', &
           output_data%vid_ctt, &
           verbose,ierr, &
           long_name     = 'cloud top temperature', &
           standard_name = 'air_temperature_at_cloud_top', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_scale, &
           add_offset    = output_data%ctt_offset, &
           valid_min     = output_data%ctt_vmin, &
           valid_max     = output_data%ctt_vmax, &
           units         = 'kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ctt_uncertainty', &
           output_data%vid_ctterror, &
           verbose,ierr, &
           long_name     = 'cloud top temperature uncertainty', &
           standard_name = 'air_temperature_at_cloud_top uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ctt_error_scale, &
           add_offset    = output_data%ctt_error_offset, &
           valid_min     = output_data%ctt_error_vmin, &
           valid_max     = output_data%ctt_error_vmax, &
           units         = 'kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp', &
           output_data%vid_cwp, &
           verbose,ierr, &
           long_name     = 'cloud liquid water path', &
           standard_name = 'atmosphere_mass_content_of_cloud_liquid_water', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_scale, &
           add_offset    = output_data%cwp_offset, &
           valid_min     = output_data%cwp_vmin, &
           valid_max     = output_data%cwp_vmax, &
           units         = 'g/m2')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp_uncertainty
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cwp_uncertainty', &
           output_data%vid_cwperror, &
           verbose,ierr, &
           long_name     = 'cloud liquid water path uncertainty', &
           standard_name = 'atmosphere_mass_content_of_cloud_liquid_water uncertainty', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cwp_error_scale, &
           add_offset    = output_data%cwp_error_offset, &
           valid_min     = output_data%cwp_error_vmin, &
           valid_max     = output_data%cwp_error_vmax, &
           units         = 'g/m2')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! convergence
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'convergence', &
           output_data%vid_convergence, &
           verbose,ierr, &
           long_name     = 'retrieval convergence flag', &
           standard_name = 'retrieval_convergence_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%convergence_scale, &
           add_offset    = output_data%convergence_offset, &
           valid_min     = output_data%convergence_vmin, &
           valid_max     = output_data%convergence_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'yes, no')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   output_data%niter_vmax=Ctrl%Invpar%MaxIter

   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'niter', &
           output_data%vid_niter, &
           verbose,ierr, &
           long_name     = 'number of retrieval iterations', &
           standard_name = 'number_of_retrieval_iterations', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%niter_scale, &
           add_offset    = output_data%niter_offset, &
           valid_min     = output_data%niter_vmin, &
           valid_max     = output_data%niter_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'phase', &
           output_data%vid_phase, &
           verbose,ierr, &
           long_name     = 'cloud phase flag', &
           standard_name = 'cloud_phase_flag', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%phase_scale, &
           add_offset    = output_data%phase_offset, &
           valid_min     = output_data%phase_vmin, &
           valid_max     = output_data%phase_vmax, &
           flag_values   = '0b, 1b, 2b', &
           flag_meanings = 'clear/unknown, liquid, ice')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costja', &
           output_data%vid_costja, &
           verbose,ierr, &
           long_name     = 'a priori cost at solution', &
           standard_name = 'a_priori_cost_at_solution', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costja_scale, &
           add_offset    = output_data%costja_offset, &
           valid_min     = output_data%costja_vmin, &
           valid_max     = output_data%costja_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'costjm', &
           output_data%vid_costjm, &
           verbose,ierr, &
           long_name     = 'measurement cost at solution', &
           standard_name = 'measurement_cost_at_solution', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%costjm_scale, &
           add_offset    = output_data%costjm_offset, &
           valid_min     = output_data%costjm_vmin, &
           valid_max     = output_data%costjm_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'lsflag', &
           output_data%vid_lsflag, &
           verbose,ierr, &
           long_name     = 'land/sea flag', &
           standard_name = 'land_binary_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%lsflag_scale, &
           add_offset    = output_data%lsflag_offset, &
           valid_min     = output_data%lsflag_vmin, &
           valid_max     = output_data%lsflag_vmax, &
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b', &
           flag_meanings = 'sea, land, sunglint, snow, ice, snow_and_ice')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

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
           verbose,ierr, &
           long_name     = 'quality control flag', &
           standard_name = 'quality_control_flag', &
           fill_value    = int(-1,kind=sint), &
           scale_factor  = output_data%qcflag_scale, &
           add_offset    = output_data%qcflag_offset, &
           valid_min     = output_data%qcflag_vmin, &
           valid_max     = output_data%qcflag_vmax, &
           flag_meanings = trim(adjustl(input_dummy2)))

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'illum', &
           output_data%vid_illum, &
           verbose,ierr, &
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
   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

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
           verbose,ierr, &
           long_name     = 'Pavolonis cloud type', &
           standard_name = 'Pavolonis_cloud_type', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldtype_scale, &
           add_offset    = output_data%cldtype_offset, &
           valid_min     = output_data%cldtype_vmin, &
           valid_max     = output_data%cldtype_vmax, &
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b', &
           flag_meanings = trim(adjustl(input_dummy2)))

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cloud mask
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'cldmask', &
           output_data%vid_cldmask, &
           verbose,ierr, &
           long_name     = 'Neural net cloud mask (radiance based)', &
           standard_name = 'Neural_net_cloud_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%cldmask_scale, &
           add_offset    = output_data%cldmask_offset, &
           valid_min     = output_data%cldmask_vmin, &
           valid_max     = output_data%cldmask_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'clear, cloudy')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! CCCOT_pre (cloud optical thickness)
   !----------------------------------------------------------------------------
   call nc_def_var_float_packed_float( &
           ncid, &
           dims_var, &
           'cccot_pre', &
           output_data%vid_cccot_pre, &
           verbose,ierr, &
           long_name     = 'neural network cloud optical thickness', &
           standard_name = 'NN_CCCOT', &
           fill_value    = sreal_fill_value, &
           scale_factor  = output_data%cccot_pre_scale, &
           add_offset    = output_data%cccot_pre_offset, &
           valid_min     = output_data%cccot_pre_vmin, &
           valid_max     = output_data%cccot_pre_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'lusflag', &
           output_data%vid_lusflag, &
           verbose,ierr, &
           long_name     = 'land use flag', &
           standard_name = 'land_use_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%lusflag_scale, &
           add_offset    = output_data%lusflag_offset, &
           valid_min     = output_data%lusflag_vmin, &
           valid_max     = output_data%lusflag_vmax, &
           flag_values   = '0b, 1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, 11b, &
           &12b, 13b, 14b, 15b, 16b, 17b, 18b, 19b, 20b, 21b, 22b, 23b, 24b', &
           flag_meanings = 'see usgs.gov')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! DEM
   !----------------------------------------------------------------------------
   call nc_def_var_short_packed_short( &
           ncid, &
           dims_var, &
           'dem', &
           output_data%vid_dem, &
           verbose,ierr, &
           long_name     = 'Digital elevation model', &
           standard_name = 'dem', &
           fill_value    = int(-1,kind=sint), &
           scale_factor  = output_data%dem_scale, &
           add_offset    = output_data%dem_offset, &
           valid_min     = output_data%dem_vmin, &
           valid_max     = output_data%dem_vmax)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! nise mask
   !----------------------------------------------------------------------------
   call nc_def_var_byte_packed_byte( &
           ncid, &
           dims_var, &
           'nisemask', &
           output_data%vid_nisemask, &
           verbose,ierr, &
           long_name     = 'NISE snow/ice mask', &
           standard_name = 'NISE_mask', &
           fill_value    = byte_fill_value, &
           scale_factor  = output_data%nisemask_scale, &
           add_offset    = output_data%nisemask_offset, &
           valid_min     = output_data%nisemask_vmin, &
           valid_max     = output_data%nisemask_vmax, &
           flag_values   = '0b, 1b', &
           flag_meanings = 'snow/ice free, snow/ice')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop
   end if


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (status .ne. 0 ) then
      write(*,*) 'def_vars_primary(): netcdf variable definintion error:', status
      call Write_Log(Ctrl,'def_vars_primary(): netcdf variable definintion error:', status)
      stop
   end if

end subroutine def_vars_primary
