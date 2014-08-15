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
! 2013/10/06, CP: Bug fix  created relative azimuth angle in primary file
! 2014/04/01, MJ: Cleanup and fix for illum
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/07/13, CP: Added AATSR time string and changed definition of land/sea mask
! 2014/xx/xx: CP: Added extra illumination options!
! 2014/08/07, GM: Fixes to attributes related to the above change.
! 2014/08/07, GM: Hoisted calls to nf90_redef() and nf90_enddef() from the
!    individual variable definition subroutines to be called once in this
!    subroutine.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_primary(Ctrl, ncid, dims_var, spixel_scan_out, status)

   use CTRL_def
   use netcdf
   use SPixel_def

   implicit none

   type(CTRL_t),                         intent(in)    :: Ctrl
   integer,                              intent(in)    :: ncid
   integer,                              intent(in)    :: dims_var(2)
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out
   integer,                              intent(inout) :: status

   character(len=20)  :: input_num
   character(len=500) :: input_dummy
   character(len=100) :: time_string
   character(len=500) :: s_input_dummy
   integer            :: ierr
   integer            :: iviews
   integer            :: wo = 0


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
   spixel_scan_out%time_scale=1.0_dreal
   spixel_scan_out%time_offset=0.00_dreal
   spixel_scan_out%time_vmin=0.0_dreal
   spixel_scan_out%time_vmax=1.0D10
   spixel_scan_out%time_fv=-32767.0

   if (trim(Ctrl%inst%name) .eq. 'AATSR') then
      time_string='Julian Date, days elapsed since 12:00 January 1, 2000'
   else
      time_string='Julian Date, days elapsed since 12:00 January 1, 4713 BC'
   end if

   call nc_defdata_double(ncid, dims_var, &
           'time', &
           spixel_scan_out%vidtime, &
           'time', &
           'time', &
           time_string, &
           spixel_scan_out%time_fv, &
           spixel_scan_out%time_scale,spixel_scan_out%time_offset, &
           spixel_scan_out%time_vmin,spixel_scan_out%time_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lon
   !----------------------------------------------------------------------------
   spixel_scan_out%lon_scale=1.0
   spixel_scan_out%lon_offset=0.00
   spixel_scan_out%lon_vmin=-180.0
   spixel_scan_out%lon_vmax=180.0

   call nc_defdata_float(ncid, dims_var, &
           'lon', &
           spixel_scan_out%vidlon, &
           'longitude', &
           'longitude', &
           'degrees_east', &
           spixel_scan_out%real_fill_value, &
           spixel_scan_out%lon_scale,spixel_scan_out%lon_offset, &
           spixel_scan_out%lon_vmin,spixel_scan_out%lon_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lat
   !----------------------------------------------------------------------------
   spixel_scan_out%lat_scale=1.0
   spixel_scan_out%lat_offset=0.00
   spixel_scan_out%lat_vmin=-90.0
   spixel_scan_out%lat_vmax=90.0

   call nc_defdata_float(ncid, dims_var, &
           'lat', &
           spixel_scan_out%vidlat, &
           'latitude', &
           'latitude', &
           'degrees_north', spixel_scan_out%real_fill_value, &
           spixel_scan_out%lat_scale,spixel_scan_out%lat_offset, &
           spixel_scan_out%lat_vmin,spixel_scan_out%lat_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! Loop over view angles
   !----------------------------------------------------------------------------
   do iviews=1,Ctrl%Ind%NViews

      write(input_num,"(i4)") iviews

      !-------------------------------------------------------------------------
      ! satellite_zenith_view_no
      !-------------------------------------------------------------------------
      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      s_input_dummy='platform_zenith_angle_for view_no_'//trim(adjustl(input_num))

      spixel_scan_out%sat_scale=1.0
      spixel_scan_out%sat_offset=0.00
      spixel_scan_out%sat_vmin=-180.0
      spixel_scan_out%sat_vmax=180.0

      call nc_defdata_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              spixel_scan_out%vidsat_zen(iviews), &
              trim(adjustl(input_dummy)), &
              trim(adjustl(s_input_dummy)), &
              'degrees', &
              sreal_fill_value, &
              spixel_scan_out%sat_scale,spixel_scan_out%sat_offset, &
              spixel_scan_out%sat_vmin,spixel_scan_out%sat_vmax,wo,ierr)

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

      !-------------------------------------------------------------------------
      ! solar_zenith_view_no
      !-------------------------------------------------------------------------
      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      s_input_dummy='solar_zenith_angle_for view_no_'//trim(adjustl(input_num))

      spixel_scan_out%sol_scale=1.0
      spixel_scan_out%sol_offset=0.00
      spixel_scan_out%sol_vmin=-180.0
      spixel_scan_out%sol_vmax=180.0

      call nc_defdata_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              spixel_scan_out%vidsol_zen(iviews), &
              trim(adjustl(input_dummy)), &
              trim(adjustl(s_input_dummy)), &
              'degrees', &
              sreal_fill_value, &
              spixel_scan_out%sol_scale,spixel_scan_out%sol_offset, &
              spixel_scan_out%sol_vmin,spixel_scan_out%sol_vmax,wo,ierr)

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

      !-------------------------------------------------------------------------
      ! rel_azimuth_view_no
      !-------------------------------------------------------------------------
      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      s_input_dummy='angle_of_rotation_from_solar_azimuth_to_platform_azimuth_for view_no_'//trim(adjustl(input_num))

      spixel_scan_out%azi_scale=1.0
      spixel_scan_out%azi_offset=0.00
      spixel_scan_out%azi_vmin=-180.0
      spixel_scan_out%azi_vmax=180.0

      call nc_defdata_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              spixel_scan_out%vidrel_azi(iviews), &
              trim(adjustl(input_dummy)), &
              trim(adjustl(s_input_dummy)), &
              'degrees', &
              sreal_fill_value, &
              spixel_scan_out%azi_scale,spixel_scan_out%azi_offset, &
              spixel_scan_out%azi_vmin,spixel_scan_out%azi_vmax,wo,ierr)

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   end do

   !----------------------------------------------------------------------------
   ! cot
   !----------------------------------------------------------------------------
   spixel_scan_out%cot_scale=0.01
   spixel_scan_out%cot_offset=0.0
   spixel_scan_out%cot_vmin=0
   spixel_scan_out%cot_vmax=32000

   call nc_defdata_short_no_units(ncid, dims_var, &
           'cot', &
           spixel_scan_out%vidcot, &
           'cloud optical thickness', &
           'atmosphere_optical_thickness_due_to_cloud', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cot_scale,spixel_scan_out%cot_offset, &
           spixel_scan_out%cot_vmin,spixel_scan_out%cot_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref
   !----------------------------------------------------------------------------
   spixel_scan_out%ref_scale=0.01
   spixel_scan_out%ref_offset=0.00
   spixel_scan_out%ref_vmin=0
   spixel_scan_out%ref_vmax=20000

   call nc_defdata_short(ncid, dims_var, &
           'ref', &
           spixel_scan_out%vidref, &
           'effective radius', &
           'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', &
           'micrometer', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ref_scale,spixel_scan_out%ref_offset, &
           spixel_scan_out%ref_vmin,spixel_scan_out%ref_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp
   !----------------------------------------------------------------------------
   spixel_scan_out%ctp_scale=0.1
   spixel_scan_out%ctp_offset=0.00
   spixel_scan_out%ctp_vmin=0
   spixel_scan_out%ctp_vmax=12000

   call nc_defdata_short(ncid, dims_var, &
           'ctp', &
           spixel_scan_out%vidctp, &
           'cloud top pressure', &
           'air_pressure_at_cloud_top', &
           'hPa', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ctp_scale,spixel_scan_out%ctp_offset, &
           spixel_scan_out%ctp_vmin,spixel_scan_out%ctp_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total
   !----------------------------------------------------------------------------
   spixel_scan_out%cct_scale=0.01
   spixel_scan_out%cct_offset=0.00
   spixel_scan_out%cct_vmin=0
   spixel_scan_out%cct_vmax=100

   call nc_defdata_short(ncid, dims_var, &
           'cc_total', &
           spixel_scan_out%vidcct, &
           'cloud fraction', &
           'cloud_area_fraction', &
           '', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cct_scale,spixel_scan_out%cct_offset, &
           spixel_scan_out%cct_vmin,spixel_scan_out%cct_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp
   !----------------------------------------------------------------------------
   spixel_scan_out%stemp_scale=0.01
   spixel_scan_out%stemp_offset=0.00
   spixel_scan_out%stemp_vmin=0
   spixel_scan_out%stemp_vmax=32000

   call nc_defdata_short(ncid, dims_var, &
           'stemp', &
           spixel_scan_out%vidstemp, &
           'surface temperature', &
           'surface_temperature', &
           'kelvin', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%stemp_scale,spixel_scan_out%stemp_offset, &
           spixel_scan_out%stemp_vmin,spixel_scan_out%stemp_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth
   !----------------------------------------------------------------------------
   spixel_scan_out%cth_scale=0.01
   spixel_scan_out%cth_offset=0.00
   spixel_scan_out%cth_vmin=0
   spixel_scan_out%cth_vmax=2000

   call nc_defdata_short(ncid, dims_var, &
           'cth', &
           spixel_scan_out%vidcth, &
           'cloud top height', &
           'altitude_at_cloud_top', &
           'kilometer', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cth_scale,spixel_scan_out%cth_offset, &
           spixel_scan_out%cth_vmin,spixel_scan_out%cth_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%cth_error_scale=0.01
   spixel_scan_out%cth_error_offset=0.00
   spixel_scan_out%cth_error_vmin=0
   spixel_scan_out%cth_error_vmax=2000

   call nc_defdata_short(ncid, dims_var, &
           'cth_uncertainty', &
           spixel_scan_out%vidctherror, &
           'cloud top height uncertainty', &
           'altitude_at_cloud_top uncertainty', &
           'kilometer', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cth_error_scale,spixel_scan_out%cth_error_offset, &
           spixel_scan_out%cth_error_vmin,spixel_scan_out%cth_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt
   !----------------------------------------------------------------------------
   spixel_scan_out%ctt_scale=0.01
   spixel_scan_out%ctt_offset=0.00
   spixel_scan_out%ctt_vmin=0
   spixel_scan_out%ctt_vmax=32000

   call nc_defdata_short(ncid, dims_var, &
           'ctt', &
           spixel_scan_out%vidctt, &
           'cloud top temperature', &
           'air_temperature_at_cloud_top', &
           'kelvin', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ctt_scale,spixel_scan_out%ctt_offset, &
           spixel_scan_out%ctt_vmin,spixel_scan_out%ctt_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%ctt_error_scale=0.01
   spixel_scan_out%ctt_error_offset=0.00
   spixel_scan_out%ctt_error_vmin=0
   spixel_scan_out%ctt_error_vmax=32000

   call nc_defdata_short(ncid, dims_var, &
           'ctt_uncertainty', &
           spixel_scan_out%vidctterror, &
           'cloud top temperature uncertainty', &
           'air_temperature_at_cloud_top uncertainty', &
           'kelvin', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ctt_error_scale,spixel_scan_out%ctt_error_offset, &
           spixel_scan_out%ctt_error_vmin,spixel_scan_out%ctt_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp
   !----------------------------------------------------------------------------
   spixel_scan_out%cwp_scale=1.0
   spixel_scan_out%cwp_offset=0.00
   spixel_scan_out%cwp_vmin=0
   spixel_scan_out%cwp_vmax=32000

   call nc_defdata_short(ncid, dims_var, &
           'cwp', &
           spixel_scan_out%vidcwp, &
           'cloud liquid water path', &
           'atmosphere_mass_content_of_cloud_liquid_water', &
           'g/m2', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cwp_scale,spixel_scan_out%cwp_offset, &
           spixel_scan_out%cwp_vmin,spixel_scan_out%cwp_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cot_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%cot_error_scale=0.01
   spixel_scan_out%cot_error_offset=0.00
   spixel_scan_out%cot_error_vmin=0
   spixel_scan_out%cot_error_vmax=25000

   call nc_defdata_short(ncid, dims_var, &
           'cot_uncertainty', &
           spixel_scan_out%vidcoterror, &
           'cloud optical thickness uncertainty', &
           'atmosphere_optical_thickness_due_to_cloud uncertainty', &
           '', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cot_error_scale,spixel_scan_out%cot_error_offset, &
           spixel_scan_out%cot_error_vmin,spixel_scan_out%cot_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%ref_error_scale=0.01
   spixel_scan_out%ref_error_offset=0.00
   spixel_scan_out%ref_error_vmin=0
   spixel_scan_out%ref_error_vmax=20000

   call nc_defdata_short(ncid, dims_var, &
           'ref_uncertainty', &
           spixel_scan_out%vidreferror, &
           'effective radius uncertainty', &
           'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty', &
           'micrometer', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ref_error_scale,spixel_scan_out%ref_error_offset, &
           spixel_scan_out%ref_error_vmin,spixel_scan_out%ref_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%ctp_error_scale=0.1
   spixel_scan_out%ctp_error_offset=0.00
   spixel_scan_out%ctp_error_vmin=0
   spixel_scan_out%ctp_error_vmax=12000

   call nc_defdata_short(ncid, dims_var, &
           'ctp_uncertainty', &
           spixel_scan_out%vidctperror, &
           'cloud top pressure uncertainty', &
           'air_pressure_at_cloud_top uncertainty', &
           'hPa', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%ctp_error_scale,spixel_scan_out%ctp_error_offset, &
           spixel_scan_out%ctp_error_vmin,spixel_scan_out%ctp_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%cct_error_scale=0.01
   spixel_scan_out%cct_error_offset=0.00
   spixel_scan_out%cct_error_vmin=0
   spixel_scan_out%cct_error_vmax=100

   call nc_defdata_short(ncid, dims_var, &
           'cc_total_uncertainty', &
           spixel_scan_out%vidccterror, &
           'cloud fraction uncertainty', &
           'cloud_area_fraction uncertainty', &
           '', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cct_error_scale,spixel_scan_out%cct_error_offset, &
           spixel_scan_out%cct_error_vmin,spixel_scan_out%cct_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%stemp_error_scale=0.01
   spixel_scan_out%stemp_error_offset=0.00
   spixel_scan_out%stemp_error_vmin=0
   spixel_scan_out%stemp_error_vmax=30000

   call nc_defdata_short(ncid, dims_var, &
           'stemp_uncertainty', &
           spixel_scan_out%vidstemperror, &
           'surface temperature uncertainty', &
           'surface_temperature uncertainty', &
           'Kelvin', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%stemp_error_scale,spixel_scan_out%stemp_error_offset, &
           spixel_scan_out%stemp_vmin,spixel_scan_out%stemp_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp_uncertainty
   !----------------------------------------------------------------------------
   spixel_scan_out%cwp_error_scale=1.0
   spixel_scan_out%cwp_error_offset=0.00
   spixel_scan_out%cwp_error_vmin=0
   spixel_scan_out%cwp_error_vmax=32000

   call nc_defdata_short(ncid, dims_var, &
           'cwp_uncertainty', &
           spixel_scan_out%vidcwperror, &
           'CWP uncertainty', &
           'atmosphere_mass_content_of_cloud_liquid_water uncertainty', &
           'g/m2', &
           spixel_scan_out%int_fill_value, &
           spixel_scan_out%cwp_error_scale,spixel_scan_out%cwp_error_offset, &
           spixel_scan_out%cwp_error_vmin,spixel_scan_out%cwp_error_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! convergence
   !----------------------------------------------------------------------------
   spixel_scan_out%con_scale=1
   spixel_scan_out%con_offset=0
   spixel_scan_out%con_vmin=0
   spixel_scan_out%con_vmax=1

   call nc_defdata_byte_flag_value(ncid, dims_var, &
           'convergence', &
           spixel_scan_out%vidconvergence, &
           'retrieval convergence', &
           'retrieval_convergence_flag', &
           '0b, 1b', &
           'yes, no', &
           spixel_scan_out%byte_fill_value, &
           spixel_scan_out%con_scale,spixel_scan_out%con_offset, &
           spixel_scan_out%con_vmin,spixel_scan_out%con_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   spixel_scan_out%niter_scale=1
   spixel_scan_out%niter_offset=0
   spixel_scan_out%niter_vmin=0
   spixel_scan_out%niter_vmax=100

   call nc_defdata_byte(ncid, dims_var, &
           'niter', &
           spixel_scan_out%vidniter, &
           'retrieval iterations', &
           'retrieval_iterations', &
           '', &
           spixel_scan_out%byte_fill_value, &
           spixel_scan_out%niter_scale,spixel_scan_out%niter_offset, &
           spixel_scan_out%niter_vmin,spixel_scan_out%niter_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   spixel_scan_out%pchange_scale=1
   spixel_scan_out%pchange_offset=0
   spixel_scan_out%pchange_vmin=0
   spixel_scan_out%pchange_vmax=1

   call nc_defdata_byte_flag_value(ncid, dims_var, &
           'phase', &
           spixel_scan_out%vidpchange, &
           'cloud phase flag', &
           'cloud_phase_flag', &
           '0b, 1b, 2b', &
           'clear/unknown, liquid, ice', &
           spixel_scan_out%byte_fill_value, &
           spixel_scan_out%pchange_scale,spixel_scan_out%pchange_offset, &
           spixel_scan_out%pchange_vmin,spixel_scan_out%pchange_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   spixel_scan_out%costja_scale=1.0
   spixel_scan_out%costja_offset=0.00
   spixel_scan_out%costja_vmin=0.0
   spixel_scan_out%costja_vmax=100000.

   call nc_defdata_float(ncid, dims_var, &
           'costja', &
           spixel_scan_out%vidcostja, &
           'costja', &
           'a_priori_cost at_solution', &
           '', &
           sreal_fill_value, &
           spixel_scan_out%costja_scale,spixel_scan_out%costja_offset, &
           spixel_scan_out%costja_vmin,spixel_scan_out%costja_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   spixel_scan_out%costjm_scale=1.0
   spixel_scan_out%costjm_offset=0.00
   spixel_scan_out%costjm_vmin=0.0
   spixel_scan_out%costjm_vmax=100000.

   call nc_defdata_float(ncid, dims_var, &
           'costjm', &
           spixel_scan_out%vidcostjm, &
           'costjm', &
           'measurement_cost at_solution', &
           '', &
           sreal_fill_value, &
           spixel_scan_out%costjm_scale,spixel_scan_out%costjm_offset, &
           spixel_scan_out%costjm_vmin,spixel_scan_out%costjm_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   spixel_scan_out%ls_scale=1
   spixel_scan_out%ls_offset=0
   spixel_scan_out%ls_vmin=0
   spixel_scan_out%ls_vmax=6

   call nc_defdata_byte_flag_value(ncid, dims_var, &
           'lsflag', &
           spixel_scan_out%vidlsflag, &
           'land/sea flag', &
           'land_binary_mask', &
           '0b, 1b, 2b, 3b, 4b, 5b, 6b', &
           'sea, land, sunglint, snow, ice, snow_and_ice', &
           spixel_scan_out%byte_fill_value, &
           spixel_scan_out%ls_scale,spixel_scan_out%ls_offset, &
           spixel_scan_out%ls_vmin,spixel_scan_out%ls_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   spixel_scan_out%qc_scale=1
   spixel_scan_out%qc_offset=0
   spixel_scan_out%qc_vmin=0
   spixel_scan_out%qc_vmax=254

   s_input_dummy='Bit 0 unused, always set to 0, Bits 1-5 set to 1 if state variable error out of bounds'
   s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 6 set to 1 if no convergence achieved'
   s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 7 set to 1 if cost too large.'
   s_input_dummy=trim(adjustl(s_input_dummy))//' Bit 1=COT Bit 2=REF Bit 3=CTP Bit 4=CCT Bit 5=STEMP'

   call nc_defdata_short_flag_value(ncid, dims_var, &
           'qcflag', &
           spixel_scan_out%vidqcflag, &
           'quality control flag', &
           'quality_control_flag', &
           trim(adjustl(s_input_dummy)), &
           int(-1,kind=sint), &
           spixel_scan_out%qc_scale,spixel_scan_out%qc_offset, &
           spixel_scan_out%qc_vmin,spixel_scan_out%qc_vmax,wo,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   spixel_scan_out%illum_scale=1
   spixel_scan_out%illum_offset=0
   spixel_scan_out%illum_vmin=1
   spixel_scan_out%illum_vmax=12

   call nc_defdata_byte_flag_value(ncid, dims_var, &
           'illum', &
           spixel_scan_out%vidillum, &
           'illumination flag', &
           'illumination_flag', &
           '1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, 11b, 12b', &
           'Day Twilight Night Daynore DayMissingSingleVisFirst, DayMissingSingleVisSecond, DayMissingSingleIRFirst, DayMissingSingleIRSecond, DayMissingSingleIRThird, NightMissingSingleIRFirst, NightMissingSingleIRSecond, NightMissingSingleIRThird', &
           spixel_scan_out%byte_fill_value, &
           spixel_scan_out%illum_scale,spixel_scan_out%illum_offset, &
           spixel_scan_out%illum_vmin,spixel_scan_out%illum_vmax,wo,ierr)

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
      write(*,*) 'def_vars_primary.inc: netcdf variable definintion error:', status
      call Write_Log(Ctrl,'def_vars_primary.inc: netcdf variable definintion error:', status)
      stop
   end if

end subroutine def_vars_primary
