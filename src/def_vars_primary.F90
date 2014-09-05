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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_primary(Ctrl, ncid, dims_var, output_data, status)

   use CTRL_def
   use nc_utils
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
   output_data%time_scale=1.0
   output_data%time_offset=0.0
   output_data%time_vmin=0.0
   output_data%time_vmax=1.0e10

   if (trim(Ctrl%inst%name) .eq. 'AATSR') then
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 2000'
   else
      input_dummy='Julian Date, days elapsed since 12:00 January 1, 4713 BC'
   end if

   call nc_def_var_double_packed_double(ncid, dims_var, &
           'time', &
           output_data%vid_time, &
           'time', &
           'time', &
           dreal_fill_value, &
           output_data%time_scale,output_data%time_offset, &
           output_data%time_vmin,output_data%time_vmax, &
           verbose,ierr, &
           units=input_dummy)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lat
   !----------------------------------------------------------------------------
   output_data%lat_scale=1.0
   output_data%lat_offset=0.0
   output_data%lat_vmin=-90.0
   output_data%lat_vmax=90.0

   call nc_def_var_float_packed_float(ncid, dims_var, &
           'lat', &
           output_data%vid_lat, &
           'latitude', &
           'latitude', &
           sreal_fill_value, &
           output_data%lat_scale,output_data%lat_offset, &
           output_data%lat_vmin,output_data%lat_vmax, &
           verbose,ierr, &
           units='degrees_north')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lon
   !----------------------------------------------------------------------------
   output_data%lon_scale=1.0
   output_data%lon_offset=0.0
   output_data%lon_vmin=-180.0
   output_data%lon_vmax=180.0

   call nc_def_var_float_packed_float(ncid, dims_var, &
           'lon', &
           output_data%vid_lon, &
           'longitude', &
           'longitude', &
           sreal_fill_value, &
           output_data%lon_scale,output_data%lon_offset, &
           output_data%lon_vmin,output_data%lon_vmax, &
           verbose,ierr, &
           units='degrees_east')

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

      output_data%sol_scale=1.0
      output_data%sol_offset=0.0
      output_data%sol_vmin=-180.0
      output_data%sol_vmax=180.0

      call nc_def_var_float_packed_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sol_zen(iviews), &
              trim(adjustl(input_dummy2)), &
              trim(adjustl(input_dummy3)), &
              sreal_fill_value, &
              output_data%sol_scale,output_data%sol_offset, &
              output_data%sol_vmin,output_data%sol_vmax, &
              verbose,ierr, &
              units='degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

      !-------------------------------------------------------------------------
      ! satellite_zenith_view_no*
      !-------------------------------------------------------------------------
      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      input_dummy2='satellite zenith angle for view no '//trim(adjustl(input_num))
      input_dummy3='satellite_zenith_angle_for_view_no_'//trim(adjustl(input_num))

      output_data%sat_scale=1.0
      output_data%sat_offset=0.0
      output_data%sat_vmin=-180.0
      output_data%sat_vmax=180.0

      call nc_def_var_float_packed_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_sat_zen(iviews), &
              trim(adjustl(input_dummy2)), &
              trim(adjustl(input_dummy3)), &
              sreal_fill_value, &
              output_data%sat_scale,output_data%sat_offset, &
              output_data%sat_vmin,output_data%sat_vmax, &
              verbose,ierr, &
              units='degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

      !-------------------------------------------------------------------------
      ! rel_azimuth_view_no*
      !-------------------------------------------------------------------------
      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      input_dummy2='relative azimuth angle for view no '//trim(adjustl(input_num))
      input_dummy3='angle_of_rotation_from_solar_azimuth_to_platform_azimuth_for_view_no_'//trim(adjustl(input_num))

      output_data%azi_scale=1.0
      output_data%azi_offset=0.0
      output_data%azi_vmin=-180.0
      output_data%azi_vmax=180.0

      call nc_def_var_float_packed_float(ncid, dims_var, &
              trim(adjustl(input_dummy)), &
              output_data%vid_rel_azi(iviews), &
              trim(adjustl(input_dummy2)), &
              trim(adjustl(input_dummy3)), &
              sreal_fill_value, &
              output_data%azi_scale,output_data%azi_offset, &
              output_data%azi_vmin,output_data%azi_vmax, &
              verbose,ierr, &
              units='degrees')

      if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   end do

   !----------------------------------------------------------------------------
   ! cot
   !----------------------------------------------------------------------------
   output_data%cot_scale=0.01
   output_data%cot_offset=0.0
   output_data%cot_vmin=0
   output_data%cot_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cot', &
           output_data%vid_cot, &
           'cloud optical thickness', &
           'atmosphere_optical_thickness_due_to_cloud', &
           sint_fill_value, &
           output_data%cot_scale,output_data%cot_offset, &
           output_data%cot_vmin,output_data%cot_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cot_uncertainty
   !----------------------------------------------------------------------------
   output_data%cot_error_scale=0.01
   output_data%cot_error_offset=0.0
   output_data%cot_error_vmin=output_data%cot_vmin
   output_data%cot_error_vmax=output_data%cot_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cot_uncertainty', &
           output_data%vid_coterror, &
           'cloud optical thickness uncertainty', &
           'atmosphere_optical_thickness_due_to_cloud uncertainty', &
           sint_fill_value, &
           output_data%cot_error_scale,output_data%cot_error_offset, &
           output_data%cot_error_vmin,output_data%cot_error_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref
   !----------------------------------------------------------------------------
   output_data%ref_scale=0.01
   output_data%ref_offset=0.0
   output_data%ref_vmin=0
   output_data%ref_vmax=20000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ref', &
           output_data%vid_ref, &
           'effective radius', &
           'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', &
           sint_fill_value, &
           output_data%ref_scale,output_data%ref_offset, &
           output_data%ref_vmin,output_data%ref_vmax, &
           verbose,ierr, &
           units='micrometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref_uncertainty
   !----------------------------------------------------------------------------
   output_data%ref_error_scale=0.01
   output_data%ref_error_offset=0.0
   output_data%ref_error_vmin=output_data%ref_vmin
   output_data%ref_error_vmax=output_data%ref_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ref_uncertainty', &
           output_data%vid_referror, &
           'effective radius uncertainty', &
           'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty', &
           sint_fill_value, &
           output_data%ref_error_scale,output_data%ref_error_offset, &
           output_data%ref_error_vmin,output_data%ref_error_vmax, &
           verbose,ierr, &
           units='micrometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp
   !----------------------------------------------------------------------------
   output_data%ctp_scale=0.1
   output_data%ctp_offset=0.0
   output_data%ctp_vmin=0
   output_data%ctp_vmax=12000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctp', &
           output_data%vid_ctp, &
           'cloud top pressure', &
           'air_pressure_at_cloud_top', &
           sint_fill_value, &
           output_data%ctp_scale,output_data%ctp_offset, &
           output_data%ctp_vmin,output_data%ctp_vmax, &
           verbose,ierr, &
           units='hPa')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp_uncertainty
   !----------------------------------------------------------------------------
   output_data%ctp_error_scale=0.1
   output_data%ctp_error_offset=0.0
   output_data%ctp_error_vmin=output_data%ctp_vmin
   output_data%ctp_error_vmax=output_data%ctp_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctp_uncertainty', &
           output_data%vid_ctperror, &
           'cloud top pressure uncertainty', &
           'air_pressure_at_cloud_top uncertainty', &
           sint_fill_value, &
           output_data%ctp_error_scale,output_data%ctp_error_offset, &
           output_data%ctp_error_vmin,output_data%ctp_error_vmax, &
           verbose,ierr, &
           units='hPa')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total
   !----------------------------------------------------------------------------
   output_data%cct_scale=0.01
   output_data%cct_offset=0.0
   output_data%cct_vmin=0
   output_data%cct_vmax=100

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cc_total', &
           output_data%vid_cct, &
           'cloud fraction', &
           'cloud_area_fraction', &
           sint_fill_value, &
           output_data%cct_scale,output_data%cct_offset, &
           output_data%cct_vmin,output_data%cct_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cc_total_uncertainty
   !----------------------------------------------------------------------------
   output_data%cct_error_scale=0.01
   output_data%cct_error_offset=0.0
   output_data%cct_error_vmin=output_data%cct_vmin
   output_data%cct_error_vmax=output_data%cct_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cc_total_uncertainty', &
           output_data%vid_ccterror, &
           'cloud fraction uncertainty', &
           'cloud_area_fraction uncertainty', &
           sint_fill_value, &
           output_data%cct_error_scale,output_data%cct_error_offset, &
           output_data%cct_error_vmin,output_data%cct_error_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp
   !----------------------------------------------------------------------------
   output_data%stemp_scale=0.01
   output_data%stemp_offset=0.0
   output_data%stemp_vmin=0
   output_data%stemp_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'stemp', &
           output_data%vid_stemp, &
           'surface temperature', &
           'surface_temperature', &
           sint_fill_value, &
           output_data%stemp_scale,output_data%stemp_offset, &
           output_data%stemp_vmin,output_data%stemp_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp_uncertainty
   !----------------------------------------------------------------------------
   output_data%stemp_error_scale=0.01
   output_data%stemp_error_offset=0.0
   output_data%stemp_error_vmin=output_data%stemp_vmin
   output_data%stemp_error_vmax=output_data%stemp_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'stemp_uncertainty', &
           output_data%vid_stemperror, &
           'surface temperature uncertainty', &
           'surface_temperature uncertainty', &
           sint_fill_value, &
           output_data%stemp_error_scale,output_data%stemp_error_offset, &
           output_data%stemp_vmin,output_data%stemp_error_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth
   !----------------------------------------------------------------------------
   output_data%cth_scale=0.01
   output_data%cth_offset=0.0
   output_data%cth_vmin=0
   output_data%cth_vmax=2000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cth', &
           output_data%vid_cth, &
           'cloud top height', &
           'cloud_top_altitide', &
           sint_fill_value, &
           output_data%cth_scale,output_data%cth_offset, &
           output_data%cth_vmin,output_data%cth_vmax, &
           verbose,ierr, &
           units='kilometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cth_uncertainty
   !----------------------------------------------------------------------------
   output_data%cth_error_scale=0.01
   output_data%cth_error_offset=0.0
   output_data%cth_error_vmin=output_data%cth_vmin
   output_data%cth_error_vmax=output_data%cth_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cth_uncertainty', &
           output_data%vid_ctherror, &
           'cloud top height uncertainty', &
           'cloud_top_altitide uncertainty', &
           sint_fill_value, &
           output_data%cth_error_scale,output_data%cth_error_offset, &
           output_data%cth_error_vmin,output_data%cth_error_vmax, &
           verbose,ierr, &
           units='kilometer')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt
   !----------------------------------------------------------------------------
   output_data%ctt_scale=0.01
   output_data%ctt_offset=0.0
   output_data%ctt_vmin=0
   output_data%ctt_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctt', &
           output_data%vid_ctt, &
           'cloud top temperature', &
           'air_temperature_at_cloud_top', &
           sint_fill_value, &
           output_data%ctt_scale,output_data%ctt_offset, &
           output_data%ctt_vmin,output_data%ctt_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctt_uncertainty
   !----------------------------------------------------------------------------
   output_data%ctt_error_scale=0.01
   output_data%ctt_error_offset=0.0
   output_data%ctt_error_vmin=output_data%ctt_vmin
   output_data%ctt_error_vmax=output_data%ctt_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctt_uncertainty', &
           output_data%vid_ctterror, &
           'cloud top temperature uncertainty', &
           'air_temperature_at_cloud_top uncertainty', &
           sint_fill_value, &
           output_data%ctt_error_scale,output_data%ctt_error_offset, &
           output_data%ctt_error_vmin,output_data%ctt_error_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp
   !----------------------------------------------------------------------------
   output_data%cwp_scale=1.0
   output_data%cwp_offset=0.0
   output_data%cwp_vmin=0
   output_data%cwp_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cwp', &
           output_data%vid_cwp, &
           'cloud liquid water path', &
           'atmosphere_mass_content_of_cloud_liquid_water', &
           sint_fill_value, &
           output_data%cwp_scale,output_data%cwp_offset, &
           output_data%cwp_vmin,output_data%cwp_vmax, &
           verbose,ierr, &
           units='g/m2')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cwp_uncertainty
   !----------------------------------------------------------------------------
   output_data%cwp_error_scale=1.0
   output_data%cwp_error_offset=0.0
   output_data%cwp_error_vmin=output_data%cwp_vmin
   output_data%cwp_error_vmax=output_data%cwp_vmax

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cwp_uncertainty', &
           output_data%vid_cwperror, &
           'CWP uncertainty', &
           'atmosphere_mass_content_of_cloud_liquid_water uncertainty', &
           sint_fill_value, &
           output_data%cwp_error_scale,output_data%cwp_error_offset, &
           output_data%cwp_error_vmin,output_data%cwp_error_vmax, &
           verbose,ierr, &
           units='g/m2')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! convergence
   !----------------------------------------------------------------------------
   output_data%convergence_scale=1
   output_data%convergence_offset=0
   output_data%convergence_vmin=0
   output_data%convergence_vmax=1

   call nc_def_var_byte_packed_byte(ncid, dims_var, &
           'convergence', &
           output_data%vid_convergence, &
           'retrieval convergence flag', &
           'retrieval_convergence_flag', &
           byte_fill_value, &
           output_data%convergence_scale,output_data%convergence_offset, &
           output_data%convergence_vmin,output_data%convergence_vmax, &
           verbose,ierr, &
           flag_values='0b, 1b', &
           flag_meanings='yes, no')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   output_data%niter_scale=1
   output_data%niter_offset=0
   output_data%niter_vmin=0
   output_data%niter_vmax=Ctrl%Invpar%MaxIter

   call nc_def_var_byte_packed_byte(ncid, dims_var, &
           'niter', &
           output_data%vid_niter, &
           'number of retrieval iterations', &
           'number_of_retrieval_iterations', &
           byte_fill_value, &
           output_data%niter_scale,output_data%niter_offset, &
           output_data%niter_vmin,output_data%niter_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   output_data%phase_scale=1
   output_data%phase_offset=0
   output_data%phase_vmin=0
   output_data%phase_vmax=2

   call nc_def_var_byte_packed_byte(ncid, dims_var, &
           'phase', &
           output_data%vid_phase, &
           'cloud phase flag', &
           'cloud_phase_flag', &
           byte_fill_value, &
           output_data%phase_scale,output_data%phase_offset, &
           output_data%phase_vmin,output_data%phase_vmax, &
           verbose,ierr, &
           flag_values='0b, 1b, 2b', &
           flag_meanings='clear/unknown, liquid, ice')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   output_data%costja_scale=1.0
   output_data%costja_offset=0.0
   output_data%costja_vmin=0.0
   output_data%costja_vmax=100000.

   call nc_def_var_float_packed_float(ncid, dims_var, &
           'costja', &
           output_data%vid_costja, &
           'a priori cost at solution', &
           'a_priori_cost_at_solution', &
           sreal_fill_value, &
           output_data%costja_scale,output_data%costja_offset, &
           output_data%costja_vmin,output_data%costja_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   output_data%costjm_scale=1.0
   output_data%costjm_offset=0.0
   output_data%costjm_vmin=0.0
   output_data%costjm_vmax=100000.

   call nc_def_var_float_packed_float(ncid, dims_var, &
           'costjm', &
           output_data%vid_costjm, &
           'measurement cost at solution', &
           'measurement_cost_at_solution', &
           sreal_fill_value, &
           output_data%costjm_scale,output_data%costjm_offset, &
           output_data%costjm_vmin,output_data%costjm_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag_scale=1
   output_data%lsflag_offset=0
   output_data%lsflag_vmin=0
   output_data%lsflag_vmax=6

   call nc_def_var_byte_packed_byte(ncid, dims_var, &
           'lsflag', &
           output_data%vid_lsflag, &
           'land/sea flag', &
           'land_binary_mask', &
           byte_fill_value, &
           output_data%lsflag_scale,output_data%lsflag_offset, &
           output_data%lsflag_vmin,output_data%lsflag_vmax, &
           verbose,ierr, &
           flag_values='0b, 1b, 2b, 3b, 4b, 5b, 6b', &
           flag_meanings='sea, land, sunglint, snow, ice, snow_and_ice')

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   output_data%qcflag_scale=1
   output_data%qcflag_offset=0
   output_data%qcflag_vmin=0
   output_data%qcflag_vmax=254

   input_dummy2='Bit 0 unused, always set to 0, Bits 1-5 set to 1 if state variable error out of bounds'
   input_dummy2=trim(adjustl(input_dummy2))//', Bit 6 set to 1 if no convergence achieved'
   input_dummy2=trim(adjustl(input_dummy2))//', Bit 7 set to 1 if cost too large.'
   input_dummy2=trim(adjustl(input_dummy2))//' Bit 1=COT Bit 2=REF Bit 3=CTP Bit 4=CCT Bit 5=STEMP'

   call nc_def_var_short_packed_short(ncid, dims_var, &
           'qcflag', &
           output_data%vid_qcflag, &
           'quality control flag', &
           'quality_control_flag', &
           int(-1,kind=sint), &
           output_data%qcflag_scale,output_data%qcflag_offset, &
           output_data%qcflag_vmin,output_data%qcflag_vmax, &
           verbose,ierr, &
           flag_meanings=trim(adjustl(input_dummy2)))

   if (ierr .ne. NF90_NOERR) status=PrimaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum_scale=1
   output_data%illum_offset=0
   output_data%illum_vmin=1
   output_data%illum_vmax=12

   call nc_def_var_byte_packed_byte(ncid, dims_var, &
           'illum', &
           output_data%vid_illum, &
           'illumination flag', &
           'illumination_flag', &
           byte_fill_value, &
           output_data%illum_scale,output_data%illum_offset, &
           output_data%illum_vmin,output_data%illum_vmax, &
           verbose,ierr, &
           flag_values='1b, 2b, 3b, 4b, 5b, 6b, 7b, 8b, 9b, 10b, 11b, 12b', &
           flag_meanings='Day Twilight Night Daynore DayMissingSingleVisFirst, DayMissingSingleVisSecond, DayMissingSingleIRFirst, DayMissingSingleIRSecond, DayMissingSingleIRThird, NightMissingSingleIRFirst, NightMissingSingleIRSecond, NightMissingSingleIRThird')

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
