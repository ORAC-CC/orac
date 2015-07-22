!-------------------------------------------------------------------------------
! Name: def_vars_secondary_pp.F90
!
! Purpose:
! Define secondary output variables for netcdf. Variable type, scale, offset,
! fill value and /or range are defined and applied to the variable definition.
! Variable names are also defined.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2011/12/19, Matthias Jerg: creates initial output for main output variables.
! 2012/01/05, Caroline Poulsen: add channel info
! 2012/01/15, Caroline Poulsen: modified scales and offsets for residuals
! 2015/07/16, Greg McGarragh: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_secondary_pp(ncid, indexing, dims_var, output_data, &
                                 global_atts, verbose)

   use global_attributes
   use input_routines
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   integer,                        intent(in)    :: ncid
   type(counts_and_indexes),       intent(in)    :: indexing
   integer,                        intent(in)    :: dims_var(2)
   type(output_data_secondary_pp), intent(inout) :: output_data
   type(global_attributes_s),      intent(in)    :: global_atts
   logical,                        intent(in)    :: verbose

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   logical            :: lcovar = .false.
   integer            :: i
   integer            :: XMax, YMax


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
   output_data%scanline_u_scale=1
   output_data%scanline_u_offset=0
   output_data%scanline_u_vmin=1
   output_data%scanline_u_vmax=Xmax

   call nc_def_var_long_packed_long( &
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
           valid_max     = output_data%scanline_u_vmax)

   !----------------------------------------------------------------------------
   ! scanline_v
   !----------------------------------------------------------------------------
   output_data%scanline_v_scale=1
   output_data%scanline_v_offset=0
   output_data%scanline_v_vmin=1
   output_data%scanline_v_vmax=Ymax

   call nc_def_var_long_packed_long( &
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
           valid_max     = output_data%scanline_v_vmax)

   !----------------------------------------------------------------------------
   ! cot_ap
   !----------------------------------------------------------------------------
   output_data%cot_ap_scale=0.01
   output_data%cot_ap_offset=0.0
   output_data%cot_ap_vmin=0
   output_data%cot_ap_vmax=32000

   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_ap', &
           output_data%vid_cot_ap, &
           verbose, &
           long_name     = 'cloud optical thickness a priori', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_ap_scale, &
           add_offset    = output_data%cot_ap_offset, &
           valid_min     = output_data%cot_ap_vmin, &
           valid_max     = output_data%cot_ap_vmax)

   !----------------------------------------------------------------------------
   ! cot_fg
   !----------------------------------------------------------------------------
   output_data%cot_fg_scale=0.01
   output_data%cot_fg_offset=0.0
   output_data%cot_fg_vmin=0
   output_data%cot_fg_vmax=32000

   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'cot_fg', &
           output_data%vid_cot_fg, &
           verbose, &
           long_name     = 'cloud optical thickness first guess', &
           standard_name = 'atmosphere_optical_thickness_due_to_cloud first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%cot_fg_scale, &
           add_offset    = output_data%cot_fg_offset, &
           valid_min     = output_data%cot_fg_vmin, &
           valid_max     = output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! ref_ap
   !----------------------------------------------------------------------------
   output_data%ref_ap_scale=0.01
   output_data%ref_ap_offset=0.0
   output_data%ref_ap_vmin=0
   output_data%ref_ap_vmax=20000

   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref_ap', &
           output_data%vid_ref_ap, &
           verbose, &
           long_name     = 'effective radius a priori', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particle_at_cloud_top a_priori', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_ap_scale, &
           add_offset    = output_data%ref_ap_offset, &
           valid_min     = output_data%ref_ap_vmin, &
           valid_max     = output_data%ref_ap_vmax, &
           units         = 'micrometer')

   !----------------------------------------------------------------------------
   ! ref_fg
   !----------------------------------------------------------------------------
   output_data%ref_fg_scale=0.01
   output_data%ref_fg_offset=0.0
   output_data%ref_fg_vmin=0
   output_data%ref_fg_vmax=20000

   call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           'ref_fg', &
           output_data%vid_ref_fg, &
           verbose, &
           long_name     = 'effective radius first guess', &
           standard_name = 'effective_radius_of_cloud_condensed_water_particle_at_cloud_top first_guess', &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%ref_fg_scale, &
           add_offset    = output_data%ref_fg_offset, &
           valid_min     = output_data%ref_fg_vmin, &
           valid_max     = output_data%ref_fg_vmax, &
           units         = 'micrometer')

   !----------------------------------------------------------------------------
   ! ctp_ap
   !----------------------------------------------------------------------------
   output_data%ctp_ap_scale=0.1
   output_data%ctp_ap_offset=0.0
   output_data%ctp_ap_vmin=500
   output_data%ctp_ap_vmax=12000

   call nc_def_var_short_packed_float( &
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
           units         = 'hPa')

   !----------------------------------------------------------------------------
   ! ctp_fg
   !----------------------------------------------------------------------------
   output_data%ctp_fg_scale=0.1
   output_data%ctp_fg_offset=0.0
   output_data%ctp_fg_vmin=500
   output_data%ctp_fg_vmax=12000

   call nc_def_var_short_packed_float( &
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
           units         = 'hPa')

   !----------------------------------------------------------------------------
   ! stemp_ap
   !----------------------------------------------------------------------------
   output_data%stemp_ap_scale=0.01
   output_data%stemp_ap_offset=100.0
   output_data%stemp_ap_vmin=0
   output_data%stemp_ap_vmax=32000

   call nc_def_var_short_packed_float( &
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
           units         = 'kelvin')

   !----------------------------------------------------------------------------
   ! stemp_fg
   !----------------------------------------------------------------------------
   output_data%stemp_fg_scale=0.01
   output_data%stemp_fg_offset=100.0
   output_data%stemp_fg_vmin=0
   output_data%stemp_fg_vmax=32000

   call nc_def_var_short_packed_float( &
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
           units         = 'kelvin')

   !----------------------------------------------------------------------------
   ! albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%NSolar

      write(input_num,"(i4)") indexing%Y_Id(i)

      output_data%albedo_scale(i)=0.0001
      output_data%albedo_offset(i)=0.0
      output_data%albedo_vmin(i)=0
      output_data%albedo_vmax(i)=10000

      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))
      input_dummy2='albedo in channel no '//trim(adjustl(input_num))

      call nc_def_var_short_packed_float( &
           ncid, &
           dims_var, &
           trim(adjustl(input_dummy)), &
           output_data%vid_albedo(i), &
           verbose, &
           long_name     = trim(adjustl(input_dummy2)), &
           standard_name = trim(adjustl(input_dummy)), &
           fill_value    = sint_fill_value, &
           scale_factor  = output_data%albedo_scale(i), &
           add_offset    = output_data%albedo_offset(i), &
           valid_min     = output_data%albedo_vmin(i), &
           valid_max     = output_data%albedo_vmax(i))
   end do

   !----------------------------------------------------------------------------
   ! reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         output_data%channels_scale(i)=0.0001
         output_data%channels_offset(i)=0.0
         output_data%channels_vmin(i)=0
         output_data%channels_vmax(i)=10000

         input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='reflectance in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              valid_max     = output_data%channels_vmax(i))
      else
         output_data%channels_scale(i)=0.01
         output_data%channels_offset(i)=100.0
         output_data%channels_vmin(i)=0
         output_data%channels_vmax(i)=32000

         input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='brightness temperature in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              units         = 'kelvin')
      end if

   end do

   !----------------------------------------------------------------------------
   ! firstguess reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         output_data%y0_scale(i)=0.0001
         output_data%y0_offset(i)=0.0
         output_data%y0_vmin(i)=0
         output_data%y0_vmax(i)=10000

         input_dummy='firstguess_reflectance_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='firstguess reflectance in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              valid_max     = output_data%y0_vmax(i))
      else
         output_data%y0_scale(i)=0.01
         output_data%y0_offset(i)=100.0
         output_data%y0_vmin(i)=0
         output_data%y0_vmax(i)=32000

         input_dummy='firstguess_brightness_temperature_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='firstguess brightness temperature in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              units         = 'kelvin')
      end if

   end do

   !----------------------------------------------------------------------------
   ! reflectances and brightness temperature _residual_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,indexing%Ny

      write(input_num,"(i4)") indexing%Y_Id(i)

      if (.not. btest(indexing%Ch_Is(i), ThermalBit)) then
         output_data%residuals_scale(i)=0.0001
         output_data%residuals_offset(i)=0.0
         output_data%residuals_vmin(i)=-10000
         output_data%residuals_vmax(i)=10000

         input_dummy='reflectance_residual_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='reflectance residual in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              valid_max     = output_data%residuals_vmax(i))
      else
         output_data%residuals_scale(i)=0.01
         output_data%residuals_offset(i)=100.0
         output_data%residuals_vmin(i)=-32000
         output_data%residuals_vmax(i)=32000

         input_dummy='brightness_temperature_residual_in_channel_no_'//trim(adjustl(input_num))
         input_dummy2='brightness temperature residual in channel no '//trim(adjustl(input_num))

         call nc_def_var_short_packed_float( &
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
              units         = 'kelvin')
      end if

   end do

   !----------------------------------------------------------------------------
   ! degrees_of_freedom_signal
   !----------------------------------------------------------------------------
   output_data%ds_scale=0.001
   output_data%ds_offset=0.0
   output_data%ds_vmin=0
   output_data%ds_vmax=10000

   call nc_def_var_short_packed_float( &
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
           valid_max     = output_data%ds_vmax)

   !----------------------------------------------------------------------------
   ! covariance_matrix_element_*
   !----------------------------------------------------------------------------
   if (lcovar) then

   end if


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine def_vars_secondary_pp
