!-------------------------------------------------------------------------------
! Name: def_vars_secondary.F90
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
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial version
! 2012/01/05, C. Poulsen: Add channel info
! 2012/01/15, C. Poulsen: Modified scales and offsets for residuals
! 2012/02/28, C. Poulsen: Bug fix to attributes of scanline_v
! 2012/02/28, C. Poulsen: Added in albedo
! 2012/02/28, C. Poulsen: Indexedy_id array correctly
! 2013/01/25, C. Poulsen: Modified how channels are labelled now reads form the
!    input file the channel names.
! 2013/05/29, G. Thomas: Added degrees of freedom for signal
! 2013/07/24, A. Povey: Fixed bug in writing output_data%vid_y0
! 2013/12/05, MJ: Fixes bug with writing of albedo and radiance and bt output.
! 2013/12/09, MJ: Slightly adapts range of reflectances.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
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

subroutine def_vars_secondary(Ctrl, conf, lcovar, ncid, dims_var, output_data, &
                              status)

   use config_def
   use CTRL_def
   use nc_utils
   use SPixel_def

   use netcdf

   implicit none

   type(CTRL_t),                intent(in)    :: Ctrl
   type(config_struct),         intent(in)    :: conf
   logical,                     intent(in)    :: lcovar
   integer,                     intent(in)    :: ncid
   integer,                     intent(in)    :: dims_var(2)
   type(output_data_secondary), intent(inout) :: output_data
   integer,                     intent(inout) :: status

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   character(len=512) :: input_dummy2
   integer            :: i, j
   integer            :: ierr
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
   ! scanline_u
   !----------------------------------------------------------------------------
   output_data%scanline_u_scale=1
   output_data%scanline_u_offset=0
   output_data%scanline_u_vmin=1
   output_data%scanline_u_vmax=Ctrl%Ind%Xmax

   call nc_def_var_long_packed_long(ncid, dims_var, &
           'scanline_u', &
           output_data%vid_scanline_u, &
           'across track pixel index', &
           'across_track_pixel_index', &
           lint_fill_value, &
           output_data%scanline_u_scale,output_data%scanline_u_offset, &
           output_data%scanline_u_vmin,output_data%scanline_u_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! scanline_v
   !----------------------------------------------------------------------------
   output_data%scanline_v_scale=1
   output_data%scanline_v_offset=0
   output_data%scanline_v_vmin=1
   output_data%scanline_v_vmax=Ctrl%Ind%Ymax

   call nc_def_var_long_packed_long(ncid, dims_var, &
           'scanline_v', &
           output_data%vid_scanline_v, &
           'along track pixel index', &
           'along_track_pixel_index', &
           lint_fill_value, &
           output_data%scanline_v_scale,output_data%scanline_v_offset, &
           output_data%scanline_v_vmin,output_data%scanline_v_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cot_ap
   !----------------------------------------------------------------------------
   output_data%cot_ap_scale=0.01
   output_data%cot_ap_offset=0.0
   output_data%cot_ap_vmin=0
   output_data%cot_ap_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cot_ap', &
           output_data%vid_cot_ap, &
           'cloud optical thickness a priori', &
           'atmosphere_optical_thickness_due_to_cloud a_priori', &
           sint_fill_value, &
           output_data%cot_ap_scale,output_data%cot_ap_offset, &
           output_data%cot_ap_vmin,output_data%cot_ap_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! cot_fg
   !----------------------------------------------------------------------------
   output_data%cot_fg_scale=0.01
   output_data%cot_fg_offset=0.0
   output_data%cot_fg_vmin=0
   output_data%cot_fg_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'cot_fg', &
           output_data%vid_cot_fg, &
           'cloud optical thickness first guess', &
           'atmosphere_optical_thickness_due_to_cloud first_guess', &
           sint_fill_value, &
           output_data%cot_fg_scale,output_data%cot_fg_offset, &
           output_data%cot_fg_vmin,output_data%cot_fg_vmax, &
           verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref_ap
   !----------------------------------------------------------------------------
   output_data%ref_ap_scale=0.01
   output_data%ref_ap_offset=0.0
   output_data%ref_ap_vmin=0
   output_data%ref_ap_vmax=20000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ref_ap', &
           output_data%vid_ref_ap, &
           'effective radius a priori', &
           'effective_radius_of_cloud_condensed_water_particle_at_cloud_top a_priori', &
           sint_fill_value, &
           output_data%ref_ap_scale,output_data%ref_ap_offset, &
           output_data%ref_ap_vmin,output_data%ref_ap_vmax, &
           verbose,ierr, &
           units='micrometer')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ref_fg
   !----------------------------------------------------------------------------
   output_data%ref_fg_scale=0.01
   output_data%ref_fg_offset=0.0
   output_data%ref_fg_vmin=0
   output_data%ref_fg_vmax=20000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ref_fg', &
           output_data%vid_ref_fg, &
           'effective radius first guess', &
           'effective_radius_of_cloud_condensed_water_particle_at_cloud_top first_guess', &
           sint_fill_value, &
           output_data%ref_fg_scale,output_data%ref_fg_offset, &
           output_data%ref_fg_vmin,output_data%ref_fg_vmax, &
           verbose,ierr, &
           units='micrometer')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp_ap
   !----------------------------------------------------------------------------
   output_data%ctp_ap_scale=0.1
   output_data%ctp_ap_offset=0.0
   output_data%ctp_ap_vmin=500
   output_data%ctp_ap_vmax=12000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctp_ap', &
           output_data%vid_ctp_ap, &
           'cloud top pressure a priori', &
           'air_pressure_at_cloud_top a_priori', &
           sint_fill_value, &
           output_data%ctp_ap_scale,output_data%ctp_ap_offset, &
           output_data%ctp_ap_vmin,output_data%ctp_ap_vmax, &
           verbose,ierr, &
           units='hPa')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! ctp_fg
   !----------------------------------------------------------------------------
   output_data%ctp_fg_scale=0.1
   output_data%ctp_fg_offset=0.0
   output_data%ctp_fg_vmin=500
   output_data%ctp_fg_vmax=12000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'ctp_fg', &
           output_data%vid_ctp_fg, &
           'cloud top pressure first guess', &
           'air_pressure_at_cloud_top first_guess', &
           sint_fill_value, &
           output_data%ctp_fg_scale,output_data%ctp_fg_offset, &
           output_data%ctp_fg_vmin,output_data%ctp_fg_vmax, &
           verbose,ierr, &
           units='hPa')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp_ap
   !----------------------------------------------------------------------------
   output_data%stemp_ap_scale=0.01
   output_data%stemp_ap_offset=0.0
   output_data%stemp_ap_vmin=0
   output_data%stemp_ap_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'stemp_ap', &
           output_data%vid_stemp_ap, &
           'surface temperature a priori', &
           'surface_temperature a_priori', &
           sint_fill_value, &
           output_data%stemp_ap_scale,output_data%stemp_ap_offset, &
           output_data%stemp_ap_vmin,output_data%stemp_ap_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! stemp_fg
   !----------------------------------------------------------------------------
   output_data%stemp_fg_scale=0.01
   output_data%stemp_fg_offset=0.0
   output_data%stemp_fg_vmin=0
   output_data%stemp_fg_vmax=32000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'stemp_fg', &
           output_data%vid_stemp_fg, &
           'surface temperature first guess', &
           'surface_temperature first_guess', &
           sint_fill_value, &
           output_data%stemp_fg_scale,output_data%stemp_fg_offset, &
           output_data%stemp_fg_vmin,output_data%stemp_fg_vmax, &
           verbose,ierr, &
           units='kelvin')

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! albedo_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,Ctrl%Ind%Nsolar

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i))

      do j=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ysolar(j)) then
            output_data%albedo_scale(i)=0.0001
            output_data%albedo_offset(i)=0.0
            output_data%albedo_vmin(i)=0
            output_data%albedo_vmax(i)=1000

            input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='albedo in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_albedo(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%albedo_scale(i),output_data%albedo_offset(i), &
                    output_data%albedo_vmin(i),output_data%albedo_vmax(i), &
                    verbose,ierr)

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do
   end do

   !----------------------------------------------------------------------------
   ! reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i))

      do j=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ysolar(j) .and. &
             conf%channel_mixed_flag_use(i) .eq. 0) then
            output_data%channels_scale(i)=0.0001
            output_data%channels_offset(i)=0.0
            output_data%channels_vmin(i)=0
            output_data%channels_vmax(i)=1000

            input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='reflectance in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_channels(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%channels_scale(i),output_data%channels_offset(i), &
                    output_data%channels_vmin(i),output_data%channels_vmax(i), &
                    verbose,ierr)

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

      do j=1,Ctrl%Ind%Nthermal
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ythermal(j) ) then
            output_data%channels_scale(i)=.01
            output_data%channels_offset(i)=0.0
            output_data%channels_vmin(i)=0
            output_data%channels_vmax(i)=32000

            input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='brightness temperature in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_channels(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%channels_scale(i),output_data%channels_offset(i), &
                    output_data%channels_vmin(i),output_data%channels_vmax(i), &
                    verbose,ierr, &
                    units='kelvin')


            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

   end do

   !----------------------------------------------------------------------------
   ! firstguess reflectance and brightness temperature _in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i))

      do j=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ysolar(j) .and. &
	     conf%channel_mixed_flag_use(i) .eq. 0) then
            output_data%y0_scale(i)=0.0001
            output_data%y0_offset(i)=0.0
            output_data%y0_vmin(i)=0
            output_data%y0_vmax(i)=1000

            input_dummy='firstguess_reflectance_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='firstguess reflectance in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_y0(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%y0_scale(i),output_data%y0_offset(i), &
                    output_data%y0_vmin(i),output_data%y0_vmax(i), &
                    verbose,ierr)

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

      do j=1,Ctrl%Ind%Nthermal
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ythermal(j)) then
            output_data%y0_scale(i)=0.01
            output_data%y0_offset(i)=0.0
            output_data%y0_vmin(i)=0
            output_data%y0_vmax(i)=32000

            input_dummy='firstguess_brightness_temperature_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='firstguess brightness temperature in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_y0(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%y0_scale(i),output_data%y0_offset(i), &
                    output_data%y0_vmin(i),output_data%y0_vmax(i), &
                    verbose,ierr, &
                    units='kelvin')

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

   end do

   !----------------------------------------------------------------------------
   ! reflectances and brightness temperature _residual_in_channel_no_*
   !----------------------------------------------------------------------------
   do i=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i))

      do j=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ysolar(j) .and. &
             conf%channel_mixed_flag_use(i) .eq. 0) then
            output_data%residuals_scale(i)=0.0001
            output_data%residuals_offset(i)=0.0
            output_data%residuals_vmin(i)=-1000
            output_data%residuals_vmax(i)=1000

            input_dummy='reflectance_residual_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='reflectance residual in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_residuals(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%residuals_scale(i),output_data%residuals_offset(i), &
                    output_data%residuals_vmin(i),output_data%residuals_vmax(i), &
                    verbose,ierr)

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

      do j=1,Ctrl%Ind%Nthermal
         if (Ctrl%Ind%Chi(i) .eq. Ctrl%Ind%Ythermal(j)) then
            output_data%residuals_scale(i)=0.01
            output_data%residuals_offset(i)=0.0
            output_data%residuals_vmin(i)=-32000
            output_data%residuals_vmax(i)=32000

            input_dummy='brightness_temperature_residual_in_channel_no_'//trim(adjustl(input_num))
            input_dummy2='brightness temperature residual in channel no '//trim(adjustl(input_num))

            call nc_def_var_short_packed_float(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    output_data%vid_residuals(i), &
                    trim(adjustl(input_dummy2)), &
                    trim(adjustl(input_dummy)), &
                    sint_fill_value, &
                    output_data%residuals_scale(i),output_data%residuals_offset(i), &
                    output_data%residuals_vmin(i),output_data%residuals_vmax(i), &
                    verbose,ierr, &
                    units='kelvin')

            if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr
         end if
      end do

   end do

   !----------------------------------------------------------------------------
   ! degrees_of_freedom_signal
   !----------------------------------------------------------------------------
   output_data%ds_scale=0.001
   output_data%ds_offset=0.0
   output_data%ds_vmin=0
   output_data%ds_vmax=10000

   call nc_def_var_short_packed_float(ncid, dims_var, &
           'degrees_of_freedom_signal', &
           output_data%vid_ds, &
           'degrees of freedom for signal', &
           'degrees_of_freedom_for_signal', &
           sint_fill_value, &
           output_data%ds_scale,output_data%ds_offset, &
           output_data%ds_vmin,output_data%ds_vmax,verbose,ierr)

   if (ierr .ne. NF90_NOERR) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! covariance_matrix_element_*
   !----------------------------------------------------------------------------
   if (lcovar) then

   end if


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
      write(*,*) 'def_vars_secondary.inc: netcdf variable definition error:', status
      call Write_Log(Ctrl,'def_vars_secondary.inc: netcdf variable definition error:', status)
      stop
   end if

end subroutine def_vars_secondary
