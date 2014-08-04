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
!    inputfile the channel names.
! 2013/05/29, G. Thomas: Added degrees of freedom for signal
! 2013/07/24, A. Povey: Fixed bug in writing spixel_scan_out_sec%vidy0
! 2013/12/05, MJ: Fixes bug with writing of albedo and radiance and bt output.
! 2013/12/09, MJ: Slightly adapts range of reflectances.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine def_vars_secondary(Ctrl, conf, lcovar, ncid, dims_var, spixel_scan_in, &
                              spixel_scan_out_sec, status)

   use config_def
   use CTRL_def
   use SPixel_def

   implicit none

   type(CTRL_t),                           intent(in)    :: Ctrl
   type(config_struct),                    intent(in)    :: conf
   logical,                                intent(in)    :: lcovar
   integer,                                intent(in)    :: ncid
   integer,                                intent(in)    :: dims_var(2)
   type(spixel_scanline_input),            intent(inout) :: spixel_scan_in
   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out_sec
   integer,                                intent(inout) :: status

   character(len=20)  :: input_num
   character(len=500) :: input_dummy
   integer            :: ierr
   integer            :: i
   integer            :: iinput
   integer            :: wo = 0

   !----------------------------------------------------------------------------
   !SCANU
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%scanline_u_scale=1.0
   spixel_scan_out_sec%scanline_u_offset=0.0
   spixel_scan_out_sec%scanline_u_vmin=0
   spixel_scan_out_sec%scanline_u_vmax=30000

   CALL nc_defdata_long(ncid, dims_var, &
           'scanline_u', &
           spixel_scan_out_sec%vidscanline_u, &
           'scanline_u', &
           'scanline_u', &
           '', &
           spixel_scan_out_sec%lint_fill_value, &
           spixel_scan_out_sec%scanline_u_scale,spixel_scan_out_sec%scanline_u_offset, &
           spixel_scan_out_sec%scanline_u_vmin,spixel_scan_out_sec%scanline_u_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   !SCANV
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%scanline_v_scale=1.0
   spixel_scan_out_sec%scanline_v_offset=0.0
   spixel_scan_out_sec%scanline_v_vmin=0
   spixel_scan_out_sec%scanline_v_vmax=30000

   CALL nc_defdata_long(ncid, dims_var, &
           'scanline_v', &
           spixel_scan_out_sec%vidscanline_v, &
           'scanline_v', &
           'scanline_v', &
           '', &
           spixel_scan_out_sec%lint_fill_value, &
           spixel_scan_out_sec%scanline_v_scale,spixel_scan_out_sec%scanline_v_offset, &
           spixel_scan_out_sec%scanline_v_vmin,spixel_scan_out_sec%scanline_v_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   !COT A PRIORY
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%cot_ap_scale=0.01
   spixel_scan_out_sec%cot_ap_offset=0.0
   spixel_scan_out_sec%cot_ap_vmin=0
   spixel_scan_out_sec%cot_ap_vmax=25000

   CALL nc_defdata_short(ncid, dims_var, &
           'cot_ap', &
           spixel_scan_out_sec%vidcotap, &
           'cloud optical thickness a priori', &
           'atmosphere_optical_thickness_due_to_cloud a_priori', &
           '', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%cot_ap_scale,spixel_scan_out_sec%cot_ap_offset, &
           spixel_scan_out_sec%cot_ap_vmin,spixel_scan_out_sec%cot_ap_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   !COT FG
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%cot_fg_scale=0.0001
   spixel_scan_out_sec%cot_fg_offset=0.0
   spixel_scan_out_sec%cot_fg_vmin=-25000
   spixel_scan_out_sec%cot_fg_vmax=25000

   CALL nc_defdata_short(ncid, dims_var, &
           'cot_fg', &
           spixel_scan_out_sec%vidcotfg, &
           'cloud optical thickness first guess', &
           'atmosphere_optical_thickness_due_to_cloud first_guess', &
           '', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%cot_fg_scale,spixel_scan_out_sec%cot_fg_offset, &
           spixel_scan_out_sec%cot_fg_vmin,spixel_scan_out_sec%cot_fg_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! REF A PRIORY
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%ref_ap_scale=0.01
   spixel_scan_out_sec%ref_ap_offset=0.0
   spixel_scan_out_sec%ref_ap_vmin=0
   spixel_scan_out_sec%ref_ap_vmax=20000

   CALL nc_defdata_short(ncid, dims_var, &
           'ref_ap', &
           spixel_scan_out_sec%vidrefap, &
           'effective radius a priori', &
           'effective_radius_of_cloud_condensed_water_particle_at_cloud_top a_priori', &
           'micrometer', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%ref_ap_scale,spixel_scan_out_sec%ref_ap_offset, &
           spixel_scan_out_sec%ref_ap_vmin,spixel_scan_out_sec%ref_ap_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! REF FG
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%ref_fg_scale=0.01
   spixel_scan_out_sec%ref_fg_offset=0.0
   spixel_scan_out_sec%ref_fg_vmin=0
   spixel_scan_out_sec%ref_fg_vmax=20000

   CALL nc_defdata_short(ncid, dims_var, &
           'ref_fg', &
           spixel_scan_out_sec%vidreffg, &
           'effective radius first guess', &
           'effective_radius_of_cloud_condensed_water_particle_at_cloud_top first_guess', &
           'micrometer', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%ref_fg_scale,spixel_scan_out_sec%ref_fg_offset, &
           spixel_scan_out_sec%ref_fg_vmin,spixel_scan_out_sec%ref_fg_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! CTP A PRIORY
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%ctp_ap_scale=0.1
   spixel_scan_out_sec%ctp_ap_offset=0.0
   spixel_scan_out_sec%ctp_ap_vmin=500
   spixel_scan_out_sec%ctp_ap_vmax=12000

   CALL nc_defdata_short(ncid, dims_var, &
           'ctp_ap', &
           spixel_scan_out_sec%vidctpap, &
           'cloud top pressure a priori', &
           'air_pressure_at_cloud_top a_priori', &
           'hPs', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%ctp_ap_scale,spixel_scan_out_sec%ctp_ap_offset, &
           spixel_scan_out_sec%ctp_ap_vmin,spixel_scan_out_sec%ctp_ap_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! CTP FG
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%ctp_fg_scale=0.1
   spixel_scan_out_sec%ctp_fg_offset=0.0
   spixel_scan_out_sec%ctp_fg_vmin=500
   spixel_scan_out_sec%ctp_fg_vmax=12000

   CALL nc_defdata_short(ncid, dims_var, &
           'ctp_fg', &
           spixel_scan_out_sec%vidctpfg, &
           'cloud top pressure first guess', &
           'air_pressure_at_cloud_top first_guess', &
           'hPa', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%ctp_fg_scale,spixel_scan_out_sec%ctp_fg_offset, &
           spixel_scan_out_sec%ctp_fg_vmin,spixel_scan_out_sec%ctp_fg_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! RESIDUALS
   !----------------------------------------------------------------------------
   do iinput=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))

      do i=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ysolar(i) .and. conf%channel_mixed_flag_use(iinput) .eq. 0) then
            spixel_scan_out_sec%res_scale(iinput)=0.0001
            spixel_scan_out_sec%res_offset(iinput)=0.000
            spixel_scan_out_sec%res_vmin(iinput)=-30000
            spixel_scan_out_sec%res_vmax(iinput)=30000

            input_dummy='reflectance_residual_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short_no_units(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidres(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%res_scale(iinput),spixel_scan_out_sec%res_offset(iinput), &
                    spixel_scan_out_sec%res_vmin(iinput),spixel_scan_out_sec%res_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

      do i=1,Ctrl%Ind%Nthermal
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ythermal(i) ) then
            spixel_scan_out_sec%res_scale(iinput)=0.01
            spixel_scan_out_sec%res_offset(iinput)=0.0000
            spixel_scan_out_sec%res_vmin(iinput)=-30000
            spixel_scan_out_sec%res_vmax(iinput)=30000

            input_dummy='brightness_temperature_residual_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidres(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    'kelvin', &
                    spixel_scan_in%int_fill_value, &
                    spixel_scan_out_sec%res_scale(iinput),spixel_scan_out_sec%res_offset(iinput), &
                    spixel_scan_out_sec%res_vmin(iinput),spixel_scan_out_sec%res_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

   enddo

   !----------------------------------------------------------------------------
   ! REFLECTANCES AND BRIGHTNESS TEMPERATURE
   !----------------------------------------------------------------------------
   do iinput=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))

      do i=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ysolar(i) .and. conf%channel_mixed_flag_use(iinput) .eq. 0) then
            spixel_scan_out_sec%chans_scale(iinput)=0.0001
            spixel_scan_out_sec%chans_offset(iinput)=0.00
            spixel_scan_out_sec%chans_vmin(iinput)=0
            spixel_scan_out_sec%chans_vmax(iinput)=30000

            input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short_no_units(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidchans(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%chans_scale(iinput),spixel_scan_out_sec%chans_offset(iinput), &
                    spixel_scan_out_sec%chans_vmin(iinput),spixel_scan_out_sec%chans_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

      do i=1,Ctrl%Ind%Nthermal

         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ythermal(i) ) then
            spixel_scan_out_sec%chans_scale(iinput)=.1
            spixel_scan_out_sec%chans_offset(iinput)=0.00
            spixel_scan_out_sec%chans_vmin(iinput)=0
            spixel_scan_out_sec%chans_vmax(iinput)=10000

            input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short(ncid, dims_var, trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidchans(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    'kelvin', &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%chans_scale(iinput),spixel_scan_out_sec%chans_offset(iinput), &
                    spixel_scan_out_sec%chans_vmin(iinput),spixel_scan_out_sec%chans_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

   enddo

   !----------------------------------------------------------------------------
   ! ALBEDO
   !----------------------------------------------------------------------------
   do iinput=1,Ctrl%Ind%Nsolar

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))

      do i=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ysolar(i)) then
            spixel_scan_out_sec%alb_scale(iinput)=0.0001
            spixel_scan_out_sec%alb_offset(iinput)=0.00
            spixel_scan_out_sec%alb_vmin(iinput)=0
            spixel_scan_out_sec%alb_vmax(iinput)=30000

            input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short_no_units(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidalb(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%alb_scale(iinput),spixel_scan_out_sec%alb_offset(iinput), &
                    spixel_scan_out_sec%alb_vmin(iinput),spixel_scan_out_sec%alb_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo
   enddo

   !----------------------------------------------------------------------------
   ! STEMP  FG
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%stemp_fg_scale=0.1
   spixel_scan_out_sec%stemp_fg_offset=0.0
   spixel_scan_out_sec%stemp_fg_vmin=0
   spixel_scan_out_sec%stemp_fg_vmax=30000.

   CALL nc_defdata_short(ncid, dims_var, &
           'stemp_fg', &
           spixel_scan_out_sec%vidstempfg, &
           'surface temperature fg', &
           'temperature_at_surface fg', &
           'hPs', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%stemp_fg_scale,spixel_scan_out_sec%stemp_fg_offset, &
           spixel_scan_out_sec%stemp_fg_vmin,spixel_scan_out_sec%stemp_fg_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   ! first FORWARD MODELLED RADIANCES
   !----------------------------------------------------------------------------
   do iinput=1,Ctrl%Ind%Ny

      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))

      do i=1,Ctrl%Ind%Nsolar
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ysolar(i) .and. &
	     conf%channel_mixed_flag_use(iinput) .eq. 0) then
            spixel_scan_out_sec%y0_scale(iinput)=0.0001
            spixel_scan_out_sec%y0_offset(iinput)=0.00
            spixel_scan_out_sec%y0_vmin(iinput)=0
            spixel_scan_out_sec%y0_vmax(iinput)=10000

            input_dummy='firstguess_reflectance_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short_no_units(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidy0(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%y0_scale(iinput),spixel_scan_out_sec%y0_offset(iinput), &
                    spixel_scan_out_sec%y0_vmin(iinput),spixel_scan_out_sec%y0_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

      do i=1,Ctrl%Ind%Nthermal
         if (Ctrl%Ind%Chi(iinput) .eq. Ctrl%Ind%Ythermal(i)) then
            spixel_scan_out_sec%y0_scale(iinput)=.1
            spixel_scan_out_sec%y0_offset(iinput)=0.00
            spixel_scan_out_sec%y0_vmin(iinput)=0
            spixel_scan_out_sec%y0_vmax(iinput)=10000

            input_dummy='firstguess_brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

            CALL nc_defdata_short(ncid, dims_var, &
                    trim(adjustl(input_dummy)), &
                    spixel_scan_out_sec%vidy0(iinput), &
                    trim(adjustl(input_dummy)), &
                    trim(adjustl(input_dummy)), 'kelvin', &
                    spixel_scan_out_sec%int_fill_value, &
                    spixel_scan_out_sec%y0_scale(iinput),spixel_scan_out_sec%y0_offset(iinput), &
                    spixel_scan_out_sec%y0_vmin(iinput),spixel_scan_out_sec%y0_vmax(iinput),wo,ierr)

            if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
         endif
      enddo

   enddo

   !----------------------------------------------------------------------------
   ! FULL COVARIANCE MATRIX
   !----------------------------------------------------------------------------

   ! SPixel is not even defined at this point!

!  if (lcovar) then
!     do is=1,SPixel%Nx
!        do js=1,SPixel%Nx
!           write(input_num1,"(i4)") is
!           write(input_num2,"(i4)") js

!           input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))

!           CALL nc_defdata_float_no_att(ncid, dims_var, &
!                   input_dummy, &
!                   spixel_scan_out_sec%vidcovar(is,js), &
!                   input_dummy, &
!                   input_dummy, &
!                   spixel_scan_out%real_fill_value_lat_lon,wo,ierr)

!           if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr
!        enddo
!     enddo

!  endif

   !----------------------------------------------------------------------------
   ! Degrees of freedom for signal
   !----------------------------------------------------------------------------
   spixel_scan_out_sec%ds_scale=0.001
   spixel_scan_out_sec%ds_offset=0.0
   spixel_scan_out_sec%ds_vmin=0
   spixel_scan_out_sec%ds_vmax=10000

   CALL nc_defdata_short(ncid, dims_var, &
           'degrees_of_freedom_signal', &
           spixel_scan_out_sec%vidds, &
           'degrees of freedom for signal', &
           '', &
           '', &
           spixel_scan_out_sec%int_fill_value, &
           spixel_scan_out_sec%ds_scale,spixel_scan_out_sec%ds_offset, &
           spixel_scan_out_sec%ds_vmin,spixel_scan_out_sec%ds_vmax,wo,ierr)

   if (ierr .ne. 0 ) status=SecondaryFileDefinitionErr

   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (status .ne. 0 ) then
      write(*,*) 'def_vars_secondary.inc: netcdf variable definintion error:', status
      call Write_Log(Ctrl,'def_vars_secondary.inc: netcdf variable definintion error:', status)
      stop
   endif

end subroutine def_vars_secondary
