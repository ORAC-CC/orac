! Name: def_vars_secondary_pp.inc
!
!
! Purpose:
! Define secondary output variables for netcdf. Variable type, scale, offset, fill value and /or range are defined and
! applied to the variable definition. Variable names are also defined.
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2011/12/19: Matthias Jerg creates initial output for main output variables.
!2012/1/5 C. Poulsen add channel info
!2012/1/15 C. Poulsen modified scales and offsets for residuals
! $Id$
!
! Bugs:
!
!none known


!===============================
!SCANU
!===============================

      spixel_scan_out_sec%scanline_u_scale=1.0
      spixel_scan_out_sec%scanline_u_offset=0.0
      spixel_scan_out_sec%scanline_u_vmin=0!ixstart-1!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%scanline_u_vmax=3000!ixstop+1!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_long(ncid_secondary, dims_var, 'scanline_u', spixel_scan_out_sec%vidscanline_u,&
           'scanline_u', &
           & 'scanline_u','', spixel_scan_out_sec%lint_fill_value, &
           & spixel_scan_out_sec%scanline_u_scale,spixel_scan_out_sec%scanline_u_offset, &
           & spixel_scan_out_sec%scanline_u_vmin,spixel_scan_out_sec%scanline_u_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr


!===============================
!SCANV
!===============================
  write(*,*) 'setting up scanv'
      spixel_scan_out_sec%scanline_v_scale=1.0
      spixel_scan_out_sec%scanline_v_offset=0.0
      spixel_scan_out_sec%scanline_v_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%scanline_v_vmax=30000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_long(ncid_secondary, dims_var, 'scanline_v', spixel_scan_out_sec%vidscanline_v,&
           'scanline_v', &
           & 'scanline_v','', spixel_scan_out_sec%lint_fill_value, &
           & spixel_scan_out_sec%scanline_v_scale,spixel_scan_out_sec%scanline_v_offset, &
           & spixel_scan_out_sec%scanline_v_vmin,spixel_scan_out_sec%scanline_v_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr



!===============================
!COT A PRIORY
!===============================
  write(*,*) 'setting up a priory cot'
      spixel_scan_out_sec%cot_ap_scale=0.01
      spixel_scan_out_sec%cot_ap_offset=0.0
      spixel_scan_out_sec%cot_ap_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%cot_ap_vmax=25000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'cot_ap', spixel_scan_out_sec%vidcotap, &
           & 'cloud optical thickness a priori', &
           & 'atmosphere_optical_thickness_due_to_cloud a_priori','', spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%cot_ap_scale,spixel_scan_out_sec%cot_ap_offset, &
           & spixel_scan_out_sec%cot_ap_vmin,spixel_scan_out_sec%cot_ap_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!===============================
!COT FG
!===============================
  write(*,*) 'setting up first guess cot'
      spixel_scan_out_sec%cot_fg_scale=0.0001
      spixel_scan_out_sec%cot_fg_offset=0.0
      spixel_scan_out_sec%cot_fg_vmin=-25000!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%cot_fg_vmax=25000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'cot_fg', spixel_scan_out_sec%vidcotfg, &
           & 'cloud optical thickness first guess', &
           & 'atmosphere_optical_thickness_due_to_cloud first_guess','', spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%cot_fg_scale,spixel_scan_out_sec%cot_fg_offset, &
           & spixel_scan_out_sec%cot_fg_vmin,spixel_scan_out_sec%cot_fg_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!===============================
!REF A PRIORY
!===============================
  write(*,*) 'setting up ap ref'
      spixel_scan_out_sec%ref_ap_scale=0.01
      spixel_scan_out_sec%ref_ap_offset=0.0
      spixel_scan_out_sec%ref_ap_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%ref_ap_vmax=20000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'ref_ap', spixel_scan_out_sec%vidrefap, &
           & 'effective radius a priori', &
           & 'effective_radius_of_cloud_condensed_water_particle_at_cloud_top a_priori','micrometer',&
           & spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%ref_ap_scale,spixel_scan_out_sec%ref_ap_offset, &
           & spixel_scan_out_sec%ref_ap_vmin,spixel_scan_out_sec%ref_ap_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!===============================
!REF FG
!===============================
  write(*,*) 'setting up fg ref'
      spixel_scan_out_sec%ref_fg_scale=0.01
      spixel_scan_out_sec%ref_fg_offset=0.0
      spixel_scan_out_sec%ref_fg_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%ref_fg_vmax=20000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'ref_fg', spixel_scan_out_sec%vidreffg, &
           & 'effective radius first guess', &
           & 'effective_radius_of_cloud_condensed_water_particle_at_cloud_top first_guess','micrometer', &
           & spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%ref_fg_scale,spixel_scan_out_sec%ref_fg_offset, &
           & spixel_scan_out_sec%ref_fg_vmin,spixel_scan_out_sec%ref_fg_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!===============================
!CTP A PRIORY
!===============================
  write(*,*) 'setting up ap ctp'
      spixel_scan_out_sec%ctp_ap_scale=0.1
      spixel_scan_out_sec%ctp_ap_offset=0.0
      spixel_scan_out_sec%ctp_ap_vmin=500!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%ctp_ap_vmax=12000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'ctp_ap', spixel_scan_out_sec%vidctpap, &
           & 'cloud top pressure a priori', &
           & 'air_pressure_at_cloud_top a_priori','hPs',&
           & spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%ctp_ap_scale,spixel_scan_out_sec%ctp_ap_offset, &
           & spixel_scan_out_sec%ctp_ap_vmin,spixel_scan_out_sec%ctp_ap_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!===============================
!CTP FG
!===============================
  write(*,*) 'setting up fg ctp'
      spixel_scan_out_sec%ctp_fg_scale=0.1
      spixel_scan_out_sec%ctp_fg_offset=0.0
      spixel_scan_out_sec%ctp_fg_vmin=500!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%ctp_fg_vmax=12000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'ctp_fg', spixel_scan_out_sec%vidctpfg, &
           & 'cloud top pressure first guess', &
           & 'air_pressure_at_cloud_top first_guess','hPa', &
           & spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%ctp_fg_scale,spixel_scan_out_sec%ctp_fg_offset, &
           & spixel_scan_out_sec%ctp_fg_vmin,spixel_scan_out_sec%ctp_fg_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr


!===============================
!RESIDUALS
!===============================
  write(*,*) 'setting up residual'

!      do iinput=1,Ny


         

         do i=1,Nsolar
            iinput=i
!            write(input_num,"(i4)") i

        input_num= Chan_Id(i)
!            if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_out_sec%res_scale(iinput)=0.00001 !0.00001
               spixel_scan_out_sec%res_offset(iinput)=0.001 !0.0001
               spixel_scan_out_sec%res_vmin(iinput)=-30000
               spixel_scan_out_sec%res_vmax(iinput)=30000
  
               input_dummy='reflectance_residual_in_channel_no_'//trim(adjustl(input_num))


               CALL nc_defdata_short_no_units(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_sec%vidres(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), spixel_scan_out_sec%int_fill_value,&
                    & spixel_scan_out_sec%res_scale(iinput),spixel_scan_out_sec%res_offset(iinput),&
                    & spixel_scan_out_sec%res_vmin(iinput),spixel_scan_out_sec%res_vmax(iinput), wo,ierr)


                if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
!            endif
         enddo


         do i=1,Nthermal

            input_num=chan_id(3+i)
!            write(input_num,"(i4)") i+3
            iinput=3+i

!            if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_out_sec%res_scale(iinput)=0.0001!0.001
               spixel_scan_out_sec%res_offset(iinput)=0.00001
               spixel_scan_out_sec%res_vmin(iinput)=-30000
               spixel_scan_out_sec%res_vmax(iinput)=30000

               input_dummy='brightness_temperature_residual_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_sec%vidres(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_out_sec%res_scale(iinput),spixel_scan_out_sec%res_offset(iinput),&
                    & spixel_scan_out_sec%res_vmin(iinput),spixel_scan_out_sec%res_vmax(iinput), wo,ierr)
            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!            endif
         enddo

!      enddo


!===============================
!REFLECTANCES AND bRIGHTNESS TEMPERATURE
!===============================
write(*,*) 'setting up reflectances and bt'

!      do iinput=1,Ny


 !        write(input_num,"(i4)") Ctrl%Ind%Y_Id(iinput)

         do i=1,Nsolar
            iinput=i
            input_num=chan_id(i)            
!write(input_num,"(i4)") i
!           if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_out_sec%chans_scale(iinput)=0.0001
               spixel_scan_out_sec%chans_offset(iinput)=0.00
               spixel_scan_out_sec%chans_vmin(iinput)=0
               spixel_scan_out_sec%chans_vmax(iinput)=10000

               input_num=chan_id(i)     
               input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short_no_units(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_sec%vidchans(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), spixel_scan_out_sec%int_fill_value,&
                    & spixel_scan_out_sec%chans_scale(iinput),spixel_scan_out_sec%chans_offset(iinput),&
                    & spixel_scan_out_sec%chans_vmin(iinput),spixel_scan_out_sec%chans_vmax(iinput), wo,ierr)

            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
 !           endif
         enddo


         do i=1,Nthermal
            
            iinput=3+i
            input_num=chan_id(3+i)
!write(input_num,"(i4)") i+3
 !           if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_out_sec%chans_scale(iinput)=.1
               spixel_scan_out_sec%chans_offset(iinput)=0.00
               spixel_scan_out_sec%chans_vmin(iinput)=0
               spixel_scan_out_sec%chans_vmax(iinput)=10000

               input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_sec%vidchans(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_out_sec%chans_scale(iinput),spixel_scan_out_sec%chans_offset(iinput),&
                    & spixel_scan_out_sec%chans_vmin(iinput),spixel_scan_out_sec%chans_vmax(iinput), wo,ierr)
            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

 !           endif
         enddo


 !     enddo


!===============================
!STEMP  FG
!===============================
  write(*,*) 'setting up fg stemp'
      spixel_scan_out_sec%stemp_fg_scale=0.1
      spixel_scan_out_sec%stemp_fg_offset=0.0
      spixel_scan_out_sec%stemp_fg_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out_sec%stemp_fg_vmax=30000.!*spixel_scan_out%lon_scale
      !      write(*,*) ncid_primary
      CALL nc_defdata_short(ncid_secondary, dims_var, 'stemp_fg', spixel_scan_out_sec%vidstempfg, &
           & 'surface temperature fg', &
           & 'temperature_at_surface fg','hPs',&
           & spixel_scan_out_sec%int_fill_value, &
           & spixel_scan_out_sec%stemp_fg_scale,spixel_scan_out_sec%stemp_fg_offset, &
           & spixel_scan_out_sec%stemp_fg_vmin,spixel_scan_out_sec%stemp_fg_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

	  !write(*,*) 'here after stemp fg'

!===============================
!first FORWARD MODELLED RADIANCES
!===============================


!       do iinput=1,Ny


!          write(input_num,"(i4)") Ctrl%Ind%Y_Id(iinput)

!  write(*,*) 'soalr'

          do i=1,Nsolar
	    iinput=i

	    !write(*,*) iinput

!write(input_num,"(i4)") i
            input_num=chan_id(i)
!             if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
                spixel_scan_out_sec%y0_scale(iinput)=0.0001
                spixel_scan_out_sec%y0_offset(iinput)=0.00
                spixel_scan_out_sec%y0_vmin(iinput)=0
                spixel_scan_out_sec%y0_vmax(iinput)=10000
                input_dummy='firstguess_reflectance_in_channel_no_'//trim(adjustl(input_num))
                CALL nc_defdata_short_no_units(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                     & spixel_scan_out_sec%vidy0(iinput), trim(adjustl(input_dummy)), &
                     & trim(adjustl(input_dummy)), spixel_scan_out_sec%int_fill_value,&
                     & spixel_scan_out_sec%y0_scale(iinput),spixel_scan_out_sec%y0_offset(iinput),&
                     & spixel_scan_out_sec%y0_vmin(iinput),spixel_scan_out_sec%y0_vmax(iinput), wo,ierr)
             if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
!             endif
          enddo

	       !write(*,*) 'thermal'

          do i=1,Nthermal
            
!write(input_num,"(i4)") i+3
            input_num=chan_id(3+i)
             iinput=3+i
	    !write(*,*) iinput

!             if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
                spixel_scan_out_sec%y0_scale(iinput)=.1
                spixel_scan_out_sec%y0_offset(iinput)=0.00
                spixel_scan_out_sec%y0_vmin(iinput)=0
                spixel_scan_out_sec%y0_vmax(iinput)=10000

                input_dummy='firstguess_brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

                CALL nc_defdata_short(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                     & spixel_scan_out_sec%vidy0(iinput), trim(adjustl(input_dummy)), &
                     & trim(adjustl(input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                     & spixel_scan_out_sec%y0_scale(iinput),spixel_scan_out_sec%y0_offset(iinput),&
                     & spixel_scan_out_sec%y0_vmin(iinput),spixel_scan_out_sec%y0_vmax(iinput), wo,ierr)
             if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!             endif
	       enddo


!       enddo

!	       write(*,*) 'after that stuff'


!===============================
!FULL COVARIANCE MATRIX
!===============================

!      if(lcovar) then
!	write(*,*) 'setting up covariance'
!         do is=1,Nx
!            do js=1,Nx
!               write(input_num1,"(i4)") is
!               write(input_num2,"(i4)") js
!               input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
!               CALL nc_defdata_float_no_att(ncid_secondary, dims_var,input_dummy, spixel_scan_out_sec%vidcovar(is,js),input_dummy, &
!                    & input_dummy, spixel_scan_out%real_fill_value_lat_lon, wo,ierr)



!         if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
 !           enddo
!         enddo
!
!      endif


	   if(status .ne. 0 ) then

	    write(*,*) 'def_vars_secondary.inc: netcdf variable definintion error:', status

	    stop

	    endif
