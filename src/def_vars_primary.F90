! Name: def_vars_primary.inc
!
!
! Purpose:
  ! Define primary output variables for netcdf. Variable type, scale, offset, fill value and /or range are defined and
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
!2012/01/06: Caroline Poulsen added cwp
!2012/01/15: Caroline Poulsen changed scale and range for cot
!2012/06/15: Caroline Poulsen changed cost scaling factor
!2012/09/20: Caroline Poulsen changed scaling factors
!2012/11/03: MJ changed range specifications of ctp and stemp
!2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
  !2013 MJ: various changes to avoid cut-offs other fixes.
!2013/10/06 CPbug fix  created relative azimuth angle in primary file
!2014/04/01: MJ cleanup and fix for illum
!2014/06/13, GM: Put the code into a subroutine.
!
! $Id$
!
! Bugs:
!
!none known

subroutine def_vars_primary(Ctrl, ncid, dims_var, spixel_scan_out, status)

   use CTRL_def
   use SPixel_def

   implicit none

   type(CTRL_t),                         intent(in)    :: Ctrl
   integer,                              intent(in)    :: ncid
   integer,                              intent(in)    :: dims_var(2)
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out
   integer,                              intent(inout) :: status

   character(len=20)  :: input_num
   character(len=500) :: input_dummy
   character(len=500) :: s_input_dummy
   integer            :: ierr
   integer            :: iviews
   integer            :: wo = 0

!===============================
!TIME
!===============================


  !write(*,*) 'setting up time'
      spixel_scan_out%time_scale=1.0_dreal
      spixel_scan_out%time_offset=0.00_dreal
      spixel_scan_out%time_vmin=0.0_dreal
      spixel_scan_out%time_vmax=1.0D10
      !write(*,*) 'ncid'
      spixel_scan_out%time_fv=-32767.0
      !      pause
      CALL nc_defdata_double(ncid, dims_var, 'time', spixel_scan_out%vidtime, 'time', &
           & 'time', 'Julian Date, days elapsed since 12:00 January 1, 4713 BC',spixel_scan_out%time_fv,&
           spixel_scan_out%time_scale,spixel_scan_out%time_offset,&
           spixel_scan_out%time_vmin,spixel_scan_out%time_vmax, wo,ierr)
!           ierr=1
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!LON
!===============================

      !write(*,*) 'setting up lon'
      spixel_scan_out%lon_scale=1.0
      spixel_scan_out%lon_offset=0.00
      spixel_scan_out%lon_vmin=-180.0!*spixel_scan_out%lon_scale
      spixel_scan_out%lon_vmax=180.0!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_float(ncid, dims_var, 'lon', spixel_scan_out%vidlon, 'longitude', &
           & 'longitude', 'degrees_east', spixel_scan_out%real_fill_value_lat_lon, &
           & spixel_scan_out%lon_scale,spixel_scan_out%lon_offset, &
                            & spixel_scan_out%lon_vmin,spixel_scan_out%lon_vmax, wo,ierr)
       if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!LAT
!===============================

      write(*,*) 'setting up lat'
      spixel_scan_out%lat_scale=1.0
      spixel_scan_out%lat_offset=0.00
      spixel_scan_out%lat_vmin=-90.0!*spixel_scan_out%lat_scale
      spixel_scan_out%lat_vmax=90.0!*spixel_scan_out%lat_scale
      !      write(*,*) ncid
      CALL nc_defdata_float(ncid, dims_var, 'lat', spixel_scan_out%vidlat, 'latitude', &
           & 'latitude', 'degrees_north', spixel_scan_out%real_fill_value_lat_lon,&
           spixel_scan_out%lat_scale,spixel_scan_out%lat_offset,&
           spixel_scan_out%lat_vmin,spixel_scan_out%lat_vmax, wo,ierr)    
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

      write(*,*) 'setting up satzen'
      do iviews=1,Ctrl%Ind%NViews

         write(input_num,"(i4)") iviews

         !===============================
         !SATZEN
         !===============================

         input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
         s_input_dummy=&
              & 'platform_zenith_angle for_view_no_'//trim(adjustl(input_num))

         !write(*,*) 'setting up satzen'

         spixel_scan_out%sat_scale=1.0
         spixel_scan_out%sat_offset=0.00
         spixel_scan_out%sat_vmin=-180.0!*spixel_scan_out%lon_scale
         spixel_scan_out%sat_vmax=180.0!*spixel_scan_out%lon_scale
         !      write(*,*) ncid
         CALL nc_defdata_float(ncid, dims_var, trim(adjustl(input_dummy)),&
              & spixel_scan_out%vidsat_zen(iviews), trim(adjustl(input_dummy)), &
              & trim(adjustl(s_input_dummy)), 'degrees', real_fill_value, &
              & spixel_scan_out%sat_scale,spixel_scan_out%sat_offset, &
              & spixel_scan_out%sat_vmin,spixel_scan_out%sat_vmax, wo,ierr)

!MJ              & trim(adjustl(input_dummy)), 'degree', real_fill_value, &
	!write(*,*)'PrimaryFileDefinitionErr',PrimaryFileDefinitionErr
          if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
         !===============================
         !SOLZEN
         !===============================

         write(*,*) 'setting up solzen'

         input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
         s_input_dummy='solar_zenith_angle for_view_no_'//trim(adjustl(input_num))

         spixel_scan_out%sol_scale=1.0
         spixel_scan_out%sol_offset=0.00
         spixel_scan_out%sol_vmin=-180.0!*spixel_scan_out%lon_scale
         spixel_scan_out%sol_vmax=180.0!*spixel_scan_out%lon_scale
         !      write(*,*) ncid
!        write(*,*) 'vor sol',iviews
         CALL nc_defdata_float(ncid, dims_var, trim(adjustl(input_dummy)),&
              & spixel_scan_out%vidsol_zen(iviews), trim(adjustl(input_dummy)), &
              & trim(adjustl(s_input_dummy)),  'degrees', real_fill_value, &
              & spixel_scan_out%sol_scale,spixel_scan_out%sol_offset, &
              & spixel_scan_out%sol_vmin,spixel_scan_out%sol_vmax, wo,ierr)
 !        write(*,*) 'nach sol',iviews,PrimaryFileDefinitionErr         


         if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
         !===============================
         !AZI
         !===============================

         write(*,*) 'setting up azi'

         input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
         s_input_dummy='angle_of_rotation_from_solar_azimuth_to_platform_azimuth for_view_no_'//trim(adjustl(input_num))

         spixel_scan_out%azi_scale=1.0
         spixel_scan_out%azi_offset=0.00
         spixel_scan_out%azi_vmin=-180.0!*spixel_scan_out%lon_scale
         spixel_scan_out%azi_vmax=180.0!*spixel_scan_out%lon_scale
         !      write(*,*) ncid

         CALL nc_defdata_float(ncid, dims_var, trim(adjustl(input_dummy)),&
              & spixel_scan_out%vidrel_azi(iviews), trim(adjustl(input_dummy)), &
              & trim(adjustl(s_input_dummy)),  'degrees', real_fill_value, &
              & spixel_scan_out%azi_scale,spixel_scan_out%azi_offset, &
              & spixel_scan_out%azi_vmin,spixel_scan_out%azi_vmax, wo,ierr)
!         write(*,*) 'nach azi',iviews,PrimaryFileDefinitionErr         
!         write(*,*) 'vor azi',iviews

!        write(*,*) 'nach azi',iviews         
         if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
      enddo

!===============================
!COT
!===============================

      !write(*,*) 'setting up cot'

      spixel_scan_out%cot_scale=0.01
      spixel_scan_out%cot_offset=0.0
      spixel_scan_out%cot_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cot_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short_no_units(ncid, dims_var, 'cot', spixel_scan_out%vidcot, 'cloud optical thickness', &
           & 'atmosphere_optical_thickness_due_to_cloud', spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cot_scale,spixel_scan_out%cot_offset, &
           & spixel_scan_out%cot_vmin,spixel_scan_out%cot_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!CPH has yet to be included!!!
!===============================





!===============================
!REFF
!===============================

      !write(*,*) 'setting up reff'

      spixel_scan_out%ref_scale=0.01
      spixel_scan_out%ref_offset=0.00
      spixel_scan_out%ref_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ref_vmax=20000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ref', spixel_scan_out%vidref, 'effective radius', &
           & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top','micrometer', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ref_scale,spixel_scan_out%ref_offset, &
           & spixel_scan_out%ref_vmin,spixel_scan_out%ref_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!CTP
!===============================

      !write(*,*) 'setting up ctp'

      spixel_scan_out%ctp_scale=0.1
      spixel_scan_out%ctp_offset=0.00
      spixel_scan_out%ctp_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ctp_vmax=12000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ctp', spixel_scan_out%vidctp, 'cloud top pressure', &
           & 'air_pressure_at_cloud_top','hPa', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ctp_scale,spixel_scan_out%ctp_offset, &
           & spixel_scan_out%ctp_vmin,spixel_scan_out%ctp_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!MASK
!===============================

      !write(*,*) 'setting up mask'

      spixel_scan_out%cct_scale=0.01
      spixel_scan_out%cct_offset=0.00
      spixel_scan_out%cct_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cct_vmax=100!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cc_total', spixel_scan_out%vidcct, 'cloud fraction', &
           & 'cloud_area_fraction', '',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cct_scale,spixel_scan_out%cct_offset, &
           & spixel_scan_out%cct_vmin,spixel_scan_out%cct_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!STEMP
!===============================

      !write(*,*) 'setting up stemp'

      spixel_scan_out%stemp_scale=0.01
      spixel_scan_out%stemp_offset=0.00
      spixel_scan_out%stemp_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%stemp_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'stemp', spixel_scan_out%vidstemp, 'surface temperature', &
           & 'surface_temperature', 'kelvin',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%stemp_scale,spixel_scan_out%stemp_offset, &
           & spixel_scan_out%stemp_vmin,spixel_scan_out%stemp_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!ALBEDO has yet to be fully included!!!
!===============================
!!$      spixel_scan_out%albedo_scale=0.1
!!$      spixel_scan_out%albedo_offset=0.00
!!$      spixel_scan_out%albedo_vmin=0!*spixel_scan_out%lon_scale
!!$      spixel_scan_out%albedo_vmax=10000!*spixel_scan_out%lon_scale
!!$      !      write(*,*) ncid
!!$
!!$      do iinput=1,Ctrl%Ind%Ny
!!$
!!$         write(input_num,"(i4)") iinput
!!$
!!$         input_dummy='surface_albedo_in_channel_no_'//trim(adjustl(input_num))
!!$
!!$         CALL nc_defdata_short(ncid_input, dims_var, trim(adjustl(input_dummy)),&
!!$              & spixel_scan_in%vidinput(iinput), trim(adjustl(input_dummy)), &
!!$              & trim(adjustl(input_dummy)), '', spixel_scan_in%int_fill_value,&
!!$              & spixel_scan_in%input_scale(iinput),spixel_scan_in%input_offset(iinput),&
!!$              & spixel_scan_in%input_vmin(iinput),spixel_scan_in%input_vmax(iinput), wo,ierr)
!!$
!!$         
!!$      enddo


!===============================
!CTH
!===============================

!      write(*,*) 'setting up cth'

      spixel_scan_out%cth_scale=0.01
      spixel_scan_out%cth_offset=0.00
      spixel_scan_out%cth_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cth_vmax=2000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cth', spixel_scan_out%vidcth, 'cloud top height', &
           & 'altitude_at_cloud_top', 'kilometer',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cth_scale,spixel_scan_out%cth_offset, &
           & spixel_scan_out%cth_vmin,spixel_scan_out%cth_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!DELCTH
!===============================

!      write(*,*) 'setting up cth'

      spixel_scan_out%cth_error_scale=0.01
      spixel_scan_out%cth_error_offset=0.00
      spixel_scan_out%cth_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cth_error_vmax=2000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cth_uncertainty', spixel_scan_out%vidctherror, 'cloud top height uncertainty', &
           & 'altitude_at_cloud_top uncertainty', 'kilometer',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cth_error_scale,spixel_scan_out%cth_error_offset, &
           & spixel_scan_out%cth_error_vmin,spixel_scan_out%cth_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!CTT
!===============================

      !write(*,*) 'setting up ctt'
      spixel_scan_out%ctt_scale=0.01
      spixel_scan_out%ctt_offset=0.00
      spixel_scan_out%ctt_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ctt_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ctt', spixel_scan_out%vidctt, 'cloud top temperature', &
           & 'air_temperature_at_cloud_top', 'kelvin',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ctt_scale,spixel_scan_out%ctt_offset, &
           & spixel_scan_out%ctt_vmin,spixel_scan_out%ctt_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!DELCTT
!===============================

      !write(*,*) 'setting up ctt'
      spixel_scan_out%ctt_error_scale=0.01
      spixel_scan_out%ctt_error_offset=0.00
      spixel_scan_out%ctt_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ctt_error_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ctt_uncertainty', spixel_scan_out%vidctterror, 'cloud top temperature uncertainty', &
           & 'air_temperature_at_cloud_top uncertainty', 'kelvin',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ctt_error_scale,spixel_scan_out%ctt_error_offset, &
           & spixel_scan_out%ctt_error_vmin,spixel_scan_out%ctt_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr



!===============================
!CWP
!===============================

      !write(*,*) 'setting up cwp'

      !MJ ORG     spixel_scan_out%cwp_scale=0.1
      spixel_scan_out%cwp_scale=1.0
      spixel_scan_out%cwp_offset=0.00
      spixel_scan_out%cwp_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cwp_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cwp', spixel_scan_out%vidcwp, 'cloud liquid water path', &
           & 'atmosphere_mass_content_of_cloud_liquid_water','g/m2', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cwp_scale,spixel_scan_out%cwp_offset, &
           & spixel_scan_out%cwp_vmin,spixel_scan_out%cwp_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr






!===============================
!DELCOT
!===============================
      !write(*,*) 'setting up delcot'
      spixel_scan_out%cot_error_scale=0.01
      spixel_scan_out%cot_error_offset=0.00
      spixel_scan_out%cot_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cot_error_vmax=25000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cot_uncertainty', spixel_scan_out%vidcoterror,&
           & 'cloud optical thickness uncertainty', &
           & 'atmosphere_optical_thickness_due_to_cloud uncertainty','', spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cot_error_scale,spixel_scan_out%cot_error_offset, &
           & spixel_scan_out%cot_error_vmin,spixel_scan_out%cot_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!DELREF
!===============================
      !write(*,*) 'setting up delref'
      spixel_scan_out%ref_error_scale=0.01
      spixel_scan_out%ref_error_offset=0.00
      spixel_scan_out%ref_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ref_error_vmax=20000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ref_uncertainty', spixel_scan_out%vidreferror,&
           & 'effective radius uncertainty', &
           & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty','micrometer', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ref_error_scale,spixel_scan_out%ref_error_offset, &
           & spixel_scan_out%ref_error_vmin,spixel_scan_out%ref_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!DELCTP
!===============================
      !write(*,*) 'setting up delctp'
      spixel_scan_out%ctp_error_scale=0.1
      spixel_scan_out%ctp_error_offset=0.00
      spixel_scan_out%ctp_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ctp_error_vmax=12000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'ctp_uncertainty', spixel_scan_out%vidctperror,&
           & 'cloud top pressure uncertainty', &
           & 'air_pressure_at_cloud_top uncertainty','hPa', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%ctp_error_scale,spixel_scan_out%ctp_error_offset, &
           & spixel_scan_out%ctp_error_vmin,spixel_scan_out%ctp_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!DELMASK
!===============================
      !write(*,*) 'setting up delmask'
      spixel_scan_out%cct_error_scale=0.01
      spixel_scan_out%cct_error_offset=0.00
      spixel_scan_out%cct_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cct_error_vmax=100!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cc_total_uncertainty', spixel_scan_out%vidccterror,&
           & 'cloud fraction uncertainty', &
           & 'cloud_area_fraction uncertainty', '',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cct_error_scale,spixel_scan_out%cct_error_offset, &
           & spixel_scan_out%cct_error_vmin,spixel_scan_out%cct_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!DELSTEMP
!===============================
      !write(*,*) 'setting up delstemp'
      spixel_scan_out%stemp_error_scale=0.01
      spixel_scan_out%stemp_error_offset=0.00
      spixel_scan_out%stemp_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%stemp_error_vmax=30000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'stemp_uncertainty', spixel_scan_out%vidstemperror,&
           & 'surface temperature uncertainty', &
           & 'surface_temperature uncertainty','Kelvin',&
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%stemp_error_scale,spixel_scan_out%stemp_error_offset, &
           & spixel_scan_out%stemp_vmin,spixel_scan_out%stemp_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!DELCWP
!===============================
      !write(*,*) 'setting up delcwp'
      spixel_scan_out%cwp_error_scale=1.0
      spixel_scan_out%cwp_error_offset=0.00
      spixel_scan_out%cwp_error_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%cwp_error_vmax=32000!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_short(ncid, dims_var, 'cwp_uncertainty', spixel_scan_out%vidcwperror,&
           & 'CWP uncertainty', &
           & 'atmosphere_mass_content_of_cloud_liquid_water uncertainty','g/m2', &
           & spixel_scan_out%int_fill_value, &
           & spixel_scan_out%cwp_error_scale,spixel_scan_out%cwp_error_offset, &
           & spixel_scan_out%cwp_error_vmin,spixel_scan_out%cwp_error_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!CONVERGENCE
!===============================
      !write(*,*) 'setting up convergence'
      spixel_scan_out%con_scale=1
      spixel_scan_out%con_offset=0
      spixel_scan_out%con_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%con_vmax=1!*spixel_scan_out%lon_scale
      !      !write(*,*) ncid
!           & 'retrieval convergence', 'yes(0)/no(1)',&
      CALL nc_defdata_byte_flag_value(ncid, dims_var, 'convergence', spixel_scan_out%vidconvergence,&
           & 'retrieval convergence', &
           & 'retrieval_convergence_flag', '0b, 1b','yes no',&
           & spixel_scan_out%byte_fill_value, &
           & spixel_scan_out%con_scale,spixel_scan_out%con_offset, &
           & spixel_scan_out%con_vmin,spixel_scan_out%con_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!NITER
!===============================
      !write(*,*) 'setting up iterations'
      spixel_scan_out%niter_scale=1
      spixel_scan_out%niter_offset=0
      spixel_scan_out%niter_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%niter_vmax=100!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_byte(ncid, dims_var, 'niter', spixel_scan_out%vidniter, 'retrieval iterations', &
           & 'retrieval_iterations', '',&
           & spixel_scan_out%byte_fill_value, &
           & spixel_scan_out%niter_scale,spixel_scan_out%niter_offset, &
           & spixel_scan_out%niter_vmin,spixel_scan_out%niter_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!cloud phase
!===============================
      spixel_scan_out%pchange_scale=1
      spixel_scan_out%pchange_offset=0
      spixel_scan_out%pchange_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%pchange_vmax=1!*spixel_scan_out%lon_scale
      CALL nc_defdata_byte_flag_value(ncid, dims_var, 'phase', spixel_scan_out%vidpchange, 'cloud phase flag', &
           & 'cloud_phase_flag', '0b, 1b, 2b', 'clear/unknown liquid ice',&
           & spixel_scan_out%byte_fill_value, &
           & spixel_scan_out%pchange_scale,spixel_scan_out%pchange_offset, &
           & spixel_scan_out%pchange_vmin,spixel_scan_out%pchange_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!PCHANGE
!===============================
!MJOLD      spixel_scan_out%pchange_scale=1
!MJOLD      spixel_scan_out%pchange_offset=0
!MJOLD      spixel_scan_out%pchange_vmin=0!*spixel_scan_out%lon_scale
!MJOLD      spixel_scan_out%pchange_vmax=100!*spixel_scan_out%lon_scale
!MJOLD      !      write(*,*) ncid
!MJOLD      CALL nc_defdata_byte(ncid, dims_var, 'pchange', spixel_scan_out%vidpchange, 'no of phase changes', &
!MJOLD           & 'number_of_phase_changes', '',&
!MJOLD           & spixel_scan_out%byte_fill_value, &
!MJOLD           & spixel_scan_out%pchange_scale,spixel_scan_out%pchange_offset, &
!MJOLD           & spixel_scan_out%pchange_vmin,spixel_scan_out%pchange_vmax, wo,ierr)
!MJOLD     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


!===============================
!COST JA
!===============================
      !write(*,*) 'setting up cost ja'
      spixel_scan_out%costja_scale=1.0
      spixel_scan_out%costja_offset=0.00
      spixel_scan_out%costja_vmin=0.0!*spixel_scan_out%lon_scale
      spixel_scan_out%costja_vmax=100000.!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_float(ncid, dims_var, 'costja', spixel_scan_out%vidcostja, 'costja', &
           & 'a_priori_cost at_solution', '',&
           & real_fill_value, &
           & spixel_scan_out%costja_scale,spixel_scan_out%costja_offset, &
           & spixel_scan_out%costja_vmin,spixel_scan_out%costja_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!COST JM
!===============================
      !write(*,*) 'setting up cost jm'
      spixel_scan_out%costjm_scale=1.0
      spixel_scan_out%costjm_offset=0.00
      spixel_scan_out%costjm_vmin=0.0!*spixel_scan_out%lon_scale
      spixel_scan_out%costjm_vmax=100000.!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
      CALL nc_defdata_float(ncid, dims_var, 'costjm', spixel_scan_out%vidcostjm, 'costjm', &
           & 'measurement_cost at_solution', '',&
           & real_fill_value, &
           & spixel_scan_out%costjm_scale,spixel_scan_out%costjm_offset, &
           & spixel_scan_out%costjm_vmin,spixel_scan_out%costjm_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!LSFLAG
!===============================
      !write(*,*) 'setting up lsflag'
      spixel_scan_out%ls_scale=1
      spixel_scan_out%ls_offset=0
      spixel_scan_out%ls_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%ls_vmax=1!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
!           & 'land_binary_mask', 'Land(1)/Sea(0)',&
      CALL nc_defdata_byte_flag_value(ncid, dims_var, 'lsflag', spixel_scan_out%vidlsflag, 'land/sea flag', &
           & 'land_binary_mask', '0b, 1b', 'sea land',&
           & spixel_scan_out%byte_fill_value, &
           & spixel_scan_out%ls_scale,spixel_scan_out%ls_offset, &
           & spixel_scan_out%ls_vmin,spixel_scan_out%ls_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

!===============================
!QCFLAG
!===============================
      !write(*,*) 'setting up qcflag'
      spixel_scan_out%qc_scale=1
      spixel_scan_out%qc_offset=0
      spixel_scan_out%qc_vmin=0!*spixel_scan_out%lon_scale
      spixel_scan_out%qc_vmax=254!*spixel_scan_out%lon_scale
!     write(*,*) 'ncid',ncid
!!$      CALL nc_defdata_float(ncid, dims_var, 'qcflag', spixel_scan_out%vidqcflag, 'quality control flag', &
!!$           & 'quality_control_flag', '',&
!!$           & real_fill_value, &
!!$           & spixel_scan_out%qc_scale,spixel_scan_out%qc_offset, &
!!$           & spixel_scan_out%qc_vmin,spixel_scan_out%qc_vmax, wo,ierr)
      s_input_dummy='Bit 0 unused, always set to 0, Bits 1-5 set to 1 if state variable error out of bounds'
      s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 6 set to 1 if no convergence achieved'
      s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 7 set to 1 if cost too large.'
      s_input_dummy=trim(adjustl(s_input_dummy))//' Bit 1=COT Bit 2=REF Bit 3=CTP Bit 4=CCT Bit 5=STEMP'
      call nc_defdata_short_flag_value(ncid, dims_var, 'qcflag', spixel_scan_out%vidqcflag, 'quality control flag', &
           & 'quality_control_flag',&
           & trim(adjustl(s_input_dummy)),&
           & int(-1,kind=sint), &
           & spixel_scan_out%qc_scale,spixel_scan_out%qc_offset, &
           & spixel_scan_out%qc_vmin,spixel_scan_out%qc_vmax, wo,ierr)
     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
!===============================
!ILLUM
!===============================
      !write(*,*) 'setting up illumination'
      spixel_scan_out%illum_scale=1
      spixel_scan_out%illum_offset=0
      spixel_scan_out%illum_vmin=1!*spixel_scan_out%lon_scale
      spixel_scan_out%illum_vmax=4!*spixel_scan_out%lon_scale
      !      write(*,*) ncid
!           & 'illumination_flag', 'Day(1)/Twilight(2)/Night(3)/Daynore(4)',&
      CALL nc_defdata_byte_flag_value(ncid, dims_var, 'illum', spixel_scan_out%vidillum, 'illumination flag', &
           & 'illumination_flag', '1b, 2b, 3b 4b','Day Twilight Night Daynore', &
           & spixel_scan_out%byte_fill_value, &
           & spixel_scan_out%illum_scale,spixel_scan_out%illum_offset, &
           & spixel_scan_out%illum_vmin,spixel_scan_out%illum_vmax, wo,ierr)
        if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

        if(status .ne. 0 ) then

           write(*,*) 'def_vars_primary.inc: netcdf variable definintion error:', status
           call Write_Log(Ctrl,'def_vars_primary.inc: netcdf variable definintion error:', status)
           stop

        endif

end subroutine def_vars_primary
