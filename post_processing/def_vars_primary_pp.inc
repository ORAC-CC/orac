  ! Name: def_vars_primary_pp.inc
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
  ! 2012/03/18 Caroline Poulsen modified to add cloud flag
  ! 2012/07/06 MJ extensively overhauls and restructures the code
  ! 2012/10/17 changing cwp_scale and cwp_error_scale to 0.1 
  ! 2012/11/27 Numerous changes to make output CF compliant
  ! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
  ! 2014/04/01 Matthias Jerg: fixes some problems/cleanup with illumination
  ! 2014/07/08 CP: added more illumination options
  ! 2014/10/24 OS: added variables lusflag, cldtype, cloudmask, DEM
  !  (currently deactivated), and nisemask
  ! 2014/11/20 OS: corrected Julian Date format; added Pavolonis cloud phase variable
  ! 2014/12/01 OS: added flag 9 = prob_opaque_ice
  ! 2014/12/03 CP: added cloud albedo
  ! 2014/12/17 CP: removed unknown form mask naming
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  
  
  !===============================
  !TIME
  !===============================
  
  
  write(*,*) 'setting up time'
  spixel_scan_out%time_scale=1.0
  spixel_scan_out%time_offset=0.00
  spixel_scan_out%time_vmin=0.0
  spixel_scan_out%time_vmax=1.0D10
  !write(*,*) 'ncid_primary'
  !spixel_scan_out%time_fv=-2.0E+9
  spixel_scan_out%time_fv=-32767.0
  !      pause
  CALL nc_defdata_double_pp(ncid_primary, dims_var, 'time', spixel_scan_out%vidtime,&
       & 'time', 'time', &
       & 'days since -4712-01-01 12:00:00' &
       & ,spixel_scan_out%time_fv,spixel_scan_out%time_scale,spixel_scan_out%time_offset&
       & ,spixel_scan_out%time_vmin,spixel_scan_out%time_vmax, wo, ierr, 'standard') 
  !	   ierr=1
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !LON
  !===============================

  write(*,*) 'setting up lon'
  spixel_scan_out%lon_scale=1.0
  spixel_scan_out%lon_offset=0.00
  spixel_scan_out%lon_vmin=-180.0!*spixel_scan_out%lon_scale
  spixel_scan_out%lon_vmax=180.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !     & 'longitude',1, 'degrees_east', spixel_scan_out%real_fill_value_lat_lon, &
  CALL nc_defdata_float_pp(ncid_primary, dims_var, 'lon', spixel_scan_out%vidlon, 'longitude', &
       & 'longitude',1, 'degrees_east', spixel_scan_out%real_fill_value, &
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
  !      write(*,*) ncid_primary
  !     & 'latitude',1, 'degrees_north', spixel_scan_out%real_fill_value_lat_lon,&
  CALL nc_defdata_float_pp(ncid_primary, dims_var, 'lat', spixel_scan_out%vidlat, 'latitude', &
       & 'latitude',1, 'degrees_north', spixel_scan_out%real_fill_value,&
       & spixel_scan_out%lat_scale,spixel_scan_out%lat_offset,&
       & spixel_scan_out%lat_vmin,spixel_scan_out%lat_vmax, wo,ierr)    
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr



  !===============================
  !SATZEN
  !===============================
  input_num='1'
  input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
  s_input_dummy=&
       & 'platform_zenith_angle'! for_view_no_'//trim(adjustl(input_num))

  write(*,*) 'setting up satzen'

  spixel_scan_out%sat_scale=1.0
  spixel_scan_out%sat_offset=0.00
  spixel_scan_out%sat_vmin=-180.0!*spixel_scan_out%lon_scale
  spixel_scan_out%sat_vmax=180.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_float_pp(ncid_primary, dims_var, trim(adjustl(input_dummy)),&
       & spixel_scan_out%vidsat_zen(1), trim(adjustl(input_dummy)), &
       & trim(adjustl(s_input_dummy)),1, 'degrees', real_fill_value, &
       & spixel_scan_out%sat_scale,spixel_scan_out%sat_offset, &
       & spixel_scan_out%sat_vmin,spixel_scan_out%sat_vmax, wo,ierr)

  !MJ              & trim(adjustl(input_dummy)), 'degree', real_fill_value, &
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !SOLZEN
  !===============================

  write(*,*) 'setting up solzen'

  input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
  s_input_dummy='solar_zenith_angle'! for_view_no_'//trim(adjustl(input_num))

  spixel_scan_out%sol_scale=1.0
  spixel_scan_out%sol_offset=0.00
  spixel_scan_out%sol_vmin=-180.0!*spixel_scan_out%lon_scale
  spixel_scan_out%sol_vmax=180.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !        write(*,*) 'vor sol',iviews
  CALL nc_defdata_float_pp(ncid_primary, dims_var, trim(adjustl(input_dummy)),&
       & spixel_scan_out%vidsol_zen(1), trim(adjustl(input_dummy)), &
       & trim(adjustl(s_input_dummy)),1,  'degrees', real_fill_value, &
       & spixel_scan_out%sol_scale,spixel_scan_out%sol_offset, &
       & spixel_scan_out%sol_vmin,spixel_scan_out%sol_vmax, wo,ierr)
  !         write(*,*) 'nach sol',iviews         
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !AZI
  !===============================

  write(*,*) 'setting up azi'

  input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
  s_input_dummy='angle_of_rotation_from_solar_azimuth_to_platform_azimuth'! for_view_no_'//trim(adjustl(input_num))

  spixel_scan_out%azi_scale=1.0
  spixel_scan_out%azi_offset=0.00
  spixel_scan_out%azi_vmin=-180.0!*spixel_scan_out%lon_scale
  spixel_scan_out%azi_vmax=180.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary

  CALL nc_defdata_float_pp(ncid_primary, dims_var, trim(adjustl(input_dummy)),&
       & spixel_scan_out%vidrel_azi(1), trim(adjustl(input_dummy)), &
       & trim(adjustl(s_input_dummy)),1, 'degrees', real_fill_value, &
       & spixel_scan_out%azi_scale,spixel_scan_out%azi_offset, &
       & spixel_scan_out%azi_vmin,spixel_scan_out%azi_vmax, wo,ierr)

  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !COT
  !===============================

  write(*,*) 'setting up cot'

  spixel_scan_out%cot_scale=0.01
  spixel_scan_out%cot_offset=0.0
  spixel_scan_out%cot_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cot_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_no_units_pp(ncid_primary, dims_var, 'cot', spixel_scan_out%vidcot, 'cloud optical thickness', &
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

  write(*,*) 'setting up reff'

  spixel_scan_out%ref_scale=0.01
  spixel_scan_out%ref_offset=0.00
  spixel_scan_out%ref_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ref_vmax=20000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ref', spixel_scan_out%vidref, 'effective radius', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top','micrometer', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ref_scale,spixel_scan_out%ref_offset, &
       & spixel_scan_out%ref_vmin,spixel_scan_out%ref_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !CTP
  !===============================

  write(*,*) 'setting up ctp'

  spixel_scan_out%ctp_scale=0.1
  spixel_scan_out%ctp_offset=0.00
  spixel_scan_out%ctp_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ctp_vmax=12000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ctp', spixel_scan_out%vidctp, 'cloud top pressure', &
       & 'air_pressure_at_cloud_top','hPa', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ctp_scale,spixel_scan_out%ctp_offset, &
       & spixel_scan_out%ctp_vmin,spixel_scan_out%ctp_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !MASK
  !===============================

  write(*,*) 'setting up mask'

  spixel_scan_out%cct_scale=0.01
  spixel_scan_out%cct_offset=0.00
  spixel_scan_out%cct_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cct_vmax=100!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cc_total', spixel_scan_out%vidcct, 'cloud fraction', &
       & 'cloud_area_fraction', '',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cct_scale,spixel_scan_out%cct_offset, &
       & spixel_scan_out%cct_vmin,spixel_scan_out%cct_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !NN OUTPUT
  !===============================

  write(*,*) 'setting up nnoutput'

  spixel_scan_out%cccot_scale=0.001
  spixel_scan_out%cccot_offset=0.00
  spixel_scan_out%cccot_vmin=-1000!*spixel_scan_out%lon_scale
  spixel_scan_out%cccot_vmax=2000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cccot', spixel_scan_out%vidcccot, 'nn result', &
       & 'neural_network_output', '',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cccot_scale,spixel_scan_out%cccot_offset, &
       & spixel_scan_out%cccot_vmin,spixel_scan_out%cccot_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !NN OUTPUT FROM PREPROCESSING
  !===============================

  write(*,*) 'setting up nnoutput pre'

  spixel_scan_out%cccot_pre_scale=0.001
  spixel_scan_out%cccot_pre_offset=0.0
  spixel_scan_out%cccot_pre_vmin=-1000
  spixel_scan_out%cccot_pre_vmax=2000
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cccot_pre', &
       & spixel_scan_out%vidcccot_pre, 'nn result from preprocessing', &
       & 'neural_network_output_pre', '',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cccot_pre_scale,spixel_scan_out%cccot_pre_offset, &
       & spixel_scan_out%cccot_pre_vmin,spixel_scan_out%cccot_pre_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr




  !===============================
  !STEMP
  !===============================

  write(*,*) 'setting up stemp'

  spixel_scan_out%stemp_scale=0.01
  spixel_scan_out%stemp_offset=0.00
  spixel_scan_out%stemp_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%stemp_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'stemp', spixel_scan_out%vidstemp, 'surface temperature', &
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
!!$      !      write(*,*) ncid_primary
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
  !      write(*,*) ncid_primary
  !& 'altitude_at_cloud_top', 'kilometer',&
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cth', spixel_scan_out%vidcth, 'cloud top height', &
       & 'height_at_cloud_top', 'kilometer',&
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
  !      write(*,*) ncid_primary
  !& 'altitude_at_cloud_top', 'kilometer',&
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cth_uncertainty',&
       & spixel_scan_out%vidctherror, 'cloud top height uncertainty', &
       & 'height_at_cloud_top uncertainty', 'kilometer',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cth_error_scale,spixel_scan_out%cth_error_offset, &
       & spixel_scan_out%cth_error_vmin,spixel_scan_out%cth_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr






  !===============================
  !CTT
  !===============================

  write(*,*) 'setting up ctt'
  spixel_scan_out%ctt_scale=0.01
  spixel_scan_out%ctt_offset=0.00
  spixel_scan_out%ctt_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ctt_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ctt', spixel_scan_out%vidctt, 'cloud top temperature', &
       & 'air_temperature_at_cloud_top', 'kelvin',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ctt_scale,spixel_scan_out%ctt_offset, &
       & spixel_scan_out%ctt_vmin,spixel_scan_out%ctt_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !DELCTT
  !===============================
  spixel_scan_out%ctt_error_scale=0.01
  spixel_scan_out%ctt_error_offset=0.00
  spixel_scan_out%ctt_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ctt_error_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ctt_uncertainty',&
       & spixel_scan_out%vidctterror, 'cloud top temperature uncertainty', &
       & 'air_temperature_at_cloud_top uncertainty', 'kelvin',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ctt_error_scale,spixel_scan_out%ctt_error_offset, &
       & spixel_scan_out%ctt_error_vmin,spixel_scan_out%ctt_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr




  !===============================
  !CWP
  !===============================

  write(*,*) 'setting up cwp'

  !MJ ORG spixel_scan_out%cwp_scale=0.1
  spixel_scan_out%cwp_scale=1.0
  spixel_scan_out%cwp_offset=0.00
  spixel_scan_out%cwp_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cwp_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cwp', spixel_scan_out%vidcwp, 'cloud liquid water path', &
       & 'atmosphere_mass_content_of_cloud_liquid_water','g/m2', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cwp_scale,spixel_scan_out%cwp_offset, &
       & spixel_scan_out%cwp_vmin,spixel_scan_out%cwp_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr




  !===============================
  ! Cloud ALBEDO
  !===============================
    spixel_scan_out%cloud_albedo_scale=0.0001
      spixel_scan_out%cloud_albedo_offset=0.0
      spixel_scan_out%cloud_albedo_vmin=0
      spixel_scan_out%cloud_albedo_vmax=11000


write(*,*)' def var primaryglobal_atts%sensor',  global_atts%sensor 
  !cloud_albedo
   if ( global_atts%sensor .eq. 'AATSR') then 
   solar_chan_id=(/ 2,3/)
   endif
   if ( global_atts%sensor .eq. 'MODIS') then
    solar_chan_id=(/ 1,2 /)
    endif

   if ( global_atts%sensor .eq. 'AVHRR') then 
   solar_chan_id=(/ 1,2 /)
   endif
   nchan_solar=2

      do iinput=1,nchan_solar

         write(input_num,"(i4)") solar_chan_id(iinput)

         input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
         write(*,*)'cloud albebo', input_dummy

 
         CALL nc_defdata_short_pp(ncid_primary, dims_var, trim(adjustl(input_dummy)),&
              & spixel_scan_out%vidcloud_albedo(iinput), trim(adjustl(input_dummy)), &
              & trim(adjustl(input_dummy)), '', &
              & spixel_scan_out%int_fill_value,&
              & spixel_scan_out%cloud_albedo_scale,spixel_scan_out%cloud_albedo_offset,&
              & spixel_scan_out%cloud_albedo_vmin,spixel_scan_out%cloud_albedo_vmax, wo,ierr)

         
      enddo



  !===============================
  !DELCOT
  !===============================
  write(*,*) 'setting up delcot'
  spixel_scan_out%cot_error_scale=0.01
  spixel_scan_out%cot_error_offset=0.00
  spixel_scan_out%cot_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cot_error_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !MJ & 'atmosphere_optical_thickness_due_to_cloud uncertainty','', spixel_scan_out%int_fill_value, &
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cot_uncertainty', spixel_scan_out%vidcoterror,&
       & 'cloud optical thickness uncertainty', &
       & 'atmosphere_optical_thickness_due_to_cloud','', spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cot_error_scale,spixel_scan_out%cot_error_offset, &
       & spixel_scan_out%cot_error_vmin,spixel_scan_out%cot_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !DELREF
  !===============================
  write(*,*) 'setting up delref'
  spixel_scan_out%ref_error_scale=0.01
  spixel_scan_out%ref_error_offset=0.00
  spixel_scan_out%ref_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ref_error_vmax=20000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !& 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top uncertainty','micrometer', &
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ref_uncertainty', spixel_scan_out%vidreferror,&
       & 'effective radius uncertainty', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top','micrometer', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ref_error_scale,spixel_scan_out%ref_error_offset, &
       & spixel_scan_out%ref_error_vmin,spixel_scan_out%ref_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !DELCTP
  !===============================
  write(*,*) 'setting up delctp'
  spixel_scan_out%ctp_error_scale=0.1
  spixel_scan_out%ctp_error_offset=0.00
  spixel_scan_out%ctp_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ctp_error_vmax=12000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !MJ & 'air_pressure_at_cloud_top uncertainty','hPa', &
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'ctp_uncertainty', spixel_scan_out%vidctperror,&
       & 'cloud top pressure', &
       & 'air_pressure_at_cloud_top','hPa', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%ctp_error_scale,spixel_scan_out%ctp_error_offset, &
       & spixel_scan_out%ctp_error_vmin,spixel_scan_out%ctp_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !DELMASK
  !===============================
  write(*,*) 'setting up delmask'
  spixel_scan_out%cct_error_scale=0.01
  spixel_scan_out%cct_error_offset=0.00
  spixel_scan_out%cct_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cct_error_vmax=100!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !& 'cloud_area_fraction uncertainty', '',&
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cc_total_uncertainty', spixel_scan_out%vidccterror,&
       & 'cloud fraction uncertainty', &
       & 'cloud_area_fraction', '',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cct_error_scale,spixel_scan_out%cct_error_offset, &
       & spixel_scan_out%cct_error_vmin,spixel_scan_out%cct_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !DELSTEMP
  !===============================
  write(*,*) 'setting up delstemp'
  spixel_scan_out%stemp_error_scale=0.01
  spixel_scan_out%stemp_error_offset=0.00
  spixel_scan_out%stemp_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%stemp_error_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !& 'surface_temperature uncertainty','Kelvin',&
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'stemp_uncertainty', spixel_scan_out%vidstemperror,&
       & 'surface temperature uncertainty', &
       & 'surface_temperature','Kelvin',&
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%stemp_error_scale,spixel_scan_out%stemp_error_offset, &
       & spixel_scan_out%stemp_vmin,spixel_scan_out%stemp_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !DELCWP
  !===============================
  write(*,*) 'setting up delcwp'
  spixel_scan_out%cwp_error_scale=1.0
  spixel_scan_out%cwp_error_offset=0.00
  spixel_scan_out%cwp_error_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%cwp_error_vmax=32000!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !MJ     & 'atmosphere_mass_content_of_cloud_liquid_water uncertainty','g/m2', &
  CALL nc_defdata_short_pp(ncid_primary, dims_var, 'cwp_uncertainty', spixel_scan_out%vidcwperror,&
       & 'CWP uncertainty', &
       & 'atmosphere_mass_content_of_cloud_liquid_water','g/m2', &
       & spixel_scan_out%int_fill_value, &
       & spixel_scan_out%cwp_error_scale,spixel_scan_out%cwp_error_offset, &
       & spixel_scan_out%cwp_error_vmin,spixel_scan_out%cwp_error_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !CONVERGENCE
  !===============================
  write(*,*) 'setting up convergence'
  spixel_scan_out%con_scale=1
  spixel_scan_out%con_offset=0
  spixel_scan_out%con_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%con_vmax=1!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !           & 'retrieval convergence', 'yes(0)/no(1)',&
  nflags=2
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'convergence', spixel_scan_out%vidconvergence,&
       & 'retrieval convergence', &
       & 'retrieval_convergence_flag',flag_vals,2,0,'yes no',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%con_scale,spixel_scan_out%con_offset, &
       & spixel_scan_out%con_vmin,spixel_scan_out%con_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !NITER
  !===============================
  write(*,*) 'setting up iterations'
  spixel_scan_out%niter_scale=1
  spixel_scan_out%niter_offset=0
  spixel_scan_out%niter_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%niter_vmax=100!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_byte_pp(ncid_primary, dims_var, 'niter', spixel_scan_out%vidniter, 'retrieval iterations', &
       & 'retrieval_iterations',0, '',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%niter_scale,spixel_scan_out%niter_offset, &
       & spixel_scan_out%niter_vmin,spixel_scan_out%niter_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !cloud phase
  !===============================
  spixel_scan_out%phase_scale=1
  spixel_scan_out%phase_offset=0
  spixel_scan_out%phase_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%phase_vmax=2!*spixel_scan_out%lon_scale
  nflags=3
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  flag_vals(3)=2_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'phase', spixel_scan_out%vidphase, 'cloud phase flag', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_vals,3,1, 'clear liquid ice',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%phase_scale,spixel_scan_out%phase_offset, &
       & spixel_scan_out%phase_vmin,spixel_scan_out%phase_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !cloud phase Pavolonis
  !===============================
  spixel_scan_out%phase_pavolonis_scale=1
  spixel_scan_out%phase_pavolonis_offset=0
  spixel_scan_out%phase_pavolonis_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%phase_pavolonis_vmax=2!*spixel_scan_out%lon_scale
  nflags=3
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  flag_vals(3)=2_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'phase_pavolonis', spixel_scan_out%vidphase_pavolonis, 'cloud phase flag Pavolonis', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_vals,3,1, 'clear liquid ice',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%phase_pavolonis_scale,spixel_scan_out%phase_pavolonis_offset, &
       & spixel_scan_out%phase_pavolonis_vmin,spixel_scan_out%phase_pavolonis_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !PCHANGE
  !===============================
  !MJOLD      spixel_scan_out%pchange_scale=1
  !MJOLD      spixel_scan_out%pchange_offset=0
  !MJOLD      spixel_scan_out%pchange_vmin=0!*spixel_scan_out%lon_scale
  !MJOLD      spixel_scan_out%pchange_vmax=100!*spixel_scan_out%lon_scale
  !MJOLD      !      write(*,*) ncid_primary
  !MJOLD      CALL nc_defdata_byte(ncid_primary, dims_var, 'pchange', spixel_scan_out%vidpchange, 'no of phase changes', &
  !MJOLD           & 'number_of_phase_changes', '',&
  !MJOLD           & spixel_scan_out%byte_fill_value, &
  !MJOLD           & spixel_scan_out%pchange_scale,spixel_scan_out%pchange_offset, &
  !MJOLD           & spixel_scan_out%pchange_vmin,spixel_scan_out%pchange_vmax, wo,ierr)
  !MJOLD     if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr


  !===============================
  !COST JA
  !===============================
  write(*,*) 'setting up cost ja'
  spixel_scan_out%costja_scale=1.0
  spixel_scan_out%costja_offset=0.00
  spixel_scan_out%costja_vmin=0.0!*spixel_scan_out%lon_scale
  spixel_scan_out%costja_vmax=100000.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_float_pp(ncid_primary, dims_var, 'costja', spixel_scan_out%vidcostja, 'costja', &
       & 'a_priori_cost at_solution',0, '',&
       & spixel_scan_out%real_fill_value, &
       & spixel_scan_out%costja_scale,spixel_scan_out%costja_offset, &
       & spixel_scan_out%costja_vmin,spixel_scan_out%costja_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !COST JM
  !===============================
  write(*,*) 'setting up cost jm'
  spixel_scan_out%costjm_scale=1.0
  spixel_scan_out%costjm_offset=0.00
  spixel_scan_out%costjm_vmin=0.0!*spixel_scan_out%lon_scale
  spixel_scan_out%costjm_vmax=100000.0!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  CALL nc_defdata_float_pp(ncid_primary, dims_var, 'costjm', spixel_scan_out%vidcostjm&
       & , 'costjm', 'measurement_cost at_solution',0, '',&
       & spixel_scan_out%real_fill_value, &
       & spixel_scan_out%costjm_scale,spixel_scan_out%costjm_offset, &
       & spixel_scan_out%costjm_vmin,spixel_scan_out%costjm_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !LSFLAG
  !===============================
  write(*,*) 'setting up lsflag'
  spixel_scan_out%ls_scale=1
  spixel_scan_out%ls_offset=0
  spixel_scan_out%ls_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%ls_vmax=6!*spixel_scan_out%lon_scale
  !      write(*,*) ncid_primary
  !           & 'land_binary_mask', 'Land(1)/Sea(0)',&
  nflags=6
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  flag_vals(3)=3_byte
  flag_vals(4)=4_byte
  flag_vals(5)=5_byte
  flag_vals(6)=6_byte


  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'lsflag',&
       & spixel_scan_out%vidlsflag, 'land/sea flag', &
       & 'land_binary_mask', flag_vals,nflags,1, 'sea land sunglint snow ice snow_and_ice',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%ls_scale,spixel_scan_out%ls_offset, &
       & spixel_scan_out%ls_vmin,spixel_scan_out%ls_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !LUSFLAG
  !===============================
  write(*,*) 'setting up lusflag'
  spixel_scan_out%lusflag_scale=1
  spixel_scan_out%lusflag_offset=0
  spixel_scan_out%lusflag_vmin=1
  spixel_scan_out%lusflag_vmax=24
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'lusflag',&
       & spixel_scan_out%vidlusflag, 'land use mask', 'land_use_mask', &
       & '1b to 24b', &
       & 2,1, 'see usgs.org',spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%lusflag_scale,spixel_scan_out%lusflag_offset, &
       & spixel_scan_out%lusflag_vmin,spixel_scan_out%lusflag_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !CLDTYPE
  !===============================
  write(*,*) 'setting up cldtype'
  spixel_scan_out%cldtype_scale=1
  spixel_scan_out%cldtype_offset=0
  spixel_scan_out%cldtype_vmin=0
  spixel_scan_out%cldtype_vmax=9
  nflags=10
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  flag_vals(3)=2_byte
  flag_vals(4)=3_byte
  flag_vals(5)=4_byte
  flag_vals(6)=5_byte
  flag_vals(7)=6_byte
  flag_vals(8)=7_byte
  flag_vals(9)=8_byte
  flag_vals(10)=9_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'cldtype',&
       & spixel_scan_out%vidcldtype, 'Pavolonis cloudtype', 'Pavolonis_cloudtype', &
       & flag_vals, 2,1, &
       & 'clear,NA,fog,water,supercooled,mixed,opaque_ice,cirrus,overlap,prob_opaque_ice',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%cldtype_scale,spixel_scan_out%cldtype_offset, &
       & spixel_scan_out%cldtype_vmin,spixel_scan_out%cldtype_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !CLOUDMASK
  !===============================
  write(*,*) 'setting up preprocessing cloudmask'
  spixel_scan_out%cldmask_scale=1
  spixel_scan_out%cldmask_offset=0
  spixel_scan_out%cldmask_vmin=0
  spixel_scan_out%cldmask_vmax=1
  nflags=2
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'cloudmask_pre',&
       & spixel_scan_out%vidcldmask, 'Preprocessing cloud mask', &
       & 'Preprocessing_cloud_mask', flag_vals, 2,1, &
       & 'cloud free, cloudy',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%cldmask_scale,spixel_scan_out%cldmask_offset, &
       & spixel_scan_out%cldmask_vmin,spixel_scan_out%cldmask_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

  !===============================
  !DEM
  !===============================
  !  write(*,*) 'setting up dem'
  !   spixel_scan_out%dem_scale=1
  !   spixel_scan_out%dem_offset=0
  !   spixel_scan_out%dem_vmin=0
  !   spixel_scan_out%dem_vmax=10000
  !   CALL nc_defdata_short_pp(ncid_primary, dims_var, 'dem', spixel_scan_out%dem&
  !        &, 'digital elevation model', &
  !        & 'digital_elevation_model', 'm', &
  !        & spixel_scan_out%int_fill_value, &
  !        & spixel_scan_out%dem_scale,spixel_scan_out%dem_offset, &
  !        & spixel_scan_out%dem_vmin,spixel_scan_out%dem_vmax, wo,ierr)
  !   if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !NISEMASK
  !===============================
  write(*,*) 'setting up nisemask'
  spixel_scan_out%nisemask_scale=1
  spixel_scan_out%nisemask_offset=0
  spixel_scan_out%nisemask_vmin=0
  spixel_scan_out%nisemask_vmax=1
  nflags=2
  allocate(flag_vals(nflags))
  flag_vals(1)=0_byte
  flag_vals(2)=1_byte
  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'nisemask',&
       & spixel_scan_out%vidnisemask, 'NISE snow/ice mask', &
       & 'NISE_mask', flag_vals, 2,1, &
       & 'snow/ice free, snow/ice',&
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%nisemask_scale,spixel_scan_out%nisemask_offset, &
       & spixel_scan_out%nisemask_vmin,spixel_scan_out%nisemask_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)

!!$!===============================
!!$!CLOUDFLAG
!!$!===============================
!!$write(*,*) 'setting up cloudflag'
!!$spixel_scan_out%cloud_scale=1
!!$spixel_scan_out%cloud_offset=0
!!$spixel_scan_out%cloud_vmin=0!*spixel_scan_out%lon_scale
!!$spixel_scan_out%cloud_vmax=1!*spixel_scan_out%lon_scale
!!$!      write(*,*) ncid_primary
!!$!           & 'land_binary_mask', 'Land(1)/Sea(0)',&
!!$CALL nc_defdata_byte_flag_value(ncid_primary, dims_var, 'cloudflag', spixel_scan_out%vidcloudflag, 'cloud flag', &
!!$     & 'land_binary_mask', '0b, 1b', 'sea land',&
!!$     & spixel_scan_out%byte_fill_value, &
!!$     & spixel_scan_out%cloud_scale,spixel_scan_out%cloud_offset, &
!!$     & spixel_scan_out%cloud_vmin,spixel_scan_out%cloud_vmax, wo,ierr)
!!$if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr

  !===============================
  !QCFLAG
  !===============================
  write(*,*) 'setting up qcflag'
  spixel_scan_out%qc_scale=1
  spixel_scan_out%qc_offset=0
  spixel_scan_out%qc_vmin=0!*spixel_scan_out%lon_scale
  spixel_scan_out%qc_vmax=254!*spixel_scan_out%lon_scale
  !     write(*,*) 'ncid_primary',ncid_primary
!!$      CALL nc_defdata_float(ncid_primary, dims_var, 'qcflag', spixel_scan_out%vidqcflag, 'quality control flag', &
!!$           & 'quality_control_flag', '',&
!!$           & real_fill_value, &
!!$           & spixel_scan_out%qc_scale,spixel_scan_out%qc_offset, &
!!$           & spixel_scan_out%qc_vmin,spixel_scan_out%qc_vmax, wo,ierr)
  s_input_dummy='Bit 0 unused, always set to 0, Bits 1-5 set to 1 if state variable error out of bounds'
  s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 6 set to 1 if no convergence achieved'
  s_input_dummy=trim(adjustl(s_input_dummy))//', Bit 7 set to 1 if cost too large.'
  s_input_dummy=trim(adjustl(s_input_dummy))//' Bit 1=COT Bit 2=REF Bit 3=CTP Bit 4=CCT Bit 5=STEMP'
  call nc_defdata_short_flag_value_pp(ncid_primary, dims_var, 'qcflag', spixel_scan_out%vidqcflag, 'quality control flag', &
       & 'quality_control_flag',&
       & trim(adjustl(s_input_dummy)),&
       & int(-1,kind=sint), &
       & spixel_scan_out%qc_scale,spixel_scan_out%qc_offset, &
       & spixel_scan_out%qc_vmin,spixel_scan_out%qc_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  !===============================
  !ILLUM
  !===============================
  write(*,*) 'setting up illumination'
  spixel_scan_out%illum_scale=1
  spixel_scan_out%illum_offset=0
  spixel_scan_out%illum_vmin=1!*spixel_scan_out%lon_scale
  spixel_scan_out%illum_vmax=12!*spixel_scan_out%lon_scale
  write(*,*) 'illum def start'
  !           & 'illumination_flag', 'Day(1)/Twilight(2)/Night(3)',&
  nflags=12
  allocate(flag_vals(nflags))
  flag_vals(1)=1_byte
  flag_vals(2)=2_byte
  flag_vals(3)=3_byte
  flag_vals(4)=4_byte
  flag_vals(5)=5_byte
  flag_vals(6)=6_byte
  flag_vals(7)=7_byte
  flag_vals(8)=8_byte
  flag_vals(9)=9_byte
  flag_vals(10)=10_byte
  flag_vals(11)=11_byte
  flag_vals(12)=12_byte

  CALL nc_defdata_byte_flag_value_pp(ncid_primary, dims_var, 'illum', spixel_scan_out%vidillum, 'illumination flag', &
       & 'illumination_flag', flag_vals,nflags,0,'Day Twilight Night Daynore IDaysinglevisfirst  IDaysinglevissecond IDaysingleirfirst   IDaysingleirsecond  IDaysingleirthird   INightsingleirfirst INightsingleirsecond INightsingleirthird' , &
       & spixel_scan_out%byte_fill_value, &
       & spixel_scan_out%illum_scale,spixel_scan_out%illum_offset, &
       & spixel_scan_out%illum_vmin,spixel_scan_out%illum_vmax, wo,ierr)
  if(ierr .ne. 0 ) status=PrimaryFileDefinitionErr
  deallocate(flag_vals)
  write(*,*) 'illum def end'

  if(status .ne. 0 ) then

     write(*,*) 'def_vars_primary.inc: netcdf variable definintion error:', status

     stop

  endif

