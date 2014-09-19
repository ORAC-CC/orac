!-------------------------------------------------------------------------------
! Name: netcdf_output_write_swath.F90
!
! Purpose:
! Write outputs of ORAC into already opened NCDF files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_flags        struct  in  Summary of land/sea/ice flags.
! imager_angles       struct  in  Summary of satellite geometry.
! imager_geolocation  struct  in  Summary of pixel positions.
! imager_measurements struct  in  Summary of satellite imagery.
! imager_time         struct  in  Summary of pixel observation time.
! netcdf_info         struct  in  Summary of NCDF file properties.
! channel_info        struct  in  Summary of imager channel parameters.
! surface             struct  in  Summary of surface properties.
!
! History:
! 2012/06/01, MJ: created initial file version.
! 2012/06/12, CP: bug fix to imager_time%time
! 2012/07/04, CP: removed nviews from data
! 2012/08/07, CP: changes 4 d array to 3d msi as no views selected
! 2012/08/07, CP: added in albedo write
! 2012/08/29, CP: added in solaz
! 2012/08/29, CP: changed starty to stayi and endy to endye
! 2013/05/23, GT: 3.7 micron albedo is now ouput in the alb file.
! 2013/09/03, AP: Removed startyi, endye.
! 2013/10/14, MJ: fixed bug with writing of albedo and emissivity.
! 2013/11/05, MJ: fixed bug with writing of albedo and emissivity (channel
!    numbers).
! 2013/11/05, GM: Fixed a copy-and-paste-and-forgot-to-change bug from the above
!    fix.
! 2014/05/26, GM: Some code clean up.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/09/02, GM: Use the nc_write_array interface from the orac_ncdf module
!    in the common library.
! 2014/09/09, AP: Remove procflag as that's controlled by ORAC driver file.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_write_swath(imager_flags,imager_angles,imager_geolocation, &
   imager_measurements,imager_time,netcdf_info,channel_info,surface,include_full_brdf)

   use channel_structures
   use imager_structures
   use netcdf
   use orac_ncdf
   use preproc_constants
   use surface_structures

   implicit none

   type(imager_flags_s),        intent(in) :: imager_flags
   type(imager_angles_s),       intent(in) :: imager_angles
   type(imager_geolocation_s),  intent(in) :: imager_geolocation
   type(imager_measurements_s), intent(in) :: imager_measurements
   type(imager_time_s),         intent(in) :: imager_time
   type(netcdf_output_info_s),  intent(in) :: netcdf_info
   type(channel_info_s),        intent(in) :: channel_info
   type(surface_s),             intent(in) :: surface
   logical,                     intent(in) :: include_full_brdf

   integer(kind=lint)                            :: i, ii
   integer(kind=lint)                            :: n_x
   integer(kind=lint), allocatable, dimension(:) :: dummy_chan_vec1d


   n_x = imager_geolocation%endx - imager_geolocation%startx + 1


   ! config file

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_instr_ch_numbers_config, &
           channel_info%channel_ids_instr, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_abs_ch_numbers_config, &
           channel_info%channel_ids_abs, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'msi_abs_ch_wl', &
           netcdf_info%vid_msi_abs_ch_wl_config, &
           channel_info%channel_wl_abs, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'msi_ch_swflag', &
           netcdf_info%vid_msi_ch_swflag_config, &
           channel_info%channel_sw_flag, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'msi_ch_lwflag', &
           netcdf_info%vid_msi_ch_lwflag_config, &
           channel_info%channel_lw_flag, &
           1, 1, channel_info%nchannels_total)

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ii=1
   do i=1,channel_info%nchannels_total
      if (channel_info%channel_sw_flag(i) .eq. 1) then
         dummy_chan_vec1d(ii)=channel_info%channel_ids_abs(i)
         ii=ii+1
      end if
   end do

   call nc_write_array( &
        netcdf_info%ncid_config, &
        'alb_abs_ch_numbers', &
        netcdf_info%vid_alb_abs_ch_numbers_config, &
        dummy_chan_vec1d, &
        1, 1, channel_info%nchannels_sw)
   deallocate(dummy_chan_vec1d)

   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ii=1
   do i=1,channel_info%nchannels_total
      if (channel_info%channel_lw_flag(i) .eq. 1) then
         dummy_chan_vec1d(ii)=channel_info%channel_ids_abs(i)
         ii=ii+1
      end if
   end do

   call nc_write_array( &
           netcdf_info%ncid_config, &
           'emis_abs_ch_numbers', &
           netcdf_info%vid_emis_abs_ch_numbers_config, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_lw)
   deallocate(dummy_chan_vec1d)


   ! alb file (albedo and emissivity)

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ii=1
   do i=1,channel_info%nchannels_total
      if (channel_info%channel_sw_flag(i) .eq. 1) then
         dummy_chan_vec1d(ii)=channel_info%channel_ids_abs(i)
         ii=ii+1
      end if
   end do

   call nc_write_array( &
        netcdf_info%ncid_alb, &
        'alb_abs_ch_numbers', &
        netcdf_info%vid_alb_abs_ch_numbers, &
        dummy_chan_vec1d, &
        1, 1, channel_info%nchannels_sw)
   deallocate(dummy_chan_vec1d)

   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ii=1
   do i=1,channel_info%nchannels_total
      if (channel_info%channel_lw_flag(i) .eq. 1) then
         dummy_chan_vec1d(ii)=channel_info%channel_ids_abs(i)
         ii=ii+1
      end if
   end do

   call nc_write_array( &
           netcdf_info%ncid_alb, &
           'alb_emis_ch_numbers', &
           netcdf_info%vid_emis_abs_ch_numbers, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_lw)
   deallocate(dummy_chan_vec1d)

   call nc_write_array( &
           netcdf_info%ncid_alb, &
           'alb_data', &
           netcdf_info%vid_alb_data, &
           surface%albedo(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, channel_info%nchannels_sw)

   call nc_write_array( &
           netcdf_info%ncid_alb, &
           'emis_data', &
           netcdf_info%vid_emis_data, &
           surface%emissivity(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, channel_info%nchannels_lw)

   if (include_full_brdf) then
      call nc_write_array( &
              netcdf_info%ncid_alb, &
              'rho_0v', &
              netcdf_info%vid_rho_0v_data, &
              surface%rho_0v(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

      call nc_write_array( &
              netcdf_info%ncid_alb, &
              'rho_0d', &
              netcdf_info%vid_rho_0d_data, &
              surface%rho_0d(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

      call nc_write_array( &
              netcdf_info%ncid_alb, &
              'rho_dv', &
              netcdf_info%vid_rho_dv_data, &
              surface%rho_dv(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

      call nc_write_array( &
              netcdf_info%ncid_alb, &
              'rho_dd', &
              netcdf_info%vid_rho_dd_data, &
              surface%rho_dd(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)
   end if


   ! clf file (cflag)

   call nc_write_array( &
           netcdf_info%ncid_clf, &
           'cflag', &
           netcdf_info%vid_cflag, &
           imager_flags%cflag(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)


   ! geo file (solzen, satzen, solaz, relazi)

   call nc_write_array( &
           netcdf_info%ncid_geo, &
           'solzen', &
           netcdf_info%vid_solzen, &
           imager_angles%solzen(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, imager_angles%nviews)

   call nc_write_array( &
           netcdf_info%ncid_geo, &
           'satzen', &
           netcdf_info%vid_satzen, &
           imager_angles%satzen(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, imager_angles%nviews)

   call nc_write_array( &
           netcdf_info%ncid_geo, &
           'solaz', &
           netcdf_info%vid_solaz, &
           imager_angles%solazi(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, imager_angles%nviews)

   call nc_write_array( &
           netcdf_info%ncid_geo, &
           'relazi', &
           netcdf_info%vid_relazi, &
           imager_angles%relazi(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, imager_angles%nviews)


   ! loc file (lat, lon)

   call nc_write_array( &
           netcdf_info%ncid_loc, &
           'lat', netcdf_info%vid_lat, &
           imager_geolocation%latitude(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)

   call nc_write_array( &
           netcdf_info%ncid_loc, &
           'lon', &
           netcdf_info%vid_lon, &
           imager_geolocation%longitude(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)


   ! lsf file (lsflag)

   call nc_write_array( &
           netcdf_info%ncid_lsf, &
           'lat', &
           netcdf_info%vid_lsflag, &
           imager_flags%lsflag(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)


   ! msi file (indexes, time, measurements)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_instr_ch_numbers, &
           channel_info%channel_ids_instr, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_abs_ch_numbers, &
           channel_info%channel_ids_abs, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_abs_ch_wl', &
           netcdf_info%vid_msi_abs_ch_wl, &
           channel_info%channel_wl_abs, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_ch_swflag', &
           netcdf_info%vid_msi_ch_swflag, &
           channel_info%channel_sw_flag, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_ch_lwflag', &
           netcdf_info%vid_msi_ch_lwflag, &
           channel_info%channel_lw_flag, &
           1, 1, channel_info%nchannels_total)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'time', &
           netcdf_info%vid_time, &
           imager_time%time(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)

   call nc_write_array( &
           netcdf_info%ncid_msi, &
           'msi_data', &
           netcdf_info%vid_msi_data, &
           imager_measurements%data(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, channel_info%nchannels_total)


   ! uv file, (uscan, vscan)

   call nc_write_array( &
           netcdf_info%ncid_scan, &
           'uscan', &
           netcdf_info%vid_uscan, &
           imager_geolocation%uscan(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)

   call nc_write_array( &
           netcdf_info%ncid_scan, &
           'vscan', &
           netcdf_info%vid_vscan, &
           imager_geolocation%vscan(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)

end subroutine netcdf_output_write_swath
