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
! 2014/09/02, GM: Use the nc_write_array interface from the orac_ncdf module in
!    the common library.
! 2014/09/09, AP: Remove procflag as that's controlled by ORAC driver file.
! 2014/10/23, OS: added new variables to be written to output: cldtype, cldmask,
!    cccot_pre, lusflag, dem, nisemask
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/07/02, OS: added writing of cldmask_uncertainty
! 2016/03/31, GM: Changes to support processing only SW or only LW channels.
! 2016/04/28, AP: Make multiple views mandatory.
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/06/21, OS: added ann phase variables
! 2018/04/26, SP: Add code to save satellite azimuth (commented out, but useful)
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_write_swath(imager_flags,imager_angles, &
     imager_geolocation,imager_measurements,imager_cloud,imager_time, &
     imager_pavolonis,netcdf_info,channel_info,surface,include_full_brdf, &
     do_cloud_emis)

   use channel_structures_m
   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m
   use surface_structures_m

   implicit none

   type(imager_flags_t),        intent(in) :: imager_flags
   type(imager_angles_t),       intent(in) :: imager_angles
   type(imager_geolocation_t),  intent(in) :: imager_geolocation
   type(imager_measurements_t), intent(in) :: imager_measurements
   type(imager_cloud_t),        intent(in) :: imager_cloud
   type(imager_time_t),         intent(in) :: imager_time
   type(imager_pavolonis_t),    intent(in) :: imager_pavolonis
   type(netcdf_output_info_t),  intent(in) :: netcdf_info
   type(channel_info_t),        intent(in) :: channel_info
   type(surface_t),             intent(in) :: surface
   logical,                     intent(in) :: include_full_brdf
   logical,                     intent(in) :: do_cloud_emis


   integer(kind=lint)                            :: i, ii
   integer(kind=lint)                            :: n_x
   integer(kind=lint), allocatable, dimension(:) :: dummy_chan_vec1d


   n_x = imager_geolocation%endx - imager_geolocation%startx + 1


   ! config file
   call ncdf_write_array( &
        netcdf_info%ncid_config, &
        'msi_instr_ch_numbers', &
        netcdf_info%vid_msi_instr_ch_numbers_config, &
        channel_info%channel_ids_instr, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_config, &
        'msi_abs_ch_wl', &
        netcdf_info%vid_msi_abs_ch_wl_config, &
        channel_info%channel_wl_abs, &
        1, 1, channel_info%nchannels_total)
   call ncdf_write_array( &
        netcdf_info%ncid_config, &
        'msi_ch_swflag', &
        netcdf_info%vid_msi_ch_swflag_config, &
        channel_info%channel_sw_flag, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_config, &
        'msi_ch_lwflag', &
        netcdf_info%vid_msi_ch_lwflag_config, &
        channel_info%channel_lw_flag, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_config, &
        'msi_ch_view', &
        netcdf_info%vid_msi_ch_view_config, &
        channel_info%channel_view_ids, &
        1, 1, channel_info%nchannels_total)

   if (channel_info%nchannels_sw .ne. 0) then
      allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
      dummy_chan_vec1d=0_lint
      ii=1
      do i=1,channel_info%nchannels_total
         if (channel_info%channel_sw_flag(i) .eq. 1) then
            dummy_chan_vec1d(ii)=i
            ii=ii+1
         end if
      end do

      call ncdf_write_array( &
           netcdf_info%ncid_config, &
           'alb_abs_ch_numbers', &
           netcdf_info%vid_alb_abs_ch_numbers_config, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_sw)
      deallocate(dummy_chan_vec1d)
   end if

   if (channel_info%nchannels_lw .ne. 0) then
      allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
      dummy_chan_vec1d=0_lint
      ii=1
      do i=1,channel_info%nchannels_total
         if (channel_info%channel_lw_flag(i) .eq. 1) then
            dummy_chan_vec1d(ii)=i
            ii=ii+1
         end if
      end do

      call ncdf_write_array( &
           netcdf_info%ncid_config, &
           'emis_abs_ch_numbers', &
           netcdf_info%vid_emis_abs_ch_numbers_config, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_lw)
      deallocate(dummy_chan_vec1d)
   end if


   ! alb file (albedo and emissivity)

   if (channel_info%nchannels_sw .ne. 0) then
      allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
      dummy_chan_vec1d=0_lint
      ii=1
      do i=1,channel_info%nchannels_total
         if (channel_info%channel_sw_flag(i) .eq. 1) then
            dummy_chan_vec1d(ii)=i
            ii=ii+1
         end if
      end do

      call ncdf_write_array( &
           netcdf_info%ncid_alb, &
           'alb_abs_ch_numbers', &
           netcdf_info%vid_alb_abs_ch_numbers, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_sw)
      deallocate(dummy_chan_vec1d)
   end if

   if (channel_info%nchannels_lw .ne. 0) then
      allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
      dummy_chan_vec1d=0_lint
      ii=1
      do i=1,channel_info%nchannels_total
         if (channel_info%channel_lw_flag(i) .eq. 1) then
            dummy_chan_vec1d(ii)=i
            ii=ii+1
         end if
      end do

      call ncdf_write_array( &
           netcdf_info%ncid_alb, &
           'alb_emis_ch_numbers', &
           netcdf_info%vid_emis_abs_ch_numbers, &
           dummy_chan_vec1d, &
           1, 1, channel_info%nchannels_lw)
      deallocate(dummy_chan_vec1d)
   end if

   if (channel_info%nchannels_sw .ne. 0) then
      call ncdf_write_array( &
           netcdf_info%ncid_alb, &
           'alb_data', &
           netcdf_info%vid_alb_data, &
           surface%albedo(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, channel_info%nchannels_sw)

      if (include_full_brdf) then
         call ncdf_write_array( &
              netcdf_info%ncid_alb, &
              'rho_0v', &
              netcdf_info%vid_rho_0v_data, &
              surface%rho_0v(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

         call ncdf_write_array( &
              netcdf_info%ncid_alb, &
              'rho_0d', &
              netcdf_info%vid_rho_0d_data, &
              surface%rho_0d(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

         call ncdf_write_array( &
              netcdf_info%ncid_alb, &
              'rho_dv', &
              netcdf_info%vid_rho_dv_data, &
              surface%rho_dv(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)

         call ncdf_write_array( &
              netcdf_info%ncid_alb, &
              'rho_dd', &
              netcdf_info%vid_rho_dd_data, &
              surface%rho_dd(imager_geolocation%startx:,:,:), &
              1, 1, n_x, &
              1, 1, imager_geolocation%ny, &
              1, 1, channel_info%nchannels_sw)
      end if
   end if

   if (channel_info%nchannels_lw .ne. 0) then
      call ncdf_write_array( &
           netcdf_info%ncid_alb, &
           'emis_data', &
           netcdf_info%vid_emis_data, &
           surface%emissivity(imager_geolocation%startx:,:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny, &
           1, 1, channel_info%nchannels_lw)
   end if


   ! clf file (cflag, cldtype, cldmask, cldmask_uncertainty, cccot_pre)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cflag', &
        netcdf_info%vid_cflag, &
        imager_flags%cflag(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

#ifdef INCLUDE_SATWX
   if (do_cloud_emis) then
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'cldemis_lw', netcdf_info%vid_cemis_lw, &
           imager_cloud%cloud_emis(imager_geolocation%startx:,:,1), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'cldemis_wv_1', netcdf_info%vid_cemis_wv1, &
           imager_cloud%cloud_emis(imager_geolocation%startx:,:,2), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'cldemis_wv_2', netcdf_info%vid_cemis_wv2, &
           imager_cloud%cloud_emis(imager_geolocation%startx:,:,3), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'trop_t', netcdf_info%vid_tropop_te, &
           imager_cloud%trop_t(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'trop_p', netcdf_info%vid_tropop_pr, &
           imager_cloud%trop_p(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
      call ncdf_write_array( &
           netcdf_info%ncid_clf, &
           'cape', netcdf_info%vid_cape, &
           imager_cloud%cape(imager_geolocation%startx:,:), &
           1, 1, n_x, &
           1, 1, imager_geolocation%ny)
   end if
#endif

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cldtype', &
        netcdf_info%vid_cldtype, &
        imager_pavolonis%cldtype(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cldmask', &
        netcdf_info%vid_cldmask, &
        imager_pavolonis%cldmask(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cldmask_uncertainty', &
        netcdf_info%vid_cldmask_unc, &
        imager_pavolonis%cldmask_uncertainty(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cccot_pre', &
        netcdf_info%vid_cccot_pre, &
        imager_pavolonis%cccot_pre(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'ann_phase', &
        netcdf_info%vid_ann_phase, &
        imager_pavolonis%ann_phase(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'ann_phase_uncertainty', &
        netcdf_info%vid_ann_phase_unc, &
        imager_pavolonis%ann_phase_uncertainty(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_clf, &
        'cphcot', &
        netcdf_info%vid_cphcot, &
        imager_pavolonis%cphcot(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   ! geo file (solzen, satzen, solaz, relazi)

   call ncdf_write_array( &
        netcdf_info%ncid_geo, &
        'solzen', &
        netcdf_info%vid_solzen, &
        imager_angles%solzen(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_geo, &
        'satzen', &
        netcdf_info%vid_satzen, &
        imager_angles%satzen(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_geo, &
        'sataz', &
        netcdf_info%vid_sataz, &
        imager_angles%satazi(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_geo, &
        'solaz', &
        netcdf_info%vid_solaz, &
        imager_angles%solazi(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)

   call ncdf_write_array( &
        netcdf_info%ncid_geo, &
        'relazi', &
        netcdf_info%vid_relazi, &
        imager_angles%relazi(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nviews)


   ! loc file (lat, lon)

   call ncdf_write_array( &
        netcdf_info%ncid_loc, &
        'lat', netcdf_info%vid_lat, &
        imager_geolocation%latitude(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   call ncdf_write_array( &
        netcdf_info%ncid_loc, &
        'lon', &
        netcdf_info%vid_lon, &
        imager_geolocation%longitude(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)


   ! lsf file (lsflag)

   call ncdf_write_array( &
        netcdf_info%ncid_lsf, &
        'lsflag', &
        netcdf_info%vid_lsflag, &
        imager_flags%lsflag(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   call ncdf_write_array( &
        netcdf_info%ncid_lsf, &
        'lusflag', &
        netcdf_info%vid_lusflag, &
        imager_flags%lusflag(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   call ncdf_write_array( &
        netcdf_info%ncid_lsf, &
        'dem', &
        netcdf_info%vid_dem, &
        imager_geolocation%dem(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   call ncdf_write_array( &
        netcdf_info%ncid_lsf, &
        'nisemask', &
        netcdf_info%vid_nisemask, &
        surface%nise_mask(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   ! msi file (indexes, time, measurements)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_instr_ch_numbers', &
        netcdf_info%vid_msi_instr_ch_numbers, &
        channel_info%channel_ids_instr, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_abs_ch_wl', &
        netcdf_info%vid_msi_abs_ch_wl, &
        channel_info%channel_wl_abs, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_ch_swflag', &
        netcdf_info%vid_msi_ch_swflag, &
        channel_info%channel_sw_flag, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_ch_lwflag', &
        netcdf_info%vid_msi_ch_lwflag, &
        channel_info%channel_lw_flag, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_ch_view', &
        netcdf_info%vid_msi_ch_view, &
        channel_info%channel_view_ids, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'time', &
        netcdf_info%vid_time, &
        imager_time%time(imager_geolocation%startx:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'msi_data', &
        netcdf_info%vid_msi_data, &
        imager_measurements%data(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'sd_data', &
        netcdf_info%vid_sd_data, &
        imager_measurements%uncertainty(imager_geolocation%startx:,:,:), &
        1, 1, n_x, &
        1, 1, imager_geolocation%ny, &
        1, 1, channel_info%nchannels_total)

   call ncdf_write_array( &
        netcdf_info%ncid_msi, &
        'cal_data', &
        netcdf_info%vid_cal_data, &
        imager_measurements%cal_gain(:), &
        1, 1, channel_info%nchannels_total)


end subroutine netcdf_output_write_swath
