!-------------------------------------------------------------------------------
! Name: netcdf_structures.F90
!
! Purpose:
! Define variables types which hold the preprocessing output information and
! file, dimension and variable IDs.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2012/05/17, MJ: produces initial version of code
! 2012/05/24, MJ: adds some commenting
! 2012/08/02, MJ: adds some more ids for writing of RTTOV output to netcdf file
! 2012/08/08, CP: added albd
! 2012/11/08, CP: added in level ids for netcdf files
! 2013/03/07, CP: added in some diagnostics albedo and q
! 2013        MJ: adds skintid_pw, lnspid_pw
! 2013        CP: removes skintid_pw,lnspid_pw declared twice
! 2013/10/23, AP: Tidying
! 2014/05/23, GM: Some more cleaning and removal of unused elements.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/09/02, GM: Got rid of many unused elements.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module netcdf_structures

   use preproc_constants

   implicit none

   type netcdf_info_s

      ! file ids
      integer :: ncid_alb, ncid_clf, ncid_config, ncid_geo, ncid_loc, ncid_lsf,  &
                 ncid_lwrtm, ncid_msi, ncid_prtm, ncid_scan, ncid_swrtm

      ! fundamental dimensions ids for the different dimensions in the different
      ! files
      integer :: dimid_lw_channels, dimid_sw_channels

      integer :: dimid_x_alb,  dimid_y_alb
      integer :: dimid_x_cf,   dimid_y_cf
      integer :: dimid_x_geo,  dimid_y_geo, dimid_v_geo
      integer :: dimid_x_loc,  dimid_y_loc
      integer :: dimid_x_lsf,  dimid_y_lsf
      integer :: dimid_x_lw,   dimid_y_lw,  dimid_v_lw
      integer :: dimid_x_msi,  dimid_y_msi, dimid_v_msi
      integer :: dimid_x_pw,   dimid_y_pw
      integer :: dimid_x_sw,   dimid_y_sw,  dimid_v_sw
      integer :: dimid_x_scan, dimid_y_scan

      integer :: dimid_xy_lw, dimid_xy_pw, dimid_xy_sw

      integer :: dimid_layers_sw, dimid_levels_sw
      integer :: dimid_layers_pw, dimid_levels_pw
      integer :: dimid_layers_lw, dimid_levels_lw

      integer :: dimid_c_alb, dimid_c_emis
      integer :: dimid_c_config, dimid_c_config_alb, dimid_c_config_emis
      integer :: dimid_x_config, dimid_y_config
      integer :: dimid_c_msi


      ! variable ids

      ! alb file
      integer :: vid_alb_abs_ch_numbers, vid_emis_abs_ch_numbers
      integer :: vid_alb_data, vid_emis_data
      integer :: vid_rho_0v, vid_rho_0d, vid_rho_dv, vid_rho_dd

      ! clf file
      integer :: vid_cflag

      ! config file:
      integer :: vid_msi_instr_ch_numbers_config, vid_msi_abs_ch_numbers_config, &
                 vid_msi_abs_channel_wl_config, vid_msi_ch_lwflag_config, &
                 vid_msi_ch_swflag_config, vid_msi_ch_procflag_config, &
                 vid_alb_abs_ch_numbers_config, vid_emis_abs_ch_numbers_config

      ! geo file
      integer :: vid_solzen, vid_satzen, vid_solaz, vid_relazi

      ! loc file
      integer :: vid_lat, vid_lon

      ! lsf file
      integer :: vid_lsflag

      ! lwrtm file:
      integer :: vid_lw_channel_abs_ids, vid_lw_channel_instr_ids, vid_lw_channel_wvl
      integer :: vid_solza_lw, vid_satza_lw, vid_relazi_lw
      integer :: vid_emiss_lw
      integer :: vid_tac_lw, vid_tbc_lw
      integer :: vid_rbc_up_lw, vid_rac_up_lw, vid_rac_down_lw

      ! msi file:
      integer :: vid_msi_instr_ch_numbers,vid_msi_abs_ch_numbers,vid_msi_abs_ch_wl
      integer :: vid_msi_ch_swflag,vid_msi_ch_lwflag,vid_msi_ch_procflag
      integer :: vid_time
      integer :: vid_msi_data

      ! prtm file:
      integer :: vid_lon_pw, vid_lat_pw
      integer :: vid_skint_pw, vid_lnsp_pw, vid_lsf_pw
      integer :: vid_satzen_pw, vid_solzen_pw
      integer :: vid_pprofile_lev_pw, vid_tprofile_lev_pw, vid_hprofile_lev_pw

      ! scan file
      integer :: vid_uscan, vid_vscan

      ! swrtm file:
      integer :: vid_sw_channel_abs_ids, vid_sw_channel_instr_ids, vid_sw_channel_wvl
      integer :: vid_tac_sw, vid_tbc_sw
      integer :: vid_solza_sw, vid_satza_sw, vid_relazi_sw

   end type netcdf_info_s

end module netcdf_structures
