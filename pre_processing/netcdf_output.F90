!-------------------------------------------------------------------------------
! Name: netcdf_output.F90
!
! Purpose:
! Module for NCDF output write routines.
!
! History:
! 2014/05/23, GM: First version.
! 2014/09/16, GM: Added NETCDF_OUTPUT_FILE_* parameters and moved the structure
!    netcdf_output_info_t into here.
! 2014/09/28, GM: Remove layer dimids as they were not used any more.
! 2014/10/23, OS: Added output variables to clf and lsf file.
! 2015/01/15, AP: Eliminate channel_abs_ids.
! 2015/01/30, AP: Eliminate skint, sp, and lsf field for PRTM. Remove uscan and
!    vscan as unnecessary.
! 2015/06/24, OS: added vid and include of netcdf_output_check.F90
! 2015/07/23, GM: Added specific humidity and ozone vids.
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/06/20, OS: Added ann phase variable IDs
! 2018/04/26, SP: Add code to save satellite azimuth (commented out, but useful)
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
! 2018/07/18, DE: Add tropopause temperature
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module netcdf_output_m

   implicit none

   integer, parameter :: NETCDF_OUTPUT_FILE_ABL    = 1
   integer, parameter :: NETCDF_OUTPUT_FILE_CLF    = 2
   integer, parameter :: NETCDF_OUTPUT_FILE_CONFIG = 3
   integer, parameter :: NETCDF_OUTPUT_FILE_GEO    = 4
   integer, parameter :: NETCDF_OUTPUT_FILE_LOC    = 5
   integer, parameter :: NETCDF_OUTPUT_FILE_LSF    = 6
   integer, parameter :: NETCDF_OUTPUT_FILE_LWRTM  = 7
   integer, parameter :: NETCDF_OUTPUT_FILE_MSI    = 8
   integer, parameter :: NETCDF_OUTPUT_FILE_PRTM   = 9
   integer, parameter :: NETCDF_OUTPUT_FILE_SWRTM  = 10

   type netcdf_output_info_t

      ! file ids
      integer :: ncid_alb, ncid_clf, ncid_config, ncid_geo, ncid_loc, ncid_lsf
      integer :: ncid_lwrtm, ncid_msi, ncid_prtm, ncid_swrtm

      ! fundamental dimensions ids for the different dimensions in the different
      ! files
      integer :: dimid_lw_channels, dimid_sw_channels

      integer :: dimid_x_alb, dimid_y_alb
      integer :: dimid_x_cf,  dimid_y_cf,  dimid_v_cf
      integer :: dimid_x_geo, dimid_y_geo, dimid_v_geo
      integer :: dimid_x_loc, dimid_y_loc
      integer :: dimid_x_lsf, dimid_y_lsf
      integer :: dimid_x_lw,  dimid_y_lw,  dimid_v_lw
      integer :: dimid_x_msi, dimid_y_msi, dimid_v_msi
      integer :: dimid_x_pw,  dimid_y_pw
      integer :: dimid_x_sw,  dimid_y_sw,  dimid_v_sw

      integer :: dimid_xy_lw, dimid_xy_pw, dimid_xy_sw

      integer :: dimid_levels_sw
      integer :: dimid_levels_pw
      integer :: dimid_levels_lw

      integer :: dimid_c_alb, dimid_c_emis
      integer :: dimid_c_config, dimid_c_config_alb, dimid_c_config_emis
      integer :: dimid_x_config, dimid_y_config
      integer :: dimid_c_msi


      ! variable ids

      ! alb file
      integer :: vid_alb_abs_ch_numbers, vid_emis_abs_ch_numbers
      integer :: vid_alb_data, vid_emis_data
      integer :: vid_rho_0v_data, vid_rho_0d_data
      integer :: vid_rho_dv_data, vid_rho_dd_data

      ! clf file
      integer :: vid_cflag, vid_cemis_lw, vid_cemis_wv1, vid_cemis_wv2, vid_cldtype, vid_cldmask, vid_cccot_pre, &
                 vid_cldmask_unc
      integer :: vid_tropop_pr, vid_tropop_te, vid_cape
      integer :: vid_ann_phase, vid_cphcot, vid_ann_phase_unc

      ! config file:
      integer :: vid_msi_instr_ch_numbers_config, &
                 vid_msi_abs_ch_wl_config, vid_msi_ch_lwflag_config, &
                 vid_msi_ch_swflag_config, vid_alb_abs_ch_numbers_config, &
                 vid_emis_abs_ch_numbers_config, vid_msi_ch_view_config

      ! geo file
      integer :: vid_solzen, vid_satzen, vid_sataz, vid_solaz, vid_relazi

      ! loc file
      integer :: vid_lat, vid_lon

      ! lsf file
      integer :: vid_lsflag, vid_lusflag, vid_dem, vid_nisemask

      ! lwrtm file:
      integer :: vid_lw_channel_abs_ids, vid_lw_channel_instr_ids
      integer :: vid_lw_channel_wvl
      integer :: vid_solza_lw, vid_satza_lw, vid_relazi_lw
      integer :: vid_emiss_lw
      integer :: vid_tac_lw, vid_tbc_lw
      integer :: vid_rbc_up_lw, vid_rac_up_lw, vid_rac_down_lw

      ! msi file:
      integer :: vid_msi_instr_ch_numbers,vid_msi_abs_ch_wl
      integer :: vid_msi_ch_swflag,vid_msi_ch_lwflag,vid_msi_ch_view
      integer :: vid_time
      integer :: vid_msi_data, vid_sd_data, vid_cal_data

      ! prtm file:
      integer :: vid_lon_pw, vid_lat_pw
      integer :: vid_satzen_pw, vid_solzen_pw
      integer :: vid_pprofile_lev_pw, vid_tprofile_lev_pw, vid_hprofile_lev_pw, &
                 vid_qprofile_lev_pw, vid_o3profile_lev_pw

      ! swrtm file:
      integer :: vid_sw_channel_abs_ids, vid_sw_channel_instr_ids
      integer :: vid_sw_channel_wvl
      integer :: vid_tac_sw, vid_tbc_sw
      integer :: vid_solza_sw, vid_satza_sw, vid_relazi_sw

   end type netcdf_output_info_t

contains

#include "netcdf_output_create.F90"
#include "netcdf_output_create_file.F90"
#include "netcdf_output_close.F90"
#include "netcdf_output_check.F90"
#include "netcdf_output_write_swath.F90"

end module netcdf_output_m
