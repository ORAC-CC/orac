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
      integer :: datedim_lw, datedim_pw, datedim_sw
      integer :: lwchanneldim, swchanneldim
      integer :: viewdim_lw, viewdim_sw

      integer :: xdim_alb, ydim_alb
      integer :: xdim_cf, ydim_cf
      integer :: xdim_geo, ydim_geo
      integer :: xdim_loc, ydim_loc
      integer :: xdim_lsf, ydim_lsf
      integer :: xdim_lw, ydim_lw
      integer :: xdim_msi, ydim_msi
      integer :: xdim_pw, ydim_pw
      integer :: xdim_sw, ydim_sw

      integer :: xydim_lw, xydim_pw, xydim_sw

      integer :: layerdim_sw, leveldim_sw
      integer :: layerdim_pw, leveldim_pw
      integer :: layerdim_lw, leveldim_lw

      integer :: xdim_scan, ydim_scan

      integer :: vdim_geo, vdim_msi
      integer :: cdim_alb, cdim_emis
      integer :: cdim_config, cdim_config_alb, cdim_config_emis, cdim_msi
      integer :: xdim_config, ydim_config


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
      integer :: lonid_pw, latid_pw
      integer :: skintid_pw, lnspid_pw, lsfid_pw
      integer :: satzenid_pw, solzenid_pw
      integer :: pprofile_lev_id_pw, tprofile_lev_id_pw, hprofile_lev_id_pw

      ! scan file
      integer :: vid_uscan, vid_vscan

      ! swrtm file:
      integer :: channels_id_sw, channels_id_instr_sw, wvn_id_sw
      integer :: tac_id_sw, tbc_id_sw
      integer :: solzaid_sw, satzaid_sw, relaziid_sw


      ! derived dimensions ids
      integer :: xycdim_lw(2)
      integer :: xyzdim_lw(2), xyzdim_pw(2), xyzdim_sw(2)
      integer :: xyzcdim_lw(3), xyzcdim_sw(3)
      integer :: xyzcvdim_lw(4), xyzcvdim_sw(4)
      integer :: xyvdim_lw(2), xyvdim_sw(2)

      ! for 2D variables (horitontal dimension)
      integer(kind=lint) :: start_1d(1), counter_1d(1), stride_1d(1)
      ! for 3D variables (horizontal+vertical)
      integer(kind=lint) :: start_2d(2), counter_2d(2), stride_2d(2)
      ! for 4D variables (space+wavelength)
      integer(kind=lint) :: start_3d(3), counter_3d(3), stride_3d(3)
      ! for 5D variables (space+wavelength+viewing direction)
      integer(kind=lint) :: start_4d(4), counter_4d(4), stride_4d(4)

   end type netcdf_info_s

end module netcdf_structures
