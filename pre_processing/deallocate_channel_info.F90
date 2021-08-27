!-------------------------------------------------------------------------------
! Name: deallocate_channel_info.F90
!
! Purpose:
! Deallocate the array parts of the types defined in channel_info.F90
!
! Description and Algorithm details:
! 1) Deallocate all fields of structure.
!
! Arguments:
! Name         Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! channel_info struct both Summary of imager channel properties
!
! History:
! 2012/06/04, MJ: produces draft code-
! 2014/10/15, GM: Added deallocation of map_ids_abs_to_ref_band_land and
!    map_ids_abs_to_ref_band_sea and removed deallocation of channel_proc_flag.
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/03/04, GM: Added map_ids_abs_to_snow_and_ice.
! 2016/07/01, GT: Added map_ids_sw_to_channel and map_ids_lw_to_channel.
! 2016/08/04, GM: Added map_ids_channel_to_sw and map_ids_channel_to_lw.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_channel_info(channel_info)

   implicit none

   type(channel_info_t), intent(inout) :: channel_info

   deallocate(channel_info%channel_ids_instr)
   deallocate(channel_info%channel_wl_abs)
   deallocate(channel_info%map_ids_sw_to_channel)
   deallocate(channel_info%map_ids_lw_to_channel)
   deallocate(channel_info%map_ids_channel_to_sw)
   deallocate(channel_info%map_ids_channel_to_lw)
   deallocate(channel_info%channel_view_ids)
   deallocate(channel_info%channel_sw_flag)
   deallocate(channel_info%channel_lw_flag)
   deallocate(channel_info%channel_ids_rttov_coef_sw)
   deallocate(channel_info%channel_ids_rttov_coef_lw)
   deallocate(channel_info%map_ids_abs_to_ref_band_land)
   deallocate(channel_info%map_ids_abs_to_ref_band_sea)
   deallocate(channel_info%map_ids_abs_to_snow_and_ice)
   deallocate(channel_info%channel_fractional_uncertainty)
   deallocate(channel_info%channel_minimum_uncertainty)
   deallocate(channel_info%channel_fm_lnd_uncertainty)
   deallocate(channel_info%channel_fm_sea_uncertainty)
   deallocate(channel_info%channel_absolute_bias)
   deallocate(channel_info%channel_relative_bias)
   deallocate(channel_info%sw_rttov_viewone_id)
   deallocate(channel_info%sw_view_ids)
   deallocate(channel_info%lw_rttov_viewone_id)
   deallocate(channel_info%lw_view_ids)

end subroutine deallocate_channel_info
