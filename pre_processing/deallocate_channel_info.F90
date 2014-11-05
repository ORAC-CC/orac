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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_channel_info(channel_info)

   implicit none

   type(channel_info_s), intent(inout) :: channel_info

   deallocate(channel_info%channel_ids_instr)
   deallocate(channel_info%channel_ids_abs)
   deallocate(channel_info%channel_wl_abs)
   deallocate(channel_info%channel_sw_flag)
   deallocate(channel_info%channel_lw_flag)
   deallocate(channel_info%channel_ids_rttov_coef_sw)
   deallocate(channel_info%channel_ids_rttov_coef_lw)
   deallocate(channel_info%map_ids_abs_to_ref_band_land)
   deallocate(channel_info%map_ids_abs_to_ref_band_sea)
   deallocate(channel_info%channel_view_ids)

end subroutine deallocate_channel_info
