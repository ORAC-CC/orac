!-------------------------------------------------------------------------------
! Name: allocate_channel_info.F90
!
! Purpose:
! Allocate the arrays within the type defined in channel_info.F90
!
! Description and Algorithm details:
! 1) Allocate arrays to have length channel_info%nchannels_total
! 2) Initialise to the appropriate fill value
!
! Arguments:
! Name         Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! channel_info struct both Structure to allocate arrays
!
! History:
! 2012/06/04, MJ: produces draft code
! 2013/09/06, AP: removed redundant arguments
! 2014/10/15, GM: Added allocation of map_ids_abs_to_ref_band_land and
!    map_ids_abs_to_ref_band_sea and removed allocation of channel_proc_flag.
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/03/04, GM: Added map_ids_abs_to_snow_and_ice.
! 2016/08/04, GM: Added map_ids_channel_to_sw and map_ids_channel_to_lw.
! 2017/06/21, OS: bug fix: with cray-fortran compiler, channel_info needs to be
!    declared as inout if input values are to be used within SR
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_channel_info(channel_info)

   use preproc_constants_m

   implicit none

   type(channel_info_t), intent(inout) :: channel_info

   allocate(channel_info%channel_ids_instr(channel_info%nchannels_total))
   channel_info%channel_ids_instr=lint_fill_value

   allocate(channel_info%channel_wl_abs(channel_info%nchannels_total))
   channel_info%channel_wl_abs=sreal_fill_value

   allocate(channel_info%map_ids_channel_to_sw(channel_info%nchannels_total))
   channel_info%channel_ids_instr=lint_fill_value
   allocate(channel_info%map_ids_channel_to_lw(channel_info%nchannels_total))
   channel_info%channel_ids_instr=lint_fill_value

   allocate(channel_info%channel_view_ids(channel_info%nchannels_total))
   channel_info%channel_view_ids=lint_fill_value

   allocate(channel_info%channel_sw_flag(channel_info%nchannels_total))
   channel_info%channel_sw_flag=lint_fill_value
   allocate(channel_info%channel_lw_flag(channel_info%nchannels_total))
   channel_info%channel_lw_flag=lint_fill_value

   allocate(channel_info%map_ids_abs_to_ref_band_land( &
        channel_info%nchannels_total))
   channel_info%map_ids_abs_to_ref_band_land=lint_fill_value
   allocate(channel_info%map_ids_abs_to_ref_band_sea( &
        channel_info%nchannels_total))
   channel_info%map_ids_abs_to_ref_band_sea=lint_fill_value

   allocate(channel_info%map_ids_abs_to_snow_and_ice( &
        channel_info%nchannels_total))
   channel_info%map_ids_abs_to_snow_and_ice=lint_fill_value

   allocate(channel_info%channel_fractional_uncertainty( &
        channel_info%nchannels_total))
   allocate(channel_info%channel_minimum_uncertainty( &
        channel_info%nchannels_total))
   allocate(channel_info%channel_fm_lnd_uncertainty( &
        channel_info%nchannels_total))
   allocate(channel_info%channel_fm_sea_uncertainty( &
        channel_info%nchannels_total))

   allocate(channel_info%channel_absolute_bias(channel_info%nchannels_total))
   allocate(channel_info%channel_relative_bias(channel_info%nchannels_total))

end subroutine allocate_channel_info
