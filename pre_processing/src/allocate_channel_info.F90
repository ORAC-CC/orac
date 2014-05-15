! Name: allocate_channel_info.F90
!
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
! 2012/06/04: MJ produces draft code
! 2013/09/06: AP removed redundant arguments
!
! $Id$
!
! Bugs:
! none known
!

subroutine allocate_channel_info(channel_info)

!  use channel_structures
   use preproc_constants

   implicit none

   type(channel_info_s) :: channel_info

   allocate(channel_info%channel_ids_instr(channel_info%nchannels_total))
   channel_info%channel_ids_instr=long_int_fill_value

   allocate(channel_info%channel_ids_abs(channel_info%nchannels_total))
   channel_info%channel_ids_abs=long_int_fill_value

   allocate(channel_info%channel_sw_flag(channel_info%nchannels_total))
   channel_info%channel_sw_flag=long_int_fill_value

   allocate(channel_info%channel_lw_flag(channel_info%nchannels_total))
   channel_info%channel_lw_flag=long_int_fill_value

   allocate(channel_info%channel_view_ids(channel_info%nchannels_total))
   channel_info%channel_view_ids=long_int_fill_value

   allocate(channel_info%channel_proc_flag(channel_info%nchannels_total))
   channel_info%channel_proc_flag=long_int_fill_value

   allocate(channel_info%channel_wl_abs(channel_info%nchannels_total))
   channel_info%channel_wl_abs=real_fill_value

end subroutine allocate_channel_info
