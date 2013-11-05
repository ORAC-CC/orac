! Name: deallocate_channel_info.f90
!
!
! Purpose:
! Deallocate the array parts of the types defined in channel_info.f90
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
! 2012/06/04: Matthias Jerg produces draft code-

! $Id$
!
! Bugs:
!
!none known



subroutine deallocate_channel_info(channel_info)
  
  use channel_structures
  
  implicit none
 
  type(channel_info_s) :: channel_info

  deallocate(channel_info%channel_ids_instr)
  deallocate(channel_info%channel_ids_abs)
  deallocate(channel_info%channel_ids_rttov_coef_sw)
  deallocate(channel_info%channel_ids_rttov_coef_lw)
  deallocate(channel_info%channel_sw_flag)
  deallocate(channel_info%channel_lw_flag)
  deallocate(channel_info%channel_view_ids)
  deallocate(channel_info%channel_proc_flag)
  deallocate(channel_info%channel_wl_abs)
  

end subroutine deallocate_channel_info
