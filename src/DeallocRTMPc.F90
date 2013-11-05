! Name:
!    Dealloc_RTM_Pc
!
! Purpose:
!    Deallocate the RTM_Pc arrays at end of ECP execution.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    RTM_Pc      alloc struct   In       RTM_Pc structure
!    status      int            Out      Error status
!    
! Algorithm:
!    Deallocates all arrays in the RTM_Pc structure
!    If any solar channels were requested
!      - Deallocates arrays in the SW sub-structure
!    If any thermal channels were requested
!      - Deallocates arrays in the LW sub-structure
!
! Local variables:
!    Name   Type   Description
!    None
!
! History:
!    22nd Oct 2001, Andy Smith: original version
!
! Bugs:
!    None known.
!
!------------------------------------------------------------------------------------
subroutine Dealloc_RTM_Pc(Ctrl, RTM_Pc, status)

   use Ctrl_def
   use RTM_Pc_def

   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(in)      :: Ctrl
   type(RTM_Pc_t), intent(inout) :: RTM_Pc   
   integer, intent(inout)        :: status

!  deallocate sizes of SW sub-structure arrays

   if (Ctrl%Ind%Ny-Ctrl%Ind%NThermal > 0) then
      deallocate(RTM_Pc%SW%Tac)
      deallocate(RTM_Pc%SW%Tbc)
      deallocate(RTM_Pc%SW%dTac_dPc)
      deallocate(RTM_Pc%SW%dTbc_dPc)
   end if

!  deallocate sizes of LW sub-structure arrays

   if (Ctrl%Ind%NThermal > 0) then
      deallocate(RTM_Pc%LW%Tac)
      deallocate(RTM_Pc%LW%Tbc)
      deallocate(RTM_Pc%LW%B)
      deallocate(RTM_Pc%LW%Rac_up)
      deallocate(RTM_Pc%LW%Rac_dwn)
      deallocate(RTM_Pc%LW%Rbc_up)
      deallocate(RTM_Pc%LW%dTac_dPc)
      deallocate(RTM_Pc%LW%dTbc_dPc)
      deallocate(RTM_Pc%LW%dB_dPc)
      deallocate(RTM_Pc%LW%dRac_up_dPc)
      deallocate(RTM_Pc%LW%dRac_dwn_dPc)
      deallocate(RTM_Pc%LW%dRbc_up_dPc)
   end if

!  deallocate sizes of the main structure arrays

   deallocate(RTM_Pc%Tac)
   deallocate(RTM_Pc%Tbc)
   deallocate(RTM_Pc%dTac_dPc)
   deallocate(RTM_Pc%dTbc_dPc)

end subroutine Dealloc_RTM_Pc
