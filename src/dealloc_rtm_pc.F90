!-------------------------------------------------------------------------------
! Name: dealloc_rtm_pc.F90
!
! Purpose:
! Deallocate the RTM_Pc arrays at end of ORAC execution.
!
! Description and Algorithm details:
! 1) Deallocates all arrays in the RTM_Pc structure
! 2) If any solar channels were requested
!      - Deallocates arrays in the SW sub-structure
! 3) If any thermal channels were requested
!      - Deallocates arrays in the LW sub-structure
!
! Arguments:
! Name   Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct       In          Control structure
! RTM_Pc alloc struct In          RTM_Pc structure
!
! History:
! 2001/10/22, AS: Original version
! 2014/05/27, GM: Some cleanup.
! 2015/01/07, AP: Eliminate write to RTM_Pc%Tac, Tbc.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Dealloc_RTM_Pc(Ctrl, RTM_Pc)

   use Ctrl_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(RTM_Pc_t), intent(inout) :: RTM_Pc

   ! Deallocate sizes of SW sub-structure arrays

   if (Ctrl%Ind%NSolar > 0) then
      deallocate(RTM_Pc%SW%Tac)
      deallocate(RTM_Pc%SW%Tbc)
      deallocate(RTM_Pc%SW%dTac_dPc)
      deallocate(RTM_Pc%SW%dTbc_dPc)
   end if

   ! Deallocate sizes of LW sub-structure arrays

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

end subroutine Dealloc_RTM_Pc
