!-------------------------------------------------------------------------------
! Name: dealloc_rtm.F90
!
! Purpose:
! Deallocate the RTM arrays at end of ORAC execution.
!
! Description and Algorithm details:
! 1)   Deallocates all arrays in the RTM structure
! 2)   If any solar channels were requested
!      - Deallocates arrays in the SW sub-structure
! 3)   If any thermal channels were requested
!      - Deallocates arrays in the LW sub-structure
!
! Arguments:
! Name   Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct       In          Control structure
! RTM    alloc struct In          RTM structure
!
! History:
! 2001/10/24, AS: original version
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-introducing changes made in late 2001/2002.
! 2012/12/12, CP: Added geopotential height
! 2011/09/22, CP: Remove SW%P as now the same as LW%P
! 2011/12/13, CP: Deallocated RTM%SW%Lon
! 2013/12/16, GM: Add deallocation of RTM%LW%skint and RTM%LW%sp and a bit of
!    cleanup.
! 2014/05/27, GM: Some more cleanup.
! 2015/01/30, AP: Eliminate redundant fields.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Dealloc_RTM(Ctrl, RTM)

   use Ctrl_m

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(RTM_t),  intent(inout) :: RTM

   deallocate(RTM%Lat)
   deallocate(RTM%Lon)
   deallocate(RTM%P)
   deallocate(RTM%T)
   deallocate(RTM%H)

   ! Deallocate SW sub-structure arrays
   if (Ctrl%Ind%NSolar > 0) then
      deallocate(RTM%SW%Tac)
      deallocate(RTM%SW%Tbc)
   end if

   ! Deallocate LW sub-structure arrays
   if (Ctrl%Ind%NThermal > 0) then
      deallocate(RTM%LW%Ems)
      deallocate(RTM%LW%Tac)
      deallocate(RTM%LW%Tbc)
      deallocate(RTM%LW%Rac_up)
      deallocate(RTM%LW%Rac_dwn)
      deallocate(RTM%LW%Rbc_up)
   end if

end subroutine Dealloc_RTM
