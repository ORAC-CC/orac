!-------------------------------------------------------------------------------
! Name: DeallocCtrl.F90
!
! Purpose:
! Deallocate the Ctrl at end of ECP execution.
!
! Description and Algorithm details:
! Deallocates all arrays in the Ctrl structure
!
! Arguments:
! Name   Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct       In          Control structure
!
! History:
! 2011/12/13, CP: Original version
! 2011/12/20, CP: Changed Ctrl to be inout (from in)
! 2014/01/12, GM: Added some missing deallocates.
! 2014/05/27, GM: Some cleanup.
! 2014/12/19, AP: Removing ysolar_msi, ythermal_msi.
! 2015/01/12, AP: Adding Ch_Is, YMixed.
! 2015/02/04, GM: Add sabotage_inputs flag and retrieval channel requirements
!    arrays.
! 2015/02/04, GM: Add ReChans array.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Dealloc_Ctrl(Ctrl)

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(inout) :: Ctrl

   deallocate(Ctrl%Ind%ViewIdx)
   deallocate(Ctrl%Ind%WvlIdx)
   deallocate(Ctrl%Ind%Y_ID)
   deallocate(Ctrl%Ind%ICh)
   deallocate(Ctrl%Ind%Ch_Is)
   deallocate(Ctrl%Ind%YSolar)
   deallocate(Ctrl%Ind%YThermal)
   deallocate(Ctrl%Ind%YMixed)

   deallocate(Ctrl%Rs%B)

   deallocate(Ctrl%Sy)

   deallocate(Ctrl%tau_chans)
   deallocate(Ctrl%r_e_chans)
   deallocate(Ctrl%ir_chans)

   if (associated(Ctrl%ReChans)) &
      deallocate(Ctrl%ReChans)

end subroutine Dealloc_Ctrl
