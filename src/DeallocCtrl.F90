!-------------------------------------------------------------------------------
! Name:
!    Dealloc_Ctrl
!
! Purpose:
!    Deallocate the Ctrl at end of ECP execution.
!
! Arguments:
!    Name   Type         In/Out/Both Description
!    Ctrl   struct       In          Control structure
!    RTM    alloc struct In          RTM structure
!    status int          Out         Error status
!
! Algorithm:
!    Deallocates all arrays in the Ctrl structure
!
! Local variables:
!    Name Type Description
!
! History:
!    13th Dec 2011, Caroline Poulsen: Original version
!    20th Dec 2011, Caroline Poulsen: Changed Ctrl to be inout (from in)
!    12th Jan 2014, Greg McGarragh: Added some missing deallocates.
!    27th May 2014, Greg McGarragh: Some cleanup.
!    19th Dec 2014, Adam Povey: Removing ysolar_msi, ythermal_msi.
!    12th Jan 2015, Adam Povey: Adding Ch_Is, YMixed.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Dealloc_Ctrl(Ctrl)

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(inout) :: Ctrl

   deallocate(Ctrl%Ind%ViewIdx)
   deallocate(Ctrl%Ind%Y_ID)
   deallocate(Ctrl%Ind%ICh)
   deallocate(Ctrl%Ind%Ch_Is)
   deallocate(Ctrl%Ind%YSolar)
   deallocate(Ctrl%Ind%YThermal)
   deallocate(Ctrl%Ind%YMixed)
   deallocate(Ctrl%Rs%B)
   deallocate(Ctrl%Sy)

end subroutine Dealloc_Ctrl
