!-------------------------------------------------------------------------------
! Name:
!    Dealloc_RTM
!
! Purpose:
!    Deallocate the RTM arrays at end of ECP execution.
!
! Arguments:
!    Name   Type         In/Out/Both Description
!    Ctrl   struct       In          Control structure
!    RTM    alloc struct In          RTM structure
!    status int          Out         Error status
!
! Algorithm:
!    Deallocates all arrays in the RTM structure
!    If any solar channels were requested
!      - Deallocates arrays in the SW sub-structure
!    If any thermal channels were requested
!      - Deallocates arrays in the LW sub-structure
!    No error handling is done at present. Since this routine is executed once
!    at the end of execution it is unclear what action should be taken in case
!    of error.
!
! Local variables:
!    Name   Type   Description
!
! History:
!    24th Oct 2001, Andy Smith: original version
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    12th Dec 2002, Caroline Poulsen: Added geopotential height
!    22nd Sept 2011, Caroline Poulsen: Remove SW%P as now the same as LW%P
!    13th Dec 2011, Caroline Poulsen: Deallocated RTM%SW%Lon
!    16th Dec 2013, Greg McGarragh: Add deallocation of RTM%LW%skint and
!       RTM%LW%sp and a bit of cleanup.
!    27th May 2014, Greg McGarragh: Some more cleanup.
!    30th Jan 2015, Adam Povey: Eliminate redundant fields.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Dealloc_RTM(Ctrl, RTM)

   use Ctrl_def

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(RTM_t),  intent(inout) :: RTM

   ! Deallocate SW sub-structure arrays
   if (Ctrl%Ind%NSolar > 0) then
      deallocate(RTM%SW%Tac)
      deallocate(RTM%SW%Tbc)
   end if

   ! Deallocate LW sub-structure arrays
   deallocate(RTM%LW%Lat)
   deallocate(RTM%LW%Lon)
   deallocate(RTM%LW%P)
   deallocate(RTM%LW%T)
   deallocate(RTM%LW%H)

   if (Ctrl%Ind%NThermal > 0) then
      deallocate(RTM%LW%Ems)
      deallocate(RTM%LW%Tac)
      deallocate(RTM%LW%Tbc)
      deallocate(RTM%LW%Rac_up)
      deallocate(RTM%LW%Rac_dwn)
      deallocate(RTM%LW%Rbc_up)
   end if

end subroutine Dealloc_RTM
