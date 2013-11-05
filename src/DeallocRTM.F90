! Name:
!    Dealloc_RTM
!
! Purpose:
!    Deallocate the RTM arrays at end of ECP execution.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    RTM         alloc struct   In       RTM structure
!    status      int            Out      Error status
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
!    None
!
! History:
!    24th Oct 2001, Andy Smith: original version
!    ******** ECV work starts here **********************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    12th December 2002 Caroline Poulsen added geopotential height
!    22nd Sept 2011 Caroline Poulsen remove sw%p as now the same as lw%p
!    13th December  Caroline Poulsen  deallocated RTM%SW%Lon
! Bugs: 
!    None known.
!
!------------------------------------------------------------------------------------
subroutine Dealloc_RTM(Ctrl, RTM, status)

   use Ctrl_def
   use RTM_def

   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(in)   :: Ctrl
   type(RTM_t), intent(inout) :: RTM   
   integer, intent(inout)     :: status

!  deallocate sizes of SW sub-structure arrays

   if (Ctrl%Ind%Ny-Ctrl%Ind%NThermal > 0) then
      deallocate(RTM%SW%Lat)
      deallocate(RTM%SW%Lon)	
       deallocate(RTM%SW%Tac)
      deallocate(RTM%SW%Tbc)
   end if

!  deallocate sizes of LW sub-structure arrays

   if (Ctrl%Ind%NThermal > 0) then
      deallocate(RTM%LW%Lat)
      deallocate(RTM%LW%Lon)
      deallocate(RTM%LW%P)
      deallocate(RTM%LW%T)
      deallocate(RTM%LW%H)
      !deallocate(RTM%LW%Bs)
      deallocate(RTM%LW%Ems)
      deallocate(RTM%LW%Tac)
      deallocate(RTM%LW%Tbc)
      deallocate(RTM%LW%Rac_up)
      deallocate(RTM%LW%Rac_dwn)
      deallocate(RTM%LW%Rbc_up)
   end if


end subroutine Dealloc_RTM
