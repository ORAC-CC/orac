! Name:
!    Dealloc_Data
!
! Purpose:
!    Deallocate the MSI_Data arrays at end of ECP execution.
!
! Arguments:
!    Name      Type         In/Out/Both Description
!    Ctrl      struct       In          Control structure
!    MSI_Data  alloc struct In          MSI Data structure
!    status    int          Out         Error status
!
! Algorithm:
!    Deallocates all arrays in the MSI_Data structure
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
!    22nd Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!     1st Nov 2001, Andy Smith:
!       Added test of allocation status before each array is deallocated.
!       If an error occurs on reading one or more of the MSI Data arrays it's
!       possible that not all arrays are associated (see Read_Sat_Data,
!       Read_ATSR_MSI etc).
!     5th Jun 2002, Caroline Poulsen:
!        Deallocate ALB data
!     8th Jul 2011, Caroline Poulsen:
!        Deallocate scan data
!    16th Dec 2013, Greg McGarragh:
!       Add deallocation of MSI_Data%illum and a bit of cleanup.
!    27th May 2014, Greg McGarragh:
!       Some cleanup.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Dealloc_Data(Ctrl, MSI_Data, status)

   use Ctrl_def

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data
   integer,      intent(inout) :: status

   if (associated(MSI_Data%ALB))          deallocate(MSI_Data%ALB)

   if (associated(MSI_Data%CloudFlags))   deallocate(MSI_Data%CloudFlags)

   if (associated(MSI_Data%Geometry%Sol)) deallocate(MSI_Data%Geometry%Sol)
   if (associated(MSI_Data%Geometry%Sat)) deallocate(MSI_Data%Geometry%Sat)
   if (associated(MSI_Data%Geometry%Azi)) deallocate(MSI_Data%Geometry%Azi)

   if (associated(MSI_Data%Location%Lat)) deallocate(MSI_Data%Location%Lat)
   if (associated(MSI_Data%Location%Lon)) deallocate(MSI_Data%Location%Lon)

   if (associated(MSI_Data%LSFlags))      deallocate(MSI_Data%LSFlags)

   if (associated(MSI_Data%time))         deallocate(MSI_Data%time)

   if (associated(MSI_Data%MSI))          deallocate(MSI_Data%MSI)

   if (associated(MSI_Data%Scan%uscan))   deallocate(MSI_Data%Scan%uscan)
   if (associated(MSI_Data%Scan%vscan))   deallocate(MSI_Data%Scan%vscan)

   if (associated(MSI_Data%illum))        deallocate(MSI_Data%illum)

end subroutine Dealloc_Data
