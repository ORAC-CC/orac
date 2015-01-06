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
!     9th Sep 2014, Greg McGarragh:
!       Changes related to new BRDF support.
!    24th Oct 2014, Oliver Sus:
!       added deallocation of CldType, CloudMask, CCCOT_pre, LUSFlags, DEM,
!       nisemask
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Dealloc_Data(Ctrl, MSI_Data)

   use Ctrl_def

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   if (associated(MSI_Data%ALB))          deallocate(MSI_Data%ALB)

   if (Ctrl%RS%use_full_brdf) then
      if (associated(MSI_Data%rho_0v))    deallocate(MSI_Data%rho_0v)
      if (associated(MSI_Data%rho_0d))    deallocate(MSI_Data%rho_0d)
      if (associated(MSI_Data%rho_dv))    deallocate(MSI_Data%rho_dv)
      if (associated(MSI_Data%rho_dd))    deallocate(MSI_Data%rho_dd)
   end if

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

   if (associated(MSI_Data%CldType))      deallocate(MSI_Data%CldType)
   if (associated(MSI_Data%CloudMask))    deallocate(MSI_Data%CloudMask)
   if (associated(MSI_Data%CCCOT_pre))    deallocate(MSI_Data%CCCOT_pre)
   if (associated(MSI_Data%LUSFlags))     deallocate(MSI_Data%LUSFlags)
   if (associated(MSI_Data%DEM))          deallocate(MSI_Data%DEM)
   if (associated(MSI_Data%nisemask))     deallocate(MSI_Data%nisemask)

end subroutine Dealloc_Data
