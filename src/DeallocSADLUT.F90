subroutine Dealloc_LUT_Grid(LUT_Grid)

   implicit none

   ! Declare arguments

   type(LUT_Grid_t), intent(inout) :: LUT_Grid

   deallocate(LUT_Grid%MaxTau)
   deallocate(LUT_Grid%MinTau)
   deallocate(LUT_Grid%dTau)
   deallocate(LUT_Grid%nTau)
   deallocate(LUT_Grid%MaxRe)
   deallocate(LUT_Grid%MinRe)
   deallocate(LUT_Grid%dRe)
   deallocate(LUT_Grid%nRe)
   deallocate(LUT_Grid%MaxSatzen)
   deallocate(LUT_Grid%MinSatzen)
   deallocate(LUT_Grid%dSatzen)
   deallocate(LUT_Grid%nSatzen)
   deallocate(LUT_Grid%MaxSolzen)
   deallocate(LUT_Grid%MinSolzen)
   deallocate(LUT_Grid%dSolzen)
   deallocate(LUT_Grid%nSolzen)
   deallocate(LUT_Grid%MaxRelazi)
   deallocate(LUT_Grid%MinRelazi)
   deallocate(LUT_Grid%dRelazi)
   deallocate(LUT_Grid%nRelazi)
   deallocate(LUT_Grid%Tau)
   deallocate(LUT_Grid%Re)
   deallocate(LUT_Grid%Solzen)
   deallocate(LUT_Grid%Satzen)
   deallocate(LUT_Grid%Relazi)

end subroutine Dealloc_LUT_Grid


!-------------------------------------------------------------------------------
! Name:
!    Dealloc_SAD_LUT
!
! Purpose:
!    Deallocate the SAD_LUT internal arrays at end of ECP execution.
!
! Arguments:
!    Name    Type           In/Out/Both  Description
!    Ctrl    struct         In           Control structure
!    SAD_LUT array of alloc structs      Array of SAD_LUT_t structs
!                           In
!    status  int            Out          Error status
!
! Algorithm:
!    Deallocates all arrays in each of the SAD_LUT structures
!    No error handling is done at present. Since this routine is executed once
!    at the end of execution it is unclear what action should be taken in case
!    of error.
!
! Local variables:
!    Name Type Description
!
! History:
!    24th Oct 2001, Andy Smith: Original version
!    **************** ECV work starts here *************************************
!    22nd Mar 2011, Andy Smith:
!       Remove phase change, phase 2. SAD_CloudClass no longer allocated to N
!       cloud classes, only 1 cloud class per run.
!       SAD_LUT is also now reduced from dimension N cloud classes to 1.
!    11th Jun 2011, Caroline Poulsen: Deallocated new LUT variables
!    13th Dec 2011, Caroline Poulsen: Deallocated new LUT wavelenght variables
!    12th Jan 2014, Greg McGarragh: Added some missing deallocates.
!    16th Jan 2014, Greg McGarragh: Added deallocation of
!       SAD_LUT%table_used_for_channel.
!    27th May 2014, Greg McGarragh: Some cleanup.
!    10th Oct 2014, Greg McGarragh: Separate out Grid deallocations to be
!       consistent with allocation routine.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Dealloc_SAD_LUT(Ctrl, SAD_LUT)

   use Ctrl_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SAD_LUT_t), intent(inout) :: SAD_LUT

   deallocate(SAD_LUT%Wavelength)

   deallocate(SAD_LUT%table_used_for_channel)

   call Dealloc_LUT_Grid(SAD_LUT%Grid)

   deallocate(SAD_LUT%Rd)
   deallocate(SAD_LUT%Rfd)
   deallocate(SAD_LUT%Td)
   deallocate(SAD_LUT%Tfd)

   if (Ctrl%Ind%NSolar > 0) then
      deallocate(SAD_LUT%Rbd)
      deallocate(SAD_LUT%Tb)
      deallocate(SAD_LUT%Tbd)
      deallocate(SAD_LUT%Tfbd)
   end if

   if (Ctrl%Ind%NThermal > 0) then
      deallocate(SAD_LUT%Em)
   end if

end subroutine Dealloc_SAD_LUT
