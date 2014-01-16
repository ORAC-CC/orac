! Name:
!    Dealloc_SAD_LUT
!
! Purpose:
!    Deallocate the SAD_LUT internal arrays at end of ECP execution.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    SAD_LUT     array of alloc structs  Array of SAD_LUT_t structs 
!                               In       
!    status      int            Out      Error status
!    
! Algorithm:
!    Deallocates all arrays in each of the SAD_LUT structures
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
!   ******************************** ECV work starts here ********************
!   22nd Mar 2011, Andy Smith:
!      Remove phase change, phase 2. SAD_CloudClass no longer allocated to 
!      N cloud classes, only 1 cloud class per run. 
!      SAD_LUT is also now reduced from dimension N cloud classes to 1. 
!   11 June 2011, Caroline poulsen: deallocated new LUT variables
!   13 Dec 2011, Caroline poulsen: deallocated new LUT wavelenght variables
!   12th Jan 2014, Greg McGarragh: Added some missing deallocates.
!   16th Jan 2014, Greg McGarragh: Added deallocation of SAD_LUT%table_used_for_channel.
!
! Bugs:
!    None known.
!
! $Id: DeallocSADLUT.f90 74 2011-08-16 16:11:53Z capoulse $
!
!------------------------------------------------------------------------------------
subroutine Dealloc_SAD_LUT(Ctrl, SAD_LUT, status)

   use Ctrl_def
   use SAD_LUT_def

   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(in)       :: Ctrl
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  
   integer, intent(inout)         :: status

   deallocate(SAD_LUT%Wavelength)

   deallocate(SAD_LUT%Td)
   deallocate(SAD_LUT%Tfd)
   deallocate(SAD_LUT%Rd)
   deallocate(SAD_LUT%Rfd)
   
   if (Ctrl%Ind%NSolar > 0) then
      deallocate(SAD_LUT%Rbd)
      deallocate(SAD_LUT%Tbd)
      deallocate(SAD_LUT%Tb)
      deallocate(SAD_LUT%Tfbd)
   end if
   
   if (Ctrl%Ind%NThermal > 0) then
      deallocate(SAD_LUT%Em)
   end if

   deallocate(SAD_LUT%Grid%Tau)
   deallocate(SAD_LUT%Grid%Re)
   deallocate(SAD_LUT%Grid%Satzen)
   deallocate(SAD_LUT%Grid%Solzen)
   deallocate(SAD_LUT%Grid%relazi)

   deallocate(SAD_LUT%Grid%MaxTau)
   deallocate(SAD_LUT%Grid%MinTau)
   deallocate(SAD_LUT%Grid%dTau)
   deallocate(SAD_LUT%Grid%nTau)
   deallocate(SAD_LUT%Grid%MaxRe)
   deallocate(SAD_LUT%Grid%MinRe)
   deallocate(SAD_LUT%Grid%dRe)
   deallocate(SAD_LUT%Grid%nRe)
   deallocate(SAD_LUT%Grid%MaxSatzen)
   deallocate(SAD_LUT%Grid%MinSatzen)
   deallocate(SAD_LUT%Grid%dSatzen)
   deallocate(SAD_LUT%Grid%nSatzen)
   deallocate(SAD_LUT%Grid%MaxSolzen)
   deallocate(SAD_LUT%Grid%MinSolzen)
   deallocate(SAD_LUT%Grid%dSolzen)
   deallocate(SAD_LUT%Grid%nSolzen)
   deallocate(SAD_LUT%Grid%MaxRelazi)
   deallocate(SAD_LUT%Grid%MinRelazi)
   deallocate(SAD_LUT%Grid%dRelazi)
   deallocate(SAD_LUT%Grid%nRelazi)

   deallocate(SAD_LUT%table_used_for_channel)

end subroutine Dealloc_SAD_LUT
