!-------------------------------------------------------------------------------
! Name: DeallocSADLUT.F90
!
! Purpose:
! Two routines for deallocating structures related to LUT gridding in
! SAD_LUT module.
!
! History:
! 2001/10/24, AS: Original version
!    **************** ECV work starts here *************************************
! 2011/03/22, AS: Remove phase change, phase 2. SAD_CloudClass no longer
!    allocated to N cloud classes, only 1 cloud class per run. SAD_LUT is also
!    now reduced from dimension N cloud classes to 1.
! 2011/06/11, CP: Deallocated new LUT variables
! 2011/12/13, CP: Deallocated new LUT wavelenght variables
! 2014/01/12, GM: Added some missing deallocates.
! 2014/01/16, GM: Added deallocation of SAD_LUT%table_used_for_channel.
! 2014/05/27, GM: Some cleanup.
! 2014/10/10, GM: Separate out Grid deallocations to be consistent with
!    allocation routine.
! 2015/01/09, CP: Added Rfbd.
! 2015/10/19, GM: Added Bext for Ctrl%do_CTP_correction.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the LUT grid wrt LUT
!    type and channel.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: Dealloc_LUT_Grid
!
! Purpose:
! Deallocate the LUT_Grid arrays at end of ORAC execution.
!
! Algorithm:
! 1) Deallocate all arrays.
!
! Arguments:
! Name     Type   In/Out/Both Description
!-------------------------------------------------------------------------------
! LUT_Grid struct Both        Structure of LUT grid parameters
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Dealloc_LUT_Grid(LUT_Grid)

   implicit none

   ! Declare arguments

   type(LUT_Grid_t), intent(inout) :: LUT_Grid

   deallocate(LUT_Grid%Tau)
   deallocate(LUT_Grid%Re)
   deallocate(LUT_Grid%Solzen)
   deallocate(LUT_Grid%Satzen)
   deallocate(LUT_Grid%Relazi)

end subroutine Dealloc_LUT_Grid


!-------------------------------------------------------------------------------
! Name: Dealloc_SAD_LUT
!
! Purpose:
! Deallocate the SAD_LUT internal arrays at end of ORAC execution.
!
! Algorithm:
! 1) Deallocates all arrays in each of the SAD_LUT structures
!
! Arguments:
! Name    Type           In/Out/Both  Description
!-------------------------------------------------------------------------------
! Ctrl    struct         In           Control structure
! SAD_LUT array of alloc structs      Array of SAD_LUT_t structs
!                         In
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Dealloc_SAD_LUT(Ctrl, SAD_LUT)

   use Ctrl_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SAD_LUT_t), intent(inout) :: SAD_LUT

   deallocate(SAD_LUT%Wavelength)

   call Dealloc_LUT_Grid(SAD_LUT%Grid)

   deallocate(SAD_LUT%Rd)
   deallocate(SAD_LUT%Rfd)
   deallocate(SAD_LUT%Td)
   deallocate(SAD_LUT%Tfd)

   if (Ctrl%Ind%NSolar > 0) then
      deallocate(SAD_LUT%Rbd)
      deallocate(SAD_LUT%Rfbd)
      deallocate(SAD_LUT%Tb)
      deallocate(SAD_LUT%Tbd)
      deallocate(SAD_LUT%Tfbd)
   end if

   if (Ctrl%Ind%NThermal > 0) then
      deallocate(SAD_LUT%Em)

      if (Ctrl%do_CTX_correction .and. Ctrl%Class .eq. ClsCldIce) then
         deallocate(SAD_LUT%Bext)
      end if
   end if

   if (Ctrl%Approach == AppAerOx .or. Ctrl%Approach == AppAerSw .or. &
       Ctrl%Approach == AppAerO1) then
      deallocate(SAD_LUT%BextRat)
   end if

end subroutine Dealloc_SAD_LUT
