!-------------------------------------------------------------------------------
! Name:
!    Alloc_SAD_LUT
!
! Purpose:
!
! Arguments:
!    Name Type In/Out/Both Description
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    10th Oct 2014, Greg McGarragh: Original version
!    9/1/2015 CP added Rfbd
! Bugs:
!    None known.
!
! $Id: AllocSADLUT.F90 2293 2014-08-13 08:56:10Z gmcgarragh $
!
!-------------------------------------------------------------------------------

subroutine Alloc_LUT_Grid(Ctrl, LUT_Grid)

   use Ctrl_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(LUT_Grid_t), intent(out)   :: LUT_Grid

   allocate(LUT_Grid%MaxTau(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%MinTau(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%dTau(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%nTau(Ctrl%Ind%Ny,maxcrprops))
   LUT_Grid%nTau=LUT_Grid%nmaxtau

   allocate(LUT_Grid%MaxRe(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%MinRe(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%dRe(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%nRe(Ctrl%Ind%Ny,maxcrprops))
   LUT_Grid%nRe=LUT_Grid%nmaxre

   allocate(LUT_Grid%MaxSatzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%MinSatzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%dSatzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%nSatzen(Ctrl%Ind%Ny,maxcrprops))
   LUT_Grid%nSatzen=LUT_Grid%nmaxsatzen

   allocate(LUT_Grid%MaxSolzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%MinSolzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%dSolzen(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%nSolzen(Ctrl%Ind%Ny,maxcrprops))
   LUT_Grid%nSolzen=LUT_Grid%nmaxsolzen

   allocate(LUT_Grid%MaxRelazi(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%MinRelazi(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%dRelazi(Ctrl%Ind%Ny,maxcrprops))
   allocate(LUT_Grid%nRelazi(Ctrl%Ind%Ny,maxcrprops))
   LUT_Grid%nRelazi=LUT_Grid%nmaxrelazi

   allocate(LUT_Grid%Tau(Ctrl%Ind%Ny,LUT_Grid%nmaxtau,maxcrprops))
   allocate(LUT_Grid%Re(Ctrl%Ind%Ny,LUT_Grid%nmaxre,maxcrprops))
   allocate(LUT_Grid%Solzen(Ctrl%Ind%Ny,LUT_Grid%nmaxsolzen,maxcrprops))
   allocate(LUT_Grid%Satzen(Ctrl%Ind%Ny,LUT_Grid%nmaxsatzen,maxcrprops))
   allocate(LUT_Grid%Relazi(Ctrl%Ind%Ny,LUT_Grid%nmaxrelazi,maxcrprops))

end subroutine Alloc_LUT_Grid


subroutine Alloc_SAD_LUT(Ctrl, SAD_LUT)

   use Ctrl_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SAD_LUT_t), intent(inout) :: SAD_LUT

   ! All arrays are allocated big enough to hold the total number of channels
   ! selected, even though not all arrays hold both thermal and solar data. This
   ! makes it easier to keep track of where each channel's data is. All other
   ! dimensions (tau etc) are set to the max. possible size as nTau etc can
   ! vary with channel number.

   allocate(SAD_LUT%Wavelength(Ctrl%Ind%Ny))

   allocate(SAD_LUT%table_used_for_channel(Ctrl%Ind%Ny, maxcrprops))

   call Alloc_LUT_Grid(Ctrl, SAD_LUT%Grid)

   allocate(SAD_LUT%Rd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
            SAD_LUT%Grid%nmaxre))
   allocate(SAD_LUT%Rfd(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxre))
   allocate(SAD_LUT%Td(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
            SAD_LUT%Grid%nmaxre))
   allocate(SAD_LUT%Tfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxre))

   if (Ctrl%Ind%NSolar > 0) then
      allocate(SAD_LUT%Rbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
               SAD_LUT%Grid%NmaxSolzen, SAD_LUT%Grid%nmaxrelazi, SAD_LUT%Grid%nmaxre))
      allocate(SAD_LUT%Tb(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxsolzen, &
               SAD_LUT%Grid%nmaxre))
      allocate(SAD_LUT%Tbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
               SAD_LUT%Grid%NmaxSolzen, SAD_LUT%Grid%nmaxrelazi, SAD_LUT%Grid%nmaxre))
      allocate(SAD_LUT%Tfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxsolzen, &
               SAD_LUT%Grid%nmaxre))
      allocate(SAD_LUT%Rfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxsolzen, &
               SAD_LUT%Grid%nmaxre))

   end if

   if (Ctrl%Ind%NThermal > 0) then
      allocate(SAD_LUT%Em(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
               SAD_LUT%Grid%nmaxre))
   end if

end subroutine Alloc_SAD_LUT
