!-------------------------------------------------------------------------------
! Name: alloc_sad_lut.F90
!
! Purpose:
! Two routines to allocate the structures to hold LUT data within SAD_LUT module
!
! History:
! 2014/10/10, GM: Original version
! 2015/01/09, CP: Added Rfbd.
! 2015/10/19, GM: Added Bext for Ctrl%do_CTP_correction.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the LUT grid wrt LUT
!    type and channel.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: Alloc_LUT_Grid
!
! Purpose:
! Allocate arrays to hold LUT grid information.
!
! Description and Algorithm details:
! 1) Allocate all arrays in structure
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct In          Control structure
! LUT_Grid struct Out         RTM_Pc structure
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Alloc_LUT_Grid(Ctrl, LUT_Grid)

   use Ctrl_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)  :: Ctrl
   type(LUT_Grid_t), intent(out) :: LUT_Grid

   allocate(LUT_Grid%Tau(LUT_Grid%NMaxTau))
   allocate(LUT_Grid%Re(LUT_Grid%NMaxRe))
   allocate(LUT_Grid%Solzen(LUT_Grid%NMaxSolZen))
   allocate(LUT_Grid%Satzen(LUT_Grid%NMaxSatZen))
   allocate(LUT_Grid%Relazi(LUT_Grid%NMaxRelAzi))

end subroutine Alloc_LUT_Grid


!-------------------------------------------------------------------------------
! Name: Alloc_SAD_LUT
!
! Purpose:
! Allocate arrays to hold contents of LUTs
!
! Description and Algorithm details:
! 1) Allocate all common arrays in structure.
! 2) Allocate solar and/or thermal channel structures as required.
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct In          Control structure
! SAD_LUT  struct Both        Look-up table structure
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Alloc_SAD_LUT(Ctrl, SAD_LUT)

   use Ctrl_m

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

   call Alloc_LUT_Grid(Ctrl, SAD_LUT%Grid)

   if (Ctrl%do_CTX_correction) then
      allocate(SAD_LUT%Bext(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxRe))
   end if

   allocate(SAD_LUT%Rd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
            SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%NMaxRe))
   allocate(SAD_LUT%Rfd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxRe))
   allocate(SAD_LUT%Td(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%NMaxRe))
   allocate(SAD_LUT%Tfd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxRe))

   if (Ctrl%Ind%NSolar > 0) then
      allocate(SAD_LUT%Rbd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%NMaxSolzen, &
               SAD_LUT%Grid%NMaxRelAzi, SAD_LUT%Grid%NMaxRe))
      allocate(SAD_LUT%Rfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSolZen, SAD_LUT%Grid%NMaxRe))
      allocate(SAD_LUT%Tb(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSolZen, SAD_LUT%Grid%NMaxRe))
      allocate(SAD_LUT%Tbd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%NmaxSolzen, &
               SAD_LUT%Grid%NMaxRelAzi, SAD_LUT%Grid%NMaxRe))
      allocate(SAD_LUT%Tfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSolZen, SAD_LUT%Grid%NMaxRe))
   end if

   if (Ctrl%Ind%NThermal > 0) then
      allocate(SAD_LUT%Em(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxTau, &
               SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%NMaxRe))
   end if

   if (Ctrl%Approach == AppAerOx .or. Ctrl%Approach == AppAerSw .or. &
       Ctrl%Approach == AppAerO1) then
      allocate(SAD_LUT%BextRat(Ctrl%Ind%Ny, SAD_LUT%Grid%NMaxRe))
   end if

end subroutine Alloc_SAD_LUT
