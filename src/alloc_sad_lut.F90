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
! 2021/03/08, AP: Gather grid dimensions into LUT_Grid_t
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
subroutine Alloc_LUT_Grid(LUT_Grid)

   implicit none

   ! Declare arguments

   type(LUT_Grid_t), intent(out) :: LUT_Grid

   LUT_Grid%Tau%NMax    = 20
   LUT_Grid%Re%NMax     = 35
   LUT_Grid%SolZen%NMax = 20
   LUT_Grid%SatZen%NMax = 20
   LUT_Grid%RelAzi%NMax = 20

   allocate(LUT_Grid%Tau%x(LUT_Grid%Tau%NMax))
   allocate(LUT_Grid%Re%x(LUT_Grid%Re%NMax))
   allocate(LUT_Grid%Solzen%x(LUT_Grid%SolZen%NMax))
   allocate(LUT_Grid%Satzen%x(LUT_Grid%SatZen%NMax))
   allocate(LUT_Grid%Relazi%x(LUT_Grid%RelAzi%NMax))

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

   call Alloc_LUT_Grid(SAD_LUT%Grid)

   allocate(SAD_LUT%Rd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
                       SAD_LUT%Grid%SatZen%NMax, SAD_LUT%Grid%Re%NMax))
   allocate(SAD_LUT%Rfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
                        SAD_LUT%Grid%Re%NMax))
   allocate(SAD_LUT%Td(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
                       SAD_LUT%Grid%SatZen%NMax, SAD_LUT%Grid%Re%NMax))
   allocate(SAD_LUT%Tfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
                        SAD_LUT%Grid%Re%NMax))

   if (Ctrl%Ind%NSolar > 0) then
      allocate(SAD_LUT%Rbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SatZen%NMax, SAD_LUT%Grid%Solzen%NMax, &
               SAD_LUT%Grid%RelAzi%NMax, SAD_LUT%Grid%Re%NMax))
      allocate(SAD_LUT%Rfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SolZen%NMax, SAD_LUT%Grid%Re%NMax))
      allocate(SAD_LUT%Tb(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SolZen%NMax, SAD_LUT%Grid%Re%NMax))
      allocate(SAD_LUT%Tbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SatZen%NMax, SAD_LUT%Grid%Solzen%Nmax, &
               SAD_LUT%Grid%RelAzi%NMax, SAD_LUT%Grid%Re%NMax))
      allocate(SAD_LUT%Tfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SolZen%NMax, SAD_LUT%Grid%Re%NMax))
   end if

   if (Ctrl%Ind%NThermal > 0) then
      allocate(SAD_LUT%Em(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
               SAD_LUT%Grid%SatZen%NMax, SAD_LUT%Grid%Re%NMax))

      if (Ctrl%do_CTX_correction .and. Ctrl%Class .eq. ClsCldIce) then
         allocate(SAD_LUT%Bext(Ctrl%Ind%Ny, SAD_LUT%Grid%Tau%NMax, &
                  SAD_LUT%Grid%Re%NMax))
      end if
   end if

   if (Ctrl%Approach == AppAerOx .or. Ctrl%Approach == AppAerSw .or. &
       Ctrl%Approach == AppAerO1) then
      allocate(SAD_LUT%BextRat(Ctrl%Ind%Ny, SAD_LUT%Grid%Re%NMax))
   end if

end subroutine Alloc_SAD_LUT
