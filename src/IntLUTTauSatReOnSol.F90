!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_TauSatReOnSol
!
! Purpose:
!    Interpolates values from a LUT array with dimensions (channel, Tau, SolZen,
!    Re) to give the value of the LUT array at a single value of Tau, SatZen, Re
!    across all channels.
!
! Description:
!    The subroutine is passed an array of data in 4 dimensions, plus info
!    relating to the Tau, Re and SolZen grids corresponding to the LUT array
!    points (the third dimension in the LUT array is the channel number, which
!    is not interpolated).
!
!    The subroutine calculates the value of the LUT at the current (Tau, SatZen,
!    Re) plus the gradients in Tau and Re.
!
! Arguments:
!    Name   Type       In/Out/Both Description
!    F      real array In          Function to be interpolated, i.e. array of
!                                  LUT values with dimensions (channels, Tau,
!                                  SolZen, Re).
!    Grid   struct     In          LUT Grid data: see SADLUT.F90
!                                  Includes the grid values in Tau, Re, SolZen,
!                                  no. of values and step size
!    Gzero  struct     In          Structure containing "zero'th" point indices,
!                                  i.e. LUT grid array indices for the nearest
!                                  neighbour point, plus dT, dR, dS values:
!                                  fraction of grid step from zero'th point to
!                                  the Tau, SolZen, Re value we want.
!    Ctrl   struct     In          Control structure: Needed for numbers of
!                                  channels and their indices
!    Ind    struct     In          The index structure from the SPixel array
!    FInt   real array Both        The interpolated values of F for all required
!                                  channels.
!    FGrads real array Both        Interpolated gradient values in Tau and Re
!                                  for all channels.
!    status int        Out         Standard status code set by ECP routines
!
! Algorithm:
!    See IntLUTTauSatRe. This routine is an exact copy with all values relating
!    to SolZen replaced by the equivalents for SatZenSolZen (e.g. GZero%Sa1). A
!    single, more general routine could have been used but this may have been
!    less easy to follow.
!
! Local variables:
!    Name Type Description
!
! History:
!    22nd May 2014, Greg McGarragh: Adapted from IntLUTTauSatRe.F90.
!    16th Oct 2014, Greg McGarragh:
!       Moved a large amount of code that was common to all IntLUT* subroutines
!       into Int_LUT_Common()
!
! Bugs:
!    None known.
!
! $Id: IntLUTTauSatRe.F90 2160 2014-05-22 16:53:08Z gmcgarragh $
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauSatReOnSol(F, Grid, GZero, Ctrl, FInt, FGrads, iCRP, &
   i_chan_to_ctrl_offset, i_chan_to_spixel_offset, status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations

   real, dimension(:,:,:,:), intent(in) :: F
                                           ! The array to be interpolated.
   type(LUT_Grid_t),       intent(in)   :: Grid
                                           ! LUT grid data
   type(GZero_t),          intent(in)   :: GZero
                                           ! Struct containing "zero'th" grid
                                           ! points
   type(CTRL_t),           intent(in)   :: Ctrl
   real, dimension(:),     intent(out)  :: FInt
                                           ! Interpolated value of F at the
					   ! required Tau, Re values, (1 value
                                           ! per channel).
   real, dimension(:,:),   intent(out)  :: FGrads
                                           ! Gradients of F wrt Tau and Re at
					   ! required Tau, Re values, (1 value
                                           ! per channel).
   integer,                intent(in)   :: iCRP
   integer,                intent(in)   :: i_chan_to_ctrl_offset
   integer,                intent(in)   :: i_chan_to_spixel_offset
   integer,                intent(out)  :: status

   ! Local variables

   integer                               :: i, ii, ii2, j, jj, k, kk
   integer                               :: NChans
   integer, parameter                    :: iXm1 = -1
   integer, parameter                    :: iX0  =  0
   integer, parameter                    :: iX1  =  1
   integer, parameter                    :: iXp1 =  2
   integer, dimension(-1:2)              :: T_index
   integer, dimension(-1:2)              :: R_index
   real, dimension(size(FInt),-1:2,-1:2) :: G ! A Matrix of dimension NTau,Nre
                                              ! used to store array only
                                              ! interpolated to current viewing
                                              ! geometry

   status = 0

   NChans = size(F,1)

   ! Construct the input Int_LUT_Common(): Function values at four LUT points
   ! around our X
   do i = 1, NChans
      ii = i_chan_to_ctrl_offset + i
      ii2 = i_chan_to_spixel_offset + i

      T_index(-1) = GZero%iTm1(ii2,iCRP)
      T_index( 0) = GZero%iT0 (ii2,iCRP)
      T_index( 1) = GZero%iT1 (ii2,iCRP)
      T_index( 2) = GZero%iTp1(ii2,iCRP)
      R_index(-1) = GZero%iRm1(ii2,iCRP)
      R_index( 0) = GZero%iR0 (ii2,iCRP)
      R_index( 1) = GZero%iR1 (ii2,iCRP)
      R_index( 2) = GZero%iRp1(ii2,iCRP)

      do j = iXm1, iXp1
         jj = T_index(j)
         do k = iXm1, iXp1
            kk = R_index(k)
            G(i,j,k) = (GZero%SaSo1 (ii2,iCRP)  * F(i,jj,GZero%iSaZSoZ0(ii2,iCRP),kk)) + &
                       (GZero%dSaZSoZ(ii2,iCRP) * F(i,jj,GZero%iSaZSoZ1(ii2,iCRP),kk))
         end do
      end do
   end do

   call Int_LUT_Common(Ctrl, NChans, iCRP, Grid, GZero, G, FInt, FGrads, &
                       i_chan_to_ctrl_offset, i_chan_to_spixel_offset, status)

end subroutine Int_LUT_TauSatReOnSol
