!-------------------------------------------------------------------------------
! Name: int_lut_tausatreonsol.F90
!
! Purpose:
! Interpolates values from a LUT array with dimensions (channel, Tau, SolZen,
! Re) to give the value of the LUT array at a single value of Tau, SatZen, Re
! across all channels.
!
! Description and Algorithm details:
! The subroutine is passed an array of data in 4 dimensions, plus info
! relating to the Tau, Re and SolZen grids corresponding to the LUT array
! points (the third dimension in the LUT array is the channel number, which
! is not interpolated).
!
! The subroutine calculates the value of the LUT at the current (Tau, SatZen,
! Re) plus the gradients in Tau and Re.
!
! See IntLUTTauSatRe. This routine is an exact copy with all values relating
! to SolZen replaced by the equivalents for SatZenSolZen (e.g. GZero%Sa1). A
! single, more general routine could have been used but this may have been
! less easy to follow.
!
! Arguments:
! Name   Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! F      real array In          Function to be interpolated, i.e. array of
!                               LUT values with dimensions (channels, Tau,
!                               SolZen, Re).
! NChans int        In          Number of channels in F
! Grid   struct     In          LUT Grid data: see SADLUT.F90
!                               Includes the grid values in Tau, Re, SolZen,
!                               no. of values and step size
! Gzero  struct     In          Structure containing "zero'th" point indices,
!                               i.e. LUT grid array indices for the nearest
!                               neighbour point, plus dT, dR, dS values:
!                               fraction of grid step from zero'th point to
!                               the Tau, SolZen, Re value we want.
! Ctrl   struct     In          Control structure: Needed for numbers of
!                               channels and their indices
! FInt   real array Both        The interpolated values of F for all required
!                               channels.
! FGrads real array Both        Interpolated gradient values in Tau and Re
!                               for all channels.
! chan_to_ctrl_index             Indices of input chs within Ctrl arrays
!        int array  In
! chan_to_spixel_index           Indices of input chs within SPixel arrays
!        int array  In
! status int        Out         Standard status code set by ORAC routines
!
! History:
! 2014/05/22, GM: Adapted from IntLUTTauSatRe.F90.
! 2014/10/16, GM: Moved a large amount of code that was common to all IntLUT*
!    subroutines into Int_LUT_Common()
! 2015/01/13, AP: Switch to array-based channel indexing rather than using
!    offsets.
! 2017/01/17, GM: Changes related to simplification of the indexing of the LUT
!    grid and the GZero parameters.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauSatReOnSol(F, NChans, Grid, GZero, Ctrl, FInt, FGrads, &
     chan_to_ctrl_index, chan_to_spixel_index, status)

   use Ctrl_m
   use GZero_m
   use Int_Routines_m
   use SAD_LUT_m

   implicit none

   ! Argument declarations

   real, dimension(:,:,:,:), intent(in) :: F
                                           ! The array to be interpolated.
   integer,                intent(in)   :: NChans
   type(LUT_Grid_t),       intent(in)   :: Grid
                                           ! LUT grid data
   type(GZero_t),          intent(in)   :: GZero
                                           ! Struct containing "zero'th" grid
                                           ! points
   type(Ctrl_t),           intent(in)   :: Ctrl
   real, dimension(:),     intent(out)  :: FInt
                                           ! Interpolated value of F at the
                                           ! required Tau, Re values, (1 value
                                           ! per channel).
   real, dimension(:,:),   intent(out)  :: FGrads
                                           ! Gradients of F wrt Tau and Re at
                                           ! required Tau, Re values, (1 value
                                           ! per channel).
   integer,                intent(in)   :: chan_to_ctrl_index(:)
                                           ! Indices for input chs wrt Ctrl
   integer,                intent(in)   :: chan_to_spixel_index(:)
                                           ! Indices for input chs wrt SPixel
   integer,                intent(out)  :: status

   ! Local variables

   integer                               :: i, ii, ii2, j, jj, k, kk
   integer, dimension(-1:2)              :: T_index
   integer, dimension(-1:2)              :: R_index
   real, dimension(size(FInt),-1:2,-1:2) :: G ! A Matrix of dimension NTau,Nre
                                              ! used to store array only
                                              ! interpolated to current viewing
                                              ! geometry

   status = 0

   ! Construct the input to Int_LUT_Common(): Function values at four LUT points
   ! around our X
   do i = 1, NChans
      ii = chan_to_ctrl_index(i)
      ii2 = chan_to_spixel_index(i)

      T_index(-1) = GZero%iTm1(ii2)
      T_index( 0) = GZero%iT0 (ii2)
      T_index( 1) = GZero%iT1 (ii2)
      T_index( 2) = GZero%iTp1(ii2)
      R_index(-1) = GZero%iRm1(ii2)
      R_index( 0) = GZero%iR0 (ii2)
      R_index( 1) = GZero%iR1 (ii2)
      R_index( 2) = GZero%iRp1(ii2)

      do j = iXm1, iXp1
         jj = T_index(j)
         do k = iXm1, iXp1
            kk = R_index(k)
            G(i,j,k) = (GZero%SaSo1 (ii2)  * F(ii,jj,GZero%iSaZSoZ0(ii2),kk)) + &
                       (GZero%dSaZSoZ(ii2) * F(ii,jj,GZero%iSaZSoZ1(ii2),kk))
         end do
      end do
   end do

   call Int_LUT_Common(Ctrl, NChans, Grid, GZero, G, FInt, FGrads, &
                       chan_to_ctrl_index, chan_to_spixel_index, status)

end subroutine Int_LUT_TauSatReOnSol
