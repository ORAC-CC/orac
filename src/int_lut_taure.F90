!-------------------------------------------------------------------------------
! Name: int_lut_taure.F90
!
! Purpose:
! Interpolates values from a LUT array with dimensions (channel, Tau, Re)
! using either linear or bicubic interpolation
! (Bicubic algorithm taken from Numerical Recipes in Fortran 77).
!
! Description and Algorithm details:
! The subroutine is passed an array of data in 3 dimensions, plus info
! relating to the Tau and Re grids corresponding to the LUT array points
! (the third dimension in the LUT array is the channel number, which is not
! interpolated).
!
! The subroutine calculates the value of the LUT at the current Tau, Re, plus
! the gradients in Tau and Re.
!
! The routine is passed the description of the LUT grid (Grid),
! with the "zero'th" point grid data (in GZero: the calling routine
! populates GZero).
!    The zero'th point is the bottom left hand corner of the Tau, Re grid
!    cell containing the required Tau, Re. GZero also contains the array
!    indices for the top right hand corner, plus the following data relating
!    to the required Tau, Re values:
!       dT: distance from zero'th point to required Tau value as a fraction
!           of the grid step size in the Tau direction
!       dR: equivalent to dT in the Re grid
!       The next-nearest neighbour LUT indices to the required point (which
!       are used in finite difference gradient calculation). At the edge of
!       LUT these indices are set to the nearest neighbour value and an
!       uncentred difference is used
!       The denominator values of the finite difference calculations are
!       also included in GZero - these will be the same as the grid spacing
!       at the edge of the LUT, and twice this elsewhere.
!
! Note the routine is NOT passed the required Tau, Re values: the info in
! GZero and Grid is sufficient.
!
! The subroutine populates 4 arrays with:
!    - The value of the LUT function at the 4 nearest LUT vertices
!    - The gradient wrt to Tau at these points
!    - The gradient wrt to Re at these points
!    - The second order cross-derivative wrt Tau and Re at these points
! These, and the dT and dR values from the GZero structure, are passed to the
! bicubic interpolation routine.
! The resulting interpolated gradients are re-scaled by the grid spacing,
! since the interpolation routine works on the (0,1) interval.
!
! Arguments:
! Name   Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! F      real array In          Function to be interpolated, i.e. 3-d array of
!                               LUT values with dimensions channels, Tau, Re.
! NChans int        In          Number of channels in F
! Grid   struct     In          LUT Grid data: see SADLUT.F90. Includes the grid
!                               values in Tau and Re, no. of values and step size
! Gzero  struct     In          Structure containing "zero'th" point indices i.e.
!                               LUT grid array indices for the nearest neighbour
!                               point, plus dT, dR values: fraction of grid step
!                               from zero'th point to the Tau, Re value we want.
!                               Also contains the next-nearest neighbour indices
!                               and the denominator for calculating finite
!                               difference gradients
! FInt   real array Both        The interpolated values of F for all required
!                               channels.
! FGrads real array Both        Interpolated gradient values in Tau and Re for
!                               all channels.
! chan_to_ctrl_index            Indices of input chs within Ctrl arrays
!        int array  In
! chan_to_spixel_index          Indices of input chs within SPixel arrays
!        int array  In
! status int        Out         Standard status code set by ORAC routines
!
! History:
! 2000/10/26, AS: Original version
! 2000/11/16, AS: Updated arguments: more data passed via structs, allows for
!    easier generalisation to more dimensions and improves performance as some
!    values used by several Int routines are calculated once only by the
!    calling routine.
! 2000/12/22, PW: Removed redundant Chans argument; all channels implicit in the
!    LUT are interpolated.
! 2001/01/19, AS: Comments updated.
! 2011/09/05, CA: Added bicubic interpolation 20th Jan 2012 changed definition
!    of Y to remove contiguous array warning!
! 2012/02/07, CA: Ctrl struct now passed to routine
! 2012/02/07, CA: Input structures Ctrl, GZero, Grid now have intent(in)
! 2012/07/08, CP: Fixed non contiguous array.
! 2013/12/03, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/01/21, GM: Cleaned up code.
! 2014/01/23, GM: Performance improvements.  Primarily through elimination of
!    unused memory references and computations.
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

subroutine Int_LUT_TauRe(F, NChans, Grid, GZero, Ctrl, FInt, FGrads, &
     chan_to_ctrl_index, chan_to_spixel_index, status)

   use Ctrl_m
   use GZero_m
   use Int_Routines_m
   use SAD_LUT_m

   implicit none

   ! Argument declarations

   real, dimension(:,:,:), intent(in)   :: F
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
   do i=1,NChans
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
            G(i,j,k) = F(ii,jj,kk)
         end do
      end do
   end do

   call Int_LUT_Common(Ctrl, NChans, Grid, GZero, G, FInt, FGrads, &
                       chan_to_ctrl_index, chan_to_spixel_index, status)

end subroutine Int_LUT_TauRe
