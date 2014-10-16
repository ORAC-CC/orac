!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_Tau_Re
!
! Purpose:
!    Interpolates values from a LUT array with dimensions (channel, Tau, Re)
!    using either linear or bicubic interpolation
!    (Bicubic algorithm taken from Numerical Recipes in Fortran 77).
!
! Description:
!    The subroutine is passed an array of data in 3 dimensions, plus info
!    relating to the Tau and Re grids corresponding to the LUT array points
!    (the third dimension in the LUT array is the channel number, which is not
!    interpolated).
!
!    The subroutine calculates the value of the LUT at the current Tau, Re, plus
!    the gradients in Tau and Re.
!
! Arguments:
!    Name   Type    In/ut/Both Description
!    F      real array In      Function to be interpolated, i.e. 3-d array of
!                              LUT values with dimensions channels, Tau, Re.
!    Grid   struct     In      LUT Grid data: see SADLUT.F90. Includes the grid
!                              values in Tau and Re, no. of values and step size
!    Gzero  struct     In      Structure containing "zero'th" point indices, i.e.
!                              LUT grid array indices for the nearest neighbour
!                              point, plus dT, dR values: fraction of grid step
!                              from zero'th point to the Tau, Re value we want.
!                              Also contains the next-nearest neighbour indices
!                              and the denominator for calculating finite
!                              difference gradients
!    Ind    struct     In      The index structure from the SPixel array
!    FInt   real array Both    The interpolated values of F for all required
!                              channels.
!    FGrads real array Both    Interpolated gradient values in Tau and Re for
!                              all channels.
!    status int        Out     Standard status code set by ECP routines
!
! Algorithm:
!    The routine is passed the description of the LUT grid (Grid),
!    with the "zero'th" point grid data (in GZero: the calling routine
!    populates GZero).
!       The zero'th point is the bottom left hand corner of the Tau, Re grid
!       cell containing the required Tau, Re. GZero also contains the array
!       indices for the top right hand corner, plus the following data relating
!        to the required Tau, Re values:
!          dT: distance from zero'th point to required Tau value as a fraction
!              of the grid step size in the Tau direction
!          dR: equivalent to dT in the Re grid
!          The next-nearest neighbour LUT indices to the required point (which
!          are used in finite difference gradient calculation). At the edge of
!          LUT these indices are set to the nearest neighbour value and an
!          uncentred difference is used
!          The denominator values of the finite difference calculations are
!          also included in GZero - these will be the same as the grid spacing
!          at the edge of the LUT, and twice this elsewhere.
!
!    Note the routine is NOT passed the required Tau, Re values: the info in
!    GZero and Grid is sufficient.
!
!    The subroutine populates 4 arrays with:
!       - The value of the LUT function at the 4 nearest LUT vertices
!       - The gradient wrt to Tau at these points
!       - The gradient wrt to Re at these points
!       - The second order cross-derivative wrt Tau and Re at these points
!    These, and the dT and dR values from the GZero structure, are passed to the
!    bicubic interpolation routine.
!    The resulting interpolated gradients are re-scaled by the grid spacing,
!    since the interpolation routine works on the (0,1) interval.
!
! Local variables:
!    Name Type Description
!
! History:
!    26th Oct 2000, Andy Smith: Original version
!    16th Nov 2000, Andy Smith:
!       Updated arguments: more data passed via structs, allows for easier
!       generalisation to more dimensions and improves performance as some
!       values used by several Int routines are calculated once only by the
!       calling routine.
!    22nd Dec 2000, Phil Watts:
!       Removed redundant Chans argument; all channels implicit in the LUT are
!       interpolated.
!    19th Jan 2001, Andy Smith:
!       Comments updated.
!    5th Sep 2011, Chris Arnold:
!       Added bicubic interpolation
!    20th Jan 2012 changed definition of Y to remove contiguous array warning!
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to routine
!     7th Feb 2012, Chris Arnold:
!       Input structures Ctrl, GZero, Grid now have intent(in)
!     8th Jul 2012, C. Poulsen:
!       Fixed non contiguous array.
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    21st Jan 2014, Greg McGarragh:
!       Cleaned up code.
!    23st Jan 2014, Greg McGarragh:
!       Performance improvements.  Primarily through elimination of unused
!       memory references and computations.
!    16th Oct 2014, Greg McGarragh:
!       Moved a large amount of code that was common to all IntLUT* subroutines
!       into Int_LUT_Common()
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauRe(F, Grid, GZero, Ctrl, FInt, FGrads, iCRP, status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations

   real, dimension(:,:,:), intent(in)   :: F
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
   integer,                intent(out)  :: status

   ! Local variables

   integer                               :: i, j, jj, k, kk
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

   NChans = size(F,1)

   ! Construct the input Int_LUT_Common(): Function values at four LUT points
   ! around our X
   do i=1,NChans
      T_index(-1) = GZero%iTm1(i,iCRP)
      T_index( 0) = GZero%iT0 (i,iCRP)
      T_index( 1) = GZero%iT1 (i,iCRP)
      T_index( 2) = GZero%iTp1(i,iCRP)
      R_index(-1) = GZero%iRm1(i,iCRP)
      R_index( 0) = GZero%iR0 (i,iCRP)
      R_index( 1) = GZero%iR1 (i,iCRP)
      R_index( 2) = GZero%iRp1(i,iCRP)

      do j = iXm1, iXp1
         jj = T_index(j)
         do k = iXm1, iXp1
            kk = R_index(k)
            G(i,j,k) = F(i,jj,kk)
         end do
      end do
   end do

   call Int_LUT_Common(Ctrl, NChans, iCRP, Grid, GZero, G, FInt, FGrads, 0, 0, status)

end subroutine Int_LUT_TauRe
