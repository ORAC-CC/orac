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
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations

   real, dimension(:,:,:), intent(in)  :: F
                                            ! The array to be interpolated.
   type(LUT_Grid_t),       intent(in)  :: Grid
                                            ! LUT grid data
   type(GZero_t),          intent(in)  :: GZero
                                            ! Struct containing "zero'th" grid
                                            ! points
   type(CTRL_t),           intent(in)  :: Ctrl
   real, dimension(:),     intent(out) :: FInt
                                            ! Interpolated value of F at the
					    ! required Tau, Re values, (1 value
                                            ! per channel).
   real, dimension(:,:),   intent(out) :: FGrads
                                            ! Gradients of F wrt Tau and Re at
					    ! required Tau, Re values,
					    ! (1 value per channel).
   integer,                intent(in)  :: icrpr
   integer,                intent(out) :: status

   ! Local variables

   integer                       :: i
   integer                       :: NChans  ! Number of Channels in LUT arrays
                                            ! etc
   real, dimension(size(FInt),4) :: Y       ! A vector to contain the values
 					    ! of F at (iT0,iR0), (iT0,iR1),
					    ! (iT1,iR1) and (iT1,iR0)
					    ! respectively (i.e. anticlockwise
					    ! from the bottom left)
   real, dimension(size(FInt),4) :: dYdTau  ! Gradients of F wrt Tau at the
					    ! same points as Y
   real, dimension(size(FInt),4) :: dYdRe   ! Gradients of F wrt Re at the
					    ! same points as Y
   real, dimension(size(FInt),4) :: ddY     ! 2nd order cross derivatives of
					    ! F wrt Tau and Re at the same
					    ! points
   real, dimension(4)            :: Yin, Yinb, dYdTauin, dYdRein, ddYin
                                            ! Temporary store for input of
					    ! BiCubic subroutine
   real                          :: a1, a2, a3
                                            ! Temporary store for output of
					    ! BiCubic subroutine

   NChans = size(F,1)

   ! Construct the input vectors for BCuInt: Function values at four LUT points
   ! around our X

   do i=1,NChans
      Y(i,1) = F(i,GZero%iT0(i,icrpr),GZero%iR0(i,icrpr))
      Y(i,4) = F(i,GZero%iT0(i,icrpr),GZero%iR1(i,icrpr))
      Y(i,3) = F(i,GZero%iT1(i,icrpr),GZero%iR1(i,icrpr))
      Y(i,2) = F(i,GZero%iT1(i,icrpr),GZero%iR0(i,icrpr))

      if (Ctrl%LUTIntflag .eq. LUTIntMethBicubic) then
         ! Function derivatives at four LUT points around our X

         ! WRT to Tau
         dYdTau(i,1) = (F(i,GZero%iT1(i,icrpr),GZero%iR0(i,icrpr)) - &
                        F(i,GZero%iTm1(i,icrpr),GZero%iR0(i,icrpr))) / &
                       (Grid%tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%tau(i,GZero%iTm1(i,icrpr),icrpr))
         dYdTau(i,2) = (F(i,GZero%iTp1(i,icrpr),GZero%iR0(i,icrpr)) - &
                        F(i,GZero%iT0(i,icrpr),GZero%iR0(i,icrpr))) / &
                       (Grid%tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%tau(i,GZero%iT0(i,icrpr),icrpr))
         dYdTau(i,3) = (F(i,GZero%iTp1(i,icrpr),GZero%iR1(i,icrpr)) - &
                        F(i,GZero%iT0(i,icrpr),GZero%iR1(i,icrpr))) / &
                       (Grid%tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%tau(i,GZero%iT0(i,icrpr),icrpr))
         dYdTau(i,4) = (F(i,GZero%iT1(i,icrpr),GZero%iR1(i,icrpr)) - &
                        F(i,GZero%iTm1(i,icrpr),GZero%iR1(i,icrpr))) / &
                       (Grid%tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%tau(i,GZero%iTm1(i,icrpr),icrpr))

         ! WRT to Re
         dYDRe(i,1) = (F(i,GZero%iT0(i,icrpr),GZero%iR1(i,icrpr)) - &
                       F(i,GZero%iT0(i,icrpr),GZero%iRm1(i,icrpr))) / &
                      (Grid%re(i,GZero%iR1(i,icrpr),icrpr) - Grid%re(i,GZero%iRm1(i,icrpr),icrpr))
         dYDRe(i,2) = (F(i,GZero%iT1(i,icrpr),GZero%iR1(i,icrpr)) - &
                       F(i,GZero%iT1(i,icrpr),GZero%iRm1(i,icrpr))) / &
                      (Grid%re(i,GZero%iR1(i,icrpr),icrpr) - Grid%re(i,GZero%iRm1(i,icrpr),icrpr))
         dYDRe(i,3) = (F(i,GZero%iT1(i,icrpr),GZero%iRp1(i,icrpr)) - &
                       F(i,GZero%iT1(i,icrpr),GZero%iR0(i,icrpr))) / &
                      (Grid%re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%re(i,GZero%iR0(i,icrpr),icrpr))
         dYDRe(i,4) = (F(i,GZero%iT0(i,icrpr),GZero%iRp1(i,icrpr)) - &
                       F(i,GZero%iT0(i,icrpr),GZero%iR0(i,icrpr))) / &
                      (Grid%re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%re(i,GZero%iR0(i,icrpr),icrpr))

         ! Cross derivatives (dY^2/dTaudRe)
         ddY(i,1) = (F(i,GZero%iT1(i,icrpr),GZero%iR1(i,icrpr)) - F(i,GZero%iT1(i,icrpr),GZero%iRm1(i,icrpr)) - &
                     F(i,GZero%iTm1(i,icrpr),GZero%iR1(i,icrpr)) + F(i,GZero%iTm1(i,icrpr),GZero%iRm1(i,icrpr))) / &
                    ((Grid%tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                     (Grid%re(i,GZero%iR1(i,icrpr),icrpr) - Grid%re(i,GZero%iRm1(i,icrpr),icrpr)))
         ddY(i,2) = (F(i,GZero%iTp1(i,icrpr),GZero%iR1(i,icrpr)) - F(i,GZero%iTp1(i,icrpr),GZero%iRm1(i,icrpr)) - &
                     F(i,GZero%iT0(i,icrpr),GZero%iR1(i,icrpr)) + F(i,GZero%iT0(i,icrpr),GZero%iRm1(i,icrpr))) / &
                    ((Grid%tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                     (Grid%re(i,GZero%iR1(i,icrpr),icrpr) - Grid%re(i,GZero%iRm1(i,icrpr),icrpr)))
         ddY(i,3) = (F(i,GZero%iTp1(i,icrpr),GZero%iRp1(i,icrpr)) - F(i,GZero%iTp1(i,icrpr),GZero%iR0(i,icrpr)) - &
                     F(i,GZero%iT0(i,icrpr),GZero%iRp1(i,icrpr)) + F(i,GZero%iT0(i,icrpr),GZero%iR0(i,icrpr))) / &
                    ((Grid%tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                     (Grid%re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%re(i,GZero%iR0(i,icrpr),icrpr)))
         ddY(i,4) = (F(i,GZero%iT1(i,icrpr),GZero%iRp1(i,icrpr)) - F(i,GZero%iT1(i,icrpr),GZero%iR0(i,icrpr)) - &
                     F(i,GZero%iTm1(i,icrpr),GZero%iRp1(i,icrpr)) + F(i,GZero%iTm1(i,icrpr),GZero%iR0(i,icrpr))) / &
                   ((Grid%tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                    (Grid%re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%re(i,GZero%iR0(i,icrpr),icrpr)))
      end if
   end do

   ! Now call the adapted Numerical Recipes BCuInt subroutine to perform the
   ! interpolation to our desired state vector [Or the equivalent linint
   ! subroutine - Oct 2011]
   if (Ctrl%LUTIntflag .eq. LUTIntMethLinear) then
      do i = 1,NChans
         Yin=Y(i,1:4)
         call linint(Yin,Grid%tau(i,GZero%iT0(i,icrpr),icrpr), &
                         Grid%tau(i,GZero%iT1(i,icrpr),icrpr), &
                         Grid%re(i,GZero%iR0(i,icrpr),icrpr), &
                         Grid%re(i,GZero%iR1(i,icrpr),icrpr), &
                         GZero%dT(i,icrpr), GZero%dR(i,icrpr), a1, a2, a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      end do
   else if (Ctrl%LUTIntflag .eq. LUTIntMethBicubic) then
      do i = 1,NChans
         Yinb=Y(i,1:4)
         dYdTauin=dYdTau(i,1:4)
         dYdRein=dYdRe(i,1:4)
         ddYin=ddY(i,1:4)
         call bcuint(Yinb, dYdTauin, dYdRein, ddYin, &
                     Grid%tau(i,GZero%iT0(i,icrpr),icrpr), &
                     Grid%tau(i,GZero%iT1(i,icrpr),icrpr), &
                     Grid%re(i,GZero%iR0(i,icrpr),icrpr), &
                     Grid%re(i,GZero%iR1(i,icrpr),icrpr), &
                     GZero%dT(i,icrpr),GZero%dR(i,icrpr), a1, a2, a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      end do
   else
      status = LUTIntflagErr
      call Write_Log(Ctrl, 'IntLUTTauRe.f90: LUT Interp flag error:', status)
   end if

end subroutine Int_LUT_TauRe
