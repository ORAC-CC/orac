!-------------------------------------------------------------------------------
! Name: int_lut_re.F90
!
! Purpose:
! Interpolates values from a LUT array with dimensions (channel, Re)
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
! status int        Out         Standard status code set by ORAC routines
!
! History:
! 2016/05/03, AP: Original version
! 2016/08/23, AP: Add cubic spline interpolation.
! 2017/01/17, GM: Changes related to simplification of the indexing of the LUT
!    grid and the GZero parameters.
! 2017/10/24, GM: Switch to official NR cubic spline code and make optional
!    through conditional compilation.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Int_LUT_Re(F, NChans, Grid, GZero, Ctrl, FInt, FGrads, status)

   use Ctrl_m
   use GZero_m
   use Int_Routines_m
   use SAD_LUT_m

   implicit none

   ! Argument declarations

   real,              intent(in)  :: F(:,:)
                                     ! The array to be interpolated.
   integer,           intent(in)  :: NChans
   type(LUT_Grid_t),  intent(in)  :: Grid
                                     ! LUT grid data
   type(GZero_t),     intent(in)  :: GZero
                                     ! Struct containing "zero'th" grid points
   type(Ctrl_t),      intent(in)  :: Ctrl
   real,              intent(out) :: FInt(:)
                                     ! Interpolated value of F at the required
                                     ! Tau, Re values, (1 value per channel).
   real,              intent(out) :: FGrads(:)
                                     ! Gradients of F wrt Tau and Re at required
                                     ! Tau, Re values, (1 value per channel).
   integer,           intent(out) :: status

   ! Local variables
   integer :: i
   real    :: delta_r, delta_F, k0, k1, l0, l1
   real    :: d2F_dr2(Grid%Re%n)
#ifdef INCLUDE_NR
   real, dimension(Grid%Re%n) :: tmp_x, tmp_f
#endif

   status = 0

   do i = 1, NChans
      if (Ctrl%LUTIntSelm .eq. LUTIntMethLinear) then
         FInt(i) = F(i,GZero%iR0(i)) * GZero%dR(i) + &
                    F(i,GZero%iR1(i)) * GZero%R1(i)
         FGrads(i) = (F(i,GZero%iR0(i)) - F(i,GZero%iR1(i))) / &
                      (Grid%Re%x(GZero%iR1(i)) - Grid%Re%x(GZero%iR0(i)))
      else if (Ctrl%LUTIntSelm .eq. LUTIntMethBicubic) then
#ifdef INCLUDE_NR
         ! Following the syntax of InterpolarSolar_spline.F90
         tmp_x = Grid%Re%x(1:Grid%Re%n)
         tmp_f = F(i,1:Grid%Re%n)
         call spline(tmp_x, tmp_f, Grid%Re%n, 1.e30, 1.e30, d2F_dr2)
#else
         write(*, *) 'ERROR: Interpol_Solar_spline(): Numerical Recipes is ' // &
            'not available for cubic spline interpolation'
         stop error_stop_code
#endif
         delta_F = F(i,GZero%iR1(i)) - F(i,GZero%iR0(i))
         delta_r = Grid%Re%x(GZero%iR1(i)) - Grid%Re%x(GZero%iR0(i))

         k0 = (3.0*GZero%dR(i)*GZero%dR(i) - 1.0) / 6.0 * delta_r
         k1 = (3.0*GZero%R1(i)*GZero%R1(i) - 1.0) / 6.0 * delta_r
         FGrads(i) = delta_F / delta_r - &
                     k0 * d2F_dr2(GZero%iR0(i)) + &
                     k1 * d2F_dr2(GZero%iR1(i))

         l0 = (GZero%dR(i)*GZero%dR(i)*GZero%dR(i) - &
               GZero%dR(i)) * delta_r * delta_r / 6.0
         l1 = (GZero%R1(i)*GZero%R1(i)*GZero%R1(i) - &
               GZero%R1(i)) * delta_r * delta_r / 6.0
         FInt(i) = GZero%dR(i) * F(i,GZero%iR0(i)) + &
                   GZero%R1(i) * F(i,GZero%iR1(i)) + &
                   l0 * d2F_dr2(GZero%iR0(i)) + &
                   l1 * d2F_dr2(GZero%iR1(i))
      end if
   end do

end subroutine Int_LUT_Re
