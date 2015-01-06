!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_TauSatSolAziRe
!
! Purpose:
!    Interpolates values from a LUT array with dimensions (channel, Tau, SatZen,
!    SunZen, RelAzi, Re) to give the value of the LUT array at a single value of
!    Tau, SatZen, SunZen, RelAzi, Re across all channels.
!
! Description:
!    The subroutine is passed an array of data in 6 dimensions, plus info
!    relating to the Tau, Re, SatZen etc grids corresponding to the LUT array
!    points (the 6th dimension in the LUT array is the channel number, which is
!    not interpolated).
!
!    The subroutine calculates the value of the LUT at the current (Tau, SatZen,
!    SunZen, RelAzi, Re) plus the gradients in Tau and Re.
!
! Arguments:
!    Name   Type        In/Out/Both Description
!    F      real array  In          Function to be interpolated, i.e. array of
!                                   LUT values with dimensions (channels, Tau,
!                                   SatZen, Re).
!    Grid   struct      In          LUT Grid data: see SADLUT.F90 Includes the
!                                   grid values in Tau, Re, SatZen, no. of
!                                   values and step size
!    Gzero  struct      In          Structure containing "zero'th" point
!                                   indices, i.e. LUT grid array indices for the
!                                   nearest neighbour point, plus dT, dR, dS
!                                   values: fraction of grid step from zero'th
!                                   point to the Tau, SatZen, Re value we want.
!    Ctrl   struct     In           Control structure: Needed for numbers of
!    Ind    struct     In           The index structure from the SPixel array
!    FInt   real array Both         The interpolated values of F for all
!                                   required channels.
!    FGrads real array Both         Interpolated gradient values in Tau and Re
!                                   for all channels.
!    status int        Out          Standard status code set by ECP routines
!
! Algorithm:
!    See IntLUTTauSolRe. This routine extends the linear interpolation from 1 to
!    3 dimensions. Therefore there are more initial intermediate interpolations
!    required. The array G is used to store the values of the F array
!    interpolated to the correct viewing geometry.
!
! Local variables:
!    Name Type Description
!
! History:
!    17th Nov 2000, Andy Smith: Original version
!     1st Dec 2000, Andy Smith:
!        Renamed: "Sol" instead of "Sun".
!        Variables using Sun or Su also renamed Sol/So
!    11th Jan 2001, Andy Smith:
!        Chans argument removed. Redundant.
!    19th Jan 2001, Andy Smith:
!        Comments updated.
!    31st May 2007, Andy Sayer:
!        The code now checks which channels are using which geometry, and
!        interpolates/fills up the output arrays accordingly. See also
!        IntRoutines.F90.
!     5th Sep 2011, Chris Arnold:
!        Added Bi-cubic interpolation in the Tau and Re plane, so that both
!        function and gradient estimates are continuous in these dimensions.
!        (Interpolation in angles is still done linearly.)
!    20th Jan 2012, C. Poulsen:
!       Created Yin variable to deal with contiguous array
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to routine
!     7th Feb 2012, Chris Arnold:
!       Input structures Ctrl, GZero, Grid now have intent(in)
!     8th Jul 2012, C. Poulsen:
!       Fixed non contiguous arrays
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    21st Jan 2014, Greg McGarragh:
!       Cleaned up code.
!    23st Jan 2014, Greg McGarragh:
!       Performance improvements.  Primarily through elimination of unused
!       memory references and computations especially when setting local
!       variable G.
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

subroutine Int_LUT_TauSatSolAziRe(F, Grid, GZero, Ctrl, FInt, FGrads, iCRP, &
   status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations

   real, dimension(:,:,:,:,:,:), intent(in) :: F
                                               ! The array to be interpolated.
   type(LUT_Grid_t),             intent(in)  :: Grid
                                                ! LUT grid data
   type(GZero_t),                intent(in)  :: GZero
                                                ! Struct containing "zero'th" grid
                                                ! points
   type(CTRL_t),                 intent(in)  :: Ctrl
   real, dimension(:),           intent(out) :: FInt
                                                ! Interpolated value of F at the
					        ! required Tau, Re values, (1 value
                                                ! per channel).
   real, dimension(:,:),         intent(out) :: FGrads
                                                ! Gradients of F wrt Tau and Re at
					        ! required Tau, Re values, (1 value
                                                ! per channel).
   integer,                      intent(in)  :: iCRP
   integer,                      intent(out) :: status

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

   status = 0

   NChans = size(F,1)

   ! Construct the input Int_LUT_Common(): Function values at four LUT points
   ! around our X
   do i = 1, NChans
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
            G(i,j,k) = 0.
            G(i,j,k) = G(i,j,k) + &
               ((GZero%RA1(i,iCRP) * GZero%Sa1(i,iCRP)  * GZero%So1(i,iCRP))  * &
               F(i,jj,GZero%iSaZ0(i,iCRP),GZero%iSoZ0(i,iCRP),GZero%iRA0(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%RA1(i,iCRP) * GZero%Sa1(i,iCRP)  * GZero%dSoZ(i,iCRP)) * &
               F(i,jj,GZero%iSaZ0(i,iCRP),GZero%iSoZ1(i,iCRP),GZero%iRA0(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%RA1(i,iCRP) * GZero%dSaZ(i,iCRP) * GZero%So1(i,iCRP))  * &
               F(i,jj,GZero%iSaZ1(i,iCRP),GZero%iSoZ0(i,iCRP),GZero%iRA0(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%RA1(i,iCRP) * GZero%dSaZ(i,iCRP) * GZero%dSoZ(i,iCRP)) * &
               F(i,jj,GZero%iSaZ1(i,iCRP),GZero%iSoZ1(i,iCRP),GZero%iRA0(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%dRA(i,iCRP) * GZero%Sa1(i,iCRP)  * GZero%So1(i,iCRP))  * &
               F(i,jj,GZero%iSaZ0(i,iCRP),GZero%iSoZ0(i,iCRP),GZero%iRA1(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%dRA(i,iCRP) * GZero%Sa1(i,iCRP)  * GZero%dSoZ(i,iCRP)) * &
               F(i,jj,GZero%iSaZ0(i,iCRP),GZero%iSoZ1(i,iCRP),GZero%iRA1(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%dRA(i,iCRP) * GZero%dSaZ(i,iCRP) * GZero%So1(i,iCRP))  * &
               F(i,jj,GZero%iSaZ1(i,iCRP),GZero%iSoZ0(i,iCRP),GZero%iRA1(i,iCRP),kk))
            G(i,j,k) = G(i,j,k) + &
               ((GZero%dRA(i,iCRP) * GZero%dSaZ(i,iCRP) * GZero%dSoZ(i,iCRP)) * &
               F(i,jj,GZero%iSaZ1(i,iCRP),GZero%iSoZ1(i,iCRP),GZero%iRA1(i,iCRP),kk))
         end do
      end do
   end do

   call Int_LUT_Common(Ctrl, NChans, iCRP, Grid, GZero, G, FInt, FGrads, 0, 0, status)

end subroutine Int_LUT_TauSatSolAziRe
