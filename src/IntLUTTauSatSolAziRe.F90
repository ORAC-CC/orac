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
!        interpolates/fills up the ouput arrays accordingly. See also
!        IntRoutines.F90.
!     5th Sep 2011, Chris Arnold:
!        Added Bi-cubic interpolation in the Tau and Re plane, so that both
!        function and gradient estimates are continuous in these dimensions.
!        (Interpolation in angles is still done linearly.)
!    20th Jan 2012, C. Poulsen:
!       Created Yin variable to deal with contiguos array
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to routine
!     7th Feb 2012, Chris Arnold:
!       Input structures Ctrl, GZero, Grid now have intent(in)
!     8th Jul 2012, C. Poulsen:
!       Fixed non contiguos arrays
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    21st Jan 2014, Greg McGarragh:
!       Cleaned up code.
!    23st Jan 2014, Greg McGarragh:
!       Performance improvements.  Primarily through elimination of unused
!       memory references and computations expecially when setting local
!       variable G.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauSatSolAziRe(F, Grid, GZero,Ctrl, FInt, FGrads, icrpr, &
   status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations

   real, dimension(:,:,:,:,:,:), intent(in)  :: F
                                	          ! The array to be interpolated.
   type(LUT_Grid_t),             intent(in)  :: Grid
                                                  ! LUT grid data
   type(GZero_t),                intent(in)  :: GZero
                                                  ! Struct containing "zero'th" grid
                                                  ! points
   type(CTRL_t),                 intent(in)  :: Ctrl
   real, dimension(:),           intent(out) :: FInt
                                                  ! Interpolated value of F at the
					          ! required Tau, SunZen, Re values
					          ! (1 value per channel).
   real, dimension(:,:),         intent(out) :: FGrads
                                	          ! Gradients of F wrt Tau and Re at
					          ! required Tau, SunZen, Re values
					          ! (1 value per channel).
   integer,                      intent(in)  :: icrpr
   integer,                      intent(out) :: status

   ! Local variables

   integer                       :: i, j, jj, k, kk
   integer                       :: NChans    ! Number of channels in LUT arrays
                                              ! etc
   real, dimension(-1:2,-1:2)    :: G         ! A Matrix of dimension NTau,Nre
                                              ! used to store array only
                                              ! interpolated to current viewing
                                              ! geometry
   real, dimension(size(FInt),4) :: Y         ! A vector to contain the values
 					      ! of F at (iT0,iR0), (iT0,iR1),
					      ! (iT1,iR1) and (iT1,iR0)
					      ! respectively (i.e. anticlockwise
					      ! from the bottom left)
   real, dimension(size(FInt),4) :: dYdTau    ! Gradients of F wrt Tau at the
					      ! same points as Y
   real, dimension(size(FInt),4) :: dYdRe     ! Gradients of F wrt Re at the
					      ! same points as Y
   real, dimension(size(FInt),4) :: ddY       ! 2nd order cross derivatives of
					      ! F wrt Tau and Re at the same
					      ! points
   real, dimension(4)            :: Yin, Yinb, dYdTauin, dYdRein, ddYin
                                              ! Temporary store for input of
					      ! BiCubic subroutine
   real                          :: a1, a2, a3
                                              ! Temporary store for output of
					      ! BiCubic subroutine

   integer, parameter            :: iXm1 = -1
   integer, parameter            :: iX0  =  0
   integer, parameter            :: iX1  =  1
   integer, parameter            :: iXp1 =  2

   integer, dimension(-1:2)      :: T_index
   integer, dimension(-1:2)      :: R_index

   NChans = size(F,1)

   ! Construct the input vectors for BCuInt: Function values at four LUT points
   ! around our X

   do i = 1, NChans
      T_index(-1) = GZero%iTm1(i,icrpr)
      T_index( 0) = GZero%iT0 (i,icrpr)
      T_index( 1) = GZero%iT1 (i,icrpr)
      T_index( 2) = GZero%iTp1(i,icrpr)
      R_index(-1) = GZero%iRm1(i,icrpr)
      R_index( 0) = GZero%iR0 (i,icrpr)
      R_index( 1) = GZero%iR1 (i,icrpr)
      R_index( 2) = GZero%iRp1(i,icrpr)

      do j = iXm1, iXp1
         jj = T_index(j)
         do k = iXm1, iXp1
            kk = R_index(k)
            G(j,k) = &
               ((GZero%RA1(i,icrpr) * GZero%Sa1(i,icrpr)  * GZero%So1(i,icrpr))  * &
               F(i,jj,GZero%iSaZ0(i,icrpr),GZero%iSoZ0(i,icrpr),GZero%iRA0(i,icrpr),kk)) + &
               ((GZero%RA1(i,icrpr) * GZero%Sa1(i,icrpr)  * GZero%dSoZ(i,icrpr)) * &
               F(i,jj,GZero%iSaZ0(i,icrpr),GZero%iSoZ1(i,icrpr),GZero%iRA0(i,icrpr),kk)) + &
               ((GZero%RA1(i,icrpr) * GZero%dSaZ(i,icrpr) * GZero%So1(i,icrpr))  * &
               F(i,jj,GZero%iSaZ1(i,icrpr),GZero%iSoZ0(i,icrpr),GZero%iRA0(i,icrpr),kk)) + &
               ((GZero%RA1(i,icrpr) * GZero%dSaZ(i,icrpr) * GZero%dSoZ(i,icrpr)) * &
               F(i,jj,GZero%iSaZ1(i,icrpr),GZero%iSoZ1(i,icrpr),GZero%iRA0(i,icrpr),kk)) + &
               ((GZero%dRA(i,icrpr) * GZero%Sa1(i,icrpr)  * GZero%So1(i,icrpr))  * &
               F(i,jj,GZero%iSaZ0(i,icrpr),GZero%iSoZ0(i,icrpr),GZero%iRA1(i,icrpr),kk)) + &
               ((GZero%dRA(i,icrpr) * GZero%Sa1(i,icrpr)  * GZero%dSoZ(i,icrpr)) * &
               F(i,jj,GZero%iSaZ0(i,icrpr),GZero%iSoZ1(i,icrpr),GZero%iRA1(i,icrpr),kk)) + &
               ((GZero%dRA(i,icrpr) * GZero%dSaZ(i,icrpr) * GZero%So1(i,icrpr))  * &
               F(i,jj,GZero%iSaZ1(i,icrpr),GZero%iSoZ0(i,icrpr),GZero%iRA1(i,icrpr),kk)) + &
               ((GZero%dRA(i,icrpr) * GZero%dSaZ(i,icrpr) * GZero%dSoZ(i,icrpr)) * &
               F(i,jj,GZero%iSaZ1(i,icrpr),GZero%iSoZ1(i,icrpr),GZero%iRA1(i,icrpr),kk))
         end do
      end do

      Y(i,1) = G(iX0,iX0)
      Y(i,4) = G(iX0,iX1)
      Y(i,3) = G(iX1,iX1)
      Y(i,2) = G(iX1,iX0)

      if (Ctrl%LUTIntflag .eq. LUTIntMethBicubic) then
         ! Function derivatives at four LUT points around our X....

         ! WRT to Tau
         if (abs(Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYdTau(i,1) = 0.0
         else
            dYdTau(i,1) = (G(iX1,iX0) - G(iXm1,iX0))/ &
                          (Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr))
         end if
         if (abs(Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYdTau(i,2) = 0.0
         else
            dYdTau(i,2) = (G(iXp1,iX0) - G(iX0,iX0))/ &
                          (Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr))
         end if
         if (abs(Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYdTau(i,3) = 0.0
         else
            dYdTau(i,3) = (G(iXp1,iX1) - G(iX0,iX1))/ &
                          (Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr))
         end if
         if (abs(Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYdTau(i,4) = 0.0
         else
            dYdTau(i,4) = (G(iX1,iX1) - G(iXm1,iX1))/ &
                          (Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr))
         end if

         ! WRT to Re
         if (abs(Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYDRe(i,1) = 0.0
         else
            dYDRe(i,1) = (G(iX0,iX1) - G(iX0,iXm1))/ &
                         (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr))
         end if
         if (abs(Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYDRe(i,2) = 0.0
         else
            dYDRe(i,2) = (G(iX1,iX1) - G(iX1,iXm1))/ &
                         (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr))
         end if
         if (abs(Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYDRe(i,3) = 0.0
         else
            dYDRe(i,3) = (G(iX1,iXp1) - G(iX1,iX0))/ &
                         (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr))
         end if
         if (abs(Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr)) &
            .le. ditherm15) then
            dYDRe(i,4) = 0.0
         else
            dYDRe(i,4) = (G(iX0,iXp1) - G(iX0,iX0))/ &
                         (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr))
         end if

         ! Cross derivatives (dY^2/dTaudRe)
         if (abs((Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr))) &
            .le. ditherm15) then
            ddY(i,1) = 0.0
         else
            ddY(i,1) = (G(iX1,iX1) - G(iX1,iXm1) - G(iXm1,iX1) + G(iXm1,iXm1)) / &
                       ((Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                        (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr)))
         end if
         if (abs((Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr))) &
            .le. ditherm15) then
            ddY(i,2) = 0.0
         else
            ddY(i,2) = (G(iXp1,iX1) - G(iXp1,iXm1) - G(iX0,iX1) + G(iX0,iXm1)) / &
                       ((Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                        (Grid%Re(i,GZero%iR1(i,icrpr),icrpr) - Grid%Re(i,GZero%iRm1(i,icrpr),icrpr)))
         end if
         if (abs((Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr))) &
            .le. ditherm15) then
            ddY(i,3) = 0.0
         else
            ddY(i,3) = (G(iXp1,iXp1) - G(iXp1,iX0) - G(iX0,iXp1) + G(iX0,iX0)) / &
                       ((Grid%Tau(i,GZero%iTp1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iT0(i,icrpr),icrpr)) * &
                        (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr)))
         end if
         if (abs((Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr))) &
            .le. ditherm15) then
            ddY(i,4) = 0.0
         else
            ddY(i,4) = (G(iX1,iXp1) - G(iX1,iX0) - G(iXm1,iXp1) + G(iXm1,iX0)) / &
                       ((Grid%Tau(i,GZero%iT1(i,icrpr),icrpr) - Grid%Tau(i,GZero%iTm1(i,icrpr),icrpr)) * &
                        (Grid%Re(i,GZero%iRp1(i,icrpr),icrpr) - Grid%Re(i,GZero%iR0(i,icrpr),icrpr)))
         end if
      end if
   end do

   ! Now call the adapted Numerical Recipes BCuInt subroutine to perform the
   ! interpolation to our desired state vector [Or the equivalent linint
   ! subroutine - Oct 2011]
   if (Ctrl%LUTIntflag .eq. LUTIntMethLinear) then
      do i = 1,NChans
         Yin=Y(i,:)
         call linint(Yin,Grid%Tau(i,GZero%iT0(i,icrpr),icrpr), &
                         Grid%Tau(i,GZero%iT1(i,icrpr),icrpr), &
                         Grid%Re(i,GZero%iR0(i,icrpr),icrpr), &
                         Grid%Re(i,GZero%iR1(i,icrpr),icrpr), &
                         GZero%dT(i,icrpr),GZero%dR(i,icrpr),a1,a2,a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      end do
   else if (Ctrl%LUTIntflag .eq. LUTIntMethBicubic) then
      do i = 1,NChans
         Yinb=Y(i,:)
         dYdTauin=dYdTau(i,:)
         dYdRein=dYdRe(i,:)
         ddYin=ddY(i,:)
         call bcuint(Yinb,dYdTauin,dYdRein,ddYin, &
                     Grid%Tau(i,GZero%iT0(i,icrpr),icrpr), &
                     Grid%Tau(i,GZero%iT1(i,icrpr),icrpr), &
                     Grid%Re(i,GZero%iR0(i,icrpr),icrpr), &
                     Grid%Re(i,GZero%iR1(i,icrpr),icrpr), &
                     GZero%dT(i,icrpr),GZero%dR(i,icrpr),a1,a2,a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      end do
   else
      status = LUTIntflagErr
      call Write_Log(Ctrl, 'IntLUTTauSatRe.f90: LUT Interp flag error:', status)
   end if

end subroutine Int_LUT_TauSatSolAziRe
