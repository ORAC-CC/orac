!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_TauSatRe
!
! Purpose:
!    Interpolates values from a LUT array with dimensions (channel, Tau, SatZen,
!    Re) to give the value of the LUT array at a single value of Tau, SatZen, Re
!    across all channels.
!
! Description:
!    The subroutine is passed an array of data in 4 dimensions, plus info
!    relating to the Tau, Re and SatZen grids corresponding to the LUT array
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
!    See IntLUTTauSolRe. This routine is an exact copy with all values relating
!    to SolZen replaced by the equivalents for SatZen (e.g. GZero%Sa1). A single,
!    more general routine could have been used but this may have been less easy
!    to follow.
!
! Local variables:
!    Name Type Description
!
! History:
!    15th Nov 2000, Andy Smith: Original version
!     1st Dec 2000, Andy Smith:
!       Renamed "Sol" instead of "Sun".
!       Variables using Sun or Su also renamed Sol/So
!    11th Jan 2001, Andy Smith:
!       Chans argument removed. Redundant.
!    19th Jan 2001, Andy Smith:
!       Comments updated.
!    31st May 2007, Andy Sayer:
!       The code now checks which channels are using which geometry, and
!       interpolates/fills up the output arrays accordingly. See also
!       IntRoutines.F90.
!    5th Sep 2011, Chris Arnold:
!       Added Bi-cubic interpolation in the Tau and Re plane, so that both
!       function and gradient estimates are continuous in these dimensions.
!       (Interpolation in angles is still done linearly.)
!    20th Jan 2012, C. Poulsen:
!       Created YIN variable to deal with contiguous array
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to routine
!     7th Feb 2012, Chris Arnold:
!       Input structures Ctrl, GZero, Grid now have intent(in)
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties
!    16st Jan 2014, Greg McGarragh:
!       Indexing fixes for the changes introduced above.  Namely introduced use
!       of i_chan_to_ctrl_offset, i_chan_to_spixel_offset.
!    21st Jan 2014, Greg McGarragh:
!       Cleaned up code.
!
! Bugs:
!    None known.
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_TauSatRe(F, Grid, GZero,Ctrl, FInt, FGrads, icrpr, &
   i_chan_to_ctrl_offset, i_chan_to_spixel_offset, status)

   use bcubic_def
   use CTRL_def
   use GZero_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations
   ! Note if these arguments are changed the interface definition in
   ! IntRoutines.f90 must be updated.

   real, dimension(:,:,:,:), intent(in)    :: F
                                	      ! The array to be interpolated.
   type(LUT_Grid_t),         intent(in)    :: Grid
                                              ! LUT grid data
   type(GZero_t),            intent(in)    :: GZero
                                              ! Struct containing "zero'th" grid
                                              ! points
   type(CTRL_t),             intent(in)    :: Ctrl
   real, dimension(:),       intent(inout) :: FInt
                                              ! Interpolated value of F at the
					      ! required Tau, SatZen, Re values
					      ! (1 value per channel).
   real, dimension(:,:),     intent(inout) :: FGrads
                                	      ! Gradients of F wrt Tau and Re at
					      ! required Tau, SatZen, Re values
					      ! (1 value per channel).
   integer,                  intent(in)    :: icrpr
   integer,                  intent(in)    :: i_chan_to_ctrl_offset
   integer,                  intent(in)    :: i_chan_to_spixel_offset

   integer,                  intent(out)   :: status

   ! Local variables

   integer                       :: i, ii, ii2
   integer                       :: NChans    ! Number of channels in LUT arrays
                                              ! etc
   real, dimension(size(FInt),Grid%nmaxTau,Grid%nmaxRe) :: G
					      ! A Matrix of dimension Nchan,
					      ! NTau,Nre - used to store array
					      ! only interpolated to current
					      ! viewing geometry
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
#ifdef BKP
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif

   NChans = size(F,1)

   ! Construct the input vectors for BCuInt: Function values at four LUT points
   ! around our X

   do i = 1, NChans
      ii = i_chan_to_ctrl_offset + i
      ii2 = i_chan_to_spixel_offset + i
      G(i,1:Grid%nTau(ii,icrpr),1:Grid%nRe(ii,icrpr)) = &
         (GZero%Sa1(ii2,icrpr)  * F(i,1:Grid%nTau(ii,icrpr),GZero%iSaZ0(ii2,icrpr),1:Grid%nRe(ii,icrpr))) + &
         (GZero%dSaZ(ii2,icrpr) * F(i,1:Grid%nTau(ii,icrpr),GZero%iSaZ1(ii2,icrpr),1:Grid%nRe(ii,icrpr)))

      Y(i,1) = G(i,GZero%iT0(ii2,icrpr),GZero%iR0(ii2,icrpr))
      Y(i,4) = G(i,GZero%iT0(ii2,icrpr),GZero%iR1(ii2,icrpr))
      Y(i,3) = G(i,GZero%iT1(ii2,icrpr),GZero%iR1(ii2,icrpr))
      Y(i,2) = G(i,GZero%iT1(ii2,icrpr),GZero%iR0(ii2,icrpr))

      ! Function derivatives at four LUT points around our X....

      ! WRT to Tau
      ii = i_chan_to_ctrl_offset + i
      ii2 = i_chan_to_spixel_offset + i
      if(abs(Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) &
         .le. ditherm15 ) then
         dYdTau(i,1) = 0.0
      else
         dYdTau(i,1) = (G(i,GZero%iT1(ii2,icrpr),GZero%iR0(ii2,icrpr)) - &
                        G(i,GZero%iTm1(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                       (Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) &
         .le. ditherm15 ) then
         dYdTau(i,2) = 0.0
      else
         dYdTau(i,2) = (G(i,GZero%iTp1(ii2,icrpr),GZero%iR0(ii2,icrpr)) - &
                        G(i,GZero%iT0(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                       (Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) &
         .le. ditherm15) then
         dYdTau(i,3) = 0.0
      else
         dYdTau(i,3) = (G(i,GZero%iTp1(ii2,icrpr),GZero%iR1(ii2,icrpr)) - &
                        G(i,GZero%iT0(ii2,icrpr),GZero%iR1(ii2,icrpr))) / &
                       (Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) &
         .le. ditherm15 ) then
         dYdTau(i,4) = 0.0
      else
         dYdTau(i,4) = (G(i,GZero%iT1(ii2,icrpr),GZero%iR1(ii2,icrpr)) - &
                        G(i,GZero%iTm1(ii2,icrpr),GZero%iR1(ii2,icrpr))) / &
                       (Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr))
      endif

      ! WRT to Re
      if(abs(Grid%re(ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr)) &
         .le. ditherm15) then
         dYDRe(i,1) = 0.0
      else
         dYDRe(i,1) = (G(i,GZero%iT0(ii2,icrpr),GZero%iR1(ii2,icrpr)) - &
                       G(i,GZero%iT0(ii2,icrpr),GZero%iRm1(ii2,icrpr))) / &
                      (Grid%re(ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr)) &
         .le. ditherm15) then
         dYDRe(i,2) = 0.0
      else
         dYDRe(i,2) = (G(i,GZero%iT1(ii2,icrpr),GZero%iR1(ii2,icrpr)) - &
                       G(i,GZero%iT1(ii2,icrpr),GZero%iRm1(ii2,icrpr))) / &
                      (Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr)) &
         .le. ditherm15) then
         dYDRe(i,3) = 0.0
      else
         dYDRe(i,3) = (G(i,GZero%iT1(ii2,icrpr),GZero%iRp1(ii2,icrpr)) - &
                       G(i,GZero%iT1(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                      (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr))
      endif
      if(abs(Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr)) &
         .le. ditherm15) then
         dYDRe(i,4) = 0.0
      else
         dYDRe(i,4) = (G(i,GZero%iT0(ii2,icrpr),GZero%iRp1(ii2,icrpr)) - &
                       G(i,GZero%iT0(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                      (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr))
      endif

      ! Cross derivatives (dY^2/dTaudRe)
      if(abs((Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) * &
             (Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr))) &
         .le. ditherm15) then
         ddY(i,1) = 0.0
      else
         ddY(i,1) = (G(i,GZero%iT1(ii2,icrpr),GZero%iR1(ii2,icrpr)) - G(i,GZero%iT1(ii2,icrpr),GZero%iRm1(ii2,icrpr)) - &
                     G(i,GZero%iTm1(ii2,icrpr),GZero%iR1(ii2,icrpr)) + G(i,GZero%iTm1(ii2,icrpr),GZero%iRm1(ii2,icrpr))) / &
                    ((Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) * &
                     (Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr)))
      endif
      if(abs((Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) * &
             (Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr))) &
         .le. ditherm15) then
         ddY(i,2) = 0.0
      else
         ddY(i,2) = (G(i,GZero%iTp1(ii2,icrpr),GZero%iR1(ii2,icrpr)) - G(i,GZero%iTp1(ii2,icrpr),GZero%iRm1(ii2,icrpr)) - &
                     G(i,GZero%iT0(ii2,icrpr),GZero%iR1(ii2,icrpr)) + G(i,GZero%iT0(ii2,icrpr),GZero%iRm1(ii2,icrpr))) / &
                    ((Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) * &
                     (Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iRm1(ii2,icrpr),icrpr)))
      endif
      if(abs((Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) * &
             (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr))) &
         .le. ditherm15) then
         ddY(i,3) = 0.0
      else
         ddY(i,3) = (G(i,GZero%iTp1(ii2,icrpr),GZero%iRp1(ii2,icrpr)) - G(i,GZero%iTp1(ii2,icrpr),GZero%iR0(ii2,icrpr)) - &
                     G(i,GZero%iT0(ii2,icrpr),GZero%iRp1(ii2,icrpr)) + G(i,GZero%iT0(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                    ((Grid%tau(ii,GZero%iTp1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr)) * &
                     (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr)))
      endif
      if(abs((Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) * &
             (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr))) &
         .le. ditherm15) then
         ddY(i,4) = 0.0
      else
         ddY(i,4) = (G(i,GZero%iT1(ii2,icrpr),GZero%iRp1(ii2,icrpr)) - G(i,GZero%iT1(ii2,icrpr),GZero%iR0(ii2,icrpr)) - &
                     G(i,GZero%iTm1(ii2,icrpr),GZero%iRp1(ii2,icrpr)) + G(i,GZero%iTm1(ii2,icrpr),GZero%iR0(ii2,icrpr))) / &
                    ((Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr) - Grid%tau(ii,GZero%iTm1(ii2,icrpr),icrpr)) * &
                     (Grid%re(Ii,GZero%iRp1(ii2,icrpr),icrpr) - Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr)))
      endif
   enddo

   ! Now call the adapted Numerical Recipes BCuInt subroutine to perform the
   ! interpolation to our desired state vector [Or the equivalent linint
   ! subroutine - Oct 2011]
   if (Ctrl%LUTIntflag .eq. 0) then
      do i = 1,NChans
         ii = i_chan_to_ctrl_offset + i
         ii2 = i_chan_to_spixel_offset + i
         YIN=Y(i,:)
         call linint(YIN,Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr), &
                         Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr), &
                         Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr), &
                         Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr), &
                         GZero%dT(ii2,icrpr),GZero%dR(ii2,icrpr),a1,a2,a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      enddo
   else if (Ctrl%LUTIntflag .eq. 1) then
      do i = 1,NChans
         ii = i_chan_to_ctrl_offset + i
         ii2 = i_chan_to_spixel_offset + i
         Yinb=Y(i,:)
         dYdTauin=dYdTau(i,:)
         dYdRein=dYdRe(i,:)
         ddYin=ddY(i,:)
         call bcuint(Yinb,dYdTauin,dYdRein,ddYin,&
                     Grid%tau(ii,GZero%iT0(ii2,icrpr),icrpr), &
                     Grid%tau(ii,GZero%iT1(ii2,icrpr),icrpr), &
                     Grid%re(Ii,GZero%iR0(ii2,icrpr),icrpr), &
                     Grid%re(Ii,GZero%iR1(ii2,icrpr),icrpr), &
                     GZero%dT(ii2,icrpr),GZero%dR(ii,icrpr),a1,a2,a3)
         FInt(i) = a1
         FGrads(i,1) = a2
         FGrads(i,2) = a3
      enddo
   else
      status = LUTIntflagErr
      call Write_Log(Ctrl, 'IntLUTTauSatRe.f90: LUT Interp flag error:', status)
   endif

end subroutine Int_LUT_TauSatRe
