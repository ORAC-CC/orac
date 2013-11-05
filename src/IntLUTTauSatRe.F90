! Name:
!    Int_LUT_TauSatRe
!
! Purpose:
!    Interpolates values from a LUT array with dimensions 
!    (channel, Tau, SatZen, Re) to give the value of the LUT array at a single 
!    value of Tau, SatZen, Re across all channels.
!
! Description:
!    The subroutine is passed an array of data in 4 dimensions, plus info 
!    relating to the Tau, Re and SatZen grids corresponding to the LUT array 
!    points (the third dimension in the LUT array is the channel number, 
!    which is not interpolated).
!
!    The subroutine calculates the value of the LUT at the current 
!    (Tau, SatZen, Re) plus the gradients in Tau and Re.
!
! Arguments:
!    Name        Type    In/Out/Both    Description
!    F           real array  In         Function to be interpolated, i.e. 
!                                       array of LUT values with dimensions
!                                       (channels, Tau, SolZen, Re.
!    Grid        struct      In         LUT Grid data: see SADLUT.F90
!                                       Includes the grid values in Tau, Re, 
!                                       SolZen, no. of values and step size
!    Gzero       struct      In         Structure containing "zero'th" point
!                                       indices, i.e. LUT grid array indices
!                                       for the nearest neighbour point, plus
!                                       dT, dR, dS values: fraction of grid step
!                                       from zero'th point to the Tau, SolZen, Re
!                                       value we want.
!    Ctrl        struct      In         Control structure: Needed for numbers of
!                                       channels and their indices
!    Ind         struct      In         The index structure from the SPixel 
!                                       array
!    FInt        real array  Both       The interpolated values of F for all 
!                                       required channels.
!    FGrads      real array  Both       Interpolated gradient values in Tau 
!                                       and Re for all channels.
!    status      int         Out        Standard status code set by ECP routines
!
! Algorithm:
!    See IntLUTTauSolRe. This routine is an exact copy with all values 
!    relating to SolZen replaced by the equivalents for SatZen (e.g. GZero%Sa1)
!    A single, more general routine could have been used but this may have
!    been less easy to follow.
!
! History:
!    15th Nov 2000, Andy Smith: Original version
!     1st Dec 2000, Andy Smith:
!        Renamed "Sol" instead of "Sun".
!        Variables using Sun or Su also renamed Sol/So
!    11th Jan 2001, Andy Smith:
!        Chans argument removed. Redundant.
!    19th Jan 2001, Andy Smith:
!        Comments updated.
!    31st May 2007, Andy Sayer:
!        The code now checks which channels are using which geometry, and
!        interpolates/fills up the ouput arrays accordingly. See also
!        IntRoutines.F90.
!    5th Sep 2011, Chris Arnold:
!        Added Bi-cubic interpolation in the Tau and Re plane, so
!        that both function and gradient estimates are continuous in 
!        these dimensions.
!        (Interpolation in angles is still done linearly.)
!  20th  jan 2012 C. Poulsen created YIN variable to deal with contiguos array
!    7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to routine
!    7th Feb 2012, Chris Arnold:
!       Input structures Ctrl, GZero, Grid now have intent(in)
!
! Bugs:
!    None known.
!
!---------------------------------------------------------------------
Subroutine Int_LUT_TauSatRe(F, Grid, GZero, Ctrl, FInt, FGrads, status)

   use CTRL_def
   use GZero_def
   use SAD_LUT_def
   use bcubic_def
 
   implicit none

!  argument declarations
!  Note if these arguments are changed the interface definition in 
!  IntRoutines.f90 must be updated. 

   type(CTRL_t), intent(in)     :: Ctrl
   real, dimension(:,:,:,:), intent(in) :: F 
                                	      ! The array to be interpolated.
   type(GZero_t), intent(in)      :: GZero                ! Struct containing "zero'th" grid
                                              ! points
   type(LUT_Grid_t), intent(in)   :: Grid                 ! LUT grid data
   real, dimension(:), intent(inout)    :: FInt	      
                                              ! Interpolated value of F at the 
					      ! required Tau, SatZen, Re values
					      ! (1 value per channel).
   real, dimension(:,:), intent(inout)  :: FGrads   
                                	      ! Gradients of F wrt Tau and Re at 
					      ! required Tau, SatZen, Re values
					      ! (1 value per channel).
   integer          :: status

!  Local variables

   real, dimension(size(FInt),Grid%nTau,Grid%nRe) :: G
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
   real :: a1, a2, a3                         ! Temporary store for output of
					      ! BiCubic subroutine
   integer :: NChans 			      ! Number of channels in LUT arrays etc
   integer :: i      			      ! Array counter
   integer :: bkp_lun			      ! Unit number for breakpoint file
   integer :: ios    			      ! I/O stat for file handling
real, dimension(4) ::Yinb,dYdTauin,dYdRein,ddYin
 real, dimension(4) :: YIN		        
   NChans = size(F,1)


   do i = 1, NChans
      G(i,1:Grid%nTau,1:Grid%nRe) = &
      (GZero%Sa1(i)  * F(i,1:Grid%nTau,GZero%iSaZ0(i),1:Grid%nRe)) + &
      (GZero%dSaZ(i) * F(i,1:Grid%nTau,GZero%iSaZ1(i),1:Grid%nRe))
   end do

   Y(:,1) = G(:,GZero%iT0,GZero%iR0)
   Y(:,4) = G(:,GZero%iT0,GZero%iR1)
   Y(:,3) = G(:,GZero%iT1,GZero%iR1)
   Y(:,2) = G(:,GZero%iT1,GZero%iR0)

!  Function derivatives at four LUT points around our X....
!  - WRT to Tau

    dYdTau(:,1) = (G(:,GZero%iT1,GZero%iR0)-G(:,GZero%iTm1,GZero%iR0))/ &
	  	  (Grid%Tau(GZero%iT1) - Grid%Tau(GZero%iTm1)) 
    dYdTau(:,2) = (G(:,GZero%iTp1,GZero%iR0)-G(:,GZero%iT0,GZero%iR0))/ &
	  	  (Grid%Tau(GZero%iTp1) - Grid%Tau(GZero%iT0))
    dYdTau(:,3) = (G(:,GZero%iTp1,GZero%iR1)-G(:,GZero%iT0,GZero%iR1))/ &
	 	  (Grid%Tau(GZero%iTp1) - Grid%Tau(GZero%iT0))
    dYdTau(:,4) = (G(:,GZero%iT1,GZero%iR1)-G(:,GZero%iTm1,GZero%iR1))/ &
	 	  (Grid%Tau(GZero%iT1) - Grid%Tau(GZero%iTm1))

!   - WRT to Re

    dYDRe(:,1) = (G(:,GZero%iT0,GZero%iR1)-G(:,GZero%iT0,GZero%iRm1))/ &
		 (Grid%Re(GZero%iR1) - Grid%Re(GZero%iRm1))
    dYDRe(:,2) = (G(:,GZero%iT1,GZero%iR1)-G(:,GZero%iT1,GZero%iRm1))/ &
		 (Grid%Re(GZero%iR1) - Grid%Re(GZero%iRm1))
    dYDRe(:,3) = (G(:,GZero%iT1,GZero%iRp1)-G(:,GZero%iT1,GZero%iR0))/ &
		 (Grid%Re(GZero%iRp1) - Grid%Re(GZero%iR0))
    dYDRe(:,4) = (G(:,GZero%iT0,GZero%iRp1)-G(:,GZero%iT0,GZero%iR0))/ &
		 (Grid%Re(GZero%iRp1) - Grid%Re(GZero%iR0))

!   - Cross derivatives (dY^2/dTaudRe)

    ddY(:,1) = (G(:,GZero%iT1,GZero%iR1) - G(:,GZero%iT1,GZero%iRm1) -&
         & G(:,GZero%iTm1,GZero%iR1) + G(:,GZero%iTm1,GZero%iRm1)) / &
         & ((Grid%Tau(GZero%iT1) - Grid%Tau(GZero%iTm1)) *&
         &(Grid%Re(GZero%iR1) - Grid%Re(GZero%iRm1)))
    ddY(:,2) = (G(:,GZero%iTp1,GZero%iR1) - G(:,GZero%iTp1,GZero%iRm1) -&
         & G(:,GZero%iT0,GZero%iR1) + G(:,GZero%iT0,GZero%iRm1)) / &
         & ((Grid%Tau(GZero%iTp1) - Grid%Tau(GZero%iT0)) * (Grid%Re(GZero%iR1) - Grid%Re(GZero%iRm1)))
    ddY(:,3) = (G(:,GZero%iTp1,GZero%iRp1) - &
         & G(:,GZero%iTp1,GZero%iR0) - G(:,GZero%iT0,GZero%iRp1) + G(:,GZero%iT0,GZero%iR0)) / &
         & ((Grid%Tau(GZero%iTp1) - Grid%Tau(GZero%iT0)) * (Grid%Re(GZero%iRp1) - Grid%Re(GZero%iR0)))
    ddY(:,4) = (G(:,GZero%iT1,GZero%iRp1) - &
         & G(:,GZero%iT1,GZero%iR0) - G(:,GZero%iTm1,GZero%iRp1) + G(:,GZero%iTm1,GZero%iR0)) / &
         & ((Grid%Tau(GZero%iT1) - Grid%Tau(GZero%iTm1)) * (Grid%Re(GZero%iRp1) - Grid%Re(GZero%iR0)))

!  Now call the adapted Numerical Recipes BCuInt subroutine to
!  perform the interpolation to our desired state vector
!  [Or the equivalent linint subroutine - Oct 2011]

   if (Ctrl%LUTIntflag .eq. 0) then
      do i = 1,NChans
      YIN=Y(i,:)
         call linint(YIN,Grid%Tau(GZero%iT0),Grid%Tau(GZero%iT1),&
              & Grid%Re(GZero%iR0), Grid%Re(GZero%iR1),GZero%dT,GZero%dR,a1,a2,a3)
         FInt(i) = a1
!	 write(*,*)' finta ',FInt(i)
         FGrads(i,1) = a2
         FGrads(i,2) = a3
    enddo
   else if (Ctrl%LUTIntflag .eq. 1) then
      do i = 1,NChans
      Yinb=Y(i,:)
      dYdTauin=dYdTau(i,:)
      dYdRein=dYdRe(i,:)
      ddYin=ddY(i,:)
 
      call bcuint(Yinb,dYdTauin,dYdRein,ddYin,&
           & Grid%Tau(GZero%iT0),  Grid%Tau(GZero%iT1),&
           & Grid%Re(GZero%iR0), Grid%Re(GZero%iR1),&
           & GZero%dT,GZero%dR,a1,a2,a3)
      FInt(i) = a1
      !write(*,*)' fintb ',FInt(i)
      FGrads(i,1) = a2
      FGrads(i,2) = a3
   enddo
else
   status = LUTIntflagErr
   call Write_Log(Ctrl, 'IntLUTTauSatRe.f90: LUT Interp flag error:', status)
endif

!write(*,*) 'IntLUTTauSat', FInt(:)

End Subroutine Int_LUT_TauSatRe
