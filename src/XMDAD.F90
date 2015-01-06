!-------------------------------------------------------------------------------
! Name:
!    X_MDAD
!
! Purpose:
!    Sets up parts of the state vector, X, depending on measurements.
!
! Description:
!    Sets a value for Tau, Pc or the cloud fraction F based on the measurement
!    values Y in the current super-pixel. Only one variable is set in a given
!    call (which one is determined by the index value supplied). Result is
!    returned in X.
!
!    Alternatively sets a phase value in SPixel%FGPhase.
!
! Arguments:
!    Name   Type    In/Out/Both Description
!    Ctrl   struct  In          Control structure
!    SPixel struct  Both        Super pixel structure
!    index  int     In          The required state parameter index
!    SetErr logical In          State parameter error flag
!    X      real    Out         The state parameter
!    Err    real    Out         (A priori) error in state parameter
!    status int     Out         Indicates success/failure of subroutine.
!
! Algorithm:
!    Use index to select which variable is set:
!    Tau:
!       if (daytime and SPixel%Ind%MDAD_SW is available)
!          calculate an overcast reflectance for the channel nearest 0.67 microns
!          determine which of a pre-determined set of bands the value falls into
!          set X=the Tau value for the band
!       else
!          can't use this method for setting Tau, set status
!    Pc:
!       if (SPixel%Ind%MDAD_LW is available)
!          Convert the measurement value nearest 11 microns to radiance
!          Calculate overcast radiance from observed radiance and cloud fraction
!          Convert radiance back to brightness temperature
!          Find at which pressure level the calculated BT best matches the
!          temperature in the RTM data
!       else
!          can't use this method for setting Pc, set status
!   ( *** Phase section redundant, as of Mar 2011
!    Phase:
!       if (SPixel%Ind%MDAD_LW is available)
!          Convert the measurement value nearest 11 microns to radiance
!          Calculate overcast radiance from observed radiance and cloud fraction
!          Convert radiance back to brightness temperature
!          check calculated BT vs Ctrl%PhaseT values for water/ice
!       else
!          can't use this method for setting Phase, set status
!   *** )
!    Cloud fraction:
!       Use the SPixel cloud fraction value
!
! Local variables:
!    Name Type Description
!
! History:
!     6th Feb 2000, Kevin Smith: Original version
!     1st Jun 2001, Andy Smith:
!       2nd original version! Changes due to updates in overall design of first
!       guess/a priori setting and coding changes elsewhere.
!     7th Jun 2001, Andy Smith:
!       Checks MDAD_Lw/MDAD_Sw are available where they need to be used.
!       The user's choice of channels as well as the illumination conditions
!       for the SPixel can determine whether or not a given variable can be set
!       by this method.
!     5th Jul 2001, Andy Smith:
!       Bug fix in indexing of R_clear with MDAD_LW.
!    16th Jul 2001, Andy Smith:
!       Added more error checking. Checks values of Rad_o used in Phase and Pc
!       calculations don't go negative.
!       Using named constants for error values.
!    26th Oct 2001, Andy Smith:
!       Fix to Tau calculation. Reflectances are now expressed as fractions
!       rather than percentages. Calculation of iFGOP updated to match.
!    **************** ECV work starts here *************************************
!    11th Mar 2011, Andy Smith:
!       Re-applying changes made in late 2001/2.
!    (!   20th August changed the fractional error flagg C poulsen )
!       Plus correction to value in FGOP array: 1.15 instead of 1.5.
!       iPc loop to set BT_o was reverse, i.e. Np to 1 rather than 1 to Np.
!       iPhase: < replaced by <= in BT_o setting.
!    21st Mar 2011, Andy Smith:
!       Removing functionality to change phase during the retrieval.
!       Commented out handling of iPhase. Phase will be fixed at the Ctrl value.
!    30th Mar 2011, Andy Smith:
!       Removed commented-out code for handling phase, following removal of
!       phase change functionality.
!       Removal of super-pixel averaging. Removed use of Ctrl%Resoln%Space in
!       cloud fraction setting.
!    26th Apr 2011, Andy Smith:
!      Extension to handle multiple instrument views. The viewing geometry
!      becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!      Sec_o used in setting first guess optical path is now one value from the
!      array.
!    1st May 2012 Caroline Poulsen:
!       Modified first guess cloud top temperature to be an interpolation
!       between layers rather than the lower layer.
!    15th Jun 2012, C. Poulsen:
!       Modified illum to be an array.
!     8th Jul 2012, C. Poulsen:
!       Fixed invalid memory access error.
!    2012/08/17, MJ: Fixed bug with divide by zero.
!    2012/10/02, CP: Changed selection of first guess height.
!    2013/11/22, MJ: Rewrites selection of ctp FG/AP based on BT interpolation.
!    2014/08/01, GM: Cleaned up the code.
!    2014/12/08, CP: Made error output more specific
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine X_MDAD(Ctrl, SAD_Chan, SPixel, index, SetErr, X, Err, status)

   use Ctrl_def
   use ECP_Constants
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(in)    :: index
   logical,          intent(in)    :: SetErr
   real,             intent(out)   :: X
   real,             intent(out)   :: Err
   integer,          intent(out)   :: status

   ! Declare local variables
   real    :: FGOP(11)
   real    :: Ref_o
   integer :: iFGOP
   real    :: BT_o
   real    :: Rad
   real    :: Rad_o
   real    :: dR_dT
   real    :: dT_dR

   status = 0

   ! Set up first guess optical depth vector

   data FGOP / 0.1, 0.3, 0.65, 0.8, 1.0, 1.15, 1.3, 1.5, 1.7, 2.0, 2.4 /

   ! Parameters supported are Tau, Pc and f.
   select case (index)

   case (iTau) ! Cloud optical depth, Tau

      if (SPixel%Illum(1) == IDay .and. &
          SPixel%Ind%MDAD_SW > 0) then
         ! Calculate overcast reflectance.
         ! Uses channel nearest 0.67 microns, index Ctrl%Ind%MDAD_SW.
         Ref_o = (SPixel%Ym(SPixel%Ind%MDAD_SW) - &
                  (SPixel%Rs(SPixel%Ind%MDAD_SW) * &
                   (1.0-SPixel%Cloud%Fraction))) / &
                 SPixel%Cloud%Fraction

         ! Convert albedo (range 0 - 1) into index (range 1 to 10)
         ! Use the first SEC_o value, assuming that all values are quite close
         ! and we only need an approximation for first guess setting.
         iFGOP = int( ( Ref_o * SPixel%Geom%SEC_o(1) * 10.0 ) + 1.5 )

         if (iFGOP > 11) then
            iFGOP = 11
         else if (iFGOP < 1) then
            iFGOP = 1
         end if

         X = FGOP(iFGOP)

         if (SetErr) Err = MDADErrTau
      else ! Can't calculate Tau unless it's daylight
         write(*,*)'ERROR: X_MDAD(): Cant calculate Tau unless its daylight'
         status = XMDADMeth
      end if

   case (iPc) ! Cloud pressure, Pc

      if (SPixel%Ind%MDAD_LW > 0) then
         ! Convert observed brightness temperature to radiance
         ! Uses channel nearest 11 microns, index Ctrl%Ind%MDAD_LW in SAD_Chan,
         ! but SPixel%Ind%MDAD_LW in the measurement array.
         call T2R(1, SAD_Chan(Ctrl%Ind%MDAD_LW), &
                  SPixel%Ym(SPixel%Ind%MDAD_LW), Rad, dR_dT, status)

         ! Calculate overcast radiance from observed radiance and cloud
         ! fraction. Note MDAD_LW must be offset for use with R_Clear since
         ! R_Clear stores thermal channels only.
         Rad_o = ( Rad - &
              SPixel%RTM%LW%R_clear(Ctrl%Ind%MDAD_LW-Ctrl%Ind%ThermalFirst+1) * &
              (1.0 - SPixel%Cloud%Fraction))  &
              / SPixel%Cloud%Fraction

         ! Exclude negative Rad_o (can arise due to approximation in the RTM)
         if (Rad_o >= 0.0) then
            ! Convert overcast radiance back to brightness temperature
            call R2T(1, SAD_Chan(Ctrl%Ind%MDAD_LW), Rad_o, BT_o, dT_dR, status)

            ! Interpolate for the BT to the rad. profile to get Pc FG/AP
            call interpolate2ctp(SPixel,Ctrl,BT_o,X,Err)
         else
            ! If no interpolation possible set BP_o and DBP_o to hardcoded
            ! values to recover:
            X = Ctrl%X0(3)

            !FG does not need Error but AP does
            Err = MDADErrPc
	    write(*,*)'ERROR: X_MDAD(): FG does not need Error but AP does'
            status = XMDADMeth
         end if
      else ! Can't calculate Pc if required LW channels not selected
      	 write(*,*)'ERROR: X_MDAD(): Cant calculate Pc if required LW channels not selected'
         status = XMDADMeth
      end if

   case (iFr) ! Cloud fraction, f
      ! Value is taken straight from the SPixel structure.
      X = SPixel%Cloud%Fraction
      if (SetErr) Err = MDADErrF1
   end select

end subroutine X_MDAD
