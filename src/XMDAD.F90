!-------------------------------------------------------------------------------
! Name: XMDAD.F90
!
! Purpose:
! Sets up parts of the state vector, X, depending on measurements.
!
! Description and Algorithm details:
! Sets a value for Tau, Pc or the cloud fraction F based on the measurement
! values Y in the current super-pixel. Only one variable is set in a given
! call (which one is determined by the index value supplied). Result is
! returned in X.
!
! Alternatively sets a phase value in SPixel%FGPhase.
!
! Use index to select which variable is set:
! Tau:
!    if (daytime and SPixel%Ind%MDAD_SW is available)
!       calculate an overcast reflectance for the channel nearest 0.67 microns
!       determine which of a pre-determined set of bands the value falls into
!       set X=the Tau value for the band
!    else
!       can't use this method for setting Tau, set status
! Pc:
!    if (SPixel%Ind%MDAD_LW is available)
!       Convert the measurement value nearest 11 microns to radiance
!       Calculate overcast radiance from observed radiance and cloud fraction
!       Convert radiance back to brightness temperature
!       Find at which pressure level the calculated BT best matches the
!       temperature in the RTM data
!    else
!       can't use this method for setting Pc, set status
!   ( *** Phase section redundant, as of Mar 2011
! Phase:
!    if (SPixel%Ind%MDAD_LW is available)
!       Convert the measurement value nearest 11 microns to radiance
!       Calculate overcast radiance from observed radiance and cloud fraction
!       Convert radiance back to brightness temperature
!       check calculated BT vs Ctrl%PhaseT values for water/ice
!    else
!       can't use this method for setting Phase, set status
!   *** )
! Cloud fraction:
!    Use the SPixel cloud fraction value
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! SAD_Chan struct  In          Structure summarising channel information
! SPixel   struct  Both        Super pixel structure
! index    int     In          The required state parameter index
! SetErr   logical In          State parameter error flag
! X        real    Out         The state parameter
! Err      real    Out         (A priori) error in state parameter
! status   int     Out         Indicates success/failure of subroutine.
!
! History:
! 2000/02/06, KS: Original version
! 2001/06/01, AS: 2nd original version! Changes due to updates in overall design
!    of first guess/a priori setting and coding changes elsewhere.
! 2001/06/07, AS: Checks MDAD_Lw/MDAD_Sw are available where they need to be
!    used.  The user's choice of channels as well as the illumination conditions
!    for the SPixel can determine whether or not a given variable can be set by
!    this method.
! 2001/07/05, AS: Bug fix in indexing of R_clear with MDAD_LW.
! 2001/06/16, AS: Added more error checking. Checks values of Rad_o used in 
!    Phase and Pc calculations don't go negative. Using named constants for
!    error values.
! 2001/10/26, AS: Fix to Tau calculation. Reflectances are now expressed as 
!    fractions rather than percentages. Calculation of iFGOP updated to match.
!    **************** ECV work starts here *************************************
! 2011/03/11, AS:  Re-applying changes made in late 2001/2.
!    CP: changed the fractional error flag
!    Plus correction to value in FGOP array: 1.15 instead of 1.5.
!    iPc loop to set BT_o was reverse, i.e. Np to 1 rather than 1 to Np.
!    iPhase: < replaced by <= in BT_o setting.
! 2011/03/21, AS: Removing functionality to change phase during the retrieval.
!    Commented out handling of iPhase. Phase will be fixed at the Ctrl value.
! 2011/03/30, AS: Removed commented-out code for handling phase, following
!    removal of  phase change functionality. Removal of super-pixel averaging. 
!    Removed use of Ctrl%Resoln%Space in cloud fraction setting.
! 2011/04/26, AS: Extension to handle multiple instrument views. The viewing
!    geometry becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!    Sec_o used in setting first guess optical path is now one value from
!    the array.
! 2012/05/01, CP: Modified first guess cloud top temperature to be an
!    interpolation between layers rather than the lower layer.
! 2012/06/15, CP: Modified illum to be an array.
! 2012/07/08, CP: Fixed invalid memory access error.
! 2012/08/17, MJ: Fixed bug with divide by zero.
! 2012/10/02, CP: Changed selection of first guess height.
! 2013/11/22, MJ: Rewrites selection of ctp FG/AP based on BT interpolation.
! 2014/08/01, GM: Cleaned up the code.
! 2014/12/08, CP: Made error output more specific
! 2015/01/12, AP: Replace use of ThermalFirst.
! 2015/01/22, AP: Bug fix in the last commit.
! 2015/02/06, AP: Switch to Int_CTP rather than interpolate2ctp.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine X_MDAD(Ctrl, SAD_Chan, SPixel, index, SetErr, X, Err, status)

   use Ctrl_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array
   use planck
   use SAD_Chan_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
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
   real    :: BT_o(1)
   real    :: Rad(1)
   real    :: Rad_o(1)
   real    :: dR_dT(1)
   real    :: dT_dR(1)
   integer :: MDAD_LW_to_ctrl_y, MDAD_LW_to_ctrl_ythermal

   status = 0

   ! Set up first guess optical depth vector

   data FGOP / 0.1, 0.3, 0.65, 0.8, 1.0, 1.15, 1.3, 1.5, 1.7, 2.0, 2.4 /

   ! Parameters supported are Tau, Pc and f.
   select case (index)

   case (iTau) ! Cloud optical depth, Tau

      if ((SPixel%Illum(1) == IDay) .and. &
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
#ifdef LEGACY_CTP_MODE
         ! Ctrl%Ind%MDAD_LW indexes the desired channel wrt Ctrl%Ind%ICh
         ! Find the corresponding index wrt Ctrl%Ind%YThermal
         MDAD_LW_to_ctrl_y = SPixel%spixel_y_to_ctrl_y_index(SPixel%Ind%MDAD_LW)
         MDAD_LW_to_ctrl_ythermal = find_in_array(Ctrl%Ind%YThermal, &
                                                  MDAD_LW_to_ctrl_y)

         ! Convert observed brightness temperature to radiance
         ! Uses channel nearest 11 microns, index Ctrl%Ind%MDAD_LW in SAD_Chan,
         ! but SPixel%Ind%MDAD_LW in the measurement array.
         call T2R(1, SAD_Chan(MDAD_LW_to_ctrl_y:MDAD_LW_to_ctrl_y), &
                  SPixel%Ym(SPixel%Ind%MDAD_LW:SPixel%Ind%MDAD_LW), &
                  Rad, dR_dT, status)

         ! Calculate overcast radiance from observed radiance and cloud
         ! fraction. Note MDAD_LW must be offset for use with R_Clear since
         ! R_Clear stores thermal channels only.
         Rad_o = (Rad(1) - SPixel%RTM%LW%R_clear(MDAD_LW_to_ctrl_ythermal) * &
                 (1.0 - SPixel%Cloud%Fraction)) / SPixel%Cloud%Fraction

         ! Exclude negative Rad_o (can arise due to approximation in the RTM)
         if (Rad_o(1) >= 0.0) then
            ! Convert overcast radiance back to brightness temperature
            call R2T(1, SAD_Chan(MDAD_LW_to_ctrl_y:MDAD_LW_to_ctrl_y), Rad_o, &
                     BT_o, dT_dR, status)

            ! Interpolate for the BT to the rad. profile to get Pc FG/AP
            call interpolate2ctp(SPixel,Ctrl,BT_o,X,Err)
         else
            ! If no interpolation possible set BP_o and DBP_o to hardcoded
            ! values to recover:
            X = Ctrl%X0(iPc)

            !FG does not need Error but AP does
            Err = MDADErrPc
            write(*,*)'ERROR: X_MDAD(): FG does not need Error but AP does'
            status = XMDADMeth
         end if
#else
         if (SPixel%Ym(SPixel%Ind%MDAD_LW) /= MissingXn) then
            ! Interpolate for the BT to the rad. profile to get Pc FG/AP
            call Int_CTP(SPixel, Ctrl, SPixel%Ym(SPixel%Ind%MDAD_LW), X, status)
            if (SetErr) Err = MDADErrPc
         else ! Invalid data available
            status = XMDADMeth
            write(*,*) 'WARNING: X_MDAD(): Invalid thermal data'
         end if
#endif
      else ! Can't calculate Pc if required LW channels not selected
         status = XMDADMeth
!         write(*,*) 'WARNING: X_MDAD(): Cant calculate Pc if required LW channels not selected'
      end if

   case (iFr) ! Cloud fraction, f
      ! Value is taken straight from the SPixel structure.
      X = SPixel%Cloud%Fraction
      if (SetErr) Err = MDADErrF1
   end select

end subroutine X_MDAD
