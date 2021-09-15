!-------------------------------------------------------------------------------
! Name: x_mdad.F90
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
!    IPc loop to set BT_o was reverse, i.e. Np to 1 rather than 1 to Np.
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
! 2015/07/27, AP: Removed consideration of cloud fraction. Now always returns
!     values for an overcast pixel (consistent with the preprocessor).
! 2016/01/08, AP: Removed old CTP interpolation entirely.
! 2017/07/05, AP: MDAD_SW, LW replaced with Y_Id_legacy.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine X_MDAD(Ctrl, SPixel, index, X, status, Err)

   use Ctrl_m
   use Int_Routines_m, only: find_in_array
   use ORAC_Constants_m
   use planck_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   integer,        intent(in)    :: index
   real,           intent(out)   :: X
   integer,        intent(out)   :: status
   real, optional, intent(out)   :: Err

   ! Declare local variables
   real, parameter :: FGOP(11) = [0.1, 0.3, 0.65, 0.8, 1.0, 1.15, 1.3, 1.5, &
                                  1.7, 2.0, 2.4] ! First guess optical depth
   real            :: Ref_o
   integer         :: iFGOP
   integer         :: Y_Id(SPixel%Ind%Ny)
   integer         :: i_spixel_06, i_spixel_11, i_spixel_06_solar

   status = 0

   ! Find 11 and 12 micron indices
   Y_Id = Ctrl%Ind%Y_Id(SPixel%spixel_y_to_ctrl_y_index(1:SPixel%Ind%Ny))
   i_spixel_06 = find_in_array(Y_Id, Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x))
   i_spixel_11 = find_in_array(Y_Id, Ctrl%Ind%Y_Id_legacy(I_legacy_11_x))

   i_spixel_06_solar = find_in_array(SPixel%Ind%YSolar(1:SPixel%Ind%NSolar), &
                                     i_spixel_06)

   ! Parameters supported are Tau, Pc and f.
   select case (index)

   case (ITau, ITau2) ! Cloud optical depth, Tau

      if (SPixel%Illum == IDay .and. i_spixel_06_solar > 0) then
         ! Calculate overcast reflectance (assuming fully cloudy pixel).
         ! Uses channel nearest 0.67 microns, index Ctrl%Ind%MDAD_SW.
         Ref_o = SPixel%Ym(i_spixel_06) - SPixel%Surface%Rs(i_spixel_06_solar)

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
         if (present(Err)) Err = MDADErrTau
      else ! Can't calculate Tau unless it's daylight
         write(*,*)'ERROR: X_MDAD(): Cant calculate Tau unless its daylight'
         status = XMDADMeth
      end if

   case (IPc, IPc2) ! Cloud pressure, Pc

      if (i_spixel_11 > 0) then
         if (SPixel%Ym(i_spixel_11) /= MissingXn) then
            ! Interpolate for the BT to the rad. profile to get Pc FG/AP
            call Int_CTP(SPixel, Ctrl, SPixel%Ym(i_spixel_11), X, status)
            if (present(Err)) Err = MDADErrPc
         else
            ! Invalid data available
            status = XMDADMeth
            write(*,*) 'WARNING: X_MDAD(): Invalid thermal data'
         end if
      else ! Can't calculate Pc if required LW channels not selected
         status = XMDADMeth
!         write(*,*) 'WARNING: X_MDAD(): Cant calculate Pc if required LW ' // &
!                    'channels not selected'
      end if

   case (IFr) ! Cloud fraction, f
      ! Always overcast
      X = 1
      if (present(Err)) Err = MDADErrF
   end select

end subroutine X_MDAD
