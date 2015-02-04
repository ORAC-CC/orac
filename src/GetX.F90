!-------------------------------------------------------------------------------
! Name:
!    Get_X
!
! Purpose:
!    Gets the first guess and a priori state vector values (X0 and Xb) for the
!    current super pixel according to the methods specified by SPixel%FG and
!    SPixel%AP.
!
! Arguments:
!    Name           Type    In/Out/Both Description
!    Ctrl           struct  In   Control structure
!    SAD_CloudClass struct  In   SAD cloud class structure
!    SPixel         struct  Both Super-pixel structure (contains the phase and
!                                AP, FG arrays to be set).
!    status         integer Out  Error status
!
! Algorithm:
!    Set "first guess" phase to the value for the retrieval (phase is no longer
!       retrieved).
!    Set cloud class depending on the FG phase
!    For each state variable
!    - Set a priori state vector and error covariance by the selected method
!    - Set first guess state vector by the selected method
!      (if the FG method matches the AP, just copy the AP value to the FG)
!    - Check both state vectors for internal consistency (i.e. check vs. the
!      limits for the cloud class)
!    (Note that not all methods are supported for all variables).
!
!    Note that when error values are associated with the a priori the values
!    must be squared for use in the ECP. It is assumed that the values obtained
!    by each method are the rms errors.
!
! Local variables:
!    Name Type Description
!
! History:
!    30th Jan 2001, Kevin M. Smith: Original version
!    24th Apr 2001, Andy Smith:
!       Using named constants for allowed selection method values.
!       Moved test for valid selection method to Read_Driver. No need to test
!       for every super-pixel.
!       Using SPixel%FGPhase instead of SPixel%X0(6) to store phase.
!       Added setting of SPixel%FGCloudClass.
!     8th Jun 2001, Andy Smith:
!       Changed structure of first guess phase setting to take account of cases
!       when XMDAD setting is requested but unsuccessful.
!       Added scaling of Sx.
!    11th Jun 2001, Andy Smith:
!       XScale must be squared where used with Sx since Sx contains the square
!       of the errors in X.
!     6th Jul 2001, Andy Smith:
!       Update to comments.
!       Added status check on SDAD a priori setting.
!    17th Jul 2001, Andy Smith:
!       Using named constants for Ts errors when set by AUX method. Errors now
!       squared.
!    **************** ECV work starts here *************************************
!    21st Mar 2011, Andy Smith:
!       Removal of functionality to change phase during retrieval.
!       Phase will be initialised to the Ctrl value in the main function.
!       Since phase is now fixed for the retrieval only 1 cloud class is used,
!       hence the first guess cloud class is the 1 available class.
!    22nd Mar 2011, Andy Smith:
!       Removal of phase change, phase 2. Only 1 cloud class rather than an
!       array of N cloud classes. Ctrl struct reorganised so that X0, Xb, limits
!       etc only refer to one fixed phase (N.B. state variable not updated, so
!       phase is still "retrieved").
!     5th Apr 2011, Andy Smith:
!       Removed selection methods SAD and SDAD. Renamed SelmMDAD to SelmMeas.
!       Mopping up from removal of phase change. Removed setting of
!       SPixel%FGPhase as it is redundant.
!     5th Aug 2011, Caroline Polsen:
!       Remove rf to sadcloudclass to simplify code. These values can be read
!       from the driver file instead.
!    20th Jun 2012, Caroline Poulsen:
!       Added in sacura option
!     2nd Oct 2012, Caroline Poulsen:
!       Set in active statearibaes error to a very low number i.e assume they
!       are well known and let information csignal go to active state variables
!     3rd Oct 2012, Caroline Poulsen:
!       Modified how Ctrl%sx is set added skint to be surface temperature first
!       guess
!    26th Feb 2013, Caroline Poulsen:
!       Added in option to process pixel if only one IR channel is present
!    15th Jan 2014, Greg McGarragh:
!       Fixed setting of SPixel%Sx for inactive state variables so that Ctrl%Sx
!       is not modified.
!     1st Aug 2014, Greg McGarragh:
!       Cleaned up the code.
!    27th Jan 2015, Adam Povey:
!       Switched first guess auxilliary surface temperature from lowest T
!       level to skint to be consistent with a priori. As rttov_driver.F90 sets
!       these to be the same, it makes little difference other than tidiness.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Get_X(Ctrl, SAD_Chan, SPixel, status)

   use CTRL_def
   use ECP_Constants
   use SAD_Chan_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   ! Local variables

   logical :: SetErr ! Tells X_MDAD/X_SDAD that an error value should be set
   integer :: i      ! Loop counter
   real    :: X      ! State variable value returned by X_MDAD
   real    :: Err    ! Error value returned by X_MDAD

   status = 0

   SetErr = .false.

   ! Set a priori and associated error covariance, then set first guess.
   ! Having set both, check for internal consistency vs the cloud class limits.
   ! Loop over all variables, checking the method for each. One loop is used for
   ! both AP and FG setting. This is intended to minimise the number of
   ! operations in incrementing the counter, but the setting of SetErr may
   ! offset this saving.

   ! Set a priori
   SPixel%Sx = 0
   do i = 1, MaxStateVar
      if (status /= 0) exit

      SetErr = .true. ! Error values are required for a priori

      select case (SPixel%AP(i))

      case (SelmMeas) ! Measurement dependent. Not supported for all variables.
         call X_MDAD(Ctrl, SAD_Chan, SPixel, i, SetErr, X, Err, status)
         if (status /= XMDADMeth) then
            SPixel%Xb(i)   = X
            SPixel%Sx(i,i) = (Err * Ctrl%Invpar%XScale(i)) ** 2
         end if

      case (SelmAUX) ! AUX method not supported for most vars.
         if (i == ITs) then
            ! Error setting could be much more sophisticated. The current scheme
            ! takes no account of relative proportions of land/sea in the
            ! current SPixel.
            SPixel%Xb(i)   = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
            if (SPixel%Surface%Sea == 1)  &
               SPixel%Sx(i,i) = (AUXErrTsSea * Ctrl%Invpar%XScale(i)) ** 2
            if (SPixel%Surface%Land == 1) &
               SPixel%Sx(i,i) = (AUXErrTsLand * Ctrl%Invpar%XScale(i)) ** 2
         end if
      end select

      ! Ctrl method, used if method is Ctrl or other methods failed.
      ! Ctrl%Sx is squared after reading in.
      if (SPixel%AP(i) == SelmCtrl .or. &
          (SPixel%AP(i) == SelmMeas .and. status == XMDADMeth)) then
         SPixel%Xb(i) = Ctrl%Xb(i)

         if (any(SPixel%X .eq. i)) then
            SPixel%Sx(i,i) = (Ctrl%Sx(i) * Ctrl%Invpar%XScale(i)) ** 2
         else
            ! Assume that the inactive state variables are well known do not try
            ! to retrieve
            SPixel%Sx(i,i) = (1.0e-5     * Ctrl%Invpar%XScale(i)) ** 2
         end if

         status = 0
      end if

      ! Having set a priori for the variable, set first guess. If the FG method
      ! is the same as the AP, just copy the AP value. The first "if" is
      ! slightly dangerous: the assumption is that the first guess method is
      ! legal and supported. This holds provided that the same methods are
      ! supported for FG and AP.

      ! Set first guess
      if (SPixel%FG(i) == SPixel%AP(i)) then

         SPixel%X0(i) = SPixel%Xb(i)
      else
         if (status /= 0) exit

         SetErr = .false. ! Error values are not required for first guess

         select case (SPixel%FG(i))

         case (SelmMeas)  ! MDAD method. Not supported for all variables.
            call X_MDAD(Ctrl, SAD_Chan, SPixel, i, SetErr, X, Err, status)
            if (status .ne. 0) then
!              write(*,*) 'WARNING: X_MDAD(): Failed with status:',status
               exit
            end if
            SPixel%X0(i) = X

         case (SelmAUX)   ! AUX method not supported for most vars.
            if (i == ITs) &
               SPixel%X0(i) = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
         end select

         ! Ctrl method, used if method is Ctrl or other methods failed.
         if (SPixel%FG(i) == SelmCtrl .or. &
             (SPixel%FG(i) == SelmMeas .and. status == XMDADMeth)) then
            SPixel%X0(i) = Ctrl%X0(i)
            status = 0
         end if

      end if

      ! Check for internal consistency with the cloud class limits. Set the a
      ! priori or first guess values equal to any limit they exceed.
      if (SPixel%Xb(i) > Ctrl%Invpar%xUlim(i)) then
         SPixel%Xb(i) = Ctrl%Invpar%xUlim(i)
      else if (SPixel%Xb(i) < Ctrl%Invpar%xLlim(i)) then
         SPixel%Xb(i) = Ctrl%Invpar%xLlim(i)
      end if

      if (SPixel%X0(i) > Ctrl%Invpar%xUlim(i)) then
         SPixel%X0(i) = Ctrl%Invpar%xUlim(i)
      else if (SPixel%X0(i) < Ctrl%Invpar%xLlim(i)) then
         SPixel%X0(i) = Ctrl%Invpar%xLlim(i)
      end if

   end do

end subroutine Get_X
