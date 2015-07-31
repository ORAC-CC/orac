!-------------------------------------------------------------------------------
! Name: GetX.F90
!
! Purpose:
! Gets the first guess and a priori state vector values (X0 and Xb) for the
! current super pixel according to the methods specified by SPixel%FG and
! SPixel%AP.
!
! Description and Algorithm details:
! Set "first guess" phase to the value for the retrieval (phase is no longer
!    retrieved).
! Set cloud class depending on the FG phase
! For each state variable
! - Set a priori state vector and error covariance by the selected method
! - Set first guess state vector by the selected method
!   (if the FG method matches the AP, just copy the AP value to the FG)
! - Check both state vectors for internal consistency (i.e. check vs. the
!   limits for the cloud class)
! (Note that not all methods are supported for all variables).
!
! Note that when error values are associated with the a priori the values
! must be squared for use in the ECP. It is assumed that the values obtained
! by each method are the rms errors.
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl           struct  In   Control structure
! SAD_Chan       struct  In   SAD channel structure
! SPixel         struct  Both Super-pixel structure (contains the phase and
!                             AP, FG arrays to be set).
! status         integer Out  Error status
!
! History:
! 2001/01/30, KS: Original version
! 2001/04/24, AS: Using named constants for allowed selection method values.
!    Moved test for valid selection method to Read_Driver. No need to test for
!    every super-pixel. Using SPixel%FGPhase instead of SPixel%X0(6) to store
!    phase. Added setting of SPixel%FGCloudClass.
! 2001/06/08, AS: Changed structure of first guess phase setting to take account
!    of cases when XMDAD setting is requested but unsuccessful.
!    Added scaling of Sx.
! 2001/06/11, AS: XScale must be squared where used with Sx since Sx contains
!    the square of the errors in X.
! 2001/07/06, AS: Update to comments. Added status check on SDAD a priori setting
! 2001/07/17, AS:  Using named constants for Ts errors when set by AUX method.
!    Errors now squared.
!    **************** ECV work starts here *************************************
! 2011/03/21, AS:  Removal of functionality to change phase during retrieval.
!    Phase will be initialised to the Ctrl value in the main function. Since
!    phase is now fixed for the retrieval only 1 cloud class is used, hence the
!    first guess cloud class is the 1 available class.
! 2011/03/22, AS:  Removal of phase change, phase 2. Only 1 cloud class rather
!    than an array of N cloud classes. Ctrl struct reorganised so that X0, Xb,
!    limits etc only refer to one fixed phase (N.B. state variable not updated,
!    so phase is still "retrieved").
! 2011/04/05, AS: Removed selection methods SAD and SDAD. Renamed SelmMDAD to
!    SelmMeas. Mopping up from removal of phase change. Removed setting of
!    SPixel%FGPhase as it is redundant.
! 2011/08/05, CP: Remove rf to sadcloudclass to simplify code. These values can
!    be read  from the driver file instead.
! 2012/06/20, CP: Added in sacura option
! 2012/10/02, CP: Set in active statearibaes error to a very low number i.e
!    assume they are well known and let information csignal go to active state
!    variables
! 2012/10/03, CP: Modified how Ctrl%sx is set added skint to be surface
!    temperature first guess
! 2013/02/26, CP: Added in option to process pixel if only one IR channel is
!    present
! 2014/01/15, GM: Fixed setting of SPixel%Sx for inactive state variables so
!    that Ctrl%Sx is not modified.
! 2014/08/01, GM: Cleaned up the code.
! 2015/01/27, AP: Switched first guess auxilliary surface temperature from
!    lowest T level to skint to be consistent with a priori. As rttov_driver.F90
!    sets these to be the same, it makes little difference other than tidiness.
! 2015/06/02, AP: Moved repeated code into subroutine.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_X(Ctrl, SAD_Chan, SPixel, status)

   use CTRL_def
   use ECP_Constants
   use SAD_Chan_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   ! Local variables

   integer :: i       ! Loop counters
   real    :: X       ! State variable value returned by X_MDAD
   real    :: Err     ! Error value returned by X_MDAD

   status = 0

   ! Set a priori and associated error covariance, then set first guess.
   ! Having set both, check for internal consistency vs the cloud class limits.
   ! Loop over all variables, checking the method for each. One loop is used for
   ! both AP and FG setting. This is intended to minimise the number of
   ! operations in incrementing the counter.

   ! Set a priori
   SPixel%Sx = 0.
   do i = 1, MaxStateVar
      call Get_State(SPixel%AP(i), i, Ctrl, SPixel, SAD_Chan, SPixel%Xb(i), status, SPixel%Sx(i,i))

      ! Having set a priori for the variable, set first guess. If the FG method
      ! is the same as the AP, just copy the AP value. The first "if" is
      ! slightly dangerous: the assumption is that the first guess method is
      ! legal and supported. This holds provided that the same methods are
      ! supported for FG and AP.

      ! Set first guess
      if (SPixel%FG(i) == SPixel%AP(i)) then
         SPixel%X0(i) = SPixel%Xb(i)
      else
         call Get_State(SPixel%FG(i), i, Ctrl, SPixel, SAD_Chan, SPixel%X0(i), &
                        status)
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


subroutine Get_State(mode, i, Ctrl, SPixel, SAD_Chan, X, status, Err)

   use CTRL_def
   use ECP_Constants
   use SAD_Chan_def

   implicit none

   ! Declare arguments
   integer,          intent(in)    :: mode
   integer,          intent(in)    :: i
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   real,             intent(out)   :: X
   integer,          intent(out)   :: status
   real,   optional, intent(out)   :: Err

   ! Local variables
   real :: Scale2

   Scale2 = Ctrl%Invpar%XScale(i) * Ctrl%Invpar%XScale(i)

   select case (mode)
   case (SelmMeas) ! Draw state from measurement vector
      call X_MDAD(Ctrl, SAD_Chan, SPixel, i, X, status, Err)
      if (status == XMDADBounds) then ! ACP: Check cause of this
!        write(*,*) 'WARNING: X_MDAD(): Out-of-bounds interpolation'
         status = 0
      else if (status /= 0) then
!        write(*,*) 'ERROR: X_MDAD(): Failed with status:',status
      end if
      if (status == 0 .and. present(Err)) Err = Err * Err * Scale2

   ! ACP: case (SelmSAD) is available in the aerosol code and used for
   ! tau and ref. This will be done via Ctrl%XB/X0 instead.

   case (SelmAUX) ! Draw from auxilliary input information
      if (i == ITs) then ! Surface temperature
         ! Error setting could be much more sophisticated. The current scheme
         ! takes no account of relative proportions of land/sea in the
         ! current SPixel.
         X = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
         if (present(Err)) then
            if (SPixel%Surface%Land) then
               Err = AUXErrTsLand * AUXErrTsLand * Scale2
            else
               Err = AUXErrTsSea * AUXErrTsSea * Scale2
            end if
         end if
      end if
   end select

   ! Draw from Ctrl structure (e.g. driver file). Used if method other
   ! methods failed. Ctrl%Sx is squared after reading in.
   if (mode == SelmCtrl .or. status /= 0) then
      ! Use a priori value from Ctrl structure
      X = Ctrl%Xb(i)

      if (present(Err)) then
         if (any(SPixel%X == i)) then
            Err = Ctrl%Sx(i) * Ctrl%Sx(i) * Scale2
         else
            ! Assume that the inactive state variables are well known do not try
            ! to retrieve
            Err = 1.0e-10 * Scale2 ! 1e-5 ** 2
         end if
      end if

      status = 0
   end if

end subroutine Get_State
