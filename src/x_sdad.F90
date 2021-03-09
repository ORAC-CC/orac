!-------------------------------------------------------------------------------
! Name: x_sdad.F90
!
! Purpose:
! Sets up parts of the state vector, X, depending on the current state.
!
! Description and Algorithm details:
! Sets the state variable value for the variable specified by index to the
! value from the previous retrieval (held in Spixel%Xn)
! If required, sets the error covariance value SPixel%Sx to the final value
! from the previous retrieval (held in Spixel%Sn)
!
! Arguments:
! Name       Type          In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl       struct        In          Control structure
! SPixel     struct        Both        Super pixel structure
! index      int           In          The required state parameter index
! SetErr     logical       In          State parameter error flag
! X          real          Out         The state parameter
! Err        Real          Out         (A priori) error in state parameter
! status     int           Out         Indicates success/failure of subroutine.
!
! History:
! 2000/02/09, KS: Original version
! 2001/06/04, AS: Updated to match changes in routines above.
! 2001/06/07, AS: Sets status 0 to avoid compilation warnings.
! 2001/07/06, AS: Added check on "distance" from last saved state.
! 2015/08/21, AP: Updated to be consistent with current form of code. Now allows
!    all variables.
! 2016/01/04, AP: Use MissingXn and MissingSn to flag invalid retrievals.
!
! Bugs:
! If desired value was not retrieved in the last "good" retrieval, this will
!    set a nonsensical value.
! Last retrieval will be along satellite track, though a nearer retrieval may
!    exist across track.
!-------------------------------------------------------------------------------

subroutine X_SDAD(Ctrl, SPixel, index, X, status, Err)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Declare arguments
   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: index
   real,           intent(out) :: X
   integer,        intent(out) :: status
   real, optional, intent(out) :: Err

   ! Declare local variables
   integer :: NPix


   status = 0

   ! Spixel%XnSav and SnSav contain the last "good" retrieval. Spixel%Loc%X0Last
   ! and Y0Last contain the location for the last good retrieval. Check we aren't
   ! too far away before using these values (e.g. if retrievals fail for a number
   ! of pixels the saved states may be "out of date").

   NPix = sqrt(real((SPixel%Loc%X0-SPixel%Loc%LastX0)) ** 2 + &
               real((SPixel%Loc%Y0-SPixel%Loc%LastY0)) ** 2)

   if (NPix > Ctrl%Max_SDAD .or. SPixel%XnSav(index) == MissingXn .or. &
        SPixel%SnSav(index, index) == MissingSn) then
      status = XSDADMeth
   else
      X = SPixel%XnSav(index)
      if (present(Err)) Err = SPixel%SnSav(index, index)
   end if

end subroutine X_SDAD
