!-------------------------------------------------------------------------------
! Name: XSDAD.F90
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine X_SDAD(Ctrl, SPixel, index, SetErr, X, Err, status)

   use ECP_Constants
   use Ctrl_def

   implicit none

!  Declare arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   integer,        intent(in)    :: index
   logical,        intent(in)    :: SetErr
   real,           intent(out)   :: X
   real,           intent(out)   :: Err
   integer,        intent(out)   :: status

!  Declare local variables

   integer :: NPix

   status = 0

!  Parameters supported are Tau, Re, Pc, f and Ts.

!  Spixel%XnSav and SnSav contain the last "good" retrieval. Spixel%Loc%X0Last
!  and Y0Last contain the location for the last good retrieval. Check we aren't
!  too far away before using these values (e.g. if retrievals fail for a number
!  of pixels the saved states may be "out of date").

   NPix = sqrt(float((SPixel%Loc%X0-SPixel%Loc%LastX0) ** 2) + &
               float((SPixel%Loc%Y0-SPixel%Loc%LastY0) ** 2))

   if (NPix > Ctrl%Max_SDAD) then
      status = XSDADMeth
   else
      status = 0
      select case (index)
	 case (iTau)        !     Cloud optical depth, Tau.
	    X = SPixel%XnSav(iTau)
	    if (SetErr) Err = SPixel%SnSav(iTau, iTau)

	 case (iRe)         !     Effective radius, Re.
	    X = SPixel%XnSav(iRe)
	    if (SetErr) Err = SPixel%SnSav(iRe, iRe)

	 case (iPc)         !     Cloud pressure, Pc.
	    X = SPixel%XnSav(iPc)
	    if (SetErr) Err = SPixel%SnSav(iPc, iPc)

	 case (iFr)         !     Cloud fraction, f.
	    X = SPixel%XnSav(iFr)
	    if (SetErr) Err = SPixel%SnSav(iFr, iFr)

	 case (iTs)         !     Surface temperature, Ts.
	    X = SPixel%XnSav(iTs)
	    if (SetErr) Err = SPixel%SnSav(iTs, iTs)

      end select
   end if

end subroutine X_SDAD
