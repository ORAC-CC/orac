! Name:
!    X_SDAD
!
! Description: 
!    Sets up parts of the state vector, X, depending on the current state.
!
! Arguments:
!    Name       Type          In/Out    Description
!    SPixel     alloc struct  In        Super pixel structure
!    index      int           In        The required state parameter index
!    SetErr     logical       In        State parameter error flag 
!    X          real          Out       The state parameter
!    Err        Real          Out       (A priori) error in state parameter
!    status     int           out       Indicates success/failure of subroutine.
!
! Algorithm:
!    Sets the state variable value for the variable specified by index to the
!    value from the previous retrieval (held in Spixel%Xn)
!    If required, sets the error covariance value SPixel%Sx to the final value
!    from the previous retrieval (held in Spixel%Sn)
!
! Local variables:
!    Name       Type    Description
!    message    string  Error message string
!    NPix       real    Number of pixels between current location and last
!                       "good" inversion.
!
! History:
!    9 Feb 2000, Kevin Smith : Original version
!    4th Jun 2001, Andy Smith:
!       Updated to match changes in routines above.
!    7th Jun 2001, Andy Smith:
!       Sets status 0 to avoid compilation warnings.
!    6th July 2001, Andy Smith:
!       Added check on "distance" from last saved state.
!
! Bugs:
!    None known.
!
!---------------------------------------------------------------------
subroutine X_SDAD(Ctrl, SPixel, index, SetErr, X, Err, status)

   use ECP_Constants
   use Ctrl_def
   use SPixel_def

   implicit none
  
!  Declare arguments

   type(Ctrl_t), intent(in)      :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel   
   integer, intent(in)           :: index
   logical, intent(in)           :: SetErr
   real, intent(out)             :: X
   real, intent(out)             :: Err
   integer, intent(out)          :: status

!  Declare local variables

   integer         :: NPix
   character(180)  :: message

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
